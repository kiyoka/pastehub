require "openssl"
require "base64"
require 'pp'

module SynchroBase
  MANDATORY_KEYS = [
                    'x-synchrobase-username',
                    'x-synchrobase-date',
                    'x-synchrobase-version'
                   ]


  class Auth
    def initialize()
      @elements = Hash.new
    end

    def addElement( key, value )
      @elements[key.downcase] = value
    end

    def calcSignature( secretKey )
      a = Set.new( @elements.keys )
      b = Set.new( MANDATORY_KEYS )
      if a.superset?( b )
        message = MANDATORY_KEYS.sort.map {|k|
          k + ':' + @elements[k]
        }.join( ',' ).downcase

        h = OpenSSL::HMAC::digest(OpenSSL::Digest::SHA256.new, secretKey, message)
        Base64.encode64(h).chomp
      else
        # not enough keys
        :NotEnoughKeys
      end
    end

    def getAuthHash( )
      @elements
    end

    def username()
      @elements['x-synchrobase-username']
    end

    def curTime()
      @elements['x-synchrobase-date']
    end
  end

  class AuthForClient
    def initialize( username, secretKey )
      util = Util.new
      @token = Auth.new( )
      @token.addElement( 'x-synchrobase-username', username )
      @token.addElement( 'x-synchrobase-date',     util.key_seconds( util.currentTime( )).to_s )
      @token.addElement( 'x-synchrobase-version',  '2012-06-16' )
      @secretKey = secretKey
    end

    def getAuthHash( )
      sign = @token.calcSignature( @secretKey )
      #pp ["sign", sign ]
      if sign.is_a? String
        @token.addElement( 'Authorization',        sign )
        @token.getAuthHash()
      else
        raise RuntimeError, "Error: can't create AuthForClient instance."
      end
    end

    # for Testing with RSpec.
    def _addElement( key, value )
      @token.addElement( key, value )
    end

    def username()
      @token.username
    end
  end

  class AuthForServer
    def initialize( basepath = "/var/synchrobase/" )
      @basepath = basepath
      @secretKeys = Hash.new
      open( @basepath + "users.tsv" ) {|f|
        f.readlines.each { |line|
          arr = line.chomp.split( /\t+/ )
          @secretKeys[ arr[0] ] = arr[1]
        }
      }
    end

    def expired?( clientTime, serverTime )
      (60*5) < ( clientTime.to_i - serverTime.to_i ).abs
    end

    # return:
    #  [ true/false, :reason ]
    def invoke( hash, serverTime )
      auth = Auth.new
      client_sign = ""
      hash.each { |k,v|
        k = k.downcase
        #pp [ "invoke", k, v ]
        if MANDATORY_KEYS.include?( k )
          auth.addElement( k, v )
        end
        if 'authorization' == k
          client_sign = v
        end
      }
      
      #pp [ "auth.getAuthHash()", auth.getAuthHash() ]
      #pp [ "client_sign", client_sign ]
      #pp [ "auth.username", auth.username ]

      if not auth.username
        # fail... username was unspecified
        [ false, :unspecified_user ]
      else
        if @secretKeys[auth.username]
          server_sign = auth.calcSignature( @secretKeys[auth.username] )
          #pp [ "server_sign", server_sign ]
          if server_sign == client_sign

            if expired?( auth.curTime, serverTime )
              # authentication success
              [ false, :expired_client_request ]
            else
              [ true,  auth.username ]
            end
          else
            # fail...
            [ false, :illegal_signature ]
          end
        else
          # unknown user
          [ false, :unknown_user ]
        end
      end
    end
  end
end
