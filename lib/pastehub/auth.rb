#
# auth.rb - PasteHub's authentication library file
#  
#   Copyright (c) 2009-2011  Kiyoka Nishiyama  <kiyoka@sumibi.org>
#   
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
#   
#   1. Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#  
#   2. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#  
#   3. Neither the name of the authors nor the names of its contributors
#      may be used to endorse or promote products derived from this
#      software without specific prior written permission.
#  
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  
#
require "openssl"
require "base64"
require 'pp'

module PasteHub
  MANDATORY_KEYS = [
                    'x-pastehub-username',
                    'x-pastehub-date',
                    'x-pastehub-version'
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
      @elements['x-pastehub-username']
    end

    def curTime()
      @elements['x-pastehub-date']
    end
  end

  class AuthForClient
    def initialize( username, secretKey )
      util = Util.new
      @token = Auth.new( )
      @token.addElement( 'x-pastehub-username', username )
      @token.addElement( 'x-pastehub-date',     util.key_seconds( util.currentTime( )).to_s )
      @token.addElement( 'x-pastehub-version',  '2012-06-16' )
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
    def initialize( users )
      # db connecter of Users table
      @users = users
    end

    def expired?( clientTime, serverTime )
      (60*5) < ( clientTime.to_i - serverTime.to_i ).abs
    end

    # return:
    #  [ true/false, "username", :reason ]
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
        [ false, 'UNSPECIFIED', :unspecified_user ]
      else
        if @users.getSecretKey(auth.username)
          server_sign = auth.calcSignature( @users.getSecretKey(auth.username) )
          #pp [ "server_sign", server_sign ]
          if server_sign == client_sign

            if expired?( auth.curTime, serverTime )
              # authentication success
              [ false, auth.username, :expired_client_request ]
            else
              [ true,  auth.username, :ok ]
            end
          else
            # fail...
            [ false, auth.username, :illegal_signature ]
          end
        else
          # unknown user
          [ false, auth.username, :unknown_user ]
        end
      end
    end
  end
end
