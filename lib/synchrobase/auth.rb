require "openssl"
require "base64"

module SynchroBase
  class Auth
    def initialize( secretKey )
      @secretKey = secretKey
      @elements = Hash.new
    end

    def currentTime( curTime )
      @curTime = curTime
    end

    def addElement( key, value )
      @elements[key] = value
    end

    def calcSignature( )
      mandatoryKeys = [
                       'x-synchrobase-username',
                       'x-synchrobase-date',
                       'x-synchrobase-version'
                      ]
      if @elements.keys.sort == mandatoryKeys.sort
        message = @elements.keys.map {|k|
          k + ':' + @elements[k]
        }.join( ',' )

        seconds = @elements['x-synchrobase-date'].to_i
        if (60*5) < ( seconds - @curTime ).abs
          return :Expired
        end

        h = OpenSSL::HMAC::digest(OpenSSL::Digest::SHA256.new, @secretKey, message)
        Base64.encode64(h).chomp
      else
        # not enough keys
        :NotEnoughKeys
      end
    end
  end
end
