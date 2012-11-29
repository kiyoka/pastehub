require 'openssl'
require 'base64'

module PasteHub
  class Crypt
    def initialize( password )
      @password = password
      @des3 = OpenSSL::Cipher.new('des3')
    end

    def en( str )
      if 0 == str.size
        ""
      else
        @des3.encrypt
        @des3.pkcs5_keyivgen( @password )
        Base64.encode64( @des3.update( str ) + @des3.final )
      end
    end

    def de( str )
      if 0 == str.size
        ""
      else
        begin
          @des3.decrypt
          @des3.pkcs5_keyivgen( @password )
          ret =  @des3.update( Base64.decode64( str ))
          ret += @des3.final
        rescue OpenSSL::Cipher::CipherError => e
          ret = nil
        end
        ret
      end
    end
  end
end
