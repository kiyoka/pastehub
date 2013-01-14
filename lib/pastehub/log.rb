#
# log.rb - PasteHub's log manager
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

module PasteHub
  class LogBase
    def initialize( hash )
      @user = nil
      if hash.is_a? Hash
        if hash.has_key?( :user )
          @user = hash[ :user ]
        else
          @user = 'UNSPECIFIED'
        end
        if hash.has_key?( :api )
          @api  = hash[ :api ]
        else
          raise ArgumentError if not @api
        end
      else
        raise ArgumentError
      end

      @uri = URI.parse( "http://localhost:9880/pastehub.webapi" )
      @http = Net::HTTP.new(@uri.host, @uri.port)
      #Fluent::Logger::FluentLogger.open(nil, :host=>'localhost', :port=>24224)
    end

    def write( hashData )
      begin
        jsonStr = hashData.to_json
        resp = @http.post( @uri.request_uri, "json=" + jsonStr )
        if "200" != resp.code
          STDERR.print "Error: can't post to fluentd(http)[1].\n"
        end
      rescue
        STDERR.print "Error: can't post to fluentd(http)[2].\n"
      end
      #Fluent::Logger.post("pastehub.webapi",  hashData)
      true
    end
  end

  class Log < LogBase
    # info without error
    def info( message, moreHash = {} )
      hashData = {
          "user"=>"#{@user}",
          "api"=>"#{@api}",
          "message"=>"#{message}",
          "error" => false}
      write( hashData.merge( moreHash ) )
    end

    # errors
    def error( message, moreHash = {} )
      hashData = {
          "user"=>"#{@user}",
          "api"=>"#{@api}",
          "message"=>"#{message}",
          "error" => true}
      write( hashData.merge( moreHash ) )
    end
  end
end
