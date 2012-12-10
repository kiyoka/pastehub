require 'pp'

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
