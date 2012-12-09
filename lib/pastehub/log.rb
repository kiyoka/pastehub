require 'fluent-logger'

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

      Fluent::Logger::FluentLogger.open(nil, :host=>'localhost', :port=>24224)
    end

    def write( hashData )
      Fluent::Logger.post("pastehub.webapi_prod",  hashData)
      Fluent::Logger.post("pastehub.webapi_rspec", hashData)
    end
  end

  class Log < LogBase
    # info without error
    def info( message, moreHash = {} )
      hashData = {
          "from"=>"#{@user}",
          "api"=>"#{@api}",
          "message"=>"#{message}",
          "error" => false}
      write( hashData.merge( moreHash ) )
    end

    # errors
    def error( message, moreHash = {} )
      hashData = {
          "from"=>"#{@user}",
          "api"=>"#{@api}",
          "message"=>"#{message}",
          "error" => true}
      write( hashData.merge( moreHash ) )
    end
  end
end
