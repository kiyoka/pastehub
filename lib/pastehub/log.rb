require 'fluent-logger'

module PasteHub
  class LogBase
    def initialize( hash )
      @user = nil
      if hash.is_a? Hash
        if hash.has_key?( :user )
          @user = hash[ :user ]
        end
        if hash.has_key?( :api )
          @api  = hash[ :api ]
        end
      else
        raise ArgumentError
      end

      raise ArgumentError if not @user
      raise ArgumentError if not @api

      Fluent::Logger::FluentLogger.open(nil, :host=>'localhost', :port=>24224)
    end

    def write( hashData )
      Fluent::Logger.post("pastehub.webapi_prod",  hashData)
      Fluent::Logger.post("pastehub.webapi_rspec", hashData)
    end
  end

  class Log < LogBase
    # info without error
    def info( message )
      hashData = {
          "from"=>"#{@user}",
          "api"=>"#{@api}",
          "message"=>"#{message}",
          "error" => false}
      write( hashData )
    end

    # errors
    def error( message )
      hashData = {
          "from"=>"#{@user}",
          "api"=>"#{@api}",
          "message"=>"#{message}",
          "error" => true}
      write( hashData )
    end
  end
end