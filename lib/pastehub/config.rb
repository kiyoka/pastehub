require 'singleton'
require 'json'

module PasteHub
  class Config
    include Singleton

    def self.instance
      @@instance ||= new
    end

    def initialize( )
      self.setupServer()
      self.setupClient()
      @listItems = 100
    end

    def setupServer( aws = false, dynamoEp = nil, memcacheEp = nil, domain = nil )
      @aws                 = if aws
                               aws
                             else
                               false
                             end
      @dynamoEp            = if dynamoEp
                               dynamoEp
                             else
                               dynamoEp = 'dynamodb.ap-northeast-1.amazonaws.com' # Default DynamoDB's endpoint is Tokyo Region
                             end
      @memcacheEp          = if memcacheEp
                               memcacheEp
                             else
                               "localhost:11211"
                             end
      @domain              = if domain
                               domain
                             else
                               "localhost"
                             end
    end

    def setupClient( targetApiHost = nil, targetNotifierHost = nil, localDbPath = nil )
      @targetApiHost       = if targetApiHost
                               targetApiHost
                             else
                               "pastehub.org:8000"
                             end
      @targetNotifierHost  = if targetNotifierHost
                               targetNotifierHost
                             else
                               "pastehub.org:8001"
                             end
      @localDbPath         = if localDbPath
                               localDbPath                        
                             else
                               File.expand_path( "~/.pastehub/" ) + "/"
                             end
    end

    def loadServer()
      name = "/etc/pastehub.conf"
      if File.exist?( name )
        open( name ) { |f|
          json = JSON.parse( f.read )
          self.setupServer( json[ 'aws' ], json[ 'dynamoEp' ], json[ 'memcacheEp' ], json[ 'domain' ] )
        }
      end
    end

    def loadClient()
      name = File.expand_path( "~/.pastehub.conf" )
      if File.exist?( name )
        open( name ) { |f|
          json = JSON.parse( f.read )
          self.setupClient( json[ 'targetApiHost' ], json[ 'targetNotifierHost' ], json[ 'localDbPath' ] )
        }
      end
    end

    attr_reader :aws, :dynamoEp, :memcacheEp, :domain, :targetApiHost, :targetNotifierHost, :localDbPath, :listItems
  end
end
