require 'singleton'
require 'json'

module PasteHub
  class Config
    include Singleton

    def self.instance
      @@instance ||= new
    end

    def initialize( )
      self.setupServer( {} )
      self.setupClient( {} )
      @listItems = 10
    end

    def setupServer( hash )
      @aws                 = if hash[ :aws ]
                               hash[ :aws ]
                             else
                               false
                             end
      @awsWarn             = if hash[ :awsWarn ]
                               hash[ :awsWarn ]
                             else
                               false
                             end
      @dynamoEp            = if hash[ :dynamoEp ]
                               hash[ :dynamoEp ]
                             else
                               dynamoEp = 'dynamodb.ap-northeast-1.amazonaws.com' # Default DynamoDB's endpoint is Tokyo Region
                             end
      @dynamoAccessKey     = hash[ :dynamoAccessKey ]
      @dynamoSecretKey     = hash[ :dynamoSecretKey ]
      @memcacheEp          = if hash[ :memcacheEp ]
                               hash[ :memcacheEp ]
                             else
                               "localhost:11211"
                             end
      @keyCacheTime        = if hash[ :keyCacheTime ]
                               hash[ :keyCacheTime ]
                             else
                               24 * 3600
                             end
      @domain              = if hash[ :domain ]
                               hash[ :domain ]
                             else
                               "localhost"
                             end
    end

    def setupClient( hash )
      @targetApiHost       = if hash[ :targetApiHost ]
                               hash[ :targetApiHost ]
                             else
                               "pastehub.org:8000"
                             end
      @targetNotifierHost  = if hash[ :targetNotifierHost ]
                               hash[ :targetNotifierHost ]
                             else
                               "pastehub.org:8001"
                             end
      @localDbPath         = if hash[ :localDbPath ]
                               hash[ :localDbPath ]
                             else
                               File.expand_path( "~/.pastehub/" ) + "/"
                             end
    end

    def loadServer()
      name = "/etc/pastehub.conf"
      if File.exist?( name )
        open( name ) { |f|
          json = JSON.parse( f.read )
          self.setupServer( { :aws                => json[ 'aws' ],
                              :awsWarn            => json[ 'awsWarn' ],
                              :dynamoEp           => json[ 'dynamoEp' ],
                              :dynamoAccessKey    => json[ 'dynamoAccessKey' ],
                              :dynamoSecretKey    => json[ 'dynamoSecretKey' ],
                              :memcacheEp         => json[ 'memcacheEp' ],
                              :keyCacheTime       => json[ 'keyCacheTime' ],
                              :domain             => json[ 'domain' ] } )
        }
      end
    end

    def loadClient()
      name = File.expand_path( "~/.pastehub.conf" )
      if File.exist?( name )
        open( name ) { |f|
          json = JSON.parse( f.read )
          self.setupClient( { :targetApiHost      => json[ 'targetApiHost' ],
                              :targetNotifierHost => json[ 'targetNotifierHost' ],
                              :localDbPath        => json[ 'localDbPath' ] } )
        }
      end
    end

    attr_reader :aws, :dynamoEp, :dynamoAccessKey, :dynamoSecretKey, :memcacheEp, :keyCacheTime, :domain, :targetApiHost, :targetNotifierHost, :localDbPath, :listItems
    attr_accessor :awsWarn
  end
end
