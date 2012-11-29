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
      @dynamoAccessKey     = if hash[ :dynamoAccessKey ]
                               hash[ :dynamoAccessKey ]
                             else
                               "xxxx"
                             end
      @dynamoSecretKey     = if hash[ :dynamoSecretKey ]
                               hash[ :dynamoSecretKey ]
                             else
                               "xxxx"
                             end
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
      @keystore            = if hash[ :keystore ]
                               hash[ :keystore ]
                             else
                               nil
                             end
      @keystorePassword    = if hash[ :keystorePassword ]
                               hash[ :keystorePassword ]
                             else
                               "password"
                             end
    end

    def appendSlash( uri )
      if uri.match( /\/$/ )
        uri
      else
        uri + "/"
      end
    end

    def setupClient( hash )
      @targetApiURL        = if hash[ :targetApiURL ]
                               appendSlash( hash[ :targetApiURL ] )
                             else
                               "https://pastehub.net/api/"
                             end
      @targetNotifierURL   = if hash[ :targetNotifierURL ]
                               appendSlash( hash[ :targetNotifierURL ] )
                             else
                               "https://pastehub.net:8001/"
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
                              :domain             => json[ 'domain' ],
                              :keystore           => json[ 'keystore' ],
                              :keystorePassword   => json[ 'keystorePassword' ] } )
        }
      end
    end

    def loadClient()
      name = File.expand_path( "~/.pastehub.conf" )
      if File.exist?( name )
        open( name ) { |f|
          json = JSON.parse( f.read )
          self.setupClient( { :targetApiURL       => json[ 'targetApiURL' ],
                              :targetNotifierURL  => json[ 'targetNotifierURL' ],
                              :localDbPath        => json[ 'localDbPath' ] } )

        }
      end
    end

    attr_reader :aws, :dynamoEp, :dynamoAccessKey, :dynamoSecretKey, :memcacheEp, :keyCacheTime, :domain, :keystore, :keystorePassword
    attr_reader :targetApiURL, :targetNotifierURL, :localDbPath, :listItems
    attr_accessor :awsWarn
  end
end
