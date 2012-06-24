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
    end

    def setupServer( dbPath = nil, memcacheHost = nil )
      @dbPath              = if dbPath
                               dbPath
                             else
                               "/var/pastehub/"
                             end
      @memcacheHost        = if memcacheHost
                               memcacheHost
                             else
                               "localhost:11211"
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
          self.setupServer( json[ 'dbPath' ], json[ 'memcacheHost' ] )
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

    def dbPath
      @dbPath
    end

    def memcacheHost
      @memcacheHost
    end

    def targetApiHost
      @targetApiHost
    end

    def targetNotifierHost
      @targetNotifierHost
    end

    def localDbPath
      @localDbPath
    end

    def listItems
      100
    end
  end
end
