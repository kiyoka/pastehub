#
# config.rb - PasteHub's config file loader and config object.
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
