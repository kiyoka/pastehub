require 'net/http'
require 'uri'
require 'open-uri'
require 'fileutils'

module PasteHub

  class Client
    def initialize( auth )
      @auth = auth
      ins = PasteHub::Config.instance
      @localdb_path         = ins.localDbPath
      @server_api_host      = ins.targetApiHost
      @server_notifier_host = ins.targetNotifierHost
      @list_items           = ins.listItems
      @server_host          = ins.targetApiHost
      @localdb_path         = ins.localDbPath
    end
    
    def getValue( key )
      uri = URI.parse("http://#{@server_api_host}/getValue")
      ret = ""
      Net::HTTP.start(uri.host, uri.port) do |http|
        http.post(uri.request_uri, key, @auth.getAuthHash().merge( {"content-type" => "plain/text"} )) { |str|
          ret = str
        }
      end
      ret
    end

    def putValue( key, value )
      uri = URI.parse("http://#{@server_api_host}/putValue")
      ret = ""
      Net::HTTP.start(uri.host, uri.port) do |http|
        http.post(uri.request_uri, value,
                  @auth.getAuthHash().merge(
                                           { "content-type" => "plain/text",
                                             "x-pastehub-key" => key    }
                                           )) { |str|
          ret = str
        }
      end
      ret
    end

    def postData( data )
      begin
        uri = URI.parse("http://#{@server_host}/insertValue")
        Net::HTTP.start(uri.host, uri.port) do |http|
          http.post(uri.request_uri, data, @auth.getAuthHash().merge( {"content-type" => "plain/text"} ) )
        end
      rescue Errno::ECONNREFUSED => e
        STDERR.puts "Error: can't connect server."
      end
    end

    def serverHasNew?( serverValue )
      # open local db
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username )
      localValue = localdb.getValue( PasteHub::SERVER_DATE_KEY )
      ret = (localValue != serverValue)
      if ret
        localdb.insertValue( PasteHub::SERVER_DATE_KEY, serverValue )
      end
      localdb.close()
      return ret
    end

    def localHasNew?( )
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username )
      localValue  = localdb.getValue( PasteHub::LOCAL_DATE_KEY )
      serverValue = localdb.getValue( PasteHub::SERVER_DATE_KEY )
      ret = if localValue and serverValue
              (localValue > serverValue)
            else
              false
            end
      if ret
        localdb.insertValue( PasteHub::LOCAL_DATE_KEY, serverValue )
      end
      localdb.close()
      return ret
    end

    def setOnlineState( online )
      # open local db
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username )
      localdb.insertValue( PasteHub::ONLINE_STATE_KEY, online ? "1" : "0" )
      localdb.close()
    end

    def online?( )
      # open local db
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username, true )
      # printf( "[%s]", localdb.getValue( PasteHub::ONLINE_STATE_KEY ))
      ret = ("1" == localdb.getValue( PasteHub::ONLINE_STATE_KEY ))
      localdb.close()
      ret
    end

    def localSaveValue( data )
      # open local db
      util = PasteHub::Util.new( )
      key = util.currentTime( ) + "=" + util.digest( data )
      
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username )
      localdb.insertValue( key, data.dup )
      localdb.insertValue( PasteHub::LOCAL_DATE_KEY, util.currentTime( ) )
      localdb.close()
    end

  end
end
