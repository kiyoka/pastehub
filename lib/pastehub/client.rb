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

    def getList( )
      uri = URI.parse("http://#{@server_api_host}/getList")
      masterList = []
      Net::HTTP.start(uri.host, uri.port) do |http|
        resp = http.get(uri.request_uri, @auth.getAuthHash().merge( {"content-type" => "plain/text"} ))
        str = resp.read_body()
        masterList = str.split( /\n/ )
        STDERR.puts "Info: masterList lines = #{masterList.size}  #{str.size}Bytes"
        masterList = masterList.select { |x|
          okSize = "1340542369=2012-06-24.12:52:49=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".size
          if okSize != x.size
            STDERR.puts "Info: masterList(NG): " + x
            false
          else
            x
          end
        }
      end
      masterList
    end

    def getValue( key )
      uri = URI.parse("http://#{@server_api_host}/getValue")
      ret = ""
      Net::HTTP.start(uri.host, uri.port) do |http|
        resp = http.post(uri.request_uri, key, @auth.getAuthHash().merge( {"content-type" => "plain/text"} ))
        ret = resp.read_body()
      end
      ret
    end

    def putValue( key, value )
      uri = URI.parse("http://#{@server_api_host}/putValue")
      ret = ""
      Net::HTTP.start(uri.host, uri.port) do |http|
        resp = http.post(uri.request_uri, value,
                         @auth.getAuthHash().merge(
                                                   { "content-type" => "plain/text",
                                                     "x-pastehub-key" => key    }
                                                   ))
        ret = resp.read_body()
      end
      ret
    end

    def postData( data )
      resp = nil
      begin
        uri = URI.parse("http://#{@server_host}/insertValue")
        Net::HTTP.start(uri.host, uri.port) do |http|
          resp = http.post(uri.request_uri, data, @auth.getAuthHash().merge( {"content-type" => "plain/text"} ) )
        end

      rescue Errno::ECONNREFUSED => e
        STDERR.puts "Error: can't connect server."
        return nil
      end
      if resp
        resp.read_body()
      else
        nil
      end
    end

    def wait_notify( auth )
      begin
        uri = URI.parse("http://#{@server_notifier_host}/")
        Net::HTTP.start(uri.host, uri.port) do |http|
          request = Net::HTTP::Get.new(uri.request_uri, auth.getAuthHash())
          http.request(request) do |response|
            raise 'Response is not chuncked' unless response.chunked?
            response.read_body do |chunk|
              serverValue = chunk.chomp
              if serverHasNew?( serverValue )
                puts "Info: server has new data: #{serverValue}"
                return chunk.chomp
              else
                puts "Info: server is stable:    #{serverValue}"
              end
              if localHasNew?( )
                puts "Info: local  has new data"
                return :local
              end
            end
            if "200" != response.code
              STDERR.puts "Error: request error result=[#{response.code}]"
              return :retry
            end
          end
        end
      rescue EOFError => e
        STDERR.puts "Error: disconnected by server."
        return :retry
      rescue Errno::ECONNREFUSED => e
        STDERR.puts "Error: can't connect server(ConnectionRefused)."
        return :retry
      rescue SocketError => e
        STDERR.puts "Error: can't connect server(SocketError)."
        return :retry
      rescue Timeout::Error => e
        return :timeout
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

    def localSaveValue( argKey = nil, data )
      # open local db
      util = PasteHub::Util.new( )
      if argKey
        key = argKey
      else
        key = util.currentTime( ) + "=" + util.digest( data )
      end
      
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username )
      localdb.insertValue( key, data.dup )
      localdb.insertValue( PasteHub::LOCAL_DATE_KEY, util.currentTime( ) )
      localdb.close()
      key
    end

  end
end
