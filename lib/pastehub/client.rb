require 'net/http'
require 'uri'
require 'open-uri'
require 'fileutils'

module PasteHub

  def self.loadUsername
    signfile = PasteHub::Config.instance.localDbPath + "authinfo.txt"

    # load authenticate information
    if not File.exist?( signfile )
      return nil
    else
      begin
        open( signfile ) {|f|
          # first line is email (is username)
          username = f.readline.chomp
          if username.match( /^[a-z]+/ )
            return username
          end
        }
      rescue
        return nil
      end
      return nil
    end
  end

  def self.signIn
    signfile = PasteHub::Config.instance.localDbPath + "authinfo.txt"

    # authenticate information
    if not File.exist?( signfile )
      3.times { |n|
        STDERR.print( "  email:" )
        username  = STDIN.readline.chomp
        STDERR.print( "  secret-key:" )
        secretKey = STDIN.readline.chomp

        auth = AuthForClient.new( username, secretKey )
        client = Client.new( auth )
        if client.authTest()
          # save authinfo with gpg
          begin
            util = Util.new
            password = util.inputSeveralTimes(
                            "Please input password for crypted file",
                            "  password            :",
                            "  password(for verify):" )
            if password
              crypt = PasteHub::Crypt.new( password )
              open( signfile, "w" ) {|f|
                f.puts(           username  )
                f.puts( crypt.en( secretKey ))
              }
              # auth OK
              return [ username, secretKey ]
            end
            STDERR.puts( "Error: password setting failed..." )
          rescue
            STDERR.puts( "Error: can't save #{signfile}" )
          end
        else
          STDERR.puts( "your email or secret key is not registerd..." )
        end
      }
    else
      begin
        open( signfile ) {|f|
          STDERR.puts( "Please input password for crypted file" )
          username = f.readline.chomp
          signStr = f.read
          3.times { |n|
            STDERR.print( "  crypt password:" )
            password = STDIN.readline.chomp
            crypt = PasteHub::Crypt.new( password )
            secretKey= crypt.de( signStr )
            if secretKey
              auth = AuthForClient.new( username, secretKey )
              client = Client.new( auth )
              if client.authTest()
                return [ username, secretKey ]
              else
                STDERR.puts( "Error: your secretKey may be old." )
              end
            end
            STDERR.puts( "Error: missing password." )
          }
        }
      rescue
        STDERR.puts( "Error: can't load #{signfile}" )
      end
    end
    return [ nil, nil ]
  end

  def self.savePid( pid )
    pidFile = PasteHub::Config.instance.localDbPath + "pid"
    open( pidFile, "w" ) {|f|
      f.puts pid
    }
  end

  def self.loadPid
    pidFile = PasteHub::Config.instance.localDbPath + "pid"
    pid = 0
    begin
      pid = open( pidFile ) {|f|
        f.readline.chomp.to_i
      }
    rescue
      pid = 0
    end
    return pid
  end


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
      @syncTrigger          = []
    end

    def authTest
      uri = URI.parse("http://#{@server_api_host}/authTest")
      begin
        Net::HTTP.start(uri.host, uri.port) do |http|
          resp = http.get(uri.request_uri, @auth.getAuthHash())
          if "200" == resp.code
            return true
          end
        end
      rescue
        STDERR.puts "Error: can't connect to server for auth test"
      end
      return nil
    end

    def getList( limit = nil )
      uri = URI.parse("http://#{@server_api_host}/getList")
      masterList = []
      Net::HTTP.start(uri.host, uri.port) do |http|
        h = {"content-type" => "plain/text"}
        if limit
          h[ 'X-Pastehub-Limit' ] = limit.to_i
        end
        resp = http.get(uri.request_uri, @auth.getAuthHash().merge( h ))
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
        STDERR.puts "Error: can't connect to server(ConnectionRefused)."
        return :retry
      rescue SocketError => e
        STDERR.puts "Error: can't connect to server(SocketError)."
        return :retry
      rescue Errno::ETIMEDOUT => e
        STDERR.puts "Error: can't connect to server(Timeout1)."
        return :retry
      rescue Timeout::Error => e
        # ONLINE and notifier has no INFO.
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
      if online
        if online?() == false
          @syncTrigger.unshift( true )
        end
      end
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

    def getTrigger()
      if 0 < @syncTrigger.size
        return @syncTrigger.pop
      end
      return false
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

    def setServerFlags( keys )
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( @auth.username )
      keys.each { |key|
        localdb.setServerFlag( key )
      }
      localdb.close()
    end
  end
end
