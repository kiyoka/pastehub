#
# client.rb - PasteHub's client main library
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
require 'net/http'
require 'net/https'
require 'uri'
require 'open-uri'
require 'fileutils'

module PasteHub

  def self.signfilePath
    signfile = PasteHub::Config.instance.localDbPath + "authinfo.txt"
  end

  def self.loadUsername
    signfile = self.signfilePath

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
      rescue Exception => e
        STDERR.puts( "Error: can't load #{signfile}: #{e.to_s}" )
        raise e
      end
      return nil
    end
  end

  # setup user's directory
  #   result: true ... success / false ... fail
  def self.setupDirectory
    localdb_path = PasteHub::Config.instance.localDbPath
    # Create directory
    if not File.exist?(  localdb_path )
      if 0 == Dir.mkdir( localdb_path, 0700 )
        STDERR.puts( "Info:  created directory #{localdb_path}" )
      else
        STDERR.puts( "Error: can't create directory #{localdb_path}" )
        return false
      end
    end
    return true
  end

  def self.signIn
    signfile = self.signfilePath
    util = Util.new

    # authenticate information
    if not File.exist?( signfile )
      3.times { |n|
        util.say( "Please input your account information" )
        username  = util.inputText("       email: ")
        secretKey = util.inputText("  secret-key: " )
        auth = AuthForClient.new( username, secretKey )
        client = Client.new( auth )
        if client.authTest()
          # save authinfo with gpg
          begin
            password = util.inputPasswordTwice(
                            "Please input password for crypted file",
                            "  password            : ",
                            "  password(for verify): " )
            if password
              crypt = PasteHub::Crypt.new( password )
              open( signfile, "w" ) {|f|
                f.puts(           username  )
                f.puts( crypt.en( secretKey ))
              }
              # auth OK
              return [ username, secretKey, password ]
            end
            STDERR.puts( "Error: password setting failed..." )
          rescue Exception => e
            STDERR.puts( "Error: can't save #{signfile}: #{e.to_s}" )
          end
        else
          STDERR.puts( "your email or secret key is not registerd..." )
        end
      }
    else
      begin
        open( signfile ) {|f|
          util.say( "Please input password for crypted file" )
          username = f.readline.chomp
          signStr = f.read
          3.times { |n|
            password  = util.inputPassword("  crypt password: ")
            crypt = PasteHub::Crypt.new( password )
            secretKey= crypt.de( signStr )
            if secretKey
              auth = AuthForClient.new( username, secretKey )
              client = Client.new( auth )
              if client.authTest()
                return [ username, secretKey, password ]
              else
                STDERR.puts( "Error: your secretKey may be old." )
              end
            end
            STDERR.puts( "Error: missing password." )
          }
        }
      rescue Exception => e
        STDERR.puts( "Error: can't load #{signfile}: #{e.to_s}" )
        raise e
      end
    end
    return [ nil, nil, nil ]
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
    rescue Exception => e
      STDERR.puts( "Warning: can't load #{pidFile}: #{e.to_s}" )
      pid = 0
    end
    return pid
  end


  class ClientBase

    # return:  Net::HTTPResponse
    def httpGet( uriStr, headerHash, errorMessage )
      uri = URI.parse( uriStr )
      begin
        https = Net::HTTP.new(uri.host, uri.port)
        if ( Net::HTTP.https_default_port == uri.port ) or ( "https" == uri.scheme )
          https.use_ssl     = true
          https.verify_mode = OpenSSL::SSL::VERIFY_PEER
        end
        https.start {|http|
          resp = http.get(uri.request_uri, headerHash)
          if "200" == resp.code
            return resp
          end
        }
      rescue Exception => e
        STDERR.puts errorMessage + ":" + e.to_s
      end
      return nil
    end

    # return:  Net::HTTPResponse
    def httpPost( uriStr, bodyStr, headerHash, errorMessage )
      uri = URI.parse( uriStr )
      begin
        https = Net::HTTP.new(uri.host, uri.port)
        if ( Net::HTTP.https_default_port == uri.port ) or ( "https" == uri.scheme )
          https.use_ssl     = true
          https.verify_mode = OpenSSL::SSL::VERIFY_PEER
        end
        https.start {|http|
          resp = http.post(uri.request_uri, bodyStr, headerHash)
          if "200" == resp.code
            return resp
          end
        }
      rescue Exception => e
        STDERR.puts errorMessage + ":" + e.to_s
      end
      return nil
    end

    # return:  [ Net::HTTP, Net::HTTP::Get ]
    def httpGetRequest( uriStr, headerHash, errorMessage )
      uri = URI.parse( uriStr )
      begin
        https = Net::HTTP.new(uri.host, uri.port)
        if ( Net::HTTP.https_default_port == uri.port ) or ( "https" == uri.scheme )
          https.use_ssl     = true
          https.verify_mode = OpenSSL::SSL::VERIFY_PEER
        end
        return [ https, Net::HTTP::Get.new(uri.request_uri, headerHash) ]
      rescue Exception => e
        STDERR.puts errorMessage + ":" + e.to_s
      end
      return nil
    end

  end


  class Client < ClientBase
    def initialize( auth, password = nil )
      @auth = auth
      ins = PasteHub::Config.instance
      @localdb_path         = ins.localDbPath
      @server_api_url       = ins.targetApiURL
      @server_notifier_url  = ins.targetNotifierURL
      @list_items           = ins.listItems
      @syncTrigger          = []
      @crypt                = if password
                                Crypt.new( password )
                              else
                                nil
                              end
    end

    def authTest
      httpGet( "#{@server_api_url}authTest", @auth.getAuthHash(), "Error: can't connect to server for auth test" )
    end


    def getList( limit = nil )
      h = {"content-type" => "plain/text"}
      if limit
        h[ 'X-Pastehub-Limit' ] = limit.to_i
      end

      resp = httpGet( "#{@server_api_url}getList", @auth.getAuthHash().merge( h ), "Error: fails 'getList' from server." )
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
      masterList
    end

    def getValue( key )
      raise RuntimeError, "Error: no encrypt password." unless @crypt

      resp = httpPost( "#{@server_api_url}getValue", key, @auth.getAuthHash().merge( {"content-type" => "plain/text"} ), "Error: fails 'getValue' from server." )
      ret = @crypt.de( resp.read_body() )
      unless ret
        STDERR.puts( "Warning: encrypt password is wrong. getValue missing..." )
        ret = "error..."
      end
      ret
    end

    def putValue( key, data )
      ret = ""
      if not key
        key = "_"
      end
      raise RuntimeError, "Error: no encrypt password." unless @crypt
      _data = @crypt.en( data )
      unless _data
        STDERR.puts( "Warning: encrypt password is wrong. putValue missing..." )
        ret = nil
      else
        resp = httpPost( "#{@server_api_url}putValue", _data,
               @auth.getAuthHash().merge(
                                   {
                                           "content-type" => "plain/text",
                                           "x-pastehub-key" => key    }),
               "Error: fails 'putValue' from server." )
        if resp
          ret = resp.read_body()
        end
      end
      ret
    end

    def wait_notify( auth )
      begin
        pair = httpGetRequest("#{@server_notifier_url}", auth.getAuthHash(), "Error: fails 'notifier' api on server." )
        https       = pair[0]
        getRequest  = pair[1]
        https.start() { |http|
          http.request(getRequest) do |response|
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
        }
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
      list = localdb.getList( )
      if util.key_digest( list[0] ) == util.digest( data )
        # duplicate
        localdb.close()
        list[0]
      else
        localdb.insertValue( key, data.dup )
        localdb.insertValue( PasteHub::LOCAL_DATE_KEY, util.currentTime( ) )
        localdb.close()
        key
      end
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
