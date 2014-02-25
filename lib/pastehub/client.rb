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
require 'socket'

module PasteHub

  # setup user's directory and sync directory
  #   result: true ... success / false ... fail
  def self.setupDirectory
    localdb_path   = PasteHub::Config.instance.localDbPath
    localsync_path = PasteHub::Config.instance.localSyncPath
    # Create directory
    if not File.exist?(  localdb_path )
      FileUtils.mkdir_p( localdb_path, { :mode => 0700 } )
    end

    # Create directory for Sync
    if not File.exist?(  localsync_path )
      FileUtils.mkdir_p( localsync_path, { :mode => 0700 } )
    end
    return true
  end

  def self.hostname( )
    hostname = open( "|hostname" ) { |f| 
      f.read.chomp
    }
    if 0 < hostname.size()
      hostname
    else
      raise RuntimeError, "Can't resolve hostname"
    end
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


  class Client
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
                return chunk.chomp.clone()
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
  end
end
