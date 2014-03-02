#
# clientsync.rb - PasteHub's client sync library
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
require 'uri'
require 'open-uri'
require 'fileutils'
require 'json'

module PasteHub

  class Status
    TIMER_INIT = 60 # second

    def initialize( )
      @comes      = 0
      @online     = false
      @errorFlag  = false
      @timer      = TIMER_INIT
      @curJsonStr = ""
    end
    
    def inc( )
      @comes += 1
      @timer     = TIMER_INIT
    end
    
    def reset( )
      @comes = 0
      @timer     = TIMER_INIT
    end
    
    def setOnline( arg )
      @online = arg
    end

    def tick( delta )
      if @timer <= 0
        @comes = 0
      else
        @timer -= delta
      end
    end

    def update( )
      prev = @curJsonStr
      h = Hash.new
      h[ 'online' ] = @online
      h[ 'error'  ] = @errorFlag
      h[ 'comes'  ] = @comes
      @curJsonStr = JSON.dump( h )
      return [ @curJsonStr, prev ]
    end

    def icon( )
      result = if @errorFlag
                 "error"
               elsif not @online
                 "offline"
               else
                 case @comes
                 when 0
                   "online"
                 when 1
                   "one"
                 when 2
                   "two"
                 when 3
                   "three"
                 else
                   "threeplus"
                 end
               end
      return result
    end
  end

  class ClientSync
    require 'win32/pipe' if PasteHub.isWin32 
    ICONSOCKET       = "/tmp/pastehub_icon"
    ICONSOCKET_WIN32 = "pastehub_icon"

    def initialize( alive_entries, localdb_path, polling_interval )
      @alive_entries        = alive_entries
      @localdb_path         = localdb_path
      @polling_interval     = polling_interval
      @status               = Status.new()
    end


    def syncDb( auth, password )
      notifyFlag = false

      STDERR.puts "synchronizing..."
      client = PasteHub::Client.new( auth, password )
      util   = PasteHub::Util.new
      # open local db
      localdb = PasteHub::LocalDB.new( @localdb_path )
      localdb.open( auth.username, true )

      masterList = localdb.getServerList()
      #pp [ "masterList.size", masterList.size ]

      # calc difference between master and local.
      localList = localdb.getList()
      #pp [ "localList.size(1)", localList.size ]

      # pickup first ALIVE_ENTRIES from localList
      #_half0 = util.takeList( localList,  @alive_entries )
      _half1 = util.dropList( masterList,  @alive_entries )
      localList = localList + _half1
      #pp [ "localList.size(2)", localList.size ]

      downList = util.diffList( masterList, localList )
      #pp [ "downList.size", downList.size ]

      upList   = util.diffList( localList,  masterList )
      #pp [ "upList.size", upList.size ]

      # push first element to clipboard.
      if 0 < downList.size
        key = downList.first
        value = client.getValue( key )

        # top of localdb
        topvalue = ""
        lst = localdb.getList(1)
        if 0 < lst.size 
          topvalue = localdb.getValue( lst.first )
        end
        #STDERR.printf( "local.top:[%s]", topvalue  )
        #STDERR.printf( "push Clip:[%s]", value.dup )

        if @prevData == value
          #p [ @prevData , value ]
          STDERR.puts "Info: did not push to OS's clipboard because prevData == donwloaded-firstEntry."
        elsif topvalue.force_encoding("UTF-8")  == value.dup.force_encoding("UTF-8")
          STDERR.puts "Info: got value == top entry of localStore."
        else
          STDERR.printf( "Info: push to OS's clipboard (size=%d).\n", value.size )
          PasteHub::AbstractClipboard.push( value.dup )
          notifyFlag = true
          @prevData = value
        end
      end

      # donwload
      downList.each {|key|
        value = client.getValue( key )
        localdb_w = PasteHub::LocalDB.new( @localdb_path )
        localdb_w.open( auth.username )
        localdb_w.insertValue( key.dup, value.dup )
        localdb_w.close
        sleep( 0.2 )
      }
      # upload
      upList.each {|key|
        value = localdb.getValue( key )
        client.putValue(     key.dup, value.dup )
      }

      STDERR.puts "Info: download #{downList.size} records."
      downList.each { |x|
        #STDERR.puts "  key=#{x}"
      }
      STDERR.puts "Info:   upload #{upList.size} records."
      upList.each { |x|
        #STDERR.puts "  key=#{x}"
      }
      localdb.close()

      if 0 < downList.size or 0 < upList.size
        STDERR.puts "Info:   send signal to Emacs."
        system( "killall -SIGUSR1 Emacs emacs" )
      end

      return notifyFlag
    end


    def gcLocalDb( auth )
      STDERR.puts "GCing..."

      # open local db
      localdb_w = PasteHub::LocalDB.new( @localdb_path )
      localdb_w.open( auth.username )
      localList = localdb_w.getList()
      deleteList = localList[@alive_entries..(localList.size)]
      if deleteList and 0 < deleteList.size
        STDERR.puts "Info:   delete #{deleteList.size} records."
        deleteList.each { |key|
          localdb_w.deleteValue( key.dup )
        }
      end
      localdb_w.close
    end


    def fetchServerList( latestKey, auth )
      client = PasteHub::Client.new( auth )
      if latestKey.is_a? String
        STDERR.puts "Info: fetch one entry"
        list = [ latestKey.clone() ]
      else
        STDERR.puts "Info: fetch ALL entries"
        list = client.getList()
      end
      client.setServerFlags( list )
    end

    def addNoitfyCallback( countUpNotifyFunc, connectNotifyFunc, disconnectNotifyFunc )
      @countUpNotifyFunc     = countUpNotifyFunc
      @connectNotifyFunc     = connectNotifyFunc
      @disconnectNotifyFunc  = disconnectNotifyFunc
    end

    def notifyCountUp()
      @countUpNotifyFunc.call() if @countUpNotifyFunc
      @status.inc( )
    end

    def notifyConnect()
      @connectNotifyFunc.call() if @connectNotifyFunc
      @status.setOnline( true )
    end

    def notifyDisconnect()
      @disconnectNotifyFunc.call() if @disconnectNotifyFunc      
      @status.setOnline( false )
    end


    def syncMain( username, secretKey, password )
      freeCounter = 0

      auth   = PasteHub::AuthForClient.new( username, secretKey )
      client = PasteHub::Client.new( auth )
      client.setOnlineState( false )
      client.setOnlineState( true )  # create Trigger token  ___-___
      client.setOnlineState( false )
      result = :start

      while true
        begin
          freeCounter += 1

          if client.getTrigger() or (0 == (freeCounter % 60))
            STDERR.puts "Info: force sync."
            result = :sync
            gcLocalDb( auth )
          else
            auth   = PasteHub::AuthForClient.new( username, secretKey )
            result = client.wait_notify( auth )
          end

          case result
          when :timeout
            STDERR.puts "waiting..."
            client.setOnlineState( true )
            notifyConnect()
          when :retry
            STDERR.puts "retrying...."
            client.setOnlineState( false )
            notifyDisconnect()
            sleep 60
          else
            if not client.online?()
              notifyConnect()
            end
            client.setOnlineState( true )
            fetchServerList( result, auth )
            if syncDb( auth, password )
              notifyCountUp()
            end
          end
        rescue Errno::EAGAIN => e
          STDERR.puts "retrying... DB is locked"
          notifyDisconnect()
          sleep 2
        rescue Errno::ECONNREFUSED => e
          STDERR.puts "retrying... pastehub server is down(1)"
          notifyDisconnect()
          sleep 60
        rescue Errno::ETIMEDOUT => e
          STDERR.puts "retrying... network is offline(1)"
          notifyDisconnect()
          sleep 60
        rescue SocketError => e
          STDERR.puts "retrying... network is offline(2)"
          notifyDisconnect()
          sleep 60
        rescue Timeout::Error => e
          # ONLINE, but server is not helthy
          notifyDisconnect()
          STDERR.puts "retrying... pastehub server is down(2)"
          sleep 60
        end
      end
    end


    def clipboardCheck( username, secretKey, password )
      STDERR.puts "Info: clipboardCheck thread start"
      @prevData = ""
      while true
        sleep @polling_interval
        data = PasteHub::AbstractClipboard.hasNew?( username )
        if data
          if @prevData != data
            #p [ @prevData, data ]
            auth = PasteHub::AuthForClient.new( username, secretKey )
            client = PasteHub::Client.new( auth, password )
            if client.online?(  )
              STDERR.puts( "Info: posted data from OS." )
              begin
                client.putValue( "_", data )
              rescue Errno::ECONNREFUSED => e
                # if postData was fail, save to local.
                client.setOnlineState( false )
                client.localSaveValue( data )
                sleep 60
              end
            else
              client.localSaveValue( data )
            end
            @prevData = data
          end
        end
      end
    end


    def syncNow( username, secretKey, password )
      store = PasteHub::LocalStore.new( username, true )
      pair = store.top()
      auth = PasteHub::AuthForClient.new( username, secretKey )
      client = PasteHub::Client.new( auth, password )
      client.putValue( pair[0], pair[1] )
      store.close()
    end

    def syncStatus( )
      while true
        ( cur , prev ) = @status.update( )
        if cur != prev 
          # save current status as json file.
          open( @localdb_path + "status.json", "w" ) {|f|
            f.puts cur
          }
          # save current icon status.
          open( @localdb_path + "status_icon.txt", "w" ) {|f|
            f.puts @status.icon( )
          }
        end

        @status.tick( 1.0 )
        sleep 1.0
      end
    end

    def statusServer( )
      if PasteHub.isWin32 
        win32StatusServer()
      else
        unixStatusServer()
      end
    end

    def win32StatusServer( )
      serv = Win32::Pipe::Server.new( ICONSOCKET_WIN32,
                                      Win32::Pipe::PIPE_TYPE_BYTE | Win32::Pipe::PIPE_READMODE_BYTE | Win32::Pipe::PIPE_WAIT )
      STDERR.puts "Info: waiting for client connection."
      serv.connect # wait for client
      STDERR.puts "Info: connected from status client."

      begin
        serv.write @status.icon()
        prev = @status.icon()
        while true
          if prev != @status.icon()
            serv.write @status.icon()
            prev = @status.icon()
          end
          sleep 0.1
        end
      rescue SystemCallError => e
        STDERR.puts "Error: Can't open PIPE."
      rescue e
        STDERR.puts "Error: Other Error"
      ensure
        serv.close if serv
      end
    end

    def unixStatusServer( )
      s = nil
      begin
        if File.exist? ( ICONSOCKET )
          File.unlink( ICONSOCKET )
        end
        UNIXServer.open( ICONSOCKET ) {|serv|
          s = serv.accept
          s.puts @status.icon()
          prev = @status.icon()
          while true
            if prev != @status.icon()
              s.puts @status.icon()
              prev = @status.icon()
            end
            sleep 0.1
          end
        }
      rescue Errno::EPIPE => e
        STDERR.puts "Error: Broken PIPE"
      rescue e
        STDERR.puts "Error: Other Error"
      ensure
        s.close if s
      end
    end
  end
end
