require 'net/http'
require 'uri'
require 'open-uri'
require 'fileutils'

module PasteHub

  class ClientSync

    def initialize( alive_entries, localdb_path, polling_interval )
      @alive_entries        = alive_entries
      @localdb_path         = localdb_path
      @polling_interval     = polling_interval
    end


    def syncDb( auth, password )
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

      # push first element to MacOS X clipboard.
      if 0 < downList.size
        key = downList.first
        value = client.getValue( key )
        PasteHub::MacOSX.push( value.dup )
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
        list = [ latestKey ]
      else
        STDERR.puts "Info: fetch ALL entries"
        list = client.getList()
      end
      client.setServerFlags( list )
    end


    def syncMain( username, secretKey, password )
      auth   = PasteHub::AuthForClient.new( username, secretKey )
      client = PasteHub::Client.new( auth )
      client.setOnlineState( false )
      client.setOnlineState( true )  # create Trigger token  ___-___
      client.setOnlineState( false )

      while true
        begin
          if client.getTrigger()
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
          when :retry
            STDERR.puts "retrying...."
            client.setOnlineState( false )
            sleep 60
          else
            client.setOnlineState( true )
            fetchServerList( result, auth )
            syncDb( auth, password )
          end
        rescue Errno::EAGAIN => e
          STDERR.puts "retrying... DB is locked"
          sleep 2
        rescue Errno::ECONNREFUSED => e
          STDERR.puts "retrying... pastehub server is down(1)"
          sleep 60
        rescue Errno::ETIMEDOUT => e
          STDERR.puts "retrying... network is offline(1)"
          sleep 60
        rescue SocketError => e
          STDERR.puts "retrying... network is offline(2)"
          sleep 60
        rescue Timeout::Error => e
          # ONLINE, but server is not helthy
          STDERR.puts "retrying... pastehub server is down(2)"
          sleep 60
        end
      end
    end


    def macosxCheck( username, secretKey, password )
      prevData = ""
      while true
        sleep @polling_interval
        data = PasteHub::MacOSX.hasNew?( username )
        if data
          if data != prevData
            auth = PasteHub::AuthForClient.new( username, secretKey )
            client = PasteHub::Client.new( auth, password )
            if client.online?(  )
              STDERR.puts( "Info: posted data from MacOS X." )
              begin
                client.putValue( "_", data )
              rescue Errno::ECONNREFUSED => e
                # if postData was fail, save to local.
                setOnlineState( false )
                client.localSaveValue( data )
                sleep 60
              end
            else
              client.localSaveValue( data )
            end
            prevData = data
          end
        end
      end
    end


    def syncNow( username, secretKey, password )
      STDERR.puts( "Info: caught signal." )
      store = PasteHub::LocalStore.new( username, true )
      pair = store.top()
      auth = PasteHub::AuthForClient.new( username, secretKey )
      client = PasteHub::Client.new( auth, password )
      client.putValue( pair[0], pair[1] )
    end

  end
end
