require 'gdbm'

module PasteHub

  ONLINE_STATE_KEY='__ONLINE_STATE'
  LOCAL_DATE_KEY  ='__LOCAL_DATE'
  SERVER_DATE_KEY ='__SERVER_DATE'

  LOCAL_PREFIX  = "local::"
  SERVER_PREFIX = "server::"

  class LocalDB
    def initialize( basepath = "/tmp/")
      @basepath = basepath
    end

    def open( username, reader = false )
      (60*2).times { |n|
        if reader
          @db = GDBM.new( @basepath + username + ".db", nil, GDBM::READER  | GDBM::NOLOCK )
        else
          @db = GDBM.new( @basepath + username + ".db", nil, GDBM::WRCREAT )
        end
        if not @db.closed?
          break
        end
        #STDERR.puts "#Warning: DB open fail(locked) retry..."
        sleep 0.5
      }
      if @db.closed?
        raise RuntimeError, sprintf( "DBM.new open error: file=%s", username + ".db" )
      end
      @username = username
    end

    def getList( limit = nil )
      arr = self._getList().reject{|x| x.match( /^_/ )}
      if limit
        arr.take( limit )
      else
        arr
      end
    end

    def _getList( )
      forward_match_keys( LOCAL_PREFIX ).sort {|a,b| -(a <=> b) }.map {|x|
        x[(LOCAL_PREFIX.size)...(x.size)]
      }
    end

    def getServerList( )
      self._getServerList().reject{|x| x.match( /^_/ )}
    end

    def _getServerList( )
      forward_match_keys( SERVER_PREFIX ).sort {|a,b| -(a <=> b) }.map {|x|
        x[(SERVER_PREFIX.size)...(x.size)]
      }
    end

    def getValue( key, fallback = false )
      val = @db[ LOCAL_PREFIX + key ]
      if val
        val.force_encoding("UTF-8")
      else
        fallback
      end
    end

    def insertValue( key, value )
      @db[ LOCAL_PREFIX + key.force_encoding("ASCII-8BIT") ] = value.force_encoding("ASCII-8BIT")
    end

    def deleteValue( key )
      val = @db[ LOCAL_PREFIX + key ]
      if val
        @db.delete( LOCAL_PREFIX + key )
        true
      else
        false
      end
    end

    def setServerFlag( key )
      @db[ SERVER_PREFIX + key ] = '1'
    end

    def onServer?( key )
      if @db[ SERVER_PREFIX + key ]
        true
      else
        false
      end
    end

    def forward_match_keys( prefix )
      @db.keys( ).select {|key|
        key.match( "^" + prefix )
      }
    end

    def clear
      @db.clear
    end

    def close
      @db.close
    end
  end
end
