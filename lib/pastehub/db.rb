require 'gdbm'

module PasteHub

  ONLINE_STATE_KEY='__ONLINE_STATE'
  LOCAL_DATE_KEY  ='__LOCAL_DATE'
  SERVER_DATE_KEY ='__SERVER_DATE'

  class LocalDB
    def initialize( basepath = "/tmp/")
      @basepath = basepath
    end

    def open( username, reader = false )
      if reader
        @db = GDBM.new( @basepath + username + ".db", nil, GDBM::READER  | GDBM::NOLOCK )
      else
        @db = GDBM.new( @basepath + username + ".db", nil, GDBM::WRCREAT | GDBM::SYNC )
      end

      if not @db
        raise RuntimeError, sprintf( "DBM.new error: file=%s", username + ".db" )
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
      forward_match_keys( "" ).sort {|a,b| -(a <=> b) }
    end

    def getValue( key, fallback = false )
      val = @db[ key ]
      if val
        val.force_encoding("UTF-8")
      else
        fallback
      end
    end

    def insertValue( key, value )
      @db[ key.force_encoding("ASCII-8BIT") ] = value.force_encoding("ASCII-8BIT")
    end

    def deleteValue( key )
      val = @db[ key ]
      if val
        @db.delete( key )
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
