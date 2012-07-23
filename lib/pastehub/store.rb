
module PasteHub

  class LocalStore
    def initialize( username, reader = false )
      @db = PasteHub::LocalDB.new( PasteHub::Config.instance.localDbPath )
      @db.open( username, reader )
    end

    def getList
      @db.getList()
    end

    def getServerList
      @db.getServerList()
    end

    def top
      lst = @db.getList( 1 )
      if 0 < lst.size
        @db.getValue( lst.first )
      else
        ""
      end
    end

    def getValue( key )
      @db.getValue( key.dup )
    end

    def latest
      [ @db.getValue( PasteHub::SERVER_DATE_KEY ).to_s,
        @db.getValue( PasteHub::LOCAL_DATE_KEY  ).to_s ]
    end

    def insertValue( key, value )
      @db.insertValue( key, value )
    end

    def close
      @db.close
    end

    def clear
      @db.clear
    end

  end

end
