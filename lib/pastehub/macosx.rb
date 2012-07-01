
module PasteHub

  class MacOSX

    def self.push( data )
      begin
        IO.popen("/usr/bin/pbcopy", "r+") {|io|
          io.write data
          io.close
        }
        STDERR.puts "Info: push to MacOS X clipboard."
      rescue IOError => e
        STDERR.puts( "Error: program /usr/bin/pbcopy not found." )
        return false
      end
    end

    def self.hasNew?( username )
      osxPaste = open( "|/usr/bin/pbpaste" ) { |f| 
        f.read
      }

      # read top data of localDB
      begin
        store = PasteHub::LocalStore.new( username, true )
        top = store.top( )
        store.close()
      rescue RuntimeError
        # alreay opened the localStore
        top = ""
      end

      if 0 == osxPaste.size or 0 == top.size
        nil
      elsif osxPaste != top
        osxPaste
      else
        nil
      end
    end
  end
end
