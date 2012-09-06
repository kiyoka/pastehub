
module PasteHub

  class MacOSX

    def self.push( data )
      pbcopy = "/usr/bin/pbcopy"
      if File.exist?( pbcopy )
        begin
          IO.popen(pbcopy, "r+") {|io|
            io.write data
            io.close
          }
          STDERR.puts "Info: push to MacOS X clipboard."
        rescue IOError => e
          STDERR.puts( "Error: program #{pbcopy} not found." )
          return false
        end
      end
    end

    def self.hasNew?( username )
      osxPaste = open( "|/usr/bin/pbpaste" ) { |f| 
        f.read
      }
      if 0 < osxPaste.size
        osxPaste
      else
        nil
      end
    end
  end
end
