require 'digest'
require 'date'
require 'set'

module PasteHub
  class Util
    def initialize()
    end

    # return message digest for str.
    def digest( str )
      Digest::SHA1.hexdigest( str )
    end

    # return the currentTime in Unixtime
    def currentTime( )
      dt = Time.new.gmtime.to_datetime()
      currentDate = dt.strftime( "%s" ) + "=" + dt.strftime( "%F.%R:%S" )
      currentDate
    end

    def currentSeconds( )
      self.key_seconds( self.currentTime() )
    end

    def _splitKey( key )
      key.split( /[=]/ )
    end

    def key_seconds( key )
      arr = _splitKey( key )
      if 1 < arr.size
        arr[0].to_i
      else
        nil
      end
    end

    def key_timestamp( key )
      arr = _splitKey( key )
      if 1 < arr.size
        arr[1]
      else
        nil
      end
    end

    def key_digest( key )
      arr = _splitKey( key )
      if 2 < arr.size
        arr[2]
      else
        nil
      end
    end

    def diffList( list1, list2 )
      set1 = Set.new
      set2 = Set.new
      list1.each { |e| set1.add(e) }
      list2.each { |e| set2.add(e) }
      set1.difference( set2 ).to_a
    end

    # Same as Gauche's take* function
    def takeList( list1, num )
      if ( num < 0 )
        list1
      elsif num <= list1.size
        list1[ 0 ... num ]
      else
        list1
      end
    end

    # Same as Gauche's drop* function
    def dropList( list1, num )
      if num < 0
        list1
      elsif num <= list1.size
        list1[ num .. list1.size ]
      else
        []
      end
    end

    # input utility
    def inputSeveralTimes( message, firstLabel, secondLabel )
      STDERR.puts( message )
      3.times { |n|
        STDERR.print( firstLabel  )
        firstStr  = STDIN.readline.chomp
        STDERR.puts ""
        STDERR.print( secondLabel )
        secondStr = STDIN.readline.chomp
        STDERR.puts ""
        if firstStr == secondStr
          return firstStr
        end
      }
      return nil
    end
  end
end
