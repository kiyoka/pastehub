require 'digest'
require 'date'
require 'set'

module PasteHub
  require 'rubygems'
  begin
    require 'highline'
    USE_HIGHLINE = true
  rescue LoadError
    USE_HIGHLINE = false
  end

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
      currentDate = dt.strftime( "%s" ) + "=" + dt.strftime( "%F.%H:%M:%S" )
      currentDate
    end

    def currentSeconds( )
      self.key_seconds( self.currentTime() )
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

    # Cut string with limit characters
    def stringLimit( str, limit )
      if limit < str.size()
        str[0...(limit)] + "..."
      else
        str
      end
    end
  end
end
