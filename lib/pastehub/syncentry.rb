#
# syncentry.rb - PasteHub's data handing of single clipboard entry.
#  
#   Copyright (c) 2014-2014  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
require 'base64'
require 'json'

module PasteHub

  class EntryBase
    def initialize( hostname )
      config = PasteHub::Config.instance
      @hostname = hostname
      @filepath = config.localSyncPath + @hostname + ".dat"
    end

    def encodeBody( bin )
      return Base64.encode64( bin )
    end

    def decodeBody( str )
      return Base64.decode64( str )
    end

    def genHeader( create_date, bin, encoded )
      h = Hash.new
      h[ :create_date ] = create_date
      h[ :hostname    ] = @hostname
      h[ :bodySize    ] = bin.size()
      h[ :encodedBodySize ] = encoded.size()
      return JSON.dump( h )
    end

  end

  class Entry < EntryBase
    def initialize( hostname )
      super( hostname )
    end

    # save as file
    def save( bin )
      create_date = Time.now()
      encoded = encodeBody( bin )
      json_str = genHeader( create_date, bin, encoded )
      open( @filepath, "w" ) { |f|
        f.puts json_str
        f.puts encoded
      }
      true
    end

    # check whether the file is complete data.
    def canLoad?()
    end

    # load from file
    def load
      open( @filepath, "r" ) { |f|
        firstline  = f.readline.chomp
        h = JSON.parse( firstline )
        secondline = f.readline.chomp
        return [h, decodeBody( secondline ) ]
      }
    end
  end
end
