#
# store.rb - PasteHub's storage sync library
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
        [ lst.first, @db.getValue( lst.first ) ]
      else
        [ nil, nil ]
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
