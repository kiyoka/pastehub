#
# clientsync.rb - PasteHub's client sync library
#  
#   Copyright (c) 2009-2014  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
require 'net/http'
require 'uri'
require 'open-uri'
require 'fileutils'
require 'json'

module PasteHub

  class Status
    TIMER_INIT = 60 # second

    def initialize( )
      @comes      = 0
      @online     = false
      @errorFlag  = false
      @timer      = TIMER_INIT
      @curJsonStr = ""
    end
    
    def inc( )
      @comes += 1
      @timer     = TIMER_INIT
    end
    
    def reset( )
      @comes = 0
      @timer     = TIMER_INIT
    end
    
    def setOnline( arg )
      @online = arg
    end

    def tick( delta )
      if @timer <= 0
        @comes = 0
      else
        @timer -= delta
      end
    end

    def update( )
      prev = @curJsonStr
      h = Hash.new
      h[ 'online' ] = @online
      h[ 'error'  ] = @errorFlag
      h[ 'comes'  ] = @comes
      @curJsonStr = JSON.dump( h )
      return [ @curJsonStr, prev ]
    end

    def icon( )
      result = if @errorFlag
                 "error"
               elsif not @online
                 "offline"
               else
                 case @comes
                 when 0
                   "online"
                 when 1
                   "one"
                 when 2
                   "two"
                 when 3
                   "three"
                 else
                   "threeplus"
                 end
               end
      return result
    end
  end

  class ClientSync
    ICONSOCKET = "/tmp/pastehub_icon"

    def initialize( hostname, polling_interval )
      @hostname             = hostname
      @polling_interval     = polling_interval
      @status               = Status.new()
      @last_modify_time     = Time.now()
    end

    def addNoitfyCallback( countUpNotifyFunc )
      @countUpNotifyFunc     = countUpNotifyFunc
    end
 
    def notifyCountUp()
      @countUpNotifyFunc.call() if @countUpNotifyFunc
      @status.inc( )
    end

    # host's sync data file excludes local hostname
    def get_other_hostfiles()
      config = PasteHub::Config.instance
      arr = Dir.glob( config.localSyncPath + "*.dat" )
      retArr = arr.select { |path|
        not path.match( "/" + @hostname + ".dat$" )
      }
      retArr
    end

    # return: pathname of sync data of other host
    def exist_sync_data?()
      # check directory changes
      config = PasteHub::Config.instance

      paths = get_other_hostfiles()
      result = paths.select { |path|
        fs = File::Stat.new( path )
        fs.mtime

        #printf( "last_modify_time: %s \n", @last_modify_time )
        #printf( "mtime of [%s]: %s \n", path, fs.mtime )
        1 == (fs.mtime <=> @last_modify_time)
      }
      if 0 < result.size
        result[0]
      else
        nil
      end
    end

    def path_to_hostname( path )
      File.basename( path, ".dat" )
    end

    def get_sync_entry( path )
      hostname = path_to_hostname( path )
      entry = Entry.new( hostname )
      if entry.can_load?()
        entry.load()[1]
      else
        nil
      end
    end

    def touch( )
      @last_modify_time     = Time.now()
    end

    def sync_main()
      free_counter = 0

      while true
        free_counter += 1

        path = exist_sync_data?()
        if path
          body = get_sync_entry( path )
          if body
            STDERR.printf( "Info: push to OS's clipboard ([%s...] size=%d).\n", body[0...3], body.size )
            PasteHub::AbstractClipboard.push( body.dup )
            touch()
          else
            STDERR.printf( "Debug: get_sync_entry => nil.\n" )
          end
        else
          STDERR.printf( "Debug: exist_sync_data? => nil.\n" )
        end
        # interval time
        sleep @polling_interval
      end
    end


    def clipboard_check()
      STDERR.puts "Info: clipboardCheck thread start"
      @prevData = ""
      while true
        sleep @polling_interval
        data = PasteHub::AbstractClipboard.hasNew?( username )
        if data
          if @prevData != data
            #p [ @prevData, data ]
            auth = PasteHub::AuthForClient.new( username, secretKey )
            client = PasteHub::Client.new( auth, password )
            if client.online?(  )
              STDERR.puts( "Info: posted data from OS." )
              begin
                client.putValue( "_", data )
              rescue Errno::ECONNREFUSED => e
                # if postData was fail, save to local.
                client.setOnlineState( false )
                client.localSaveValue( data )
                sleep 60
              end
            else
              client.localSaveValue( data )
            end
            @prevData = data
          end
        end
      end
    end
  end
end
