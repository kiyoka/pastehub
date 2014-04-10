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
      @plugin               = PasteHub::Plugin
      @plugin.load_plugins
    end

    def display_config
      @plugin.display_config
    end
 
    def notifyToReceive(message)
      @plugin.distribute_newly_arrived(message)
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

    def get_sync_entry_body( path )
      hostname = path_to_hostname( path )
      entry = Entry.new( hostname )
      if entry.can_load?()
        entry.load()[1]
      else
        nil
      end
    end

    def get_latest_entry( )
      # check directory changes
      config = PasteHub::Config.instance

      paths = get_other_hostfiles()
      result = paths.map { |path|
        hostname = path_to_hostname( path )
        entry = Entry.new( hostname )
        if entry.can_load?()
          ret = entry.load()
          [ ret[0]['create_unixtime'].to_i, ret[0], ret[1] ]
        else
          [ 0, nil, nil ]
        end
      }
      if 0 == result.size()
        nil
      else 
        result = result.sort {|a,b| a[0] <=> b[0]}.reverse
        [
         result[0][1], result[0][2]
        ]
      end
    end

    def touch( )
      @last_modify_time     = Time.now()
    end

    def sync_main()
      config = PasteHub::Config.instance
      util   = PasteHub::Util.new

      STDERR.puts "Info: sync_main thread start"
      free_counter = 0

      while true
        free_counter += 1

        path = exist_sync_data?()
        if path
          body = get_sync_entry_body( path )
          if body
            STDERR.printf( "Info: push to OS's clipboard ([[%s]] size=%d).\n", util.stringLimit(body,config.notifyMessageMax), body.size )
            PasteHub::AbstractClipboard.push( body.dup )
            touch()
          else
            STDERR.printf( "X" )   if config.getVerbose( )
          end
        else
          STDERR.printf( "x" )    if config.getVerbose( )
        end
        # interval time
        sleep @polling_interval
      end
    end

    def clipboard_check()
      config = PasteHub::Config.instance
      util   = PasteHub::Util.new

      STDERR.puts "Info: clipboardCheck thread start"
      @prevData = ""
      while true
        sleep @polling_interval
        data = PasteHub::AbstractClipboard.hasNew?( )
        if data
          if @prevData != data
            entry = Entry.new( @hostname )
            entry.save( data )
            STDERR.printf( "Info: clipboard to File ([[%s]]] size=%d).\n", util.stringLimit(data,config.notifyMessageMax) , data.size )
            notifyToReceive( data )
            @prevData = data
          end
        end
        STDERR.printf( "." )    if config.getVerbose( )
      end
    end
  end
end
