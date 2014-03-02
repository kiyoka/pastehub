#
# client.rb - PasteHub's client main library
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
require 'net/http'
require 'net/https'
require 'uri'
require 'open-uri'
require 'fileutils'
require 'socket'

module PasteHub

  # setup user's directory and sync directory
  #   result: true ... success / false ... fail
  def self.setupDirectory
    localdb_path   = PasteHub::Config.instance.localDbPath
    localsync_path = PasteHub::Config.instance.localSyncPath
    # Create directory
    if not File.exist?(  localdb_path )
      FileUtils.mkdir_p( localdb_path, { :mode => 0700 } )
    end

    # Create directory for Sync
    if not File.exist?(  localsync_path )
      FileUtils.mkdir_p( localsync_path, { :mode => 0700 } )
    end
    return true
  end

  def self.hostname( )
    hostname = open( "|hostname" ) { |f| 
      f.read.chomp
    }
    if 0 < hostname.size()
      hostname
    else
      raise RuntimeError, "Can't resolve hostname"
    end
  end
  
  def self.savePid( pid )
    pidFile = PasteHub::Config.instance.localDbPath + "pid"
    open( pidFile, "w" ) {|f|
      f.puts pid
    }
  end

  def self.loadPid
    pidFile = PasteHub::Config.instance.localDbPath + "pid"
    pid = 0
    begin
      pid = open( pidFile ) {|f|
        f.readline.chomp.to_i
      }
    rescue Exception => e
      STDERR.puts( "Warning: can't load #{pidFile}: #{e.to_s}" )
      pid = 0
    end
    return pid
  end
end
