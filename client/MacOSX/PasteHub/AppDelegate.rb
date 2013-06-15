#
# AppDelegate.rb - PasteHub's MacOS X client application.
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
require 'socket'


class AppDelegate
    
    attr_accessor :accountInfo
    
    SIGNFILEPATH = File.expand_path( "~/.pastehub/authinfo.txt" )

    def applicationDidFinishLaunching(a_notification)
        @pid = nil

        # regist observer for "auth_complete"
        NSNotificationCenter.defaultCenter.addObserver(self,
                                                       selector:"respond_to_auth_complete:",
                                                       name:"auth_complete", object:nil)
    end
    
    def batchProcess( )
        begin
            IO.popen( "/usr/local/bin/PastehubSync batch", "r+" ) { |io|
                io.puts @password
                @pid = io.pid
            while io.readline.chomp
                STDERR.puts "#"
            end
            }
        rescue e
            STDERR.puts "batchProcess(): error #{e}"
            exit( 1 )
        end
    end
    
    def statusCheckClient( )
        sleep 5
        begin
            UNIXSocket.open("/tmp/pastehub_icon") {|c|
                while true
                    iconstat = c.readline.chomp
                    STDERR.puts "iconstat=#{iconstat}"
                    NSNotificationCenter.defaultCenter.postNotificationName("change_status",
                                                                            object: nil,
                                                                            userInfo: {:status => iconstat.to_sym()} )
                end
            }
        rescue e
            STDERR.puts "PastehubSync is down"
        end
        STDERR.puts "notifyClient(): error #{e}"
        NSNotificationCenter.defaultCenter.postNotificationName("change_status",
                                                                object: nil,
                                                                userInfo: {:status => :ng} )
    end
    
    def respond_to_auth_complete(a_notification)
        STDERR.puts "AppDelegate:respond_to_auth_complete"

        @password  = accountInfo.password.stringValue
        
        @threads = []
        @threads.push( Thread.new { batchProcess( ) } )
        @threads.push( Thread.new { statusCheckClient( ) } )
    end

    def quit(sender)
        STDERR.puts "AppDelegate.quit: sending TERM signal to PasteHub server pid=#{@pid}"
        if @pid
            Process.kill( :SIGTERM, @pid )
        end
        app = NSApplication.sharedApplication
        app.terminate(sender)
    end
end
