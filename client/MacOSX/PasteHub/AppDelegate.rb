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
require 'rubygems'
require 'pastehub'


class AppDelegate
    
    attr_accessor :accountInfo

    def applicationDidFinishLaunching(a_notification)
        # save pid file
        PasteHub.savePid( Process.pid )

        # regist observer for "auth_complete"
        NSNotificationCenter.defaultCenter.addObserver(self,
                                                       selector:"respond_to_auth_complete:",
                                                       name:"auth_complete", object:nil)
    end
    
    def notifyCountUp()
        @notify_count += 1
        status =
        case @notify_count
        when 1
            :one
        when 2
            :two
        when 3
            :three
        else
            :threeplus
        end
        NSNotificationCenter.defaultCenter.postNotificationName("change_status", object:nil,
                                                                userInfo: {:status => status})
        puts '<< COUNTUP >>'
    end
    
    def notifyConnect()
        @notify_count = 0
        NSNotificationCenter.defaultCenter.postNotificationName("change_status", object:nil,
                                                                userInfo: {:status => :checked})
        puts '<< ONLINE >>'
    end

    def notifyDisconnect()
        @notify_count = 0
        NSNotificationCenter.defaultCenter.postNotificationName("change_status", object:nil,
                                                                userInfo: {:status => :normal})
        puts '<< offline >>'
    end
    
    def respond_to_auth_complete(a_notification)
        p "AppDelegate:respond_to_auth_complete"
        p "AppDelegate:email     = " + accountInfo.email.stringValue

        email     = accountInfo.email.stringValue
        secretKey = accountInfo.secretKey.stringValue
        password  = accountInfo.password.stringValue

        # create clientSync
        clientSync = PasteHub::ClientSync.new(
                                                PasteHub::Config.instance.listItems / 2,
                                                PasteHub::Config.instance.localDbPath,
                                                0.5 )
        @notify_count = 0
        
        clientSync.addNoitfyCallback( lambda { notifyCountUp() }, lambda { notifyConnect() }, lambda { notifyDisconnect() } )
        
        notifyConnect()
        
        @threads = []
        @threads.push(Thread.new { clientSync.syncMain(    email, secretKey, password ) })
        @threads.push(Thread.new { clientSync.macosxCheck( email, secretKey, password ) })
    end
end
