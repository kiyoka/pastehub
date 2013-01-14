#
#  AppDelegate.rb
#  PasteHub
#
#  Created by Kiyoka Nishiyama on 12/12/31.
#  Copyright 2012 Kiyoka Nishiyama. All rights reserved.
#

require 'rubygems'
require 'pastehub'


class AppDelegate
    
    attr_accessor :accountInfo

    def applicationDidFinishLaunching(a_notification)
        puts "start!"
        # save pid file
        PasteHub.savePid( Process.pid )

        # regist observer for "auth_complete"
        NSNotificationCenter.defaultCenter.addObserver(self,
                                                       selector:"respond_to_auth_complete:",
                                                       name:"auth_complete", object:nil)
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
        @threads = []
        @threads.push(Thread.new { clientSync.syncMain(    email, secretKey, password ) })
        @threads.push(Thread.new { clientSync.macosxCheck( email, secretKey, password ) })
    end
end
