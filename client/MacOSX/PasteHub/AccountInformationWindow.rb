#
#  PasswordController.rb
#  PasteHub
#
#  Created by Kiyoka Nishiyama on 2013.01.10.
#  Copyright 2013 Kiyoka Nishiyama All rights reserved.
#
        
require 'rubygems'
require 'pastehub'

class AccountInformationWindow < NSWindow

    attr_accessor :email, :secretKey, :password
    attr_accessor :signin_button, :ok_button
    attr_accessor :success_view
    
    def awakeFromNib
        username = PasteHub.loadUsername
        
        if not File.exist?( PasteHub.signfilePath )
            step1()
        else
            step2()
        end
    end
    
    def signin_button_pushed(sender)
        email     = @email.stringValue
        secretKey = @secretKey.stringValue

        auth = PasteHub::AuthForClient.new( email, secretKey )
        client = PasteHub::Client.new( auth )
        if client.authTest()
            @saveOnly = true
            step2()
        else
            p "auth Error(1)"
        end
    end

    def step1()
        p "step1()"
        @password.setEnabled      false
        @ok_button.setEnabled     false
        
        select_the_field( @email )
    end

    def step2()
        p "step2()"
        @email.setEnabled         false
        @secretKey.setEnabled     false
        @signin_button.setEnabled false
        
        @password.setEnabled      true
        @ok_button.setEnabled     true

        select_the_field( @password )
    end
    
    def select_the_field( textObject )
        textObject.selectText self
        range = NSRange.new(textObject.stringValue.length, 0)
        textObject.currentEditor.setSelectedRange range
    end

    def auth_and_save
        success = false
        
        signfile = PasteHub.signfilePath
        if not File.exist?( signfile )
            p "Error : Can't open file [#{signfile}]"
        else
            open( signfile ) {|f|
                username = f.readline.chomp
                signStr = f.read
        
                crypt = PasteHub::Crypt.new( @password.stringValue )
                secretKey = crypt.de( signStr )
                if secretKey
                    auth = PasteHub::AuthForClient.new( username, secretKey )
                    client = PasteHub::Client.new( auth )
                    if client.authTest()
                        p "auth Success"
                        @email.setStringValue      username
                        @secretKey.setStringValue  secretKey
                        success = true
                    else
                        p "auth Error(2)"
                    end
                end
            }
        end
        success
    end

    def ok_button_pushed(sender)
        success = if @saveOnly
            save()
        else
            auth_and_save()
        end
    
        if success
            @password.setEnabled  false
            @ok_button.setEnabled false

            @success_view.setHidden false

            # You must not send a auth information as notify object! for security reason.
            NSNotificationCenter.defaultCenter.postNotificationName("auth_complete", object:nil)
            p "send notify 'auth_complete'"    
        end
    end
end
