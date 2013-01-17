#
# AccountInformationWindow.rb - PasteHub's MacOS X client application.
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
    
    def save()
        crypt = PasteHub::Crypt.new( @password.stringValue )
        open( PasteHub.signfilePath, "w" ) {|f|
            f.puts( @email.stringValue )
            f.puts( crypt.en( @secretKey.stringValue ))
        }
        true
    end

    def auth_and_save()
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
