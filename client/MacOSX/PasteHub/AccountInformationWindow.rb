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

class AccountInformationWindow < NSWindow

    attr_accessor :emailLabel, :secretKeyLabel
    attr_accessor :email, :secretKey, :password
    attr_accessor :signin_button
    attr_accessor :message_area
 
    SIGNFILEPATH = File.expand_path( "~/.pastehub/authinfo.txt" )

    def awakeFromNib
        if not File.exist?( SIGNFILEPATH )
            firstSignIn()
        else
            signIn()
        end
    end
    
    def signin_button_pushed(sender)
        email     = @email.stringValue
        secretKey = @secretKey.stringValue

        if not File.exist?( SIGNFILEPATH )
            if email.size < 1
                @message_area.setStringValue "Please input your email address."
                @message_area.textColor = NSColor.redColor
                return
            elsif secretKey.size < 1
                @message_area.setStringValue "Please input your secretKey."
                @message_area.textColor = NSColor.redColor
                return
            end
        end

        if @password.stringValue.size < 8
            @message_area.setStringValue "A password must be at least eight characters."
            @message_area.textColor = NSColor.redColor
            p "less than 9 chars"
            return
        end
        
        if not File.exist?( SIGNFILEPATH )
            if auth3( @password.stringValue, @email.stringValue, @secretKey.stringValue )
                @message_area.setStringValue "Success! please close window."
                @message_area.textColor = NSColor.blackColor
            else
                @message_area.setStringValue "email or secretkey or Password do not match."
                @message_area.textColor = NSColor.redColor
                return
            end
        else
            if auth1( @password.stringValue )
                @message_area.setStringValue "Success! please close window."
                @message_area.textColor = NSColor.blackColor
            else
                @message_area.setStringValue "Password do not match."
                @message_area.textColor = NSColor.redColor
                return
            end
        end
        
        # You must not send a auth information as notify object! for security reason.
        NSNotificationCenter.defaultCenter.postNotificationName("auth_complete", object:nil)
        p "send notify 'auth_complete'"
        
        @password.setEnabled      false
        @signin_button.setEnabled false
    end

    def firstSignIn()
        p "firstSignIn"
        @message_area.setEnabled  true
        select_the_field( @email )
    end

    def signIn()
        p "signIn()"
        @emailLabel.setTextColor      NSColor.grayColor
        @secretKeyLabel.setTextColor  NSColor.grayColor
        @email.setEnabled             false
        @secretKey.setEnabled         false
        @password.setEnabled          true

        select_the_field( @password )
    end
    
    def select_the_field( textObject )
        textObject.selectText self
        range = NSRange.new(textObject.stringValue.length, 0)
        textObject.currentEditor.setSelectedRange range
    end
    
    def auth1( password )
        ret = false
        begin
            IO.popen( "/usr/local/bin/PastehubSync auth", "r+" ) { |io|
                io.puts password
                @pid = io.pid
                line = io.readline.chomp
                STDERR.puts "Info: auth1: result line = [#{line}]"
                if "OK" == line
                    ret = true
                end
            }
        rescue e
            STDERR.puts "Error: missing auth1"
        end
        return ret
    end
    
    def auth3( password, email, secretKey )
        ret = false
        begin
            IO.popen( "/usr/local/bin/PastehubSync saveauth", "r+" ) { |io|
                io.puts password
                io.puts email
                io.puts secretKey
                @pid = io.pid
                line = io.readline.chomp
                STDERR.puts "Info: auth3: result line = [#{line}]"
                if "OK" == line
                    ret = true
                end
            }
            rescue e
            STDERR.puts "Error: missing auth3"
        end
        return ret
    end
end
