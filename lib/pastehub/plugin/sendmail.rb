#
# sendmail.rb - Plugin: send clipboard message by e-mail.
#  
#   Copyright (c) 2014-2014  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
require 'pastehub/plugin_base'

module PasteHub
  class MailSetting
    attr_accessor :tag
    attr_accessor :mail

    def to_s()
      return @tag + "," + @mail
    end
  end

  class SendMail < PluginBase
    attr_reader   :mail_settings
    attr_accessor :mail_command

    def initialize()
      @mail_command = "mail"
      # load env variables
      @mail_settings = 10.times.collect {|n|
        mail_setting = MailSetting.new
        val = ENV[ sprintf("PASTEHUB_MAIL%d", n) ]
        if val
          if val.match(/^[a-zA-Z]+[,][a-zA-Z0-9_.-]+[@].+$/)
            arr = val.split(/,/)
            mail_setting.tag  = arr[0]
            mail_setting.mail = arr[1]
            mail_setting
          else
            nil
          end
        else
          nil
        end
      }.select( ) {|x| x}
    end

    def display_config
      # display settings
      @mail_settings.each { |setting|
        STDERR.printf( "Info: SendMail-plugin   tag:[%s] mail[%s]\n",
                       setting.tag,
                       setting.mail )
      }
    end

    def sendmail?(message)
      arr = mail_settings.collect { |setting|
        tag_exp = sprintf("(^|[ ])[#]%s([ ]|$)", setting.tag)
        regexp = Regexp.new(tag_exp)
        if regexp.match(message)
          setting.mail
        else
          nil
        end
      }.select( ) {|x| x}
      if 0 < arr.size
        arr[0]
      else
        nil
      end
    end

    def encode_subject(subject)
      enc_str = Base64.strict_encode64(subject)
      "=?utf-8?B?" + enc_str + "?="
    end

    def newly_arrived(message)
      mailaddress = sendmail?(message)
      lines = message.split( /\n/ )
      if mailaddress
        command = sprintf( "|%s %s -i -s '%s'", @mail_command, mailaddress, encode_subject(lines[0]))
        if @mail_command
          p @mail_command
          open( command, "w" ) {|f|
            f.print message
          }
          STDERR.printf( "Info: send mail [%s]\n", command )
          nil
        else
          command
        end
      else
        nil
      end
    end
  end

  Plugin.register_plugin(SendMail.new)
end
