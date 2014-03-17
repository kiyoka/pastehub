#
# notification_center.rb - NotificationCenter plugin for MacOS X
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
  class NotificationCenter < PluginBase
    def initialize()
      @enable = false
      case RbConfig::CONFIG['host_os']
      when /^darwin/
        # MacOS X
        @notifier_path = RbConfig::CONFIG['bindir'] + "/" + "terminal-notifier"
        if File.exist?( @notifier_path )
          STDERR.puts( "Info: found terminal-notifier for MacOS X." )
          @enable = true
        end
      end
    end

    def newly_arrived(message,max_length)
      if @enable
        str = if max_length < message.size
                message[0...max_length] + " ..."
              else
                message
              end
        url_option = ''
        util = PasteHub::Util.new
        if util.pulloutURL( str )
          url_option = '-open \'' + util.pulloutURL( str ) + '\''
        end
        command = sprintf( "%s -title 'PasteHub' -message '\\%s' %s", @notifier_path, str, url_option )
        system( command )
      end
      nil
    end
  end

  Plugin.register_plugin(NotificationCenter.new)
end
