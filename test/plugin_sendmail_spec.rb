#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# plugin_sendmail_spec.rb - "RSpec file for SendMail plugin"
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
require 'pastehub'
require 'pastehub/plugin/sendmail'

describe PasteHub::SendMail, "when sendmail plugin exec... " do
  before do
    ENV[ 'PASTEHUB_MAIL0' ] = "ever,myaddresss_ever@example.com"
    ENV[ 'PASTEHUB_MAIL1' ] = "onenote,myaddress_onenote@example.com"
    ENV[ 'PASTEHUB_MAIL2' ] = "other,mail_aaa-123abc@example.com"
    ENV[ 'PASTEHUB_MAIL3' ] = "Other,MAIL_aaa-123ABC@example.com"
    config = PasteHub::Config.instance
    config.reinitialize

    @sendmail = PasteHub::SendMail.new
    @sendmail.mail_command = nil
  end
  
  it "should" do
    expect( @sendmail.mail_settings.size    ).to     eq( 4 )
    expect( @sendmail.mail_settings[0].to_s ).to     eq( "ever,myaddresss_ever@example.com" )
    expect( @sendmail.mail_settings[1].to_s ).to     eq( "onenote,myaddress_onenote@example.com" )
    expect( @sendmail.mail_settings[2].to_s ).to     eq( "other,mail_aaa-123abc@example.com" )
    expect( @sendmail.mail_settings[3].to_s ).to     eq( "Other,MAIL_aaa-123ABC@example.com" )
    
    expect( @sendmail.sendmail?( "#ever message1" )).to     eq( "myaddresss_ever@example.com" )
    expect( @sendmail.sendmail?( "aaa #ever bbb" )).to      eq( "myaddresss_ever@example.com" )
    expect( @sendmail.sendmail?( "message1 #ever" )).to     eq( "myaddresss_ever@example.com" )
    expect( @sendmail.sendmail?( "#eve  message1" )).to     be_nil
    expect( @sendmail.sendmail?( "#onenote message1" )).to  eq( "myaddress_onenote@example.com")
    expect( @sendmail.sendmail?( "#other   message2" )).to  eq( "mail_aaa-123abc@example.com" )
    expect( @sendmail.sendmail?( "#others  message2" )).to  be_nil
    expect( @sendmail.sendmail?( "#Other   message3" )).to  eq( "MAIL_aaa-123ABC@example.com" )
    expect( @sendmail.sendmail?( "#OtheR   message3" )).to  be_nil

    expect( @sendmail.newly_arrived("#ever message1") ).to  eq( "| myaddresss_ever@example.com -i -s '=?utf-8?B?I2V2ZXIgbWVzc2FnZTE=?='" )
    expect( @sendmail.newly_arrived("#ever Evernoteへの新規ノート作成テスト
1行目
2行目
") ).to  eq( "| myaddresss_ever@example.com -i -s '=?utf-8?B?I2V2ZXIgRXZlcm5vdGXjgbjjga7mlrDopo/jg47jg7zjg4jkvZzmiJDjg4bjgrnjg4g=?='" )

  end
  
end
