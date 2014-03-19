#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# plugin_dropbox_todo_spec.rb - "RSpec file for Dropbox todo plugin"
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
require 'pastehub/plugin/dropbox_todo'

describe PasteHub::DropboxTodo, "when todo plugin exec... " do
  before do
    ENV[ 'HOME' ] = "/tmp/home/user1"
    config = PasteHub::Config.instance
    config.reinitialize

    @todo = PasteHub::DropboxTodo.new
    @path = '/tmp/home/user1/Dropbox/Public/pastehub_todo/'
  end
  
  it "should" do
    expect( @todo.todo?("message1") ).to                          be_false
    expect( @todo.todo?("#todomessage1") ).to                     be_false
    expect( @todo.todo?("#todo message1") ).to                    eq( "message1" )
    expect( @todo.todo?("message1#todo") ).to                     be_false
    expect( @todo.todo?("message2 #todo") ).to                    eq( "message2" )
    expect( @todo.todo?("message3 #todo #todo") ).to              eq( "message3" )
    expect( @todo.todo?("日本語TODO #todo #todo") ).to            eq( "日本語TODO" )
    expect( @todo.todo?("with white space. #todo #todo") ).to     eq( "with white space." )
    expect( @todo.todo?("with  white  space . #todo #todo") ).to  eq( "with  white  space ." )

    expect( @todo.newly_arrived("message1") ).to       be_nil
    expect( File.exist?( @path + "message1.txt" )).to  be_false
    
    expect( @todo.newly_arrived("message2 #todo") ).to    be_nil
    expect( File.exist?( @path + "message2.txt" )).to     be_true

    expect( @todo.newly_arrived("#todo message3") ).to    be_nil
    expect( File.exist?( @path + "message3.txt" )).to     be_true

    expect( @todo.newly_arrived("#todo 日本語TODO") ).to  be_nil
    expect( File.exist?( @path + "日本語TODO.txt" )).to   be_true

    expect( @todo.newly_arrived("#todo with white spaces.") ).to  be_nil
    expect( File.exist?( @path + "with white spaces..txt" )).to   be_true

    expect( @todo.newly_arrived("#todo  with  white  spaces .") ).to  be_nil
    expect( File.exist?( @path + " with  white  spaces ..txt" )).to   be_true

    expect( @todo.newly_arrived("#todo dies ist todo-Stück") ).to  be_nil
    expect( File.exist?( @path + "dies ist todo-Stück.txt" )).to   be_true
  end
  
end
