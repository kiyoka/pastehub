#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libclientsync_spec.rb -  "RSpec file for clientsync.rb"
#
#   Copyright (c) 2012-2014  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
include PasteHub

describe ClientSync, "when file check function use ... " do
  before do
    ENV[ 'HOME' ] = "/tmp/home/user1"
    PasteHub::Config.instance
    PasteHub.setupDirectory( )

    @cs = ClientSync.new( "myhostname",
                          1              ) # interval time
  end
  
  it "should" do
    expect( File.exist?( "/tmp/home/user1/.pastehub" )).to        be true
    expect( File.exist?( "/tmp/home/user1/Dropbox/pastehub" )).to be true
    
    expect( @cs.get_other_hostfiles).to eq([])
  end
end


describe ClientSync, "when paste from localhost " do
  before do
    @cs = ClientSync.new( "myhostname",
                          1              ) # interval time
    sleep 2
    open( "/tmp/home/user1/Dropbox/pastehub/myhostname.dat", "w" ) { |f|
      f.write( "abc" );
    }
  end
  
  it "should" do
    expect( @cs.get_other_hostfiles ).to    eq( [] )
    expect( @cs.exist_sync_data? ).to       be_nil
  end
end


describe ClientSync, "when paste data comes from other host " do
  before do
    @cs = ClientSync.new( "myhostname",
                          1              ) # interval time
    sleep 2
    open( "/tmp/home/user1/Dropbox/pastehub/otherhostname.dat", "w" ) { |f|
      f.write( "def" );
    }
  end
  
  it "should" do
    expect( @cs.get_other_hostfiles ).to    eq( [ "/tmp/home/user1/Dropbox/pastehub/otherhostname.dat" ] )
    expect( @cs.exist_sync_data? ).to       eq( "/tmp/home/user1/Dropbox/pastehub/otherhostname.dat" )
    expect( @cs.path_to_hostname( "/tmp/home/user1/Dropbox/pastehub/otherhostname.dat" )).to    eq( "otherhostname" )
    
  end
end


