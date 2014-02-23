#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libsyncentry_spec.rb -  "RSpec file for syncentry.rb"
#
#   Copyright (c) 2012-2012  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
require 'json'
include PasteHub

describe Entry, "When sync entry saved to a file" do

  before do
    ENV[ 'HOME' ] = "/tmp/home/user1"
    @entry = Entry.new( "myhostname" )
  end

  it "should" do
    @entry.save( "paste data 1" ).should    be_true

    open( "/tmp/home/user1/Dropbox/pastehub/myhostname.dat" ) { |f|
      firstline = f.readline( )
      json =JSON.parse( firstline )
      [ json[ 'hostname' ], json[ 'bodySize' ], json[ 'encodedBodySize' ] ]
    }.should == [ 'myhostname', 12, 17 ]

    open( "/tmp/home/user1/Dropbox/pastehub/myhostname.dat" ) { |f|
      firstline  = f.readline( )
      secondline = f.readline( )
      secondline
    }.should == "cGFzdGUgZGF0YSAx\n"
  end
end

describe Entry, "When sync entry loaded from a file" do

  before do
    ENV[ 'HOME' ] = "/tmp/home/user1"
    @entry = Entry.new( "myhostname" )

    #{"create_date":"2014-02-23 17:31:24 +0900","hostname":"myhostname","bodySize":25,"encodedBodySize":37}
    #"bGFyZ2UgcGFzdGUgc3RyaW5nIC4uLi4uLg=="

    open( "/tmp/home/user1/Dropbox/pastehub/myhostname.dat", "w" ) { |f|
      json = JSON.dump( { :hostname => "myhostname", :bodySize => 25, :encodedBodySize => 37 } )
      f.puts json
      f.puts "bGFyZ2UgcGFzdGUgc3RyaW5nIC4uLi4uLg=="
    }
  end

  it "should" do
    expect( @entry.load( )[0][ 'hostname' ] ).to          eq( "myhostname" )
    expect( @entry.load( )[0][ 'bodySize' ] ).to          eq( 25 )
    expect( @entry.load( )[0][ 'encodedBodySize' ] ).to   eq( 37 )
    expect( @entry.load( )[1].should ).to                 eq( "large paste string ......" )
  end

end




