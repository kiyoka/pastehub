#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libutil_spec.rb -  "RSpec file for util.rb"
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
include PasteHub


describe Util, "When message digest util is called...  " do

  before do
    @util = Util.new
  end

  it "should" do
    expect( @util.digest( "text data1" )).to eq( "f0c62da87f30bff2543cbd44733c17ea9ba84f68" )
    expect( @util.digest( "line 1
line 2
line 3
line 4
"
                  )).to                      eq( "0a95120b8f964aed834e1781898d5243f6878a69" )
    expect( @util.currentTime().match( /^[0-9]+=[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].[0-9][0-9]:[0-9][0-9]:[0-9][0-9]/ )).to be_truthy
  end
end


describe Util, "When use list utils " do

  before do
    @util = Util.new

    @list1 = [ 1,2,3,4,5,6,7,8,9,10 ]
  end

  it "should" do
    expect( @util.takeList( @list1, -2 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.takeList( @list1, -1 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.takeList( @list1,  0 )).to     eq( [                      ] )
    expect( @util.takeList( @list1,  1 )).to     eq( [ 1                    ] )
    expect( @util.takeList( @list1,  2 )).to     eq( [ 1,2                  ] )
    expect( @util.takeList( @list1,  3 )).to     eq( [ 1,2,3                ] )
    expect( @util.takeList( @list1,  8 )).to     eq( [ 1,2,3,4,5,6,7,8      ] )
    expect( @util.takeList( @list1,  9 )).to     eq( [ 1,2,3,4,5,6,7,8,9    ] )
    expect( @util.takeList( @list1, 10 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.takeList( @list1, 11 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.dropList( @list1, -2 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.dropList( @list1, -1 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.dropList( @list1,  0 )).to     eq( [ 1,2,3,4,5,6,7,8,9,10 ] )
    expect( @util.dropList( @list1,  1 )).to     eq( [   2,3,4,5,6,7,8,9,10 ] )
    expect( @util.dropList( @list1,  2 )).to     eq( [     3,4,5,6,7,8,9,10 ] )
    expect( @util.dropList( @list1,  9 )).to     eq( [                   10 ] )
    expect( @util.dropList( @list1, 10 )).to     eq( [                      ] )
    expect( @util.dropList( @list1, 11 )).to     eq( [                      ] )
  end
end


describe Util, "When master's diffList is larger " do

  before do
    @util = Util.new

    @masterList = [ "a", "b", "c", "d" ]
    @localList  = [      "b", "c"      ]
  end

  it "should" do
    expect( @util.diffList( @masterList, @localList  )).to  eq( [ "a", "d" ] )
    expect( @util.diffList( @localList,  @masterList )).to  eq( [ ] )
  end
end

describe Util, "When local's diffList is larger " do

  before do
    @util = Util.new

    @masterList = [      "2",      "4" ]
    @localList  = [ "1", "2", "3", "4" ]
  end

  it "should" do
    expect( @util.diffList( @masterList, @localList  )).to  eq( [ ] )
    expect( @util.diffList( @localList,  @masterList )).to  eq( [ "1", "3" ] )
  end
end

describe Util, "When differs in first 5 entries " do

  before do
    @util = Util.new

    @masterList = [      "2", "3",      "5", "6", "7", "8", "9", "10", "11", "12", "13", "7", "8", "9", "10", "11", "12", "13" ]
    @localList  = [ "1",      "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13" ]
  end

  it "should" do
    expect( @util.diffList( @masterList, @localList  )).to  eq( [ "2" ] )
    expect( @util.diffList( @localList,  @masterList )).to  eq( [ "1", "4" ] )
  end
end


describe Util, "When string util is called...  " do

  before do
    @util = Util.new
  end

  it "should" do
    expect( @util.stringLimit( '123456789A', 5 )).to      eq( '12345...' )
    expect( @util.stringLimit( '123456789ABCD', 10 )).to  eq( '123456789A...' )
    expect( @util.stringLimit( '1', 2 )).to               eq( '1' )
    expect( @util.stringLimit( '12', 2 )).to              eq( '12' )
    expect( @util.stringLimit( '123', 2 )).to             eq( '12...' )

    expect( @util.pulloutURL( 'abc' )).to                 be_nil
    expect( @util.pulloutURL( 'this is normal text' )).to be_nil
    expect( @util.pulloutURL( 'http://localhost/dir1/dir2/a.html' )).to               eq( 'http://localhost/dir1/dir2/a.html' )
    expect( @util.pulloutURL( 'aaa http://localhost/dir1/dir2/a.html bbb' )).to       eq( 'http://localhost/dir1/dir2/a.html' )
    expect( @util.pulloutURL( 'aaa https://localhost/dir1/dir2/a.html bbb' )).to      eq( 'https://localhost/dir1/dir2/a.html' )
    expect( @util.pulloutURL( 'ahttp://localhost/dir1/dir2/a.html'  )).to             be_nil
    expect( @util.pulloutURL( 'ahttps://localhost/dir1/dir2/a.html' )).to             be_nil
    expect( @util.pulloutURL( 'ahttp://localhost/dir1/dir2/a.html '  )).to            be_nil
    expect( @util.pulloutURL( 'ahttps://localhost/dir1/dir2/a.html ' )).to            be_nil
    expect( @util.pulloutURL( 'http://localhost/dir1/dir2/a.html bbb' )).to          eq( 'http://localhost/dir1/dir2/a.html' )
    expect( @util.pulloutURL( 'https://localhost/dir1/dir2/a.html bbb' )).to         eq( 'https://localhost/dir1/dir2/a.html' )
  end
end
