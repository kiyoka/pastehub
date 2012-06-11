#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libutil_spec.rb -  "RSpec file for libutil"
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
require 'libdb'
require 'libutil'
include DBSync


describe Util, "When message digest util is called...  " do

  before do
    @util = Util.new
  end

  it "should" do
    @util.digest( "text data1" ).should   == "f0c62da87f30bff2543cbd44733c17ea9ba84f68"
    @util.digest( "line 1
line 2
line 3
line 4
"
                  ).should                == "0a95120b8f964aed834e1781898d5243f6878a69"
  end
end


describe Util, "When key util is called...  " do

  before do
    @util = Util.new

    @key1 = "1338738983=06/04/12:00:56:22=f0c62da87f30bff2543cbd44733c17ea9ba84f68"
    @key2 = "1338814085=06/04/12:21:48:04=0a95120b8f964aed834e1781898d5243f6878a69"
  end

  it "should" do
    @util.key_seconds( @key1 ).should    == 1338738983
    @util.key_timestamp( @key1 ).should  == '06/04/12:00:56:22'
    @util.key_digest( @key1 ).should     == 'f0c62da87f30bff2543cbd44733c17ea9ba84f68'
  end
end


describe Util, "When diffList util is called...  " do

  before do
    @util = Util.new

    @masterList = [ "a", "b", "c", "d" ]
    @localList  = [      "b", "c"      ] 
  end

  it "should" do
    @util.diffList( @masterList, @localList ) == [ "a", "d" ]
  end
end
