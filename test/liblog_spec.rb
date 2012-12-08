#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# liblog_spec.rb -  "RSpec file for log.rb"
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

require 'pastehub/log'

describe Log, "When Log class is used ...  " do

  before do
    @log_for_userA   = Log.new( { :api => 'api_a',     :user => 'userA' } )
    @log_for_userB   = Log.new( { :api => 'authTest',  :user => 'userB' } )
    @log_for_unknown = Log.new( { :api => 'api_u'                       } )
  end

  it "should" do
    @log_for_userA.info(    'RSpec test(info) '         ).should   == true
    @log_for_userB.error(   'RSpec test(error)'         ).should   == true
    @log_for_unknown.info(  'RSpec test(unknown.info)'  ).should   == true
    @log_for_unknown.error( 'RSpec test(unknown.error)' ).should   == true
  end
end

describe Log, "When log use in illegal manner ...  " do

  it "should" do
    lambda { Log.new( )                                        }.should      raise_error(ArgumentError)
    lambda { Log.new( { :api  => 'api1'  } )                   }.should_not  raise_error
    lambda { Log.new( { :user => 'userA' } )                   }.should      raise_error(ArgumentError)
    lambda { Log.new( { :aaa  => 'userA' } )                   }.should      raise_error(ArgumentError)
    lambda { Log.new( { :api  => 'api_a', :user => 'userA' } ) }.should_not  raise_error
  end
end
