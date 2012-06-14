#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libauth_spec.rb -  "RSpec file for auth.rb"
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
require 'synchrobase'
include SynchroBase


describe Auth, "When auth libraries used (OK)...  " do

  before do
    @auth = Auth.new( 'ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg==' )
    @auth.currentTime( 1339639491 )
  end

  it "should" do
    @auth.addElement( 'x-synchrobase-username', 'userA' )
    @auth.calcSignature( ).should                                    == :NotEnoughKeys
    @auth.addElement( 'x-synchrobase-date',     '1339639491' )
    @auth.calcSignature( ).should                                    == :NotEnoughKeys
    @auth.addElement( 'x-synchrobase-version',  '2012-06-16' )
    @auth.calcSignature( ).should                                    == "BucjEIGUhZkSUy+w3m4h9W+mOJx4zfj/XAo6kroNTpM="
  end
end

describe Auth, "When auth libraries used (Illegal access)...  " do
  before do
    @auth = Auth.new( 'ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg==' )
    @auth.currentTime( 1339639491 )
  end

  it "should" do
    @auth.addElement( 'x-synchrobase-username', 'unknownUser' )
    @auth.addElement( 'x-synchrobase-date',     '1339639491' )
    @auth.addElement( 'x-synchrobase-version',  '2012-06-16' )
    @auth.calcSignature( ).should_not                                == "BucjEIGUhZkSUy+w3m4h9W+mOJx4zfj/XAo6kroNTpM="
  end
end

describe Auth, "When auth libraries used (time expired)...  " do
  before do
    @auth = Auth.new( 'ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg==' )
  end

  it "should" do
    @auth.currentTime( 1339639491 + 60 )       # after 1 minutes

    @auth.addElement( 'x-synchrobase-username', 'userA' )
    @auth.addElement( 'x-synchrobase-date',     '1339639491' )
    @auth.addElement( 'x-synchrobase-version',  '2012-06-16' )
    @auth.calcSignature( ).should                                    == "BucjEIGUhZkSUy+w3m4h9W+mOJx4zfj/XAo6kroNTpM="

    @auth.currentTime( 1339639491 + (60 * 6) ) # after 6 minutes

    @auth.addElement( 'x-synchrobase-username', 'userA' )
    @auth.addElement( 'x-synchrobase-date',     '1339639491' )
    @auth.addElement( 'x-synchrobase-version',  '2012-06-16' )
    @auth.calcSignature( ).should                                    == :Expired

    @auth.currentTime( 1339639491 - (60 * 6) ) # before 6 minutes

    @auth.addElement( 'x-synchrobase-username', 'userA' )
    @auth.addElement( 'x-synchrobase-date',     '1339639491' )
    @auth.addElement( 'x-synchrobase-version',  '2012-06-16' )
    @auth.calcSignature( ).should                                    == :Expired
  end
end
