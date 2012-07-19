#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libuserdb_spec.rb -  "RSpec file for user database"
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


# setup fake DynamoDB
conf = PasteHub::Config.instance
conf.setupServer( { :memcacheEp         => 'localhost:11211',
                    :domain             => 'rspec' } )
require 'pastehub/masterdb'

describe Users, "User Database API operations are " do

  before do
    @users = Users.new( )

    ###         user,    secretKey
    @userA = [ "userA",  "ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg==" ]
    @userB = [ "userB",  "MjZmZDc5OWVhMDc0OTQ5MTZlOWRhOWI5MWIyYWFjNjQ2Mzk3ZjM0YQ==" ]
    @userC = [ "userC",  "YzlkYTliOTFiMmFhYzY0NjM5N2YzNGEyNmZkNzk5ZWEwNzQ5NDkxNg==" ]
      
  end

  it "should" do
    @users.getList.size().should          == 0

    @users.addUser( @userA[0], @userA[1] ).should be_true
    @users.getList().should                 == [ "userA" ]

    @users.getSecretKey( "userB" )          == nil

    @users.addUser( @userB[0], @userB[1] ).should be_true
    @users.getList().should                 == [ "userA", "userB" ]

    @users.getSecretKey( "userB" )          == @userB[1]

    @users.addUser( @userC[0], @userC[1] ).should be_true
    @users.getList().should                 == [ "userA", "userB", "userC" ]

    @users.getSecretKey( "userA" )          == @userA[1]
    @users.getSecretKey( "userB" )          == @userB[1]
    @users.getSecretKey( "userC" )          == @userC[1]
    @users.getSecretKey( "userD" )          == nil

    @users.updateSecretKey( "userC", "modified!" )  == true

    @users.getList().should                 == [ "userA", "userB", "userC" ]
    @users.getSecretKey( "userA" )          == @userA[1]
    @users.getSecretKey( "userB" )          == @userB[1]
    @users.getSecretKey( "userC" )          == "modified!"
    @users.getSecretKey( "userD" )          == nil

  end
end
