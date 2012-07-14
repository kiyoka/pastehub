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
require 'pastehub'
include PasteHub

# setup fake DynamoDB
conf = PasteHub::Config.instance
conf.setupServer( false, 'localhost:11211', 'rspec' )
require 'pastehub/masterdb'


describe Auth, "When client auth library is used (OK)...  " do

  before do
    @secretKey = 'ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg=='
    @auth = Auth.new
  end

  it "should" do
    @auth.addElement( 'x-pastehub-username', 'userA'      )
    @auth.calcSignature( @secretKey ).should                        == :NotEnoughKeys
    @auth.addElement( 'x-pastehub-date',     '1339639491' )
    @auth.calcSignature( @secretKey ).should                        == :NotEnoughKeys
    @auth.addElement( 'x-pastehub-version',  '2012-06-16' )
    @auth.calcSignature( @secretKey ).should                        == "Nzvcsfnkbu/mlM+r5/YaZ676j08WiPikoS7KrpZf/RM="

    authForClient = AuthForClient.new( 'userA', @secretKey )
    authForClient._addElement( 'x-pastehub-date', '10000000' )
    authForClient.getAuthHash( ).should == {
      "x-pastehub-username"=>"userA",
      "x-pastehub-date"=>"10000000",
      "x-pastehub-version"=>"2012-06-16",
      "authorization"=>"JdBdwsmcJ7jIy3EQ0lX5MjlKUprYump10UDxr0fxnRA="
    }
    authForClient.username.should == "userA"
  end
end

describe Auth, "When server auth library is used ...  " do
  before do
    @auth = Auth.new

    @userA = [ "userA",  "ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg==" ]
    users = Users.new( )
    users.addUser( @userA[0], @userA[1] )
    @authForServer = AuthForServer.new( users )

    @ok_signature  = "JdBdwsmcJ7jIy3EQ0lX5MjlKUprYump10UDxr0fxnRA="
  end

  it "should" do
    @auth.addElement( 'x-pastehub-username', 'userA' )
    @auth.addElement( 'x-pastehub-date',     '10000000' )
    @auth.addElement( 'x-pastehub-version',  '2012-06-16' )
    @auth.calcSignature( @userA[1] ).should                       == @ok_signature


    @authForServer.invoke( {
                             "x-pastehub-username"=>"userA",
                             "x-pastehub-date"=>"10000000",
                             "x-pastehub-version"=>"2012-06-16",
                             "authorization"=>"JdBdwsmcJ7jIy3EQ0lX5MjlKUprYump10UDxr0fxnRA="
                           }, 10000000
                           ).should  == [ true, "userA" ]

    @auth.addElement( 'x-pastehub-username', 'unknownUser' )
    @auth.addElement( 'x-pastehub-date',     '10000000' )
    @auth.addElement( 'x-pastehub-version',  '2012-06-16' )
    @auth.calcSignature( @userA[1] ).should_not                   == @ok_signature

    @authForServer.invoke( {
                             'x-pastehub-username' => 'unknownUser',
                             "x-pastehub-date"=>"10000000",
                             'x-pastehub-version'=>'2012-06-16'
                           }, 10000000
                           ).should  == [ false, :unknown_user ]

    @authForServer.invoke( {
                             "x-pastehub-username"=>"userA",
                             "x-pastehub-date"=>"10000000",
                             "x-pastehub-version"=>"2012-06-16",
                             "authorization"=>"XXXXXXXXXXXX"
                           }, 10000000
                           ).should  == [ false, :illegal_signature ]
  end
end

describe Auth, "When server auth library invokes time expire check ...  " do
  before do
    @userA = [ "userA",  "ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg==" ]
    users = Users.new( )
    users.addUser( @userA[0], @userA[1] )
    @authForServer = AuthForServer.new( users )

    @headers = {
      "x-pastehub-username"=>"userA",
      "x-pastehub-date"=>"1339639491",
      "x-pastehub-version"=>"2012-06-16",
      "authorization"=>"Nzvcsfnkbu/mlM+r5/YaZ676j08WiPikoS7KrpZf/RM="
    }
  end

  it "should" do
    @authForServer.invoke( @headers, 1339639491            ).should          == [ true,  "userA" ]
    # after  1 minutes
    @authForServer.invoke( @headers, 1339639491 + 60       ).should          == [ true,  "userA" ]
    # after  6 minutes
    @authForServer.invoke( @headers, 1339639491 + (60 * 6) ).should          == [ false, :expired_client_request ]
    # before 6 minutes
    @authForServer.invoke( @headers, 1339639491 - (60 * 6) ).should          == [ false, :expired_client_request ]
  end
end
