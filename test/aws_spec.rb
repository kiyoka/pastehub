#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# aws_spec.rb -  "RSpec file for using DynamoDB on AWS"
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


if ENV['AMAZON_ACCESS_KEY_ID'] and ENV['AMAZON_SECRET_ACCESS_KEY']
  printf( "Amazon DynamoDB access_key_id:     %s\n", ENV['AMAZON_ACCESS_KEY_ID'])
  printf( "Amazon DynamoDB secret_access_key: %s\n", ENV['AMAZON_SECRET_ACCESS_KEY'])
else
  puts "Please set env-vars for accessing DynamoDB"
  puts "  AMAZON_ACCESS_KEY_ID"
  puts "  AMAZON_SECRET_ACCESS_KEY"
  exit( 1 )
end

conf.setupServer( { :aws                => true,
                    :dynamoAccessKey    => ENV['AMAZON_ACCESS_KEY_ID'],
                    :dynamoSecretKey    => ENV['AMAZON_SECRET_ACCESS_KEY'],
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

    @users.touch( "userA" )                 == true
    end
end



describe Entries, "masterDB API operations are " do

  before do
    @entries = Entries.new( "usertmp" ) # create table
    @util = Util.new

    ###         date,                           value


    @data = [[ "1338738983=06/04/12:00:56:22",  "first  data" ],
             [ "1338814085=06/04/12:21:48:04",  "second data" ]]
  end

  it "should" do
    @entries.getList( ).should == []

    @data.each { |entry|
      date  = entry[0]
      value = entry[1]
      digest = @util.digest( value )
      key = date + "=" + digest
      @entries.insertValue( key, value )
    }

    keys = @entries.getList( )
    keys.should ==
      ["1338814085=06/04/12:21:48:04=e2b6e6c71d8fd8f22b5a96cfc0fe797999405d59",
       "1338738983=06/04/12:00:56:22=30aac3a6f968fc5983a0f62a287e79516d701ea5"]

    @entries.getValue( keys[0] ).should == 'second data'
    @entries.getValue( keys[1] ).should == 'first  data'

    date = "1338814090=06/04/12:21:48:09"
    key = date + "=" + @util.digest( 'last  data' )
    @entries.insertValue( key, 'last  data' )

    keys = @entries.getList( )
    keys.size.should                     == 3
    keys.should                          ==
      ["1338814090=06/04/12:21:48:09=4dbccf6bf4ca71c6d1ec8f08350222c93cb23ebb",
       "1338814085=06/04/12:21:48:04=e2b6e6c71d8fd8f22b5a96cfc0fe797999405d59",
       "1338738983=06/04/12:00:56:22=30aac3a6f968fc5983a0f62a287e79516d701ea5"]

    keys = @entries.getList( 2 )
    keys.size.should                     == 2
    keys.should                          ==
      ["1338814090=06/04/12:21:48:09=4dbccf6bf4ca71c6d1ec8f08350222c93cb23ebb",
       "1338814085=06/04/12:21:48:04=e2b6e6c71d8fd8f22b5a96cfc0fe797999405d59"]

    @entries.getValue( keys[0] ).should == 'last  data'

    @entries.deleteValue( keys[1] ).should == true

    keys = @entries.getList( )
    keys.size.should                     == 2
    keys.should                          ==
      ["1338814090=06/04/12:21:48:09=4dbccf6bf4ca71c6d1ec8f08350222c93cb23ebb",
       "1338738983=06/04/12:00:56:22=30aac3a6f968fc5983a0f62a287e79516d701ea5"]
  end
end
