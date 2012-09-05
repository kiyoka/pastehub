#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libmasterdb_spec.rb -  "RSpec file for masterDB"
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

describe Entries, "masterDB API operations are " do

  before do
    @entries = Entries.new( "usertmp" )
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


    date = "1338814090=09/04/10:00:00:00"
    key = date + "=" + @util.digest( '10' )
    @entries.insertValue( key, '10' )

    date = "1338814090=09/04/11:00:00:00"
    key = date + "=" + @util.digest( '11' )
    @entries.insertValue( key, '11' )

    date = "1338814090=09/04/12:00:00:00"
    key = date + "=" + @util.digest( '12' )
    @entries.insertValue( key, '12' )

    keys = @entries.getList( )
    keys.size.should                     == 5
    keys.should                          ==
      ["1338814090=09/04/12:00:00:00=7b52009b64fd0a2a49e6d8a939753077792b0554",
       "1338814090=09/04/11:00:00:00=17ba0791499db908433b80f37c5fbc89b870084b",
       "1338814090=09/04/10:00:00:00=b1d5781111d84f7b3fe45a0852e59758cd7a87e5",
       "1338814090=06/04/12:21:48:09=4dbccf6bf4ca71c6d1ec8f08350222c93cb23ebb",
       "1338738983=06/04/12:00:56:22=30aac3a6f968fc5983a0f62a287e79516d701ea5"]

    @entries.deleteLast( )
    @entries.getList( )                  ==
      ["1338814090=09/04/12:00:00:00=7b52009b64fd0a2a49e6d8a939753077792b0554",
       "1338814090=09/04/11:00:00:00=17ba0791499db908433b80f37c5fbc89b870084b",
       "1338814090=09/04/10:00:00:00=b1d5781111d84f7b3fe45a0852e59758cd7a87e5",
       "1338814090=06/04/12:21:48:09=4dbccf6bf4ca71c6d1ec8f08350222c93cb23ebb"]

    @entries.deleteLast( )
    @entries.getList( )                  ==
      ["1338814090=09/04/12:00:00:00=7b52009b64fd0a2a49e6d8a939753077792b0554",
       "1338814090=09/04/11:00:00:00=17ba0791499db908433b80f37c5fbc89b870084b",
       "1338814090=09/04/10:00:00:00=b1d5781111d84f7b3fe45a0852e59758cd7a87e5"]

    @entries.deleteLast( )
    @entries.deleteLast( )
    @entries.deleteLast( )
    @entries.getList( )                  == []
  end
end
