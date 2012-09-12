#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libstore_spec.rb -  "RSpec file for store.rb"
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


describe LocalStore, "when LocalStore API use ... " do

  before do
    # Create dbm file
    @store1 = LocalStore.new( "usertmp" )
    @store1.insertValue( "key1", "value1" )
    @store1.close

    @store1 = LocalStore.new( "usertmp", true )
  end

  it "should" do
    store2 = LocalStore.new( "usertmp", true )
    store2.should_not nil
    store2.close.should_not nil
  end

  after do
    @store1.close
  end

end


describe LocalStore, "when LocalStore API use ... " do

  before do
    @store1 = LocalStore.new( "usertmp" )
    @store1.clear
  end

  it "should" do
    @store1.insertValue( "key1", "value1" ).should_not   == nil
    @store1.getList().should                             == [ "key1" ]
    @store1.top().should                                 == [ "key1", "value1" ]
    @store1.insertValue( "key2", "value2" ).should_not   == [ nil, nil ]
    @store1.getList().should                             == [ "key2", "key1" ]
    @store1.top().should                                 == [ "key2", "value2" ]
    @store1.insertValue( "key3", "value3" ).should_not   == [ nil, nil ]
    @store1.getList().should                             == [ "key3", "key2", "key1" ]
    @store1.top().should                                 == [ "key3", "value3" ]

    @store1.latest().should                                 == [ "false", "false" ]

    @store1.insertValue( PasteHub::ONLINE_STATE_KEY, "1" )
    @store1.getList().should                                == [ "key3", "key2", "key1" ]
    @store1.latest().should                                 == [ "false", "false" ]

    @store1.insertValue( PasteHub::LOCAL_DATE_KEY, "local_date" )
    @store1.getList().should                                == [ "key3", "key2", "key1" ]
    @store1.latest().should                                 == [ "false", "local_date" ]

    @store1.insertValue( PasteHub::SERVER_DATE_KEY, "server_date" )
    @store1.getList().should                                == [ "key3", "key2", "key1" ]
    @store1.latest().should                                 == [ "server_date", "local_date" ]

  end

end
