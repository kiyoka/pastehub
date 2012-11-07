#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libconfig_spec.rb -  "RSpec file for config.rb"
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

describe PasteHub::Config, "When use config object...  " do

  before do
    @config = PasteHub::Config.instance
  end

  it "should" do
    @config.aws.should                 == false
    @config.awsWarn.should             == false
    @config.dynamoEp.should            == 'dynamodb.ap-northeast-1.amazonaws.com'
    @config.dynamoAccessKey.should     == 'xxxx'
    @config.dynamoSecretKey.should     == 'xxxx'
    @config.memcacheEp.should          == "localhost:11211"
    @config.keyCacheTime.should        == 24 * 3600
    @config.targetApiHost.should       == "pastehub.net:8000"
    @config.targetNotifierHost.should  == "pastehub.net:8001"
    @config.localDbPath.should         == File.expand_path( "~/.pastehub/" ) + "/"
    @config.scheme.should              == "https"
  end
end


describe PasteHub::Config, "When use config object...  " do

  before do
    @config = PasteHub::Config.instance
    @config.setupServer( { :dynamoEp           => 'dynamodb.xxxxxxx.amazonaws.com',
                           :dynamoAccessKey    => 'accessKey',
                           :dynamoSecretKey    => 'secretKey',
                           :memcacheEp         => 'memcache.example.com:11211',
                           :keyCacheTime       => 10 } )
    @config.setupClient( { :targetApiHost      => "localhost:8000",
                           :targetNotifierHost => "localhost:8001",
                           :localDbPath        => "/tmp/local/",
                           :scheme             => "http" } )
  end

  it "should" do
    @config.aws.should                 == false
    @config.dynamoEp.should            == 'dynamodb.xxxxxxx.amazonaws.com'
    @config.dynamoAccessKey.should     == 'accessKey'
    @config.dynamoSecretKey.should     == 'secretKey'
    @config.memcacheEp.should          == "memcache.example.com:11211"
    @config.keyCacheTime.should        == 10
    @config.domain                     == "localhost"
    @config.targetApiHost.should       == "localhost:8000"
    @config.targetNotifierHost.should  == "localhost:8001"
    @config.localDbPath.should         == "/tmp/local/"
    @config.scheme.should              == "http"
  end
end

describe PasteHub::Config, "When use config object...  " do

  before do
    @config = PasteHub::Config.instance
    @config.setupServer( { :aws                => true,
                           :awsWarn            => true,
                           :dynamoEp           => nil,
                           :dynamoAccessKey    => 'a',
                           :dynamoSecretKey    => 'b',
                           :memcacheEp         => "memcache.example.com:11211",
                           :keyCacheTime       => 1000,
                           :domain             => "domain.example.com" } )
    @config.setupClient( { :targetApiHost      => "host.example.com:8000",
                           :targetNotifierHost => "host.example.com:8001",
                           :localDbPath        => "/tmp/tmp/tmp",
                           :scheme             => "ftp" } )
  end

  it "should" do
    @config.aws.should                 == true
    @config.awsWarn.should             == true
    @config.dynamoEp.should            == 'dynamodb.ap-northeast-1.amazonaws.com'
    @config.dynamoAccessKey.should     == 'a'
    @config.dynamoSecretKey.should     == 'b'
    @config.memcacheEp.should          == "memcache.example.com:11211"
    @config.keyCacheTime.should        == 1000
    @config.domain                     == "domain.example.com"
    @config.targetApiHost.should       == "host.example.com:8000"
    @config.targetNotifierHost.should  == "host.example.com:8001"
    @config.localDbPath.should         == "/tmp/tmp/tmp"
    @config.scheme.should              == "ftp"
  end
end
