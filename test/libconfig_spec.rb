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
    @config.targetApiURL.should        == "https://pastehub.net/api/"
    @config.targetNotifierURL.should   == "https://pastehub.net:8001/"
    @config.localDbPath.should         == File.expand_path( "~/.pastehub/" ) + "/"
    @config.keystore.should               be_nil
    @config.keystorePassword.should    == "password"
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
    @config.setupClient( { :targetApiURL       => "http://localhost:8000",
                           :targetNotifierURL  => "http://localhost:8001",
                           :localDbPath        => "/tmp/local/" } )
  end

  it "should" do
    @config.aws.should                 == false
    @config.dynamoEp.should            == 'dynamodb.xxxxxxx.amazonaws.com'
    @config.dynamoAccessKey.should     == 'accessKey'
    @config.dynamoSecretKey.should     == 'secretKey'
    @config.memcacheEp.should          == "memcache.example.com:11211"
    @config.keyCacheTime.should        == 10
    @config.domain                     == "localhost"
    @config.targetApiURL.should        == "http://localhost:8000/"
    @config.targetNotifierURL.should   == "http://localhost:8001/"
    @config.localDbPath.should         == "/tmp/local/"
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
                           :domain             => "domain.example.com",
                           :keystore           => "/etc/keystore/keystore.jks",
                           :keystorePassword   => "long_long_password" } )
    @config.setupClient( { :targetApiURL       => "http://host.example.com:8000/abc/",
                           :targetNotifierURL  => "http://host.example.com:8001/def",
                           :localDbPath        => "/tmp/tmp/tmp" } )
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
    @config.keystore                   == "/etc/keystore/keystore.jks"
    @config.keystorePassword           == "long_long_password"
    @config.targetApiURL.should        == "http://host.example.com:8000/abc/"
    @config.targetNotifierURL.should   == "http://host.example.com:8001/def/"
    @config.localDbPath.should         == "/tmp/tmp/tmp"
  end
end
