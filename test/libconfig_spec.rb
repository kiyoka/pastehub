#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libconfig_spec.rb -  "RSpec file for config.rb"
#
#   Copyright (c) 2012-2014  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
    expect( @config.localDbPath ).to   eq( File.expand_path( "~/.pastehub/" ) + "/" )
    expect( @config.localSyncPath ).to eq( File.expand_path( "~/Dropbox/pastehub/" ) + "/" )
  end
end


describe PasteHub::Config, "When on/off verboser mode...  " do

  before do
    @config = PasteHub::Config.instance
  end

  it "should" do
    expect( @config.getVerbose ).to           be_false
    expect( @config.setVerbose( true )).to    be_true
    expect( @config.getVerbose ).to           be_true
  end
end

describe PasteHub::Config, "When use config object...  " do

  before do
    @config = PasteHub::Config.instance
    @config.setupClient( { :localDbPath        => "/tmp/local/",
                           :localSyncPath      => "/tmp/pastehub_sync/" } )
  end

  it "should" do
    expect( @config.localDbPath ).to   eq( "/tmp/local/" )
    expect( @config.localSyncPath ).to eq( "/tmp/pastehub_sync/" )
  end
end


describe PasteHub::Config, "When use config object...  " do

  before do
    @config = PasteHub::Config.instance
    @config.setupClient( { :localDbPath        => "/tmp/tmp/tmp/",
                           :localSyncPath      => "/tmp/tmp/tmp/pastehub_sync/" } )
  end

  it "should" do
    expect( @config.localDbPath ).to   eq( "/tmp/tmp/tmp/" )
    expect( @config.localSyncPath ).to eq( "/tmp/tmp/tmp/pastehub_sync/" )
  end
end
