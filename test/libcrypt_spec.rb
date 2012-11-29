#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
#
# libutil_spec.rb -  "RSpec file for util.rb"
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


describe Crypt, "When message encrypt ...  " do

  before do
    @crypt = Crypt.new( "password1234" )
  end

  it "should" do
    @crypt.en( "text data1" ).should   == "X3682kx+c2LzDCk4jPGCRw==\n"
    @crypt.en( "text data2" ).should   == "X3682kx+c2Jrxi70TWzJFg==\n"
    @crypt.en( "abcdefghijklmnopqrstuvwxyz" ).should   == "6oXV2sV9eETLLa9uPEPDJLHj0pwdLCSiHp0foZ3MuXo=\n"
    @crypt.en( "line 1
line 2
line 3
line 4
"
           ).should                                    == "tfvawjcqv9RjMZ0ZnBanz4LSh6B32J5hZIkdpa3Lvdo=\n"
    @crypt.en( "" ).should             == ""
  end
end

describe Crypt, "When message decrypt ...  " do

  before do
    @crypt = Crypt.new( "password1234" )
  end

  it "should" do
    @crypt.de( "X3682kx+c2LzDCk4jPGCRw==\n" ).should   == "text data1"
    @crypt.de( "X3682kx+c2LzDCk4jPGCRw=="   ).should   == "text data1"
    @crypt.de( "X3682kx+c2Jrxi70TWzJFg==\n" ).should   == "text data2"
    @crypt.de( "X3682kx+c2Jrxi70TWzJFg=="   ).should   == "text data2"
    @crypt.de( "6oXV2sV9eETLLa9uPEPDJLHj0pwdLCSiHp0foZ3MuXo=\n" ).should == "abcdefghijklmnopqrstuvwxyz"
    @crypt.de( "tfvawjcqv9RjMZ0ZnBanz4LSh6B32J5hZIkdpa3Lvdo=\n" ).should ==
      "line 1
line 2
line 3
line 4
"

    @crypt.de( "" ).should                             == ""
  end
end

describe Crypt, "When message password is wrong...  " do

  before do
    @crypt = Crypt.new( "badpassword" )
  end

  it "should" do
    @crypt.de( "X3682kx+c2LzDCk4jPGCRw==\n" ).should   == nil
    @crypt.de( "X3682kx+c2LzDCk4jPGCRw=="   ).should   == nil
    @crypt.de( "X3682kx+c2Jrxi70TWzJFg==\n" ).should   == nil
    @crypt.de( "X3682kx+c2Jrxi70TWzJFg=="   ).should   == nil
    @crypt.de( "6oXV2sV9eETLLa9uPEPDJLHj0pwdLCSiHp0foZ3MuXo=\n" ).should == nil
    @crypt.de( "tfvawjcqv9RjMZ0ZnBanz4LSh6B32J5hZIkdpa3Lvdo=\n" ).should == nil
  end
end
