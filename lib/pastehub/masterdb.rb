#
# masterdb.rb - PasteHub's master database manager
#  
#   Copyright (c) 2009-2011  Kiyoka Nishiyama  <kiyoka@sumibi.org>
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
#
require 'dynamoid'
require 'date'

module PasteHub

  # fake DynamoDB on localhost
  Dynamoid.configure do |conf|
    config = PasteHub::Config.instance
    conf.adapter         = 'aws_sdk'
    conf.namespace       = config.domain              # To namespace tables created by Dynamoid from other tables you might have.
    conf.warn_on_scan    = config.awsWarn             # Output a warning to the logger when you perform a scan rather than a query on a table.
    conf.partitioning    = false                      # Spread writes randomly across the database. See "partitioning" below for more.
    conf.partition_size  = 1                          # Determine the key space size that writes are randomly spread across.
    conf.read_capacity   = 1                          # Read capacity for your tables
    conf.write_capacity  = 1                          # Write capacity for your tables
    conf.access_key      = config.dynamoAccessKey     # If connecting to DynamoDB, your access key is required.
    conf.secret_key      = config.dynamoSecretKey     # So is your secret key.
    conf.endpoint        = config.dynamoEp            # Set the regional endpoint
    conf.port            = '443'                      # Use real dynamo
    if not config.aws
      conf.endpoint      = 'localhost'                # Set the regional endpoint
      conf.use_ssl       = false                      # Don't use SSL
      conf.port          = '4567'                     # Use fake_dynamo
    end
  end

  class User
    include Dynamoid::Document

    table :name => :users, :key => :username

    field :username
    field :secretKey
    field :created_datetime, :datetime
    field :touched_datetime, :datetime
    #field :friends, :set

#    index :username

    validates_presence_of :username
    validates_presence_of :secretKey
  end

  class Entry
    include Dynamoid::Document

    table :name => :entries, :key => :userkey

    field :userkey
    field :username
#    field :comment
#    field :from
#    field :to
#    field :cc
#    field :type
    field :data

    index [:username]

    validates_presence_of :userkey
#    validates_presence_of :data

    def id
      userkey
    end
  end


  class Users
    def addUser( username, secretKey )
      user = User.new( :username => username, :secretKey => secretKey, :created_datetime => DateTime.new())
      user.save
    end

    def updateSecretKey( username, secretKey )
      arr = User.where( :username => username ).all
      if 0 < arr.size
        arr.first.secretKey = secretKey
        arr.first.save
        true
      else
        false
      end
    end

    def getSecretKey( username )
      arr = User.where( :username => username ).all
      if 0 < arr.size
        arr.first.secretKey
      else
        nil
      end
    end

    def touch( username )
      arr = User.where( :username => username ).all
      if 0 < arr.size
        arr.first.touched_datetime = DateTime.new()
        arr.first.save
        true
      else
        nil
      end
    end

    def getList()
      arr = User.all
      arr.map { |x| x.username }.sort
    end

    def __deleteUser( username )
      user = User.find( username )
      if user
        user.delete
        true
      else
        false
      end
    end

  end


  class Entries
    def initialize( username )
      @holdUsername = username
    end

    def getList( limit = nil )
      arr = Entry.where( :username => @holdUsername ).map {|x|
        # remove username from `userkey'
        field = x.userkey.split( /::/ )
        field[1]
      }.sort {|a,b| -(a <=> b) }
      if limit
        arr.take( limit )
      else
        arr
      end
    end

    def getValue( key, fallback = false )
      p "username=#{@holdUsername}"
      p "key=#{key}"
      entry = Entry.find( @holdUsername + "::" + key, :consistent_read => true )
      if entry
        if entry.data
          entry.data.force_encoding("UTF-8")
        end
      else
        fallback
      end
    end

    def insertValue( key, value )
      entry = Entry.new( :username => @holdUsername,
                    :userkey => @holdUsername + "::" + key,
                    :data => value.force_encoding("UTF-8") )
      entry.save
    end

    def deleteLast( )
      arr = getList( )
      if 0 < arr.size
        deleteValue( arr[arr.size-1] )
      end
    end

    def deleteValue( key )
      # caution: This method is non consistent read.
      entry = Entry.find( @holdUsername + "::" + key )
      if entry
        entry.delete
        true
      else
        false
      end
    end
  end
end
