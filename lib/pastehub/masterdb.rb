require 'dynamoid'
require 'date'

module PasteHub

  # fake DynamoDB on localhost
  Dynamoid.configure do |dynamoConfig|
    config = PasteHub::Config.instance
    dynamoConfig.adapter         = config.aws ?     'aws_sdk' : 'local'
    dynamoConfig.namespace       = config.domain            # To namespace tables created by Dynamoid from other tables you might have.
    dynamoConfig.warn_on_scan    = config.awsWarn           # Output a warning to the logger when you perform a scan rather than a query on a table.
    dynamoConfig.partitioning    = false                    # Spread writes randomly across the database. See "partitioning" below for more.
    dynamoConfig.partition_size  = 1                        # Determine the key space size that writes are randomly spread across.
    dynamoConfig.read_capacity   = 5                        # Read capacity for your tables
    dynamoConfig.write_capacity  = 5                        # Write capacity for your tables
    dynamoConfig.access_key      = config.dynamoAccessKey   # If connecting to DynamoDB, your access key is required.
    dynamoConfig.secret_key      = config.dynamoSecretKey   # So is your secret key.
    dynamoConfig.endpoint        = config.dynamoEp          # Set the regional endpoint
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
    field :delete, :integer
    field :delete_datetime, :datetime
    field :data

    index [:username, :delete]

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
  end


  class Entries
    def initialize( username )
      @holdUsername = username
    end

    def getList( limit = nil )
      arr = Entry.where( :username => @holdUsername, :delete => 0 ).map {|x|
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
                    :data => value.force_encoding("UTF-8"), :delete => 0 )
      entry.save
    end

    def deleteValue( key )
      # caution: This method is non consistent read.
      entry = Entry.find( @holdUsername + "::" + key )
      if entry
        entry.delete = 1
        entry.data = entry.data.force_encoding("UTF-8")
        entry.save
        true
      else
        false
      end
    end
  end
end
