require 'dynamoid'
require 'date'

module PasteHub

  # fake DynamoDB on localhost
  Dynamoid.configure do |dynamoConfig|
    config = PasteHub::Config.instance
    dynamoConfig.adapter         = config.aws ?     'aws_sdk' : 'local'
    dynamoConfig.namespace       = config.domain            # To namespace tables created by Dynamoid from other tables you might have.
    dynamoConfig.warn_on_scan    = !config.aws              # Output a warning to the logger when you perform a scan rather than a query on a table.
    dynamoConfig.partitioning    = false                    # Spread writes randomly across the database. See "partitioning" below for more.
    dynamoConfig.partition_size  = 1                        # Determine the key space size that writes are randomly spread across.
    dynamoConfig.read_capacity   = 10                       # Read capacity for your tables
    dynamoConfig.write_capacity  = 5                        # Write capacity for your tables
    dynamoConfig.access_key      = config.dynamoAccessKey   # If connecting to DynamoDB, your access key is required.
    dynamoConfig.secret_key      = config.dynamoSecretKey   # So is your secret key.
    dynamoConfig.endpoint        = config.dynamoEp          # Set the regional endpoint
  end

  class User
    include Dynamoid::Document

    table :name => :users

    field :username
    field :secretKey
    field :created_datetime, :datetime
    field :touched_datetime, :datetime
    field :twitter_account
    #field :friends, :set

    index :username
    
    validates_presence_of :username
  end

  class Entry
    include Dynamoid::Document

    table :name => :Entries

    field :userkey
    field :username
    field :comment
    field :from
    field :to
    field :cc
    field :type
    field :delete, :integer
    field :delete_datetime, :datetime
    field :data

    index :username
    index [:username, :userkey, :delete]
    index :delete_datetime, :range => true
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

    def getList()
      arr = User.all
      arr.map { |x| x.username }
    end
  end


  class Entries
    def initialize( username )
      @holdUsername = username
    end

    def getList( limit = nil )
      arr = Entry.where( :username => @holdUsername, :delete => 0 ).map {|x|
        x.userkey
      }.sort {|a,b| -(a <=> b) }
      if limit
        arr.take( limit )
      else
        arr
      end
    end

    def getValue( key, fallback = false )
      arr = Entry.where( :username => @holdUsername, :userkey => key, :delete => 0 ).all
      if 0 < arr.size
        arr[0].data.force_encoding("UTF-8")
      else
        fallback
      end
    end

    def insertValue( key, value )
      entry = Entry.new( :username => @holdUsername, :userkey => key.force_encoding("ASCII-8BIT"), :data => value.force_encoding("ASCII-8BIT"), :delete => 0 )
      entry.save
    end

    def deleteValue( key )
      entry = Entry.where( :username => @holdUsername, :userkey => key, :delete => 0 ).first
#      pp [ "entry1", entry ]
#      pp [ "table_name", Entry.table_name ]
#      pp [ "id", entry.id ]
      entry.delete = 1
      entry.save
#      pp [ "entry2", entry ]
      true
    end
  end
end
