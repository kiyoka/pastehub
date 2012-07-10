
require 'dynamoid'
require 'pp'

# fake DynamoDB on localhost
Dynamoid.configure do |config|
  config.adapter         = 'local'
  config.namespace       = 'pastehub_org'   # To namespace tables created by Dynamoid from other tables you might have.
  config.warn_on_scan    = true          # Output a warning to the logger when you perform a scan rather than a query on a table.
  config.partitioning    = false          # Spread writes randomly across the database. See "partitioning" below for more.
  config.partition_size  = 1       # Determine the key space size that writes are randomly spread across.
  config.read_capacity   = 10         # Read capacity for your tables
  config.write_capacity  = 5         # Write capacity for your tables
  #config.endpoint = 'dynamodb.us-east-1.amazonaws.com' # Set the regional endpoint
end



module PasteHub

  class DynamoidConfig
    def self.config( domain_name = 'pastehub_org', access_key = nil, secret_key = nil )
      if access_key
        # real DyanamoDB on AWS
        Dynamoid.configure do |config|
          config.adapter = 'local'
          config.namespace = domain_name # To namespace tables created by Dynamoid from other tables you might have.
          config.warn_on_scan = true     # Output a warning to the logger when you perform a scan rather than a query on a table.
          config.partitioning = true     # Spread writes randomly across the database. See "partitioning" below for more.
          config.partition_size = 200    # Determine the key space size that writes are randomly spread across.
          config.read_capacity = 50      # Read capacity for your tables
          config.write_capacity = 5      # Write capacity for your tables
          config.endpoint = 'dynamodb.us-east-1.amazonaws.com' # Set the regional endpoint
        end
      else
        # fake DynamoDB on localhost
        Dynamoid.configure do |config|
          config.adapter = 'local'
          config.namespace = domain_name # To namespace tables created by Dynamoid from other tables you might have.
          config.warn_on_scan = true     # Output a warning to the logger when you perform a scan rather than a query on a table.
          config.partitioning = true     # Spread writes randomly across the database. See "partitioning" below for more.
          #config.partition_size = 200   # Determine the key space size that writes are randomly spread across.
          #config.read_capacity = 100    # Read capacity for your tables
          #config.write_capacity = 20    # Write capacity for your tables
          #config.endpoint = 'dynamodb.us-east-1.amazonaws.com' # Set the regional endpoint
        end
      end
    end
  end


  class User
    include Dynamoid::Document

    table :name => :users, :key => :username

    field :username
    field :secretKey
    field :created_datetime, :datetime
    field :touched_datetime, :datetime
    field :email
    field :twitter_account
    #field :friends, :set

    index :username
    index :email
    
    validates_presence_of :username
    validates_format_of :email, :with => /@/
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
