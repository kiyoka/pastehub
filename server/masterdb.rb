require 'vertx'
require 'json/pure'
require 'cgi'
require 'date'
require 'memcache'

$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'pastehub'
PasteHub::Config.instance.loadServer

# display config info
ins = PasteHub::Config.instance
printf( "Use AWS:                 %s\n", ins.aws )
printf( "Domain:                  %s\n", ins.domain )
printf( "Dynamo   Endpoint:       %s\n", ins.dynamoEp )
printf( "Memcache Endpoint:       %s\n", ins.memcacheEp )

# initialize master database
require 'pastehub/masterdb'


# setup user table for Fake DynamoDB
users = PasteHub::Users.new( )
if not ins.aws
  open( "/var/pastehub/users.tsv", "r" ) {|f|
    f.readlines.each { |line|
      pair = line.chomp.split( /[\t ]+/ )
      printf( "Added local user table:  %s\n", pair[0] )
      users.addUser( pair[0], pair[1] )
    }
  }
end


notifyHash = Memcache.new( :server => PasteHub::Config.instance.memcacheEp )

masterdb_server = Vertx::HttpServer.new
masterdb_server.request_handler do |req|

  req.body_handler do |body|
    util = PasteHub::Util.new
    auth = PasteHub::AuthForServer.new( users )
    ret = auth.invoke( req.headers, util.currentSeconds() )

    if ret[0]
      username = ret[1]
      puts "Connected from user [#{username}]"
    else
      puts "Error: " + ret[1].to_s
      req.response.status_code = 403
      req.response.status_message = "Authorization failure."
      req.response.end
      return
    end

    entries = PasteHub::Entries.new( username )
    users   = PasteHub::Users.new( )

    case req.path
    when "/insertValue"
      data = body.to_s.dup

      # duplicate check
      digest = util.digest( data )
      arr = entries.getList( ).reject{|x| x.match( /^_/ )}
      insertFlag = true
      key = ""
      if 0 < arr.size
        if util.key_digest( arr.first ) == digest
          puts "[#{username}]:insertValue: canceled because data is duplicate. "
          insertFlag = false
          key = arr.first
        end
      end

      if insertFlag
        # update db
        key = util.currentTime( ) + "=" + digest
        puts "[#{username}]:insertValue: key=[#{key}] "
        entries.insertValue( key, data )
        users.touch( username )
        # notify to client
        notifyHash.set( username, key, PasteHub::Config.instance.keyCacheTime )
      end
      req.response.end( key )

    when "/putValue"
      data = body.to_s.dup

      # update db
      key = req.headers[ 'X-Pastehub-Key' ].dup
      puts "[#{username}]:putValue: key=[#{key}] "
      entries.insertValue( key, data )
      users.touch( username )

      # notify to all client
      notifyHash.set( username, key, PasteHub::Config.instance.keyCacheTime )
      req.response.end( key )

    when "/getList"
      limit = req.headers[ 'X-Pastehub-Limit' ]
      if limit
        str = entries.getList( ).take( limit.to_i ).join( "\n" )
      else
        str = entries.getList( ).join( "\n" )
      end
      puts "[#{username}]:getList (#{limit}) response: "
      puts str
      req.response.end( str )

    when "/getValue"
      k = body.to_s.chomp
      if 0 < k.size
        str = entries.getValue( k.dup )
      else
        str = ""
      end
      puts "[#{username}]:getValue:" + k
      req.response.end( str )
    end

  end
end.listen(8000)
