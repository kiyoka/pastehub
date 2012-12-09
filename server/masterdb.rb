require 'vertx'
require 'json/pure'
require 'cgi'
require 'date'
require 'memcache'

$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'pastehub'
require 'pastehub/log'
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

    log = PasteHub::Log.new( :api => req.path, :user => ret[1] )
    if ret[0]
      log.info( "connected" )
    else
      log.error( 'Auth failure:' + ret[2].to_s, { :reason => ret[2].to_s } )
      req.response.status_code = 403
      req.response.status_message = "Authorization failure."
      req.response.end
      return
    end

    entries = PasteHub::Entries.new( username )
    users   = PasteHub::Users.new( )

    case req.path
    when "/authTest"
      req.response.end

    when "/putValue"
      data = body.to_s.dup
      digest = util.digest( data )

      # update db
      key = req.headers[ 'X-Pastehub-Key' ].dup
      puts "[#{username}]:putValue: key=[#{key}] "

      # client have no specified key
      if "_" == key
        key = util.currentTime( ) + "=" + digest
      end

      # data duplicate check
      insertFlag = true
      prevKey = notifyHash.get( username )
      if prevKey
        if util.key_digest( prevKey ) == digest
          log.info( "canceled because data is duplicate. ", { :key => key } )
          insertFlag = false
          key = prevKey
        end
      end

      if insertFlag
        Vertx.set_timer(1000) do
          log.info( "START: delayed job." )
          # update db
          log.info( "insert", { :key => key } )
          entries.insertValue( key, data )
          users.touch( username )
          # notify to client
          notifyHash.set( username, key, PasteHub::Config.instance.keyCacheTime )

          # remove Last entry
          arr = entries.getList( )
          if PasteHub::Config.instance.listItems < arr.size
            entries.deleteValue( arr[arr.size-1] )
          end
          log.info( "END:   delayed job", { :entries => arr.size } )
        end
      end
      req.response.end( key )


    when "/getList"
      limit = req.headers[ 'X-Pastehub-Limit' ]
      if limit
        str = entries.getList( ).take( limit.to_i ).join( "\n" )
      else
        str = entries.getList( ).join( "\n" )
      end
      log.info( "getList", { :limit => limit, :entries => entries.getList( ).size } )
      puts str
      req.response.end( str )

    when "/getValue"
      k = body.to_s.chomp
      if 0 < k.size
        str = entries.getValue( k.dup )
      else
        str = ""
      end
      log.info( "getValue", { :key => k } )
      puts "[#{username}]:getValue:" + k
      req.response.end( str )

    else
      mes = "Error: Unknown API #{req.path}"
      log.error( mes )
      req.response.status_code = 400
      req.response.status_message = mes
      req.response.end

    end
  end
end.listen(8000)
