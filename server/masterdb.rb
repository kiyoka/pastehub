require 'vertx'
require 'json/pure'
require 'cgi'
require 'date'
require 'memcache'

$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'pastehub'
PasteHub::Config.instance.loadServer
LIST_ITEMS           = PasteHub::Config.instance.listItems

notifyHash = Memcache.new( :server => PasteHub::Config.instance.memcacheHost )

masterdb_server = Vertx::HttpServer.new
masterdb_server.request_handler do |req|

  req.body_handler do |body|
    util = PasteHub::Util.new
    auth = PasteHub::AuthForServer.new( PasteHub::Config.instance.dbPath )
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

    masterdb = PasteHub::MasterDB.new( PasteHub::Config.instance.dbPath )
    masterdb.open( username )

    case req.path
    when "/insertValue"
      data = body.to_s.dup

      # update db
      key = util.currentTime( ) + "=" + util.digest( data )
      puts "[#{username}]:insertValue: key=[#{key}] : " + data
      masterdb.insertValue( key, data )
      cur = util.currentTime( )
      masterdb.insertValue( PasteHub::SERVER_DATE_KEY, cur )

      # notify to all client
      notifyHash[ username ] = cur
      masterdb.close()
      req.response.end()

    when "/putValue"
      data = body.to_s.dup

      # update db
      key = req.headers[ 'X-Pastehub-Key' ].dup
      puts "[#{username}]:putValue: key=[#{key}] : " + data
      masterdb.insertValue( key, data )
      cur = util.currentTime( )
      masterdb.insertValue( PasteHub::SERVER_DATE_KEY, cur )

      # notify to all client
      notifyHash[ username ] = cur
      masterdb.close()
      req.response.end()

    when "/getList"
      str = masterdb.getList( ).reject{|x| x.match( /^_/ )}.take( LIST_ITEMS ).join( "\n" )
      puts "[#{username}]:getList's response: "
      puts str
      masterdb.close()
      req.response.end( str )

    when "/getValue"
      k = body.to_s.chomp
      str = masterdb.getValue( k.dup )
      if str
        str
      else
        ""
      end
      puts "[#{username}]:getValue:" + k
      masterdb.close()
      req.response.end( str )
    end

  end
end.listen(8000)
