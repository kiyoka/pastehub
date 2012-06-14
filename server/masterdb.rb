require 'vertx'
require 'json/pure'
require 'cgi'
require 'date'
require 'memcache'

$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'synchrobase'

notifyHash = Memcache.new( :server => "localhost:11211" )

masterdb_server = Vertx::HttpServer.new
masterdb_server.request_handler do |req|

  req.body_handler do |body|
    query = CGI::parse( req.query )
    username = query[ 'username' ].first

    masterdb = SynchroBase::MasterDB.new
    masterdb.open( username )

    case req.path
    when "/insertValue"
      kv = JSON::parse( body.to_s )
      puts "[#{username}]:insertValue: " + kv.keys[0]
      
      # update db
      kv.each { |k,v|
        masterdb.insertValue( k.dup, v.dup )
      
        # notify to all client
        notifyHash[ username ] = k
      }
      masterdb.close()
      req.response.end()

    when "/getList"
      str = masterdb.getList( ).join( "\n" )
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
end.listen(8081, 'localhost')
