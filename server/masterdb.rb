require 'vertx'
require 'json/pure'
require 'pp'
require 'cgi'
require 'date'
require 'memcache'

$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'libutil'
require 'libdb'

notifyHash = Memcache.new( :server => "localhost:11211" )

masterdb_server = Vertx::HttpServer.new
masterdb_server.request_handler do |req|

  puts "MasterDB: An HTTP request has been received"

  req.body_handler do |body|
    puts "The total body received was #{body.length} bytes, path is #{req.path}."

    query = CGI::parse( req.query )
    username = query[ 'username' ].first

    masterdb = DBSync::MasterDB.new
    masterdb.open( username )

    case req.path
    when "/insertValue"
      kv = JSON::parse( body.to_s )
      pp ["insertValue", kv ]
      
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
      pp ["getList", str ]
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
      pp ["getValue", k, str ]
      masterdb.close()
      req.response.end( str )
    end

  end
end.listen(8081, 'localhost')
