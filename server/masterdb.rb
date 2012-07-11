require 'vertx'
require 'json/pure'
require 'cgi'
require 'date'
require 'memcache'

$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'pastehub'
PasteHub::Config.instance.loadServer
LIST_ITEMS           = PasteHub::Config.instance.listItems


# initialize master database
require 'pastehub/masterdb'


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

    entries = PasteHub::Entries.new( username )

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
        puts "[#{username}]:insertValue: key=[#{key}] : " + data
        entries.insertValue( key, data )
        cur = util.currentTime( )
        entries.insertValue( PasteHub::SERVER_DATE_KEY, cur )

        # notify to client
        notifyHash[ username ] = cur
      end

      req.response.end( key )

    when "/putValue"
      data = body.to_s.dup

      # update db
      key = req.headers[ 'X-Pastehub-Key' ].dup
      puts "[#{username}]:putValue: key=[#{key}] : " + data
      entries.insertValue( key, data )
      cur = util.currentTime( )
      entries.insertValue( PasteHub::SERVER_DATE_KEY, cur )

      # notify to all client
      notifyHash[ username ] = cur
      req.response.end()

    when "/getList"
      str = entries.getList( ).reject{|x| x.match( /^_/ )}.take( LIST_ITEMS ).join( "\n" )
      puts "[#{username}]:getList's response: "
      puts str
      req.response.end( str )

    when "/getValue"
      k = body.to_s.chomp
      str = entries.getValue( k.dup )
      if str
        str
      else
        ""
      end
      puts "[#{username}]:getValue:" + k
      req.response.end( str )
    end

  end
end.listen(8000)
