require 'vertx'
require 'cgi'
require 'memcache'
require 'pp'
$LOAD_PATH.push( File.dirname(__FILE__) + "/../lib" )
require 'synchrobase'

# use http://en.wikipedia.org/wiki/Chunked_transfer_encoding

INTERVAL    = 0.5
POLLING_SEC = 60

notifyHash = Memcache.new( :server => "localhost:11211" )

def notify( res, str )
  res.write_str( "#{str}\n"  )
end

notifier = Vertx::HttpServer.new
notifier.request_handler do |req|
  util = SynchroBase::Util.new
  auth = SynchroBase::AuthForServer.new( "/var/synchrobase/" )

  ret = auth.invoke( req.headers, util.currentSeconds() )
  # Now send back a response
  req.response.chunked = true

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

  got         = nil
  timer_count = 0

  Vertx::set_periodic (1000 * INTERVAL) { |timer_id|
    timer_count += INTERVAL
    if notifyHash[ username ]
      if got != notifyHash[ username ]
        got = notifyHash[ username ]
        notify( req.response, got )
      end
    end

    if ( POLLING_SEC < timer_count )
      req.response.end
      Vertx::cancel_timer(timer_id)
    end
  }
end.listen(8080, 'localhost')
