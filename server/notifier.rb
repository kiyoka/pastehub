require 'vertx'
require 'cgi'
require 'memcache'

# use http://en.wikipedia.org/wiki/Chunked_transfer_encoding

INTERVAL    = 0.5
POLLING_SEC = 60

notifyHash = Memcache.new( :server => "localhost:11211" )

def notify( res, str )
  res.write_str( "#{str}\n"  )
end

notifier = Vertx::HttpServer.new
notifier.request_handler do |req|
  query = CGI::parse( req.query )
  username = query[ 'username' ].first
  puts "Connected from user [#{username}]"

  # Now send back a response
  req.response.chunked = true
  
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
