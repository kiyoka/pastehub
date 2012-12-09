require 'vertx'
require 'cgi'
require 'memcache'
require 'pp'
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
printf( "ssl keystore file:       %s\n", ins.keystore )

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


INTERVAL    = 0.5
POLLING_SEC = 60

notifyHash = Memcache.new( :server => PasteHub::Config.instance.memcacheEp )

def notify( res, str )
  res.write_str( "#{str}\n"  )
end

notifier = Vertx::HttpServer.new
if ins.keystore
  notifier.ssl = true
  notifier.key_store_path     = ins.keystore
  notifier.key_store_password = ins.keystorePassword
end

notifier.request_handler do |req|
  util = PasteHub::Util.new
  auth = PasteHub::AuthForServer.new( users )

  ret = auth.invoke( req.headers, util.currentSeconds() )
  # Now send back a response
  req.response.chunked = true

  log = PasteHub::Log.new( :api => 'notifier', :user => ret[1] )
  if ret[0]
    log.info( "connected" )
  else
    log.error( 'Auth failure:' + ret[2].to_s, { :reason => ret[2].to_s } )
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
        log.info( 'notify', { :notify => got } )
        notify( req.response, got )
      end
    end

    if ( POLLING_SEC < timer_count )
      log.info( 'timeout', { :notify => nil } )
      req.response.end
      Vertx::cancel_timer(timer_id)
    end
  }
end.listen(8001)
