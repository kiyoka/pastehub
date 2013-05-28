#!/usr/local/bin/macruby

require 'socket'

UNIXSocket.open("/tmp/pastehub_icon") {|c|
  i = 0
  while true
    val = c.readline.chomp 
    printf( "%05d: %s\n", i, val )
    i += 1
  end
}
