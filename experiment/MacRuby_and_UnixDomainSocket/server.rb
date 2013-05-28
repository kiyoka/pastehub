#!/usr/local/bin/macruby

require 'socket'

File.unlink("/tmp/pastehub_icon")

UNIXServer.open("/tmp/pastehub_icon") {|serv|
  s = serv.accept

  10.times { |n|
    str = s.puts n
    sleep 1
  }
  s.close
}
