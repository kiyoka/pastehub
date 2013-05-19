#!/usr/local/bin/macruby

require 'socket'

File.unlink("/tmp/sock")

UNIXServer.open("/tmp/sock") {|serv|
  s = serv.accept

  3.times {
    str = s.readline
    p "a message from client #{str}"
  }
  s.close
}
