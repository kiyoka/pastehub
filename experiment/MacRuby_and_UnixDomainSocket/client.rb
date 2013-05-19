#!/usr/local/bin/macruby

require 'socket'

UNIXSocket.open("/tmp/sock") {|c|
  c.send "I am CLIENT!(1)\n", 0
  sleep 2
  c.send "I am CLIENT!(2)\n", 0
  sleep 2
  c.send "I am CLIENT!(3)\n", 0
}
