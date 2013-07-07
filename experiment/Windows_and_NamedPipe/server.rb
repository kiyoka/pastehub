# please use CRuby on Windows
# gem install win32-pipe

require 'win32/pipe'

serv = Win32::Pipe::Server.new("pastehub_icon")
serv.connect

10.times { |n|
  serv.write n.to_s
  printf( "send[%s]\n", n.to_s )
  sleep 1
}
serv.close
