# please use CRuby on Windows
# gem install win32-pipe

require 'win32/pipe'

def main()
  client = Win32::Pipe::Client.new("pastehub_icon")
  
  begin
    i = 0
    while true
      val = client.read
      printf( "%05d: %s\n", i, val )
      i += 1
    end
  rescue SystemCallError => e
    puts "Warning: already disconnected named pipe from server."
  ensure
    client.close
  end
end

main()

