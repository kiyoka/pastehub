# please use CRuby on Windows
# gem install win32-pipe

require 'win32/pipe'

def main()
  client = Win32::Pipe::Client.new("pastehub_icon")
  STDERR.puts "Info: connect to PastehubSync"
  begin
    while true
      val = client.read
      printf( "Info: status=[%s]\n", val )
    end
  rescue SystemCallError => e
    puts "Warning: Disconnected named pipe from server."
  ensure
    client.close
  end
end

main()

