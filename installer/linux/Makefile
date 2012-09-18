

./pastehub-installer.tar.gz	: /opt/pastehub/bin/PastehubSync
	tar zcf ./pastehub-installer.tar.gz /opt/pastehub

/opt/pastehub/bin/PastehubSync : /opt/pastehub/lib/libz.so.1
	./geminstall.sh

/opt/pastehub/lib/libz.so.1 : /opt/pastehub/ruby-1.9.3-p194/bin/ruby
	./socopy.sh

/opt/pastehub/ruby-1.9.3-p194/bin/ruby : ruby-1.9.3-p194.tar.gz
	./build.sh

ruby-1.9.3-p194.tar.gz :
	wget http://ftp.ruby-lang.org/pub/ruby/1.9/ruby-1.9.3-p194.tar.gz

clean:
	rm -rf /opt/pastehub


