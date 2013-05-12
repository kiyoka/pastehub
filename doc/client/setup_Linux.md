Install client software (Linux)
=======================

## Required platform

- CentOS 6.x   (32bit/64bit)
- Debian 6.x   (32bit/64bit)
- Ubuntu 11_10 or later (32bit/64bit)

## installation instruction

- CentOS

download rpm and install by root account

	# wget http://s3-ap-northeast-1.amazonaws.com/pastehub/release/Linux/pastehub-0.2.5-1.i386.rpm
	# rpm -ihv pastehub-0.2.5-1.i386.rpm

sync start ( on [gnu screen](http://www.gnu.org/software/screen/) or [tmux](http://tmux.sourceforge.net/) )


	$ /opt/pastehub/bin/PastehubSync


- Debian or Ubuntu

prepare 32bit emulation environment by root account

	# aptitude install ia32-libs

download deb and install by root account

	# wget http://s3-ap-northeast-1.amazonaws.com/pastehub/release/Linux/pastehub_0.2.5-2_i386.deb
	# dpkg -i --force-architecture pastehub_0.2.5-2_i386.deb

sync start ( on [gnu screen](http://www.gnu.org/software/screen/) or [tmux](http://tmux.sourceforge.net/) )

	$ /opt/pastehub/bin/PastehubSync 
	Please input your account information
	       email: your@mail.example.com
	  secret-key: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX==
	Please input password for crypted file
	  password            : ********
	  password(for verify): ********

## History

+ 0.2.5 Support hourly GC of local database(gdbm).
+ 0.2.2 First release.
