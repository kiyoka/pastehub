Install client software (Linux)
=======================

# Required platform

- CentOS 6.x   (32bit/64bit)
- Debian 6.x   (32bit/64bit)
- Ubuntu 11_10 or later (32bit/64bit)

# installation instruction

- CentOS

download rpm and install by root account

	# wget https://s3-ap-northeast-1.amazonaws.com/pastehub/release/Linux/pastehub-0.2.2-1.i386.rpm
	# rpm -ihv pastehub-0.2.2-1.i386.rpm

sync start
   
	$ /opt/pastehub/bin/PastehubSync


- Debian or Ubuntu

download deb and install by root account

	# https://s3-ap-northeast-1.amazonaws.com/pastehub/release/Linux/pastehub_0.2.2-2_i386.deb
	# dpkg -i --force-architecture pastehub_0.2.2-2_i386.deb

start to use

	$ /opt/pastehub/bin/PastehubSync 
	Please input your account information
	       email: your@mail.example.com
	  secret-key: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX==
	Please input password for crypted file
	  password            : ********
	  password(for verify): ********
