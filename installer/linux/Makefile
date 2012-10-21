#
# This makefile expects Amazon linux platform.
#

VERSION=$(shell ruby1.9 ./getVersion.rb)
BASEDIR=./work/pastehub-${VERSION}/opt/pastehub


all : ./pastehub.spec  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz
	rpmbuild -ba pastehub.spec

~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz	: ${BASEDIR}/bin/PastehubSync
	mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
	tar zcfC  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz ./work pastehub-${VERSION}

${BASEDIR}/bin/PastehubSync : ${BASEDIR}/lib/libz.so.1
	./geminstall.sh ${BASEDIR}

${BASEDIR}/lib/libz.so.1 : ${BASEDIR}/ruby-1.9.3-p194/bin/ruby
	./socopy.sh ${BASEDIR}

${BASEDIR}/ruby-1.9.3-p194/bin/ruby : ruby-1.9.3-p194.tar.gz
	./build.sh ${BASEDIR}

ruby-1.9.3-p194.tar.gz :
	wget http://ftp.ruby-lang.org/pub/ruby/1.9/ruby-1.9.3-p194.tar.gz


clean:
	rm -rf ./work

clean2:
	rm -f ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz
	rm -rf ~/rpmbuild/BUILDROOT/*
