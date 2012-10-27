#
# This makefile expects Amazon linux platform.
#

VERSION=$(shell ruby ./getVersion.rb)
ROOTDIR=./work/pastehub-${VERSION}
BASEDIR=./work/pastehub-${VERSION}/opt/pastehub


all : ./pastehub.spec  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz
	rpmbuild -ba pastehub.spec

~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz	: ${BASEDIR}/ruby-1.9.3-p194/bin/ruby
	./gemcopy.sh ${BASEDIR}
	./socopy.sh ${BASEDIR}
	mkdir -p ${BASEDIR}/bin
	cp -f ./bin/pastehubDump  ${BASEDIR}/bin
	cp -f ./bin/pastehubPost  ${BASEDIR}/bin
	cp -f ./bin/PastehubSync  ${BASEDIR}/bin
	mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
	tar zcfC  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz ./work pastehub-${VERSION}

${BASEDIR}/ruby-1.9.3-p194/bin/ruby : ruby-1.9.3-p194.tar.gz
	./build.sh ${ROOTDIR}

ruby-1.9.3-p194.tar.gz :
	wget http://ftp.ruby-lang.org/pub/ruby/1.9/ruby-1.9.3-p194.tar.gz


clean:
	rm -rf ./work

clean2:
	rm -f ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz
	rm -rf ~/rpmbuild/BUILDROOT/*

deb:
	fakeroot alien -v --to-deb --scripts pastehub-${VERSION}-1.i386.rpm

