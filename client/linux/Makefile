#
# This makefile expects Amazon linux platform.
#

VERSION=$(shell ruby ./getVersion.rb)
ROOTDIR=./work/pastehub-${VERSION}
BASEDIR=./work/pastehub-${VERSION}/opt/pastehub
RUBYNAME=ruby-1.9.3-p374

all : pastehub.spec  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz
	rpmbuild -ba pastehub.spec

pastehub.spec : pastehub.spec.in
	echo -n "Version: "  >   pastehub.spec
	echo ${VERSION}      >>  pastehub.spec
	cat pastehub.spec.in >>  pastehub.spec


~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz	: ${BASEDIR}/${RUBYNAME}/bin/ruby
	./gemcopy.sh ${BASEDIR}
	./socopy.sh ${BASEDIR}
	mkdir -p ${BASEDIR}/bin
	cp -f ./bin/pastehubDump  ${BASEDIR}/bin
	cp -f ./bin/pastehubPost  ${BASEDIR}/bin
	cp -f ./bin/PastehubSync  ${BASEDIR}/bin
	mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
	tar zcfC  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz ./work pastehub-${VERSION}

${BASEDIR}/${RUBYNAME}/bin/ruby : ${RUBYNAME}.tar.gz
	./build.sh ${RUBYNAME} ${ROOTDIR}

${RUBYNAME}.tar.gz :
	wget http://ftp.ruby-lang.org/pub/ruby/1.9/${RUBYNAME}.tar.gz


clean:
	rm -f pastehub.spec
	rm -rf ./work

clean2:
	rm -f pastehub.spec
	rm -f ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz
	rm -rf ~/rpmbuild/BUILDROOT/*

deb:
	fakeroot alien -v --to-deb --scripts pastehub-${VERSION}-1.i386.rpm

