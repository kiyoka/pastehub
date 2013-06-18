#
# This makefile expects MacOS X Lion.
#

RUBYNAME=ruby-1.9.3-p429
ROOTDIR=/opt/pastehub/

all :  ${BASEDIR}/${RUBYNAME}/bin/ruby
	echo "nothing to do"

#~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz	: ${BASEDIR}/${RUBYNAME}/bin/ruby
#	./gemcopy.sh ${BASEDIR}
#	./socopy.sh ${BASEDIR}
#	mkdir -p ${BASEDIR}/bin
#	cp -f ./bin/pastehubDump  ${BASEDIR}/bin
#	cp -f ./bin/pastehubPost  ${BASEDIR}/bin
#	cp -f ./bin/PastehubSync  ${BASEDIR}/bin
#	mkdir -p ${BASEDIR}/certs
#	cp -f /etc/pki/tls/certs/ca-bundle.crt ${BASEDIR}/certs
#	mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
#	tar zcfC  ~/rpmbuild/SOURCES/pastehub-${VERSION}.tar.gz ./work pastehub-${VERSION}

${BASEDIR}/${RUBYNAME}/bin/ruby : ${RUBYNAME}.tar.gz
	./build.sh ${RUBYNAME}

${RUBYNAME}.tar.gz :
	wget http://ftp.ruby-lang.org/pub/ruby/1.9/${RUBYNAME}.tar.gz

clean:
	rm -rf ./work

