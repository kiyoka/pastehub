#
# This makefile expects MacOS X Lion.
#

RUBYNAME=ruby-1.9.3-p429
ROOTDIR=/opt/pastehub

all: ${ROOTDIR}/bin/PastehubSync
	echo "nothing to do"

${ROOTDIR}/bin/PastehubSync : ${ROOTDIR}/${RUBYNAME}/bin/ruby
	${ROOTDIR}/${RUBYNAME}/bin/gem install   ../../pastehub-server*.gem
	mkdir -p ${ROOTDIR}/bin
	ln -sf ${ROOTDIR}/${RUBYNAME}/bin/PastehubSync  ${ROOTDIR}/bin/PastehubSync
	ln -sf ${ROOTDIR}/${RUBYNAME}/bin/pastehubPost  ${ROOTDIR}/bin/pastehubPost
	ln -sf ${ROOTDIR}/${RUBYNAME}/bin/pastehubDump  ${ROOTDIR}/bin/pastehubDmp

${ROOTDIR}/${RUBYNAME}/bin/ruby : ${RUBYNAME}.tar.gz
	./build.sh ${RUBYNAME}

${RUBYNAME}.tar.gz :
	wget http://ftp.ruby-lang.org/pub/ruby/1.9/${RUBYNAME}.tar.gz

chown:
	sudo chown -R root /opt/pastehub

clean:
	rm -rf ./work

