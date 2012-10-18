#!/bin/bash -x

RUBY=ruby-1.9.3-p194
RUBYPATH=./work/opt/pastehub/${RUBY}
BINPATH=./work/opt/pastehub/bin

${RUBYPATH}/bin/gem install bundler
${RUBYPATH}/bin/bundle
${RUBYPATH}/bin/gem install ../../pkg/pastehub-0.1.5.gem 

mkdir -p ${BINPATH}
/bin/cp ./bin/PastehubSync ${BINPATH}
/bin/cp ./bin/pastehubDump ${BINPATH}
/bin/cp ./bin/pastehubPost ${BINPATH}
