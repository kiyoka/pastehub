#!/bin/bash -x

RUBY=ruby-1.9.3-p194
TARGET=`pwd`/${1}/${RUBY}

rm -rf ./work
rm -rf ${TARGET}
mkdir -p work
pushd .

  cd work
  tar zxf ../${RUBY}.tar.gz
  cd ./${RUBY}
  ./configure --prefix=${TARGET}
  make
  make install

popd
