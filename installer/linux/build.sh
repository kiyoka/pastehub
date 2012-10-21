#!/bin/bash -x

RUBY=ruby-1.9.3-p194
TARGET=${1}
echo ${TARGET}

rm -rf ./work
mkdir -p work
pushd .

  cd work
  tar zxf ../${RUBY}.tar.gz
  cd ./${RUBY}
  ./configure --prefix=/opt/pastehub/${RUBY}
  make
  make install DESTDIR=../../${TARGET}

popd
