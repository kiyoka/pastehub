#!/bin/bash -x

RUBYNAME=${1}
TARGET=${2}
echo ${TARGET}

rm -rf ./work
mkdir -p work
pushd .

  cd work
  tar zxf ../${RUBYNAME}.tar.gz
  cd ./${RUBYNAME}
  ./configure --prefix=/opt/pastehub/${RUBYNAME}
  make
  make install DESTDIR=../../${TARGET}

popd
