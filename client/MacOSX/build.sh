#!/bin/bash -x

RUBYNAME=${1}

rm -rf ./work
mkdir -p work
pushd .

  cd work
  tar zxf ../${RUBYNAME}.tar.gz
  cd ./${RUBYNAME}
  env CC=/usr/bin/gcc ./configure --prefix=/opt/pastehub/${RUBYNAME}
  make
  make install

popd
