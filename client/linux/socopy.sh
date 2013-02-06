#!/bin/bash

# lbog entry about ldd.
#   http://gihyo.jp/lifestyle/serial/01/ganshiki-soushi/0032

TARGET=${1}/lib

#ldd ./work/ruby-1.9.3-*/bin/ruby
solist='
/lib/libcom_err.so.2
/lib/libcrypto.so.10
/lib/libgssapi_krb5.so.2
/lib/libk5crypto.so.3
/lib/libkeyutils.so.1
/lib/libkrb5.so.3
/lib/libkrb5support.so.0
/lib/libselinux.so.1
/lib/libz.so.1
/usr/lib/libz.so
/usr/lib/libssl.so
/usr/lib/libssl.so.10
/usr/lib/libyaml.so
/usr/lib/libyaml-0.so.2
/usr/lib/libgdbm.so.2
/usr/lib/libgdbm.so
/lib/libreadline.so.6.0
/lib/libreadline.so.6
/lib/libncurses.so.5.7
/lib/libncurses.so.5
/lib/libtinfo.so.5.7
/lib/libtinfo.so.5'

mkdir -p ${TARGET}
for i in ${solist} ;
do
  echo "  installed [" ${i} "]"
  /bin/cp -f ${i} ${TARGET}
done
