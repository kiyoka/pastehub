#!/bin/bash

# lbog entry about ldd.
#   http://gihyo.jp/lifestyle/serial/01/ganshiki-soushi/0032

RUBY=ruby-1.9.3-p194
TARGET=./work/opt/pastehub/lib

#ldd ./work/${RUBY}/bin/ruby
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
/usr/lib/libssl.so.10
/usr/lib/libgdbm.so.2'

mkdir -p ${TARGET}
for i in ${solist} ;
do
  echo "  installed [" ${i} "]"
  /bin/cp -f ${i} ${TARGET}
done
