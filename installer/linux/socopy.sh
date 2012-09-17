#!/bin/bash

# lbog entry about ldd.
#   http://gihyo.jp/lifestyle/serial/01/ganshiki-soushi/0032

RUBY=ruby-1.9.3-p194
TARGET=/opt/pastehub/lib

#ldd ./work/${RUBY}/bin/ruby
solist='/lib/libpthread.so.0 /lib/librt.so.1 /lib/libdl.so.2 /lib/libcrypt.so.1 /lib/libm.so.6 /lib/libc.so.6'


mkdir -p ${TARGET}
for i in ${solist} ;
do
  echo "  installed [" ${i} "]"
  /bin/cp -f ${i} ${TARGET}
done




