#!/bin/bash -x

ADMIN=./pastehub-admin

list=`${ADMIN} list`

for username in ${list} ; do
    echo ${username}
    command="${ADMIN} gc ${username}"
    `${command}`
    sleep 60
done

