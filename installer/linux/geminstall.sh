#!/bin/bash -x

TARGET="${1}/gems"
mkdir -p ${TARGET}

/bin/cp -f ../../pkg/bundler-1.2.1.gem    ${TARGET}
/bin/cp -f ../../pkg/highline-1.6.15.gem  ${TARGET}
/bin/cp -f ../../pkg/pastehub-*gem        ${TARGET}
