#!/bin/sh

CONF=`pwd`/lcf.conf
TESTS=hordad_rooms_tests

cd ../../ && ./start.sh -config test -hordad_lcf conf \"$CONF\" \
    -tests $TESTS
