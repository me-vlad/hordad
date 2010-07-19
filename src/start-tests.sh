#!/bin/sh
#
# Run application tests
# Max E. Kuznecov <mek@mek.uz.ua> 2010
#

CONF=$1/tests/lcf.conf

if [ ! -f $CONF ];
then
    exit 0;
fi

TESTS=""

for TEST in $(echo $1/ebin/*_tests.beam); do
    X=$(basename $TEST)
    TESTS=${X%.beam}" "$TESTS
done

./start.sh -config test -hordad_lcf conf \"$CONF\" -tests $TESTS
