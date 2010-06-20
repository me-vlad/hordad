#!/bin/sh
#
# Hordad starter
#

erl -smp auto -sname hordad_sys -boot hordad -pz */ebin \
    -ssl protocol_version '[sslv3]' # -config hordad