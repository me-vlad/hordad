#!/bin/sh
#
# Hordad starter
#

export ERL_LIBS=/usr/lib

erl +K true -smp auto -sname hordad_sys -boot hordad -pz */ebin \
    -ssl protocol_version '[sslv3]' -config hordad $@
