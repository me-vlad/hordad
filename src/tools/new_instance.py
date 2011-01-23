#!/usr/bin/env python

"Create new hordad instance"

import os
import re

START_SH_TPL = """\
#!/bin/sh
#
# Hordad starter
#

export ERL_LIBS=../../src:../../lib

erl +K true -smp auto -sname hordad_sys_%(name)s -boot hordad \\
    -ssl protocol_version '[sslv3]' \\
    -hordad_system_base '.' \\
    -hordad_log_base 'log' \\
    -hordad_lcf conf '"etc/lcf.conf"' \\
    $@
"""

def write_lcf_conf(path, ip, port, entry_ip, entry_port):
    wlcf = open(join(path, "etc", "lcf.conf"), "w")
    re_ip = re.compile(r'hordad,\s+bind_ip')
    re_port = re.compile(r'hordad,\s+bind_port')
    entry = re.compile(r'hordad_ddb,\s+entry_point')

    def ipf(raw):
        return "{%s}" % ",".join(raw.split("."))

    with open("../../etc/lcf.conf") as f:
        for line in f:
            if re_ip.search(line):
                wlcf.write("{{hordad, bind_ip}, %s}.\n" % ipf(ip))
            elif re_port.search(line):
                wlcf.write("{{hordad, bind_port}, %s}.\n" % port)
            elif entry.search(line):
                wlcf.write("{{hordad_ddb, entry_point}, {%s, %s}}.\n" %
                           (ipf(entry_ip), entry_port))
            else:
                wlcf.write(line)

    wlcf.close()

def write_start_sh(path, string):
    with open(path, "w") as f:
        f.write(string)

    os.chmod(path, 0700)

name = raw_input("Input instance name: ")
ip = raw_input("Input IP address: ")
port = raw_input("Input port: ")
entry_ip = raw_input("Input entry node IP address: ")
entry_port = raw_input("Input entry node port: ")

join = os.path.join
full_path = join("../../sandbox", name)

os.makedirs(join(full_path, "db"))
os.makedirs(join(full_path, "etc"))
os.makedirs(join(full_path, "ssl"))
os.makedirs(join(full_path, "www"))
os.makedirs(join(full_path, "log"))

write_lcf_conf(full_path, ip, port, entry_ip, entry_port)
write_start_sh(join(full_path, "start.sh"), START_SH_TPL % locals())
os.symlink("../../src/hordad.boot", join(full_path, "hordad.boot"))
