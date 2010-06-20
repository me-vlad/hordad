#!/usr/bin/python
#
# Max E. Kuznecov <mek@mek.uz.ua> 2010
#

"Replace erlang components versions in release file"

import re
import glob
import os
import sys
import platform

ARCH = platform.architecture()

if ARCH[0] == '32bit':
    ERL_BASE = "/usr/lib/erlang"
elif ARCH[0] == '64bit':
    ERL_BASE = "/usr/lib64/erlang"
    
VER_RE = r"-([\.\d]+)$"

def get_version(comp, erts=False):
    other = []

    if erts:
        other = ["erts-*"]
    else:
        other = ["lib", "%s-*" % comp]

    d = glob.glob(os.path.join(ERL_BASE, *other))

    if not d:
        return None
    else:
        norm_comp = os.path.basename(d[0])
        match = re.search(VER_RE, norm_comp)

        if match is None:
            return None
        else:
            return match.group(1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def either_replace(line, c, ver, r):
    if re.search(r, line):
        return '{%s, "%s"},' % (c, ver)
    else:
        return line
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if __name__ == "__main__":
    comps = [("erts", True, r'^\s*{erts, ".+"},\s*$'),
             ("kernel", False, r'^\s*{kernel, ".+"},\s*$'),
             ("stdlib", False, r'^\s*{stdlib, ".+"},\s*$'),
             ("sasl", False, r'^\s*{sasl, ".+"},\s*$'),
             ("crypto", False, r'^\s*{crypto, ".+"},\s*$'),
             ("ssl", False, r'^\s*{ssl, ".+"},\s*$'),
             ("public_key", False, r'^\s*{public_key, ".+"},\s*$')
             ]

    rel_file = sys.argv[1]

    f = open(rel_file, "r")
    data = [x.strip() for x in f.readlines()]
    f.close()
    
    for c, flag, r in comps:
        ver = get_version(c, flag)

        # Replace in file

        data = map(lambda x: either_replace(x, c, ver, r), data)

    f = open(rel_file, "w")
    f.write("\n".join(data))
    f.close()
