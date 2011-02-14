#!/usr/bin/env python
#
# Run application tests
# Max E. Kuznecov <mek@mek.uz.ua> 2010
#

import re
import os
import sys
import glob

CONF="%s/tests/lcf.conf" % sys.argv[1]

if not os.path.exists(CONF):
    sys.exit(0)

TESTS=[]

pat = re.compile(r'(.+?)\.beam$')

for test in glob.glob("%s/ebin/*_tests.beam" % sys.argv[1]):
    res = pat.search(test)

    if res:
        TESTS.append(os.path.basename(res.group(1)))
    
if not TESTS:
    sys.exit(0)
else:
    cmd = './start.sh -sasl errlog_type error -hordad_lcf conf \\"%s\\" -tests %s' % (CONF, " ".join(TESTS))
    os.system(cmd)
