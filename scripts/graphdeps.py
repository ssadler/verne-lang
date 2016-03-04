#!/usr/bin/env python

import re
import subprocess

cmd = ['grep', '-r', '^import Verne', 'src/']
proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
assert 0 == proc.wait()

print 'digraph modules {'

for line in proc.stdout.readlines():
    m = re.match('^src/(.*).purs:import (?:qualified |)([^\s]+)', line)
    a = m.group(1).replace('/', '.')
    b = m.group(2)
    print '"%s" -> "%s";\n' % (a, b)

print '}'
