#!/usr/bin/env python3
"""fnbytes.py UNIT.bin NAME OUT.dis : disassemble NAME from an emit1.el unit (labels in UNIT.bin.labels)."""
import sys, re, subprocess
u, name, out = sys.argv[1:4]
labs = []
for l in open(u + '.labels'):
    m = re.search(r':name "([^"]+)" :value (\d+)', l)
    if m: labs.append((int(m.group(2)), m.group(1)))
labs.sort(); data = open(u, 'rb').read()
st = [v for v, n in labs if n == name][0]
en = min([v for v, n in labs if v > st] + [len(data)])
open(out + '.bin', 'wb').write(data[st:en])
d = subprocess.run(['objdump', '-D', '-b', 'binary', '-mi386:x86-64', '-M', 'intel', '--no-show-raw-insn', out + '.bin'], capture_output=True, text=True).stdout
open(out, 'w').write(d)
print(name, 'size', en - st, 'insns', sum(1 for l in d.splitlines() if re.match(r'\s+[0-9a-f]+:\t', l)))
