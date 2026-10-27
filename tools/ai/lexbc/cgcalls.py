#!/usr/bin/env python3
"""Total Ir and call counts to named functions in a callgrind output file.
usage: cgcalls.py [--nm BINARY] FILE.cg FN...   -> prints "TOTAL <Ir>" and "CALLS <fn> <n>".
--nm resolves valgrind's "0x..." names (stripped dynamic symbols) through `nm BINARY'."""
import re, subprocess, sys
args = sys.argv[1:]
addr = {}
if args[0] == '--nm':
    for l in subprocess.run(['nm', args[1]], capture_output=True, text=True).stdout.splitlines():
        q = l.split()
        if len(q) == 3 and q[1] in 'Tt': addr.setdefault(int(q[0], 16), q[2])
    args = args[2:]
path, targets = args[0], set(args[1:])
names, calls, total = {}, {t: 0 for t in targets}, 0
cfn = None
def name(tok):
    m = re.match(r'\((\d+)\)(?: (.*))?$', tok.strip())
    if not m: return tok.strip()
    if m.group(2):
        n = m.group(2).split("'")[0]
        if n.startswith('0x') and addr: n = addr.get(int(n, 16), n)
        names[m.group(1)] = n
    return names.get(m.group(1), '?')
for line in open(path, errors='replace'):
    if line.startswith('summary:') or line.startswith('totals:'):
        total = int(line.split()[1])
    elif line.startswith('fn='): name(line[3:])
    elif line.startswith('cfn='): cfn = name(line[4:])
    elif line.startswith('calls=') and cfn in targets:
        calls[cfn] += int(line[6:].split()[0])
print('TOTAL', total)
for t in sorted(targets): print('CALLS', t, calls[t])
