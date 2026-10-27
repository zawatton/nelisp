#!/usr/bin/env python3
"""spillcall: Ir per trip of FUNC's instructions that name each register or frame slot (TAG runs).
usage: python3 regs.py TAG BIN FUNC ROW"""
import os, sys, re, glob, collections, subprocess
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from rows import WT
tag, bin_, func, row = sys.argv[1], os.path.realpath(sys.argv[2]), sys.argv[3], sys.argv[4]
src = open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'fsl.py')).read(); g = {'collections': collections}
exec(src[src.index('def parse'):src.index('nm = subprocess')], g)
nm = subprocess.run(['nm', bin_], capture_output=True, text=True).stdout.splitlines()
syms = sorted((int(l.split()[0], 16), l.split()[2]) for l in nm if len(l.split()) == 3 and l.split()[1] in 'tT')
st = [a for a, n in syms if n == func][0]; en = min(a for a, n in syms if a > st)
dis = subprocess.run(['objdump', '-d', '--no-show-raw-insn', f'--start-address={st}', f'--stop-address={en}', bin_], capture_output=True, text=True).stdout
ins = {}
for l in dis.splitlines():
    t = l.split('\t', 1)
    try: ins[int(t[0].strip().rstrip(':'), 16)] = t[1].strip()
    except (ValueError, IndexError): pass
fs = sorted(glob.glob(f'{WT}/target/spillcall/cg/{tag}-{row}-*.cg'), key=lambda p: int(p.rsplit('-', 1)[1][:-3]))
nlo, nhi = (int(p.rsplit('-', 1)[1][:-3]) for p in fs)
a, b = g['parse'](fs[0]), g['parse'](fs[1])
c = collections.Counter(); tot = 0
for ad in b:
    if st <= ad < en and ad in ins:
        v = (b[ad] - a.get(ad, 0)) / (nhi - nlo); tot += v
        for r in set(re.findall(r'%(r1[2-5]|rbx|r8|r9)\b', ins[ad])): c[r] += v
        m = re.search(r'(-?0x[0-9a-f]+)\(%rbp\)', ins[ad])
        if m and not ins[ad].startswith('lea'): c['slot%d' % (int(m.group(1), 16) * -1 // 8 - 1)] += v
print(f'{func} {row} Ir/trip {tot:.0f}: ' + ' '.join(f'{k}:{v:.0f}' for k, v in c.most_common(24)))
