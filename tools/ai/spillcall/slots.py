#!/usr/bin/env python3
"""spillcall: frame traffic of one function per %rbp offset (Ir per trip, ld/st), from TAG's runs.
usage: python3 slots.py TAG BIN FUNC ROW... -> 'off ld st' lines, hottest first."""
import os, sys, re, glob, collections, subprocess
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from rows import WT
import importlib.util
tag, bin_, func = sys.argv[1], os.path.realpath(sys.argv[2]), sys.argv[3]; rows = sys.argv[4:]
spec = importlib.util.spec_from_file_location('fsl_parse', os.path.join(os.path.dirname(os.path.abspath(__file__)), 'fsl.py'))
src = open(spec.origin).read(); g = {'collections': collections}; exec(src[src.index('def parse'):src.index('nm = subprocess')], g)
nm = subprocess.run(['nm', bin_], capture_output=True, text=True).stdout.splitlines()
syms = sorted((int(l.split()[0], 16), l.split()[2]) for l in nm if len(l.split()) == 3 and l.split()[1] in 'tT')
st = [a for a, n in syms if n == func][0]; en = min(a for a, n in syms if a > st)
dis = subprocess.run(['objdump', '-d', '--no-show-raw-insn', f'--start-address={st}', f'--stop-address={en}', bin_], capture_output=True, text=True).stdout
ins = {}
for l in dis.splitlines():
    t = l.split('\t', 1)
    try: ins[int(t[0].strip().rstrip(':'), 16)] = t[1].strip()
    except (ValueError, IndexError): pass
for row in rows:
    fs = sorted(glob.glob(f'{WT}/target/spillcall/cg/{tag}-{row}-*.cg'), key=lambda p: int(p.rsplit('-', 1)[1][:-3]))
    nlo, nhi = (int(p.rsplit('-', 1)[1][:-3]) for p in fs)
    a, b = g['parse'](fs[0]), g['parse'](fs[1])
    c = collections.defaultdict(lambda: [0.0, 0.0])
    for ad in b:
        if st <= ad < en and ad in ins:
            s = ins[ad]; m = re.search(r'(-?0x[0-9a-f]+)\(%rbp\)', s)
            if m and not s.startswith('lea'):
                v = (b[ad] - a.get(ad, 0)) / (nhi - nlo)
                stq = not s.startswith(('cmp', 'test', 'bt')) and bool(re.search(r',\s*-?0x[0-9a-f]+\(%rbp\)$', s))
                c[m.group(1)][1 if stq else 0] += v
    print(f'== {row} {func}: ' + ' '.join(f'{o}:{ld:.0f}/{sv:.0f}' for o, (ld, sv) in sorted(c.items(), key=lambda kv: -sum(kv[1])) if ld + sv >= 0.5))
