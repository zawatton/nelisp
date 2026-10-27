#!/usr/bin/env python3
"""spillcall frame-slot attribution: per row, Ir per trip by function and instruction class
(frame-ld / frame-st = %rbp-relative loads/stores, lea excluded; push/pop; call/ret; other),
from TAG's callgrind --dump-instr runs (two-N difference) and objdump/nm of BIN.
usage: python3 fsl.py TAG BIN [TOPN] -> prints the table; FSL-ROW lines carry the row shares."""
import os, sys, re, glob, bisect, collections, subprocess
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from rows import ROWS, WT
tag, bin_ = sys.argv[1], os.path.realpath(sys.argv[2]); topn = int(sys.argv[3]) if len(sys.argv) > 3 else 8
cgd = os.path.join(WT, 'target/spillcall/cg')
def parse(path):
    cost = collections.Counter(); last = 0; skip = False
    for line in open(path):
        if line.startswith('calls='): skip = True; continue
        c = line[:1]
        if not c or c not in '0123456789+-*': continue
        p = line.split(); a = p[0]
        if a == '*': addr = last
        elif a[0] in '+-': addr = last + int(a, 0)
        else: addr = int(a, 0)
        last = addr
        if skip: skip = False; continue
        if len(p) > 2: cost[addr] += int(p[2])
    return cost
nm = subprocess.run(['nm', bin_], capture_output=True, text=True).stdout.splitlines()
syms = sorted((int(l.split()[0], 16), l.split()[2]) for l in nm if len(l.split()) == 3 and l.split()[1] in 'tT')
starts = [s for s, _ in syms]
def fn(a):
    i = bisect.bisect_right(starts, a) - 1
    return syms[i][1] if i >= 0 else '?'
per_row = {}
for row in ROWS:
    fs = sorted(glob.glob(f'{cgd}/{tag}-{row}-*.cg'), key=lambda p: int(p.rsplit('-', 1)[1][:-3]))
    if len(fs) != 2: continue
    nlo, nhi = (int(p.rsplit('-', 1)[1][:-3]) for p in fs)
    a, b = parse(fs[0]), parse(fs[1])
    per_row[row] = {ad: (b[ad] - a.get(ad, 0)) / (nhi - nlo) for ad in b}
need = set().union(*[set(p) for p in per_row.values()]) if per_row else set()
ins = {}
dis = subprocess.Popen(['objdump', '-d', '--no-show-raw-insn', bin_], stdout=subprocess.PIPE, text=True)
for l in dis.stdout:
    t = l.split('\t', 1)
    try: ad = int(t[0].strip().rstrip(':'), 16)
    except ValueError: continue
    if ad in need: ins[ad] = t[1].strip() if len(t) > 1 else ''
dis.wait()
def cls(s):
    op = s.split()[0] if s else '?'
    if op.startswith('call') or op.startswith('ret'): return 'call/ret'
    if op in ('push', 'pop'): return 'push/pop'
    if op != 'lea' and '(%rbp)' in s and op.startswith(('cmp', 'test')): return 'frame-cmp'
    if op != 'lea' and '(%rbp)' in s:
        ops = s.split(None, 1)[1] if ' ' in s else ''
        return 'frame-st' if not op.startswith(('cmp', 'test', 'bt')) and re.search(r',\s*-?0x[0-9a-f]+\(%rbp\)$', ops) else 'frame-ld'
    if op == '?': return 'unknown'
    return 'other'
for row, p in per_row.items():
    c = collections.Counter(); byf = collections.defaultdict(collections.Counter)
    for ad, v in p.items():
        k = cls(ins.get(ad, '')); c[k] += v; byf[fn(ad)][k] += v; byf[fn(ad)]['total'] += v
    t = sum(c.values()); fr = c['frame-ld'] + c['frame-st'] + c['frame-cmp']
    print(f"FSL-ROW {row} Ir/trip {t:.1f} frame {fr:.1f} ({100*fr/t:.1f}%) ld {c['frame-ld']:.1f} st {c['frame-st']:.1f} cmp {c['frame-cmp']:.1f} push/pop {c['push/pop']:.1f} call/ret {c['call/ret']:.1f}")
    for f, cc in sorted(byf.items(), key=lambda kv: -kv[1]['total'])[:topn]:
        print(f"   {cc['total']:8.1f}  frame {cc['frame-ld']+cc['frame-st']+cc['frame-cmp']:7.1f} (cmp {cc['frame-cmp']:5.1f})  push/pop {cc['push/pop']:5.1f}  {f}")
