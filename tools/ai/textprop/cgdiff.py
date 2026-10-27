#!/usr/bin/env python3
import subprocess, sys, re, collections
def load(p):
    out = subprocess.run(['callgrind_annotate', p], capture_output=True, text=True).stdout
    d = {}; tot = 0
    for l in out.splitlines():
        m = re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+(.*)$', l)
        if m:
            v = int(m.group(1).replace(',', '')); name = m.group(2).split(' [')[0].strip()
            name = name.split(':')[-1] if ':' in name else name
            if 'PROGRAM TOTALS' in l: tot = v
            else: d[name] = d.get(name, 0) + v
    return d, tot
import bisect
syms=[]
if len(sys.argv)>4:
    for l in subprocess.run(['nm','-n',sys.argv[4]],capture_output=True,text=True).stdout.splitlines():
        w=l.split()
        if len(w)==3 and w[1] in 'tTwW': syms.append((int(w[0],16),w[2]))
addrs=[x[0] for x in syms]
def res(n):
    m=re.match(r"0x([0-9a-f]+)('\d+)?$",n)
    if not m or not syms: return n
    a=int(m.group(1),16); i=bisect.bisect_right(addrs,a)-1
    return (syms[i][1] if i>=0 else n)+(m.group(2) or '')
_load=load
def load(p):
    d,t=_load(p); o={}
    for k,v in d.items(): o[res(k)]=o.get(res(k),0)+v
    return o,t
a, ta = load(sys.argv[1]); b, tb = load(sys.argv[2]); n = int(sys.argv[3])
print(f"TOTAL Ir/trip {(tb-ta)/n:.1f}")
r = [((b.get(k, 0) - a.get(k, 0)) / n, k) for k in set(a) | set(b)]
for v, k in sorted(r, reverse=True)[:25]:
    if v >= 1: print(f"{v:9.1f} {k}")
