#!/usr/bin/env python3
"""bindshare.py NM CGDIR PLAN: per attrib34 row, the inclusive Ir share of the dynamic-binding entry points
(wf_bytecode_varbind, wf_bytecode_unbind, wf_bytecode_w_lean_bind, m5_fx_unbind) in the hi-trip callgrind file."""
import sys, re, bisect, subprocess, json
nm = sorted((int(l.split()[0], 16), l.split()[2]) for l in open(sys.argv[1]) if len(l.split()) == 3 and l.split()[1] in 'Tt')
addrs = [a for a, _ in nm]
def sym(ad):
    i = bisect.bisect_right(addrs, ad) - 1
    return nm[i][1] if i >= 0 else '?'
WANT = {'wf_bytecode_varbind', 'wf_bytecode_unbind', 'wf_bytecode_w_lean_bind', 'm5_fx_unbind'}
plan = json.load(open(sys.argv[3]))
for row, p in plan.items():
    cg = f"{sys.argv[2]}/nl-{row}-{p['hi']}.cg"
    out = subprocess.run(['callgrind_annotate', '--inclusive=yes', '--threshold=100', cg], capture_output=True, text=True).stdout
    tot = 0; got = {}
    for l in out.splitlines():
        m = re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+PROGRAM TOTALS', l)
        if m: tot = int(m[1].replace(',', '')); continue
        m = re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+\?\?\?:(0x[0-9a-f]+)(\S*)', l)
        if m and m[3] == '':
            s = sym(int(m[2], 16))
            if s in WANT: got[s] = got.get(s, 0) + int(m[1].replace(',', ''))
    share = sum(got.values()) / tot if tot else 0
    print(f"{row:32s} bindshare={share:6.3f} " + ' '.join(f'{k}={v/tot:.3f}' for k, v in sorted(got.items())))
