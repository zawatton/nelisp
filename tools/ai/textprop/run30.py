#!/usr/bin/env python3
"""run30.py ENGINE_PATH OUT.json [CPU] [REPS]: ns/trip of the 30 text-property rows (two trip counts, median of REPS, fresh process each)."""
import json, os, subprocess, sys, statistics, re, time
HERE = os.path.dirname(os.path.abspath(__file__))
R = os.environ.get('RESURVEY24') or os.path.realpath(HERE + '/../../../../resume-4943-20261024/target/resurvey24')
eng, out = sys.argv[1], sys.argv[2]; cpu = sys.argv[3] if len(sys.argv) > 3 else '15'; reps = int(sys.argv[4]) if len(sys.argv) > 4 else 5
gnu = os.path.basename(eng).startswith('emacs')
def run(drv, n):
    cmd = ['taskset', '-c', cpu, 'timeout', '300'] + ([eng, '-Q', '--batch', '-l', drv] if gnu else [eng, '--load', drv])
    env = dict(os.environ, BENCH_N=str(n))
    p = subprocess.run(cmd, env=env, capture_output=True, text=True, stdin=subprocess.DEVNULL)
    m = re.search(r'BENCH-CHECK n=\d+ result=(.*) elapsed=([\d.]+)', p.stdout)
    return (m.group(1), float(m.group(2))) if m else (None, None)
res = {}
if os.path.exists(out): res = json.load(open(out))
for sv, row in json.load(open(HERE + '/rows30.json')):
    if row in res: continue
    drv = f'{R}/survey{sv}/drv/{row}.el'
    r, t = run(drv, 4)
    if r is None: res[row] = {'err': 'no result'}; print(row, 'ERR'); continue
    per = max(t / 4, 1e-7); hi = max(8, int(0.25 / per)); lo = max(2, hi // 4)
    ts = {lo: [], hi: []}; rs = set()
    for i in range(reps):
        for n in ((lo, hi) if i % 2 == 0 else (hi, lo)):
            r, t = run(drv, n); rs.add((n, r)); ts[n].append(t)
    ns = (statistics.median(ts[hi]) - statistics.median(ts[lo])) / (hi - lo) * 1e9
    res[row] = dict(ns=ns, lo=lo, hi=hi, results=sorted(map(str, rs))[:2])
    print(f'{row:34s} {ns:12.0f} ns/trip', flush=True)
    json.dump(res, open(out, 'w'), indent=1)
