#!/usr/bin/env python3
"""rowcg.py OUTDIR [ROW...]: callgrind of the attrib34 rows (two trip counts) on $VARACCESS_BIN, then per-trip Ir
against the v34 callgrind files of attrib34 (cg/nl-ROW-N.cg).  Paths are resolved from $ATTRIB34 (the attrib34
directory) and $VARACCESS_BIN; every job goes through heavy-run.sh pinned to CPUs 0-7."""
import json, os, subprocess, sys, re, concurrent.futures as cf
A34 = os.environ['ATTRIB34']; T = os.path.dirname(A34); BIN = os.environ['VARACCESS_BIN']
OUT = sys.argv[1]; only = sys.argv[2:]
HR = os.environ.get('HEAVY_RUN', 'heavy-run.sh')
plan = json.load(open(A34 + '/plan.json')); os.makedirs(OUT, exist_ok=True)
def run(j):
    row, n, drv = j
    base = f'{OUT}/nl-{row}-{n}'
    if os.path.exists(base + '.done') and open(base + '.done').read() == '0': return
    cmd = [HR, 'taskset', '-c', '0-7', 'timeout', '1800', 'valgrind', '--tool=callgrind', f'--callgrind-out-file={base}.cg', BIN, '--load', drv]
    env = dict(os.environ, BENCH_N=str(n), HEAVY_RUN_CPUS='0-7')
    with open(base + '.out', 'w') as o:
        rc = subprocess.run(['bash', '-c', 'ulimit -v 8388608; exec "$@"', 'x'] + cmd, env=env, stdin=subprocess.DEVNULL, stdout=o, stderr=subprocess.STDOUT).returncode
    open(base + '.done', 'w').write(str(rc))
jobs = []
for row, p in plan.items():
    if only and row not in only: continue
    drv = f"{T}/resurvey29/survey{p['survey']}/drv/{row}.el"
    for n in (p['lo'], p['hi']): jobs.append((row, n, drv))
with cf.ThreadPoolExecutor(4) as ex: list(ex.map(run, jobs))
def tot(f):
    for l in open(f, errors='replace'):
        if l.startswith('summary:') or l.startswith('totals:'): return int(l.split()[1])
print(f'{"row":32s} {"v34/trip":>10s} {"new/trip":>10s} {"ratio":>6s}')
for row, p in plan.items():
    if only and row not in only: continue
    try:
        b = (tot(f'{A34}/cg/nl-{row}-{p["hi"]}.cg') - tot(f'{A34}/cg/nl-{row}-{p["lo"]}.cg')) / (p['hi'] - p['lo'])
        a = (tot(f'{OUT}/nl-{row}-{p["hi"]}.cg') - tot(f'{OUT}/nl-{row}-{p["lo"]}.cg')) / (p['hi'] - p['lo'])
        print(f'{row:32s} {b:10.1f} {a:10.1f} {a / b:6.3f}')
    except Exception as e: print(f'{row:32s} ERR {e}')
