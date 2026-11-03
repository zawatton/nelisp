#!/usr/bin/env python3
"""rowcg.py OUTDIR TAG BIN [ROW...]: callgrind of the attrib34 rows (two trip counts, plan.json) on BIN, files
OUTDIR/TAG-ROW-N.cg; every job through heavy-run.sh pinned to CPUs 0-7.  `rowcg.py --cmp OUTDIR A B` prints per-trip Ir
of tag A and tag B per row and their ratio.  $ATTRIB34 is the attrib34 directory (plan.json, drivers via resurvey29)."""
import json, os, subprocess, sys, concurrent.futures as cf
A34 = os.environ['ATTRIB34']; T = os.path.dirname(A34)
HR = os.environ.get('HEAVY_RUN', 'heavy-run.sh')
plan = json.load(open(A34 + '/plan.json'))
def tot(f):
    for l in open(f, errors='replace'):
        if l.startswith('summary:') or l.startswith('totals:'): return int(l.split()[1])
def per(out, tag, row):
    p = plan[row]
    return (tot(f'{out}/{tag}-{row}-{p["hi"]}.cg') - tot(f'{out}/{tag}-{row}-{p["lo"]}.cg')) / (p['hi'] - p['lo'])
if sys.argv[1] == '--cmp':
    out, a, b = sys.argv[2:5]
    print(f'{"row":32s} {a + "/trip":>12s} {b + "/trip":>12s} {"ratio":>6s}')
    for row in plan:
        try:
            x, y = per(out, a, row), per(out, b, row)
            print(f'{row:32s} {x:12.1f} {y:12.1f} {y / x:6.3f}')
        except Exception as e: print(f'{row:32s} ERR {e}')
    sys.exit(0)
OUT, TAG, BIN = sys.argv[1:4]; only = sys.argv[4:]; os.makedirs(OUT, exist_ok=True)
def run(j):
    row, n, drv = j
    base = f'{OUT}/{TAG}-{row}-{n}'
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
print('ROWCG-DONE', TAG)
