#!/usr/bin/env python3
"""spillcall callgrind campaign: --dump-instr=yes at two trip counts per row.
usage: python3 cg.py TAG BIN [ROW...]   -> target/spillcall/cg/TAG-ROW-N.{cg,out,done}
Each job runs through heavy-run.sh pinned to CPUs 0-7."""
import os, sys, subprocess, concurrent.futures as cf
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from rows import ROWS, WT, HR
tag, bin_ = sys.argv[1], os.path.realpath(sys.argv[2]); only = sys.argv[3:]
out = os.path.join(WT, 'target/spillcall/cg'); os.makedirs(out, exist_ok=True)
jobs = []
for row, (drv, ns) in ROWS.items():
    if only and row not in only: continue
    hi = max(8, int(0.08e9 / ns)); lo = max(2, hi // 4)
    for n in (lo, hi): jobs.append((row, n, drv))
def run(j):
    row, n, drv = j
    base = f"{out}/{tag}-{row}-{n}"
    if os.path.exists(base + '.done') and open(base + '.done').read() == '0': return base, 'skip'
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec "$@"', 'x', HR, 'taskset', '-c', '0-7', 'timeout', '1800',
           'valgrind', '--tool=callgrind', '--dump-instr=yes', f'--callgrind-out-file={base}.cg', bin_, '--load', drv]
    env = dict(os.environ, BENCH_N=str(n), HEAVY_RUN_CPUS='0-7')
    with open(base + '.out', 'w') as o:
        rc = subprocess.run(cmd, env=env, stdin=subprocess.DEVNULL, stdout=o, stderr=subprocess.STDOUT).returncode
    open(base + '.done', 'w').write(str(rc))
    return base, rc
with cf.ThreadPoolExecutor(4) as ex:
    for b, rc in ex.map(run, jobs): print(os.path.basename(b), rc, flush=True)
print('ALLDONE')
