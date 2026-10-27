#!/usr/bin/env python3
"""lenO1.py BIN DRIVER: Ir of (build n chars + first length) minus Ir of (build n chars) at n=90000 and 180000
under callgrind.  Passes iff the first length costs O(1): D(90k) < 1000000 and D(180k) - D(90k) < 100000."""
import os, re, subprocess, sys, tempfile
HR = '/home/madblack-21/Cowork/Notes/bin/heavy-run.sh'
def ir(binp, drv, n, measure):
    with tempfile.TemporaryDirectory() as td:
        cmd = f'ulimit -v 8388608; exec {HR} timeout 900 valgrind --tool=callgrind --callgrind-out-file={td}/o.cg {binp} --load {drv}'
        r = subprocess.run(['bash', '-c', cmd], env=dict(os.environ, BENCH_N=str(n), LEN_MEASURE=str(measure)),
                           capture_output=True, text=True, stdin=subprocess.DEVNULL)
        if r.returncode != 0:
            sys.stderr.write(r.stdout[-300:] + r.stderr[-300:]); sys.exit(2)
        return int(re.search(r'Collected\s*:\s*(\d+)', r.stderr)[1])
binp, drv = sys.argv[1], sys.argv[2]
d = {n: ir(binp, drv, n, 1) - ir(binp, drv, n, 0) for n in (90000, 180000)}
print('first-length Ir: D(90k)=%d D(180k)=%d growth=%d' % (d[90000], d[180000], d[180000] - d[90000]))
sys.exit(0 if d[90000] < 1000000 and d[180000] - d[90000] < 100000 else 1)
