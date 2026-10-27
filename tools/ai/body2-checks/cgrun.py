#!/usr/bin/env python3
"""cgrun.py BIN DRIVER LO HI -> per-trip Ir of DRIVER (BENCH_N=LO and HI under callgrind), one number on stdout.
Runs valgrind under ulimit -v 8388608 via heavy-run.sh; set HEAVY_RUN_CPUS to pin."""
import os, re, subprocess, sys, tempfile
HR = '/home/madblack-21/Cowork/Notes/bin/heavy-run.sh'
def ir(binp, drv, n):
    with tempfile.TemporaryDirectory() as td:
        cg = td + '/o.cg'
        cmd = f'ulimit -v 8388608; exec {HR} timeout 900 valgrind --tool=callgrind --callgrind-out-file={cg} {binp} --load {drv}'
        r = subprocess.run(['bash', '-c', cmd], env=dict(os.environ, BENCH_N=str(n)), capture_output=True, text=True, stdin=subprocess.DEVNULL)
        if r.returncode != 0:
            sys.stderr.write(r.stdout[-500:] + r.stderr[-500:]); sys.exit(2)
        m = re.search(r'Collected\s*:\s*(\d+)', r.stderr)
        return int(m[1])
binp, drv, lo, hi = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
a, b = ir(binp, drv, lo), ir(binp, drv, hi)
print((b - a) // (hi - lo))
