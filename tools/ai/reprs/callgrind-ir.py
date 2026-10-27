#!/usr/bin/env python3
"""Instructions (Ir) per trip of a survey4 row: callgrind at two trip counts, difference / (hi - lo).
Usage: callgrind-ir.py BINARY|gnu DRIVER.el LO HI   (under ulimit -v 8388608; prints "IR_PER_TRIP x").
GNU runs as `emacs -Q --batch -l`; anything else as `BINARY --load DRIVER --`."""
import os, re, subprocess, sys, tempfile
eng, drv, lo, hi = sys.argv[1], os.path.abspath(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4])
def ir(n):
    cmd = ['valgrind', '--tool=callgrind', '--callgrind-out-file=' + tempfile.mktemp()]
    cmd += ['/usr/local/bin/emacs', '-Q', '--batch', '-l', drv] if eng == 'gnu' else [os.path.abspath(eng), '--load', drv, '--']
    r = subprocess.run(cmd, env=dict(os.environ, BENCH_N=str(n)), cwd=os.path.dirname(drv), stdin=subprocess.DEVNULL,
                       capture_output=True, text=True, timeout=3000)
    m = re.search(r'Collected : (\d+)', r.stderr)
    chk = re.search(r'BENCH-CHECK n=\d+ result=(\S+)', r.stdout)
    if not m or not chk: sys.exit('callgrind failed: ' + r.stderr[-300:])
    return int(m.group(1)), chk.group(1)
(a, ra), (b, rb) = ir(lo), ir(hi)
print('IR lo=%d hi=%d result_lo=%s result_hi=%s' % (a, b, ra, rb))
print('IR_PER_TRIP %.1f' % ((b - a) / (hi - lo)))
