#!/usr/bin/env python3
"""matrix.py OUTDIR [ROW...]: the integ-1045 matrix (specs.json, calib.json, drv/) as paired timing
v28 vs candidate (REGALLOC_BIN, default this lane's build) vs GNU on CPU 13 (MCPU), 5 reps.
Skips rows whose report.json exists; retries a busy CPU like integ-1045/matrix/runall.py."""
import json, os, subprocess, sys, time
HERE = os.path.dirname(os.path.abspath(__file__)); ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
M = os.path.dirname(ROOT) + '/resume-4943-20261024/target/integ-1045/matrix'
V28 = os.path.dirname(ROOT) + '/resume-4943-20261024/target/integ-1045-build/dynamic/nelisp-dynamic'
CAND = os.path.realpath(os.environ.get('REGALLOC_BIN', ROOT + '/target/regalloc-build/dynamic/nelisp-dynamic'))
out_root = sys.argv[1]; only = sys.argv[2:]; cpu = os.environ.get('MCPU', '13')
cal = json.load(open(M + '/calib.json')); specs = json.load(open(M + '/specs.json'))
for sp in specs:
    if only and sp['id'] not in only: continue
    out = f"{out_root}/{sp['id']}"
    if os.path.exists(out + '/report.json'): continue
    hi = cal[sp['id']][3]; hi = int(float(f'{hi:.2g}')); lo = hi // 4
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec /home/madblack-21/Cowork/Notes/bin/heavy-run.sh timeout 2400 python3 '
           '/home/madblack-21/Cowork/Notes/.claude/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py '
           f'--engine v28={V28} --engine cand={CAND} --engine gnu=/usr/local/bin/emacs '
           f"--driver {M}/drv/{sp['id']}.el --low {lo} --high {hi} --reps 5 --cpu {cpu} --out {out}"
           + (' --allow-busy' if os.environ.get('ALLOW_BUSY') else '')]
    for att in range(10):
        subprocess.run(['rm', '-rf', out])
        r = subprocess.run(cmd, capture_output=True, text=True)
        if 'is busy' not in r.stdout + r.stderr: break
        print('busy retry', att, flush=True); time.sleep(20)
    print(sp['id'], lo, hi, 'rc', r.returncode, (r.stdout + r.stderr).strip().replace('\n', ' | ')[-200:], flush=True)
