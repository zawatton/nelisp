#!/usr/bin/env python3
"""paired.py ROW... : paired wall time and PMC cycles, v19 vs candidate vs GNU, on one CPU (MCPU, default 14).
Env: V19_BIN, VMCORE_BIN, MATRIX, PMC.  Output: target/vmcore/paired/ROW/report.json."""
import json,os,subprocess,sys
root=os.path.dirname(os.path.abspath(__file__))+'/../../..'
M=os.environ['MATRIX']; cal=json.load(open(M+'/calib.json')); cpu=os.environ.get('MCPU','14')
cal.update({k:[0,0,0,1000000] for k in ('cfs-read','cfs-base','cfs-plain','cfs-bufloc')})
for rid in sys.argv[1:]:
    drv=f'{root}/tools/ai/vmcore/drv/{rid}.el'
    if not os.path.exists(drv): drv=f'{M}/drv/{rid}.el'
    hi=cal[rid][3]; hi=int(float(f'{hi:.2g}')); lo=hi//4; out=f'{root}/target/vmcore/paired/{rid}'
    subprocess.run(['rm','-rf',out])
    cmd=(f'ulimit -v 8388608; exec timeout 1800 python3 /home/madblack-21/Cowork/Notes/.claude/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py '
         f'--engine v19={os.environ["V19_BIN"]} --engine cand={os.environ["VMCORE_BIN"]} --engine gnu=/usr/local/bin/emacs '
         f'--driver {drv} --low {lo} --high {hi} --reps 7 --cpu {cpu} --pmc {os.environ["PMC"]} --out {out}')
    r=subprocess.run(['bash','-c',cmd],capture_output=True,text=True)
    print(rid,lo,hi,'rc',r.returncode,(r.stdout+r.stderr).strip().replace('\n',' | ')[-300:],flush=True)
