#!/usr/bin/env python3
"""measure.py BINARY ROW... : instr and cycles per trip (two-N difference) via pmcrun2 on CPU 14."""
import json,os,re,subprocess,sys,statistics
HERE=os.path.dirname(os.path.abspath(__file__))
M=os.environ.get('MATRIX')  # the integ-1036 matrix directory (drv/, calib.json)
PMC=os.environ.get('PMC')  # compiled .claude/skills/nelisp-gnu-gap-bench/scripts/pmcrun2.c
CPU=os.environ.get('MCPU','14'); REPS=int(os.environ.get('REPS','3'))
cal=json.load(open(M+'/calib.json'))
cal.update({k:[0,0,0,1000000] for k in ('cfs-read','cfs-base','cfs-plain','cfs-bufloc')})
def one(b,drv,n):
    e=dict(os.environ,BENCH_N=str(n))
    r=subprocess.run(['bash','-c',f'ulimit -v 8388608; exec timeout 600 taskset -c {CPU} {PMC} {b} --load {drv}'],env=e,capture_output=True,text=True,stdin=subprocess.DEVNULL)
    l=[x for x in r.stdout.splitlines() if x.startswith('cycles=')]
    m=re.search(r'cycles=(\d+) instr=(\d+)',l[-1]) if l else None
    if not m or f'n={n} ' not in r.stdout: sys.exit(f'fail {drv} {n}: {r.stdout[-200:]} {r.stderr[-200:]}')
    return int(m[1]),int(m[2]),re.search(r'result=(\S+)',r.stdout)[1]
def row(b,rid):
    drv=f'{HERE}/drv/{rid}.el' if os.path.exists(f'{HERE}/drv/{rid}.el') else f'{M}/drv/{rid}.el'
    hi=cal[rid][3]; hi=int(float(f'{hi:.2g}')); lo=hi//4
    cs=[];ins=[]
    for _ in range(REPS):
        c1,i1,_r=one(b,drv,lo); c2,i2,_r=one(b,drv,hi)
        cs.append((c2-c1)/(hi-lo)); ins.append((i2-i1)/(hi-lo))
    return statistics.median(ins),statistics.median(cs),min(cs),max(cs)
if __name__=='__main__':
    b=sys.argv[1]
    for rid in sys.argv[2:]:
        i,c,cmin,cmax=row(b,rid); print(f'{rid}\tinstr/trip={i:.1f}\tcyc/trip={c:.1f} (min {cmin:.1f} max {cmax:.1f})',flush=True)
