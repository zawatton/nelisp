#!/usr/bin/env python3
"""[nl-changed-rows copy of notes-97 rb/rbbench.py; changes: engine spec name=nl:BIN:DUMP (flat dump + dump-reinit), ulimit -v 8388608, stdin closed, --timeout.]
GC-aligned rebaseline runner.  Fresh process per (rep, engine, mult, driver),
engine order alternating per rep, two trip multipliers.  Each case line gives its
own n; ns/call = (t_hi - t_lo)/(n_hi - n_lo).  The BENCH-GC line before a case
gives GNU's GC seconds/count in that window (-1 on NeLisp)."""
import argparse,json,os,re,subprocess,sys,time
ap=argparse.ArgumentParser()
ap.add_argument('--engine',action='append',required=True)
ap.add_argument('--drivers',required=True,help='file with one driver path per line')
ap.add_argument('--mults',default='1,4');ap.add_argument('--reps',type=int,default=7)
ap.add_argument('--cpu',type=int,required=True);ap.add_argument('--out',required=True)
ap.add_argument('--allow-busy',action='store_true');ap.add_argument('--timeout',type=int,default=1800)
a=ap.parse_args();os.makedirs(a.out,exist_ok=True)
def util(cpus):
    def snap():
        d={}
        for l in open('/proc/stat'):
            if l.startswith('cpu') and l[3].isdigit():
                p=l.split();d[int(p[0][3:])]=list(map(int,p[1:]))
        return d
    x=snap();time.sleep(1);y=snap()
    return {c:round(100*(1-((y[c][3]-x[c][3])+(y[c][4]-x[c][4]))/max(1,sum(y[c])-sum(x[c]))),1) for c in cpus}
u=util([a.cpu,a.cpu+16])
if any(v>5 for v in u.values()) and not a.allow_busy: sys.exit(f'busy {u}')
engs=[e.split('=',1) for e in a.engine]
import shlex;drivers=[l.strip() for l in open(a.drivers) if l.strip()]
mults=[int(x) for x in a.mults.split(',')]
LINE=re.compile(r'(?:BENCH|STATEFUL)-CASE case=(\S+) .*?n=(\d+) (?:result|status)=(.*) elapsed=([0-9.]+)')
def run(path,m,drv):
    if path.startswith('nl:'):
        _,bin_,dump=path.split(':',2)
        cmd=[bin_,'--cold-load-from',dump,'--eval','(progn (nemacs-runtime-dump-reinit) (load "%s" nil t t))'%drv]
    else:
        cmd=([path,'-Q','--batch','-l',drv] if os.path.basename(path).startswith('emacs') else [path,'--load',drv])
    sh='ulimit -v 8388608; exec timeout %d taskset -c %d %s'%(a.timeout,a.cpu,' '.join(shlex.quote(c) for c in cmd))
    try: p=subprocess.run(['bash','-c',sh],env=dict(os.environ,BENCH_N=str(m)),capture_output=True,text=True,timeout=a.timeout+60,stdin=subprocess.DEVNULL)
    except subprocess.TimeoutExpired: return {'_error':'timeout'}
    out={};gc=None
    for l in p.stdout.splitlines():
        g=re.match(r'BENCH-GC gcsec=(-?[0-9.]+) gcs=(-?\d+)(?: loopn=(-?\d+))?',l)
        if g: gc=(float(g.group(1)),int(g.group(2)),int(g.group(3) or -1));continue
        c=LINE.search(l)
        if c: out[c.group(1)]=dict(n=(gc[2] if gc and gc[2]>0 else int(c.group(2))),printed_n=int(c.group(2)),result=c.group(3),t=float(c.group(4)),gcsec=gc[0] if gc else None,gcs=gc[1] if gc else None);gc=None
    if 'BENCH-CHECK' not in p.stdout: out['_error']=(p.stdout[-400:]+p.stderr[-800:])
    return out
raw=[]
for r in range(a.reps):
    order=engs if r%2==0 else engs[::-1]
    for name,path in order:
        for m in mults:
            for drv in drivers:
                raw.append(dict(rep=r,engine=name,mult=m,driver=drv,cases=run(path,m,drv)))
    json.dump(dict(cpu=a.cpu,util=u,reps=a.reps,mults=mults,engines=engs,raw=raw),open(f'{a.out}/raw.json','w'))
print('done',a.out)
