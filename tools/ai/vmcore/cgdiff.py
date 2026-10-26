#!/usr/bin/env python3
"""cgdiff.py BIN DRIVER LO HI [CPU] : per-function exclusive Ir/trip (callgrind difference of two N), symbolised via nm."""
import sys,subprocess,re,bisect,os,collections
b,drv,lo,hi=sys.argv[1],sys.argv[2],int(sys.argv[3]),int(sys.argv[4])
cpu=sys.argv[5] if len(sys.argv)>5 else '14'
nm=subprocess.run(['nm','-n',b],capture_output=True,text=True).stdout.splitlines()
addrs=[];names=[]
for l in nm:
    p=l.split()
    if len(p)==3 and p[1] in 'tTwW': addrs.append(int(p[0],16));names.append(p[2])
def sym(a):
    i=bisect.bisect_right(addrs,a)-1
    return names[i] if i>=0 else hex(a)
def prof(n):
    out=f'/tmp/_cgd_{os.getpid()}_{n}.out'
    e=dict(os.environ,BENCH_N=str(n))
    subprocess.run(['bash','-c',f'ulimit -v 8388608; taskset -c {cpu} timeout 1200 valgrind --tool=callgrind --callgrind-out-file={out} {b} --load {drv} >/dev/null 2>&1'],env=e)
    r=subprocess.run(['callgrind_annotate',out],capture_output=True,text=True).stdout
    os.unlink(out)
    c=collections.Counter()
    for l in r.splitlines():
        m=re.match(r'\s*([\d,]+) \(\s*[\d.]+%\)\s+\S*?:(0x[0-9a-f]+|\S+)',l)
        if m:
            f=m[2]
            c[sym(int(f,16)) if f.startswith('0x') else f]+=int(m[1].replace(',',''))
    return c
a=prof(lo); h=prof(hi)
d={k:(h[k]-a.get(k,0))/(hi-lo) for k in h}
tot=sum(d.values())
print(f'total {tot:.1f} Ir/trip')
for k,v in sorted(d.items(),key=lambda x:-x[1])[:int(os.environ.get("TOP","14"))]:
    if v>0.5: print(f'{v:9.1f} {k}')
