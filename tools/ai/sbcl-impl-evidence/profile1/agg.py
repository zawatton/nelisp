import re,subprocess,bisect,collections
syms=[]
for l in subprocess.check_output(["nm","-n","../../build-on/nelisp-dynamic"],text=True).splitlines():
    p=l.split()
    if len(p)==3 and p[1] in "TtWw": syms.append((int(p[0],16),p[2]))
ad=[a for a,_ in syms]
def nm(h):
    if not h.startswith("0x"): return h
    a=int(h,16);i=bisect.bisect_right(ad,a)-1
    return syms[i][1] if i>=0 and a-ad[i]<0x40000 else h
def load(f):
    d=collections.Counter()
    for l in open(f):
        m=re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+(\S+)',l)
        if m:
            k=re.sub(r"^\S*?:(?=0x)","",m.group(2)).split("'")
            if k[0]=="PROGRAM": continue
            d[nm(k[0])]+=int(m.group(1).replace(',',''))
    return d
for row,n in (("function-call2",65536),("vector-read",65536),("hash-get1024",4096)):
    a=load(f"callgrind.{row}.{n}.self.txt");b=load(f"callgrind.{row}.1.self.txt")
    dd={k:a[k]-b.get(k,0) for k in a};T=sum(dd.values())
    print(f"== {row} N={n} sum-self-diff={T:,} ({T/n:.0f}/it)")
    for k,v in sorted(dd.items(),key=lambda x:-x[1])[:16]: print(f"{v:>13,} {v/n:8.1f}/it {100*v/T:5.1f}% {k}")
