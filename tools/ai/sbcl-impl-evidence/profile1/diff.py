import re,sys
def load(f):
    d={};t=None
    for l in open(f):
        m=re.match(r'\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+(?:\S+:)?(\S.*)$',l)
        if m: d[m.group(2).split(' [')[0].strip()]=int(m.group(1).replace(',',''))
    return d
for row,n in (("function-call2",65536),("vector-read",65536),("hash-get1024",4096)):
  for m in("self","incl"):
    a=load(f"callgrind.{row}.{n}.{m}.txt");b=load(f"callgrind.{row}.1.{m}.txt")
    dd={k:a[k]-b.get(k,0) for k in a}
    tot=sum(v for k,v in dd.items() if m=="self")
    print(f"== {row} {m} N={n}")
    for k,v in sorted(dd.items(),key=lambda x:-x[1])[:15]: print(f"{v:>14,} {v/n:9.1f}/it {k[:70]}")
