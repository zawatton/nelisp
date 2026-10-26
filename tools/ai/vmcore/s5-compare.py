#!/usr/bin/env python3
"""s5-compare.py BASE-DIR CAND-DIR: no test that passes (NeLisp column) on BASE may fail on CAND; prints counts."""
import sys,collections
def load(d):
    r={}
    for l in open(d+'/results.tsv'):
        f=l.rstrip('\n').split('\t')
        if len(f)>=6: r[(f[0],f[1])]=f[4]
    return r
b,c=load(sys.argv[1]),load(sys.argv[2])
lost=[k for k,v in b.items() if v=='PASS' and c.get(k)!='PASS']
gained=[k for k,v in c.items() if v=='PASS' and b.get(k)!='PASS']
cb=collections.Counter(b.values()); cc=collections.Counter(c.values())
print('base',dict(cb),'cand',dict(cc),'lost',len(lost),'gained',len(gained))
for k in lost[:20]: print('LOST',k)
sys.exit(1 if lost or not b else 0)
