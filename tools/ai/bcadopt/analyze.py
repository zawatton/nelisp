import os,collections,json,sys
D='target/bcadopt/probe/run'
tup=collections.OrderedDict()
for l in open('target/bcadopt/work/tuples.tsv'):
    n,k,form,res=l.rstrip('\n').split('\t')
    tup.setdefault(n,{'kind':k,'rows':[]})['rows'].append((form,res))
def parse(p):
    if not os.path.exists(p): return None
    d={}
    done=False
    for l in open(p,errors='replace').read().split('\n'):
        if l=='DONE': done=True; continue
        if l.startswith('DEF\t'): d['DEF']=l[4:]; continue
        if '\t' in l:
            i,r=l.split('\t',1)
            if i.isdigit(): d[int(i)]=r
    return d,done
results={}
for n in open(D+'/names.txt').read().split():
    d=tup[n]
    B=parse('%s/%s.B.out'%(D,n)); C=parse('%s/%s.C.out'%(D,n))
    rcB=open('%s/%s.B.rc'%(D,n)).read().strip() if os.path.exists('%s/%s.B.rc'%(D,n)) else '?'
    rcC=open('%s/%s.C.rc'%(D,n)).read().strip() if os.path.exists('%s/%s.C.rc'%(D,n)) else '?'
    res={'kind':d['kind'],'rcB':rcB,'rcC':rcC}
    if not B or not B[1]: res['status']='reject'; res['reason']='source-run-incomplete-rc%s'%rcB; results[n]=res; continue
    if not C or not C[1]: res['status']='reject'; res['reason']='compiled-run-incomplete-rc%s'%rcC; results[n]=res; continue
    Bd,Cd=B[0],C[0]
    if Bd.get('DEF')!='t': res['status']='reject'; res['reason']='live-definition-differs-from-prelude-source'; results[n]=res; continue
    rows=d['rows']; kept=[]; diverge=[]; 
    for i,(form,a) in enumerate(rows):
        b=Bd.get(i); c=Cd.get(i)
        if b!=c: diverge.append((i,b,c)); continue
        if b is None or 'driver-err' in b: continue
        if d['kind']=='gnu':
            if a!=b: continue
        kept.append((i,form,b))
    nonerr=[k for k in kept if not k[2].startswith('(:err')]
    if diverge: res['status']='reject'; res['reason']='parity-divergence-source-vs-compiled'; res['diverge']=diverge[:2]
    elif d['kind']=='gnu' and len(nonerr)>=1 and len(kept)>=2: res['status']='pass'
    elif d['kind']=='int' and len(nonerr)>=2: res['status']='pass'
    else: res['status']='reject'; res['reason']='insufficient-agreeing-nonerror-cases(%d)'%len(nonerr)
    if res['status']=='pass':
        kept=kept[:8]
        res['kept']=[(f,r) for i,f,r in kept]
    results[n]=res
json.dump(results,open('target/bcadopt/work/results.json','w'))
c=collections.Counter((r['kind'],r['status'],r.get('reason','')[:40]) for r in results.values())
for k,v in sorted(c.items()): print(v,k)
