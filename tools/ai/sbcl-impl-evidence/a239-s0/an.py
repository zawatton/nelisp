import collections,sys
def load(n):
    c=collections.Counter(); bins=collections.Counter(); sw=0; B=0; wants=collections.Counter(); lens=collections.defaultdict(list); small=collections.Counter(); rej=collections.Counter()
    on=0
    for l in open(f'ev.{n}'):
        if l.startswith('SW'): sw+=1; on=1; continue
        if not on: continue
        if l[0]=='B': B+=1
        elif l[0]=='H':
            p=l.split(); d=dict(x.split('=',1) for x in p[1:9])
            if d['want']!='528': wants['other want=%s'%d['want']]+=1; continue
            b=int(d['bin']); cls='bin1(want bin)' if b==1 else ('bin0' if b==0 else 'higher bin')
            c[(cls,d['out'],int(d['vis']),int(d['small']),int(d['nosplit']))]+=1
            lens[cls].append(int(d['len']))
            for t in d['bts'].split(','):
                if t and int(t)<528: rej[int(t)]+=1
    return c,B,sw,wants,lens,rej
a=load(2000); b=load(8000)
print('sweeps(after first incl) 2000/8000:',a[2],b[2],' bump B events:',a[1],b[1])
print('count diff (8000-2000), per 6000 steps:')
keys=set(a[0])|set(b[0])
for k in sorted(keys,key=lambda k:-(b[0][k]-a[0][k])):
    d=b[0][k]-a[0][k]
    if d: print(k,d,'%.3f/step'%(d/6000))
print('other wants diff:',{k:b[3][k]-a[3][k] for k in set(b[3])|set(a[3]) if b[3][k]!=a[3][k]})
for cls in b[4]:
    L=b[4][cls]; print(cls,'listlen n=%d mean=%.1f max=%d'%(len(L),sum(L)/len(L),max(L)))
print('rejected small BT histogram (8000):',dict(b[5].most_common(6)))
