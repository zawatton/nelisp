import json,sys,collections
# usage: mkfixtures.py results.json out.tsv [exclude.json]
res=json.load(open(sys.argv[1])); out=sys.argv[2]
excl=set(json.load(open(sys.argv[3]))) if len(sys.argv)>3 else set()
dig={}
for l in open('target/bcadopt/work/report-host1.tsv'):
    if l.startswith('#'): continue
    f=l.rstrip('\n').split('\t')
    if len(f)>5: dig.setdefault(f[2],f[5])
base=open('target/bcadopt/seed/test/nelisp-prelude-bytecode-parity-fixtures.tsv').read()
rows=[]
for n,r in res.items():
    if r['status']!='pass' or n in excl: continue
    kept=r['kept']
    case='(list '+' '.join(f for f,_ in kept)+')'
    hostres='('+' '.join(x for _,x in kept)+')'
    reason='gnu-31.1-reference-source-compiled-parity' if r['kind']=='gnu' else 'runtime-source-compiled-parity-no-host-equivalent'
    assert '\t' not in case and '\n' not in case and '\t' not in hostres and '\n' not in hostres,n
    rows.append('\t'.join([n,case,hostres,'pass',reason,'bytecode',dig[n],'-']))
open(out,'w').write(base+''.join(r+'\n' for r in rows))
print(len(rows))
