import sys,re,collections
# usage: cgparse.py callgrind.out  -> per-address Ir for fn matching wf_bytecode_w_fast_run, sorted by addr
f=sys.argv[1]; want=sys.argv[2] if len(sys.argv)>2 else 'wf_bytecode_w_fast_run'
cur=None;addr=0;cost=collections.Counter();names={};infn=False;skip=0
tot=collections.Counter()
for l in open(f):
    l=l.rstrip('\n')
    m=re.match(r'fn=\((\d+)\)(?: (.*))?',l)
    if m:
        if m[2]: names[m[1]]=m[2]
        infn=want in names.get(m[1],''); continue
    if l.startswith(('cfn=','cfi=','cfl=','fl=','fi=','fe=','ob=','cob=')):
        m=re.match(r'cfn=\((\d+)\)(?: (.*))?',l)
        if m and m[2]: names[m[1]]=m[2]
        continue
    if l.startswith('calls='): skip=1; continue
    m=re.match(r'(0x[0-9a-f]+|\+\d+|-\d+|\*)\s+(\S+)\s+(\d+)',l)
    if m:
        a=m[1]
        if a.startswith('0x'): addr=int(a,16)
        elif a[0]=='+': addr+=int(a[1:])
        elif a[0]=='-': addr-=int(a[1:])
        if skip: skip=0; continue
        if infn: cost[addr]+=int(m[3])
for a in sorted(cost): print(hex(a),cost[a])
