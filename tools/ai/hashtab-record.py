#!/usr/bin/env python3
"""Record a focused fixture without losing exit status or stderr."""
import argparse,hashlib,json,os,resource,subprocess,re
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2];OUT=ROOT/'target/hashtab'
p=argparse.ArgumentParser();p.add_argument('kind',choices=['layout','semantics','final']);p.add_argument('dynamic');p.add_argument('static',nargs='?');p.add_argument('--common');p.add_argument('--standalone');p.add_argument('--mcp');p.add_argument('--wine');p.add_argument('--return-codes');a=p.parse_args()
def ident(p):
 p=Path(p).resolve();return dict(path=str(p.relative_to(ROOT)) if p.is_relative_to(ROOT) else str(p),sha256=hashlib.sha256(p.read_bytes()).hexdigest())
def limit():resource.setrlimit(resource.RLIMIT_AS,(8388608*1024,)*2)
inputs=[ident(ROOT/'scripts/nelisp-standalone-build.el'),ident(ROOT/'scripts/nelisp-stdlib-prelude.el'),ident(ROOT/'lisp/nelisp-cc-reader-parser.el')];failures=[]
if a.kind=='final':
 inputs.extend(ident(b) for b in (a.dynamic,a.static))
 logs={}
 for name in ('common','standalone','mcp','wine'):
  path=Path(getattr(a,name));inputs.append(ident(path));logs[name]=path.read_text()
 codes=list(map(int,a.return_codes.split(',')))
 if len(codes)!=4 or any(codes):failures.append('nonzero final command return code')
 groups=re.findall(r'^([\w-]+) rc=(-?[0-9]+) [0-9]+s$',logs['common'],re.M)
 if len(groups)!=11 or any(rc!='0' for _,rc in groups) or 'GATE-COUNT: 11 cases, 0 failures' not in logs['common']:
  failures.append('COMMON needs 11 successful groups')
 if not re.search(r'^GATE-COUNT: [1-9][0-9]* cases, 0 failures$',logs['standalone'],re.M) or 'STANDALONE-CHECKS PASS' not in logs['standalone']:
  failures.append('standalone checks did not pass')
 tools=[]
 for line in logs['mcp'].splitlines():
  try:reply=json.loads(line)
  except json.JSONDecodeError:continue
  if isinstance(reply,dict) and isinstance(reply.get('result'),dict) and isinstance(reply['result'].get('tools'),list):tools.append(reply['result']['tools'])
 if len(tools)!=1 or len(tools[0])!=40:failures.append('MCP needs one response with 40 tools')
 if not re.search(r'^GATE-COUNT: [1-9][0-9]* cases, 0 failures$',logs['wine'],re.M):failures.append('wine smoke did not pass')
 (OUT/'final.json').write_text(json.dumps(dict(inputs=inputs,variants=['dynamic']+(['static'] if a.static else []),**{'pass':not failures},reason='; '.join(failures)),indent=2)+'\n')
 print('FINAL', 'PASS' if not failures else '; '.join(failures))
 raise SystemExit(0 if not failures else 1)
for variant,binary in [('dynamic',a.dynamic)]+([('static',a.static)] if a.static else []):
 inputs.append(ident(binary))
 if Path(binary+'.cold').exists():inputs.append(ident(binary+'.cold'))
 fixtures=[('layout','tools/ai/hashtab-layout.el',None)] if a.kind=='layout' else [('eqhash','tools/ai/eqhash-semantics.el','tools/ai/eqhash-semantics.expected'),('extra','tools/ai/hashtab-semantics.el','tools/ai/hashtab-semantics.expected')]
 for label,fixture,expected in fixtures:
  inputs.append(ident(fixture));stem=OUT/f'{a.kind}-{variant}-{label}'
  with stem.with_suffix('.out').open('wb') as out,stem.with_suffix('.err').open('wb') as err:
   r=subprocess.run([str(Path(binary).resolve()),'--load',fixture],stdout=out,stderr=err,timeout=180,preexec_fn=limit,env=dict(os.environ,TMPDIR=str(ROOT/'target/tmp')))
  inputs.extend(ident(stem.with_suffix(ext)) for ext in ('.out','.err'))
  output=stem.with_suffix('.out').read_text();errors=stem.with_suffix('.err').read_text()
  if r.returncode or errors:failures.append(f'{variant}/{label}: exit={r.returncode}, stderr_bytes={len(errors)}')
  if expected:
   inputs.append(ident(expected));wanted=Path(expected).read_text()
   if output!=wanted:
    actual={l.split(' ',2)[1]:l for l in output.splitlines() if l.startswith('(eqhash ')}
    expected_cases={l.split(' ',2)[1]:l for l in wanted.splitlines() if l.startswith('(eqhash ')}
    names=[k for k,v in expected_cases.items() if actual.get(k)!=v]
    failures.append(f'{variant}/{label}: mismatches '+','.join(names or ['output']))
  elif not output.endswith('HASHTAB-LAYOUT-COMPLETE\n'):failures.append(variant+': missing layout marker')
  print(variant,label,'rc',r.returncode,'stderr_bytes',len(errors),flush=True)
(OUT/(a.kind+'.json')).write_text(json.dumps(dict(inputs=inputs,variants=['dynamic']+(['static'] if a.static else []),**{'pass':not failures},reason='; '.join(failures)),indent=2)+'\n')
print('FAILURES',failures)
