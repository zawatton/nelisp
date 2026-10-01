#!/usr/bin/env bash
set -euo pipefail
mode=${1:?usage: doc211-baseline-check.sh produce|ledgers|usable|ccore|all}
root=$(cd "$(dirname "$0")/../.." && pwd)
notes=${NOTES_ROOT:-}
if [[ -z $notes ]]; then
 d=$root
 for _ in 1 2 3 4 5 6; do
  if [[ -x $d/bin/progress-meter.sh ]]; then notes=$d; break; fi
  if [[ -x $d/Cowork/Notes/bin/progress-meter.sh ]]; then notes=$d/Cowork/Notes; break; fi
  parent=$(dirname "$d"); [[ $parent == "$d" ]] && break; d=$parent
 done
fi
meter=${PROGRESS_METER:-${notes:+$notes/bin/progress-meter.sh}}
out=${DOC211_EVIDENCE_DIR:-$root/target/progress}
evidence=${DOC211_EVIDENCE:-$out/doc211-baseline-evidence.json}
bin=${NELISP_BIN:-}; eln_bin=${ELN_PROGRESS_BIN:-$bin}
[[ -x ${meter:-} ]] || { echo 'set PROGRESS_METER to Notes/bin/progress-meter.sh' >&2; exit 2; }
mkdir -p "$out"
source_digest() {
 python3 - "$root" <<'PY'
import hashlib,pathlib,subprocess,sys
r=pathlib.Path(sys.argv[1]); paths=subprocess.check_output(['git','-C',str(r),'ls-files','-co','--exclude-standard','-z']).decode().split('\0'); h=hashlib.sha256()
for s in sorted(x for x in paths if x and not x.startswith(('.git/','target/','build/'))):
 p=r/s
 if p.is_file(): h.update(s.encode()+b'\0'+hashlib.sha256(p.read_bytes()).digest())
print(h.hexdigest())
PY
}
sha() { sha256sum "$1" | cut -d' ' -f1; }
case "$mode" in
produce|produce-usable|produce-ccore|produce-eln|produce-gates)
 [[ -x $bin && -x $eln_bin ]] || { echo 'set NELISP_BIN and ELN_PROGRESS_BIN to executable in-tree binaries' >&2; exit 2; }
 before=$(source_digest); bsha=$(sha "$bin"); esha=$(sha "$eln_bin")
 export NELISP_BIN="$bin" ELN_PROGRESS_BIN="$eln_bin" PROGRESS_OUT_DIR="$out"
 family=${mode#produce-}; [[ $mode == produce ]] && family=all
 python3 - "$root" "$out" "$evidence" "$meter" "$before" "$bsha" "$esha" "$family" <<'PY'
import json,os,pathlib,re,subprocess,sys,time,collections,signal
r,out,evidence,meter,source,bsha,esha,family=sys.argv[1:]; r=pathlib.Path(r); out=pathlib.Path(out); rows=[]
def cold_identity(path):
 p=pathlib.Path(path+'.cold')
 return (str(p),__import__('hashlib').sha256(p.read_bytes()).hexdigest()) if p.is_file() else (None,None)
cold_n,cold_n_sha=cold_identity(os.environ['NELISP_BIN']); cold_e,cold_e_sha=cold_identity(os.environ['ELN_PROGRESS_BIN'])
def source_hash():
 paths=subprocess.check_output(['git','-C',str(r),'ls-files','-co','--exclude-standard','-z']).decode().split('\0'); h=__import__('hashlib').sha256()
 for s in sorted(x for x in paths if x and not x.startswith(('.git/','target/','build/'))):
  p=r/s
  if p.is_file(): h.update(s.encode()+b'\0'+__import__('hashlib').sha256(p.read_bytes()).digest())
 return h.hexdigest()
def process_table():
 processes={}
 for stat in pathlib.Path('/proc').glob('[0-9]*/stat'):
  try:
   fields=stat.read_text().rsplit(')',1)[1].split()
   # After comm: state, ppid, pgrp, session, ..., starttime.
   processes[int(stat.parent.name)]=(int(fields[1]),int(fields[19]))
  except (OSError,ValueError,IndexError): pass
 return processes
def descendant_identities(root,processes):
 owned={root:processes[root][1]} if root in processes else {}
 return expand_identities(owned,processes)
def expand_identities(owned,processes):
 owned=owned.copy()
 changed=True
 while changed:
  changed=False
  for pid,(ppid,starttime) in processes.items():
   if pid not in owned and ppid in owned: owned[pid]=starttime; changed=True
 return owned
def signal_owned(owned,sig):
 current=process_table()
 for pid,starttime in owned.items():
  if current.get(pid,(-1,-1))[1]!=starttime: continue
  try: os.kill(pid,sig)
  except ProcessLookupError: pass
def run(label,cmd,timeout=55):
 t=time.monotonic()
 p=subprocess.Popen(cmd,cwd=r,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True,env=os.environ.copy(),start_new_session=True)
 try: output,_=p.communicate(timeout=timeout); rc=p.returncode
 except subprocess.TimeoutExpired as e:
  rc=124
  before_table=process_table()
  before=descendant_identities(p.pid,before_table) # Capture descendants before signaling the leader.
  signal_owned({pid:start for pid,start in before.items() if pid!=p.pid},signal.SIGTERM)
  signal_owned({p.pid:before[p.pid]} if p.pid in before else {},signal.SIGTERM)
  time.sleep(0.5)
  after_table=process_table() # One refresh; includes children in new PGIDs or SIDs.
  after=descendant_identities(p.pid,after_table)
  survivors={pid:start for pid,start in before.items() if after_table.get(pid,(-1,-1))[1]==start}
  refreshed=expand_identities({**survivors,**after},after_table)
  signal_owned({pid:start for pid,start in refreshed.items() if pid!=p.pid},signal.SIGKILL)
  signal_owned({p.pid:refreshed[p.pid]} if p.pid in refreshed else {},signal.SIGKILL)
  try: output,_=p.communicate(timeout=2)
  except subprocess.TimeoutExpired as final:
   output=final.output or e.output or ''
   if isinstance(output,bytes): output=output.decode(errors='replace')
   if p.stdout: p.stdout.close()
   try: p.wait(timeout=1)
   except subprocess.TimeoutExpired: p.kill(); p.wait()
  if output is None: output=''
  if isinstance(output,bytes): output=output.decode(errors='replace')
  output+='\nPRODUCER_TIMEOUT'
 rows.append({'id':label,'cmd':cmd,'rc':rc,'seconds':round(time.monotonic()-t,3),'output':output if label=='preflight' else output[-4000:]}); return rows[-1]
ledger_names={'usable':['tools/ai/usable-progress.org'],'ccore':['tools/ai/c-core-progress.org'],'eln':['tools/ai/eln-progress.org'],'all':['tools/ai/usable-progress.org','tools/ai/c-core-progress.org','tools/ai/eln-progress.org'],'gates':[]}[family]
for ledger in ledger_names:
 ids=re.findall(r'^\*\* ([SC]\d+\.\d+)',(r/ledger).read_text(),re.M); seen=collections.Counter()
 for cid in ids:
  seen[cid]+=1; key=ledger+':'+cid+'#'+str(seen[cid])
  row={'id':key,'criterion_id':cid,'occurrence':seen[cid],'cmd':[meter,'--ledger',ledger,'--root',str(r),'--only',cid,'--json','--checkpoint']}
  result=run(key,row['cmd'],65); row.update(rc=result['rc'],seconds=result['seconds'],output=result['output']); row['id']=key
  matches=re.findall(r'checkpoint:\s+'+re.escape(cid)+r'\s+(PASS|FAIL|SKIP|UNKNOWN|PENDING)',result['output'])
  rows[-1].update(criterion_id=cid,occurrence=seen[cid],criterion_status=matches[seen[cid]-1] if len(matches)>=seen[cid] else None)
if family in ('gates','all'):
 listing=run('preflight-list',['bash','tools/ai/preflight.sh','--list'])
 gate_specs=re.findall(r'"([A-Za-z0-9_-]+)\|([^"\n]+)"',listing['output'])
 for name,command in gate_specs: run('gate:'+name,['bash','-c',command],3600)
 run('bundle-boot',[os.environ['NELISP_BIN'],'--eval','(progn (load (expand-file-name "build/nemacs-bootstrap.el") nil t) (princ "DOC211-BOOT-OK"))'])
pathlib.Path(evidence).parent.mkdir(parents=True,exist_ok=True)
if source_hash()!=source or __import__('hashlib').sha256(pathlib.Path(os.environ['NELISP_BIN']).read_bytes()).hexdigest()!=bsha or __import__('hashlib').sha256(pathlib.Path(os.environ['ELN_PROGRESS_BIN']).read_bytes()).hexdigest()!=esha or cold_identity(os.environ['NELISP_BIN'])!=(cold_n,cold_n_sha) or cold_identity(os.environ['ELN_PROGRESS_BIN'])!=(cold_e,cold_e_sha): raise SystemExit('inputs changed during producer; no evidence written')
payload={'schema':1,'source_sha256':source,'nelisp_bin':os.environ['NELISP_BIN'],'nelisp_sha256':bsha,'nelisp_cold_path':cold_n,'nelisp_cold_sha256':cold_n_sha,'eln_bin':os.environ['ELN_PROGRESS_BIN'],'eln_sha256':esha,'eln_cold_path':cold_e,'eln_cold_sha256':cold_e_sha,'rows':rows}
ep=pathlib.Path(evidence)
if ep.exists():
 old=json.loads(ep.read_text())
 if all(old.get(k)==payload[k] for k in ('source_sha256','nelisp_bin','nelisp_sha256','nelisp_cold_path','nelisp_cold_sha256','eln_bin','eln_sha256','eln_cold_path','eln_cold_sha256')):
  replaced={z['id'] for z in rows}; payload['rows']=[z for z in old.get('rows',[]) if z['id'] not in replaced]+rows
tmp=ep.with_suffix(ep.suffix+'.tmp'); tmp.write_text(json.dumps(payload,indent=2)+'\n'); os.replace(tmp,ep)
print('evidence:',evidence,'rows:',len(rows),'failed:',sum(x['rc']!=0 for x in rows))
PY
 [[ $(source_digest) == "$before" ]] || { echo 'source changed during production; discard evidence and rerun' >&2; exit 1; }
 ;;
ledgers|usable|ccore|all)
 python3 - "$mode" "$root" "$evidence" "$bin" "$eln_bin" "$out" <<'PY'
import hashlib,json,pathlib,re,subprocess,sys
mode,root,ev,bin,eln,out=sys.argv[1:]; r=pathlib.Path(root); out=pathlib.Path(out); p=pathlib.Path(ev)
def fail(msg): raise SystemExit('FAIL '+msg)
if not p.is_file(): fail('missing evidence artifact '+str(p))
x=json.loads(p.read_text()); rows=x.get('rows',[])
paths=subprocess.check_output(['git','-C',root,'ls-files','-co','--exclude-standard','-z']).decode().split('\0'); h=hashlib.sha256()
for s in sorted(q for q in paths if q and not q.startswith(('.git/','target/','build/'))):
 f=r/s
 if f.is_file(): h.update(s.encode()+b'\0'+hashlib.sha256(f.read_bytes()).digest())
if x.get('source_sha256')!=h.hexdigest(): fail('stale source identity')
for k,path in [('nelisp_sha256',bin),('eln_sha256',eln)]:
 if not path or not pathlib.Path(path).is_file(): fail('missing binary artifact '+str(path))
 if x.get(k)!=hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest(): fail('stale binary '+str(path))
if x.get('nelisp_bin')!=bin or x.get('eln_bin')!=eln: fail('binary path identity changed')
for prefix,path in [('nelisp',bin),('eln',eln)]:
 cold=pathlib.Path(path+'.cold'); stored_path=x.get(prefix+'_cold_path'); stored_hash=x.get(prefix+'_cold_sha256')
 current_path=str(cold) if cold.is_file() else None
 current_hash=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None
 if (stored_path,stored_hash)!=(current_path,current_hash): fail('stale/missing companion cold image '+str(cold))
expected=[]
for ledger in ('tools/ai/usable-progress.org','tools/ai/c-core-progress.org','tools/ai/eln-progress.org'):
 ids=re.findall(r'^\*\* ([SC]\d+\.\d+)',(r/ledger).read_text(),re.M); seen={}
 for i in ids:
  seen[i]=seen.get(i,0)+1; expected.append(ledger+':'+i+'#'+str(seen[i]))
expected += ['preflight-list','bundle-boot']; by={z.get('id'):z for z in rows}
if mode=='all':
 listing=by.get('preflight-list',{}).get('output','')
 current=subprocess.run(['bash','tools/ai/preflight.sh','--list'],cwd=r,text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
 if current.returncode: fail('current preflight --list failed rc='+str(current.returncode))
 def gate_map(text):
  pairs=re.findall(r'"([A-Za-z0-9_-]+)\|([^"\n]+)"',text)
  if not pairs: fail('preflight --list contains no parseable gates')
  names=[name for name,_ in pairs]
  if len(names)!=len(set(names)): fail('preflight --list has duplicate gate names')
  return dict(pairs)
 recorded_gates=gate_map(listing); current_gates=gate_map(current.stdout)
 if recorded_gates!=current_gates: fail('recorded preflight gate list is stale or incomplete')
 expected += ['gate:'+name for name in current_gates]
if mode in ('usable','ccore','ledgers'):
 wanted={'usable':['tools/ai/usable-progress.org:'],'ccore':['tools/ai/c-core-progress.org:'],'ledgers':['tools/ai/usable-progress.org:','tools/ai/c-core-progress.org:']}[mode]
 expected=[i for i in expected if any(i.startswith(prefix) for prefix in wanted)]
if len(by)!=len(rows): fail('duplicate evidence rows')
missing=set(expected)-set(by)
if missing: fail('omitted required rows '+','.join(sorted(missing)))
for i in expected:
 z=by[i]
 if not isinstance(z.get('rc'),int) or not isinstance(z.get('seconds'),(int,float)) or 'cmd' not in z: fail('missing actual exit/status/timing '+i)
 if i.split(':',1)[0].endswith('.org') and not isinstance(by[i].get('criterion_status'),str): fail('missing criterion status '+i)
 if not i.split(':',1)[0].endswith('.org') and z['rc']: fail(i+' failed rc='+str(z['rc']))
if mode=='all':
 eln_rows=[by[i] for i in expected if i.startswith('tools/ai/eln-progress.org:')]
 if len(eln_rows)!=70 or any(z.get('criterion_status')!='PASS' for z in eln_rows): fail('ELN baseline is not 70/70 PASS')
if mode in ('usable','all'):
 f={i.rsplit(':',1)[1].split('#',1)[0] for i in expected if i.startswith('tools/ai/usable-progress.org:') and by[i].get('criterion_status')=='FAIL'}
 usable_rows=[by[i] for i in expected if i.startswith('tools/ai/usable-progress.org:')]
 if len(usable_rows)!=30 or sum(z.get('criterion_status')=='PASS' for z in usable_rows)!=27 or f!={'S2.5','S5.4','S6.2'}: fail('usable counts/fail set differ: '+repr(sorted(f)))
if mode in ('ccore','all'):
 snap=r/'target/progress/doc211-ccore-freeze.json'; got=out/'c-core-progress.json'
 if not snap.is_file() or not got.is_file(): fail('missing c-core snapshot/current artifact')
 def status(q): return {c['id']:c['status'] for s in json.loads(q.read_text())['stages'] for c in s['criteria']}
 baseline,current=status(snap),status(got)
 if current!=baseline: fail('c-core snapshot differs')
 for i in expected:
  if i.startswith('tools/ai/c-core-progress.org:'):
   cid=by[i].get('criterion_id') or i.split(':',1)[1].split('#',1)[0]
   if by[i].get('criterion_status')!=baseline.get(cid): fail('c-core recorded criterion differs from snapshot '+cid)
if mode=='all':
 gates=[i for i in expected if i.startswith('gate:')]
 if not gates: fail('cannot enumerate preflight gates')
 for i in gates:
  name=i[5:]
  if by[i].get('cmd')!=['bash','-c',current_gates[name]]: fail('gate command mismatch '+name)
 failed=[i for i in gates if by[i]['rc']!=0]
 if failed: fail('preflight gates not PASS: '+','.join(i[5:] for i in failed))
 if by['bundle-boot']['rc'] or by['bundle-boot']['seconds']>=50 or 'DOC211-BOOT-OK' not in by['bundle-boot']['output']: fail('bundle boot acceptance failed')
 print('PASS Doc211 baseline evidence: usable=27/30, c-core snapshot, ELN, preflight, boot <50s')
else: print('PASS',mode,'evidence validated')
PY
 ;;
*) echo "usage: $0 produce[-usable|-ccore|-eln|-gates]|ledgers|usable|ccore|all" >&2; exit 2 ;;
esac
