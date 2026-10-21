#!/usr/bin/env python3
"""Alternating CPU-pinned seven-round own-cost wall measurements."""
import argparse, hashlib, json, os
from pathlib import Path
import re, resource, statistics, subprocess, time, shutil, fcntl
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'target/hashtab'
p=argparse.ArgumentParser();p.add_argument('role',choices=['base','candidate']);p.add_argument('binary');p.add_argument('--variant',default='dynamic');p.add_argument('--base');p.add_argument('--calls',type=int,default=100000);p.add_argument('--layouts',default='0,700,1400,2100');a=p.parse_args()
probe=ROOT/'tools/ai/eqhash-wall.el';binary=Path(a.binary).resolve();gnu=Path(os.environ.get('GNU_EMACS') or shutil.which('emacs'))
dims=[(kind,test,'nil',1000,op) for test in ('eq','equal') for kind,op in [('string','hit'),('string','miss'),('string','put')]]
cases='('+' '.join('('+' '.join(map(str,d))+')' for d in dims)+')'
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
def limit():resource.setrlimit(resource.RLIMIT_AS,(8388608*1024,)*2)
def frequency():
 try:return int(Path('/sys/devices/system/cpu/cpu15/cpufreq/scaling_cur_freq').read_text())
 except OSError:return None
rows=[]
for layout in map(int,a.layouts.split(',')):
 for sample in range(7):
  for runtime,b in ([(a.role,binary),('gnu',gnu)]+([('base',Path(a.base).resolve()),('gnu-base',gnu)] if a.role=='candidate' and a.base else [])):
   stem=OUT/'wall-long'/a.role/a.variant/f'pad{layout}-run{sample}-{runtime}';stem.parent.mkdir(parents=True,exist_ok=True)
   env=dict(os.environ,ABI_LAYOUT_PAD='x'*layout,EQHASH_CASES=cases,EQHASH_OWN='1',EQHASH_CALLS=str(a.calls),TMPDIR=str(ROOT/'target/tmp'))
   cmd=['taskset','-c','15',str(b)]+(['-Q','--batch','--load'] if runtime.startswith('gnu') else ['--load'])+[str(probe)]
   # Each process is a small unit; the heavy queue does not span rounds.
   if not runtime.startswith('gnu'):
    heavy=os.environ.get('HEAVY_RUN') or next((str(parent/'bin/heavy-run.sh') for parent in (ROOT,*ROOT.parents) if (parent/'bin/heavy-run.sh').is_file()),None)
    assert heavy, 'set HEAVY_RUN to the workspace heavy-run.sh'
    cmd=[heavy,*cmd]
   cpu_lock=open(os.environ.get('HASHTAB_CPU_LOCK', '/tmp/nelisp-measure-cpu15.lock'), 'a')
   fcntl.flock(cpu_lock,fcntl.LOCK_EX)
   freq=frequency();started=time.monotonic()
   with stem.with_suffix('.out').open('wb') as out,stem.with_suffix('.err').open('wb') as err:
    r=subprocess.run(cmd,env=env,stdout=out,stderr=err,timeout=240,preexec_fn=limit)
   cpu_lock.close()
   stdout=stem.with_suffix('.out').read_text();stderr=stem.with_suffix('.err').read_text()
   assert r.returncode==0 and not stderr and stdout.endswith('EQHASH-WALL-COMPLETE\n'), (str(stem),r.returncode,stderr[:100])
   values={}
   for fields in re.findall(r'^EQHASH-WALL (\w+) (\w+) (\w+) (\d+) (\w+) ([01]) ([0-9.]+) (\d+)$',stdout,re.M):
    kind,test,weak,n,op,control,secs,calls=fields
    values[f'{test}/{op}/{control}']=float(secs)*1e9/int(calls)
   assert len(values)==12
   row=dict(runtime=runtime,layout=layout,sample=sample,calls=a.calls,cpu=15,frequency_before=freq,frequency_after=frequency(),elapsed=time.monotonic()-started,values=values,binary=dict(path=str(b),sha256=sha(b)),probe_sha256=sha(probe))
   stem.with_suffix('.json').write_text(json.dumps(row,indent=2)+'\n');rows.append(row)
   print(f'{a.role}/{a.variant} pad{layout} round{sample} {runtime} {row["elapsed"]:.2f}s',flush=True)
(OUT/f'wall-{a.role}-{a.variant}.json').write_text(json.dumps(rows,indent=2)+'\n')
for test in ('eq','equal'):
 for op in ('hit','miss','put'):
  ratios=[];own=[]
  for layout in sorted({r['layout'] for r in rows}):
   for sample in range(7):
    costs=[]
    for runtime in (a.role,'gnu'):
     v=next(r['values'] for r in rows if (r['layout'],r['sample'],r['runtime'])==(layout,sample,runtime))
     costs.append(v[f'{test}/{op}/0']-v[f'{test}/{op}/1'])
    assert all(c>0 for c in costs),(test,op,costs)
    own.append(costs[0]);ratios.append(costs[0]/costs[1])
  print(test,op,'median_paired_ratio',statistics.median(ratios),'own_ns',statistics.median(own))
