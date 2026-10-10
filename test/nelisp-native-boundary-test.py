#!/usr/bin/env python3
"""Bounded source-pinned native boundary/admission qualification (P3.4/P3.5/P3.6)."""
import argparse, hashlib, json, os, re, resource, signal, subprocess, time
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
NAMES = ['file-exists-p', 'file-name-directory', 'p34-arith3', 'p34-optional', 'p34-rest', 'p34-values',
         'expand-file-name', 'directory-files', 'locate-file', 'emacs-redisplay--ml-spans']
def sha(p):
    with Path(p).open('rb') as f: return hashlib.file_digest(f, 'sha256').hexdigest()
def run(cmd, env, prefix, bound):
    start = time.monotonic(); before = os.getloadavg(); peak=before[0]
    def prepare(): resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    with prefix.with_suffix('.out').open('w') as out, prefix.with_suffix('.err').open('w') as err:
        executable_command=['timeout','-k','5',str(bound),*cmd]
        proc = subprocess.Popen(executable_command, cwd=ROOT, env=env, stdout=out, stderr=err,
                                stdin=subprocess.DEVNULL, start_new_session=True, preexec_fn=prepare)
        events=[]; seen=0; stage=prefix.with_suffix('.stages')
        while proc.poll() is None:
            peak=max(peak,os.getloadavg()[0])
            if stage.exists():
                lines=stage.read_text().splitlines()
                for line in lines[seen:]:
                    events.append(dict(seconds=time.monotonic()-start, label=line.split()[0]))
                seen=len(lines)
            if time.monotonic()-start>=bound:
                os.killpg(proc.pid, signal.SIGKILL); proc.wait(); rc=124; break
            time.sleep(.1)
        else: rc=proc.returncode
    return dict(rc=rc, seconds=time.monotonic()-start, timeout=bound, stage_events=events,
                load_before=before, load_after=os.getloadavg(), load_peak=peak, command=cmd, executable_command=executable_command)
def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--binary', type=Path, required=True)
    ap.add_argument('--work', type=Path, required=True)
    ap.add_argument('--backend', choices=['in-house','gccjit','template'], required=True)
    ap.add_argument('--name', choices=NAMES, required=True)
    ap.add_argument('--phase', choices=['compile','parity','timing'], required=True)
    ap.add_argument('--calls', type=int, default=100000)
    args = ap.parse_args(); work=args.work.resolve(); work.mkdir(parents=True,exist_ok=True)
    binary=args.binary.resolve(); image=Path(str(binary)+'.cold')
    if args.phase=='timing':
        waitstart=time.monotonic()
        while os.getloadavg()[0]>=4:
            if time.monotonic()-waitstart>=1800: raise SystemExit('Load <4 unavailable within 1800s')
            print('Waiting for load <4', os.getloadavg(),flush=True); time.sleep(10)
    cache=work/(args.backend+'-cache'); cache.mkdir(mode=0o700, exist_ok=True)
    prefix=work/(args.backend+'-'+args.name+'-'+args.phase)
    prefix.with_suffix('.stages').unlink(missing_ok=True)
    pins={str(p):sha(p) for p in [binary,image,Path(str(binary)+'.native-startup.el'),
          work/('input-'+args.name+'.elc'),work/('source-'+args.name+'.el'),
          ROOT/'test/nelisp-native-boundary-driver.el',ROOT/'test/nelisp-native-boundary-test.py']}
    if args.name=='emacs-redisplay--ml-spans':
        pins[str(work/'gui-helpers.el')]=sha(work/'gui-helpers.el')
    if args.name=='p34-values' and args.phase=='parity':
        for p in [work/'vm-vector-helpers.el',work/'vm-call-difference-helpers.el',
                  work/'vm-frame-helpers.el',work/'vm-dynamic-frame-helpers.el',
                  ROOT/'test/nelisp-native-vm-frame-callers.el',
                  ROOT/'test/nelisp-native-vm-frame-cold-fixture.el',
                  ROOT/'test/nelisp-native-vm-dynamic-frame-callers.el',
                  ROOT/'test/nelisp-native-vm-dynamic-frame-cold-fixture.el',
                  ROOT/'test/nelisp-native-vm-call-difference-callers.el',
                  ROOT/'test/nelisp-native-vm-call-difference-fixture.el',ROOT/'test/nelisp-native-vm-vector-callers.el',
                  ROOT/'test/nelisp-native-vm-vector-fixture.el',ROOT/'test/nelisp-native-vm-arithmetic-fixture.el']:
            pins[str(p)]=sha(p)
    if args.phase=='timing':
        pins[str(work/('caller-'+args.name+'.elc'))]=sha(work/('caller-'+args.name+'.elc'))
    env=dict(os.environ,FP_OUT=str(work),FP_FIXTURE=str(work/'fixture'),FP_NAME=args.name,
             FP_BACKEND=args.backend,FP_PHASE=args.phase,FP_CALLS=str(args.calls),
             FP_ROOT=str(ROOT),NELISP_HOME=str(ROOT),NELISP_NATIVE_CACHE=str(cache),
             NELISP_ROOTED_CFG_STAGE_LOG=str(prefix.with_suffix('.stages')))
    if args.name=='p34-values' and args.phase=='parity':
        symbols=subprocess.check_output(['nm',str(binary)],text=True)
        env['FP_VM_ARENA_BASE']=re.search(r'^([0-9a-f]+) B nl_arena_base$',symbols,re.M)[1]
    cmd=[str(binary),'--cold-load-from',str(image)]
    for p in ['lisp','src','scripts','packages/nl-ffi/src','packages/nl-prelude/src']:
        cmd+=['-L',str(ROOT/p)]
    cmd+=['--load',str(ROOT/'test/nelisp-native-boundary-driver.el')]
    # P3.6 has a 900-second execution bound and a separate 600-second
    # acceptance limit. P3.5 retains its original 290-second deadline.
    bound=(900 if args.name=='emacs-redisplay--ml-spans' else 290) if args.phase=='compile' and args.name in ['expand-file-name','directory-files','locate-file','emacs-redisplay--ml-spans'] else 1800
    if args.phase=='timing':
        runs=[]
        for mode in ['before','native','after']:
            waitstart=time.monotonic()
            while os.getloadavg()[0]>=4:
                if time.monotonic()-waitstart>=1800:raise SystemExit('Load <4 unavailable within 1800s')
                time.sleep(10)
            part=prefix.with_name(prefix.name+'-'+mode)
            runrow=run(cmd,dict(env,FP_TIMING_MODE=mode),part,bound)
            out=part.with_suffix('.out').read_text();err=part.with_suffix('.err').read_text()
            runrow.update(timing_mode=mode,stdout_sha256=sha(part.with_suffix('.out')),
                          stderr_sha256=sha(part.with_suffix('.err')))
            match=re.search(r'P34-TIME mode='+mode+r' seconds=([0-9.]+) calls=(\d+)',out)
            runrow.update(measured_seconds=float(match[1]) if match else None,
                          calls=int(match[2]) if match else None,
                          passed=runrow['rc']==0 and not err and 'P34-DONE\n' in out and bool(match))
            runs.append(runrow)
        stdout=''.join(prefix.with_name(prefix.name+'-'+mode).with_suffix('.out').read_text() for mode in ['before','native','after'])
        stderr=''.join(prefix.with_name(prefix.name+'-'+mode).with_suffix('.err').read_text() for mode in ['before','native','after'])
        prefix.with_suffix('.out').write_text(stdout);prefix.with_suffix('.err').write_text(stderr)
        row=dict(rc=max(r['rc'] for r in runs),seconds=sum(r['seconds'] for r in runs),
                 timeout=1800,command=cmd,runs=runs,load_before=runs[0]['load_before'],load_after=runs[-1]['load_after'])
    else:
        row=run(cmd,env,prefix,bound);stdout=prefix.with_suffix('.out').read_text();stderr=prefix.with_suffix('.err').read_text()
    artifact_match=re.search(r'P34-ARTIFACT file=(\".*\")',stdout)
    artifact=Path(json.loads(artifact_match[1])) if artifact_match else None
    artifacts={str(p):sha(p) for p in [artifact,Path(str(artifact)+'.nelh')] if p and p.is_file()}
    row.update(artifacts=artifacts,name=args.name,backend=args.backend,phase=args.phase,pins=pins,
               identity_unchanged=all(sha(p)==v for p,v in pins.items()),
               stdout_sha256=sha(prefix.with_suffix('.out')),stderr_sha256=sha(prefix.with_suffix('.err')))
    row['passed']=row['rc']==0 and 'P34-DONE\n' in stdout and not stderr and row['identity_unchanged']
    if args.phase=='timing':
        for part in runs:
            part.update(name=args.name,backend=args.backend,phase=args.phase,pins=pins,
                        artifacts=artifacts,identity_unchanged=row['identity_unchanged'])
        row['passed']=row['passed'] and all(r['passed'] for r in runs)
        if all(r['measured_seconds'] is not None for r in runs):
            before,native,after=[r['measured_seconds'] for r in runs]
            row.update(interpreted_before=before,native=native,interpreted_after=after,calls=args.calls,
                       ratio=native/min(before,after),
                       timing_valid=all(r['load_before'][0]<4 and r['load_after'][0]<4 and r['load_peak']<4 for r in runs),
                       speed_pass=native<=min(before,after)/2 and all(r['calls']==100000 for r in runs))
    prefix.with_suffix('.json').write_text(json.dumps(row,indent=2)+'\n')
    print(json.dumps({k:v for k,v in row.items() if k not in ['command','pins']},indent=2),flush=True)
    raise SystemExit(0 if row['passed'] else 1)
if __name__=='__main__': main()
