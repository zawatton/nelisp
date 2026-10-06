#!/usr/bin/env bash
# One bounded reader answers evaluator/VM parity and registry-lifetime questions.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
python3 - "${1:?Usage: standalone-throw-no-catch-smoke.sh BINARY}" <<'PY'
import hashlib, json, os, shutil, subprocess, sys, tempfile, time
from pathlib import Path
root=Path.cwd()
work=Path(tempfile.mkdtemp(prefix='throw-nocatch-',dir=root/'target'))
binary=Path(sys.argv[1]).resolve(strict=True)
reader=work/'reader'; shutil.copyfile(binary,reader); reader.chmod(0o500)
cold=Path(str(binary)+'.cold')
if cold.is_file(): shutil.copyfile(cold,Path(str(reader)+'.cold'))
source=root/'test/support/throw-no-catch-fixtures.el'
fixture=work/'fixture.el'; shutil.copyfile(source,fixture)
env=os.environ.copy(); env['THROW_NOCATCH_FIXTURE']=str(fixture)
emacs=env.get('EMACS','emacs')
def run(command,prefix,limit):
    start=time.monotonic()
    with (work/(prefix+'.out')).open('w') as out,(work/(prefix+'.err')).open('w') as err:
        r=subprocess.run(command,env=env,stdout=out,stderr=err,timeout=limit)
    output=(work/(prefix+'.out')).read_text(); errors=(work/(prefix+'.err')).read_text()
    elapsed=time.monotonic()-start
    if r.returncode or errors:
        raise RuntimeError(f'{prefix}: rc={r.returncode} stderr={errors[-2000:]} evidence={work}')
    return output,elapsed
# Require a real compiled fixture, with compilation warnings treated as errors.
run([emacs,'-Q','--batch','--eval',
     '(progn (require (quote bytecomp)) (setq byte-compile-error-on-warn t) '
     '(unless (byte-compile-file (getenv "THROW_NOCATCH_FIXTURE")) (error "Fixture compile failed")))'],
    'compile',30)
env['THROW_NOCATCH_FIXTURE']=str(fixture.with_suffix('.elc'))
expected,_=run([emacs,'-Q','--batch','-l',str(source),'--eval',
                '(progn (throw-nocatch-observe (quote eval)) (throw-nocatch-observe (quote vm)))'],
               'oracle',30)
nm=subprocess.run(['nm','--defined-only',str(reader)],capture_output=True,text=True,check=True,timeout=30)
heads=[line.split()[0] for line in nm.stdout.splitlines() if line.split()[-1]=='nl_catch_head']
if len(heads)!=1: raise RuntimeError(f'Missing unique catch registry symbol: {work}')
env['THROW_NOCATCH_HEAD']=str(int(heads[0],16))
command=['timeout','-k','5','290',str(reader)]
if Path(str(reader)+'.cold').is_file(): command+=['--cold-load-from',str(reader)+'.cold']
for folder in ('lisp','src','scripts','packages/nl-ffi/src','packages/nl-prelude/src'):
    command+=['-L',folder]
command+=['--load','test/standalone-throw-no-catch-driver.el']
output,seconds=run(command,'reader',299)
records=''.join(line+'\n' for line in output.splitlines() if line.startswith(('THROW-OBS ','THROW-COMPLETE ')))
passed=records==expected and records.count('THROW-OBS ')>0 and output.splitlines().count('THROW-BALANCED')==1
receipt=dict(passed=passed,seconds=seconds,binary_sha256=hashlib.sha256(reader.read_bytes()).hexdigest(),
             observations=records.count('THROW-OBS'),oracle_version=subprocess.check_output([emacs,'--version'],text=True).splitlines()[0])
(work/'receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
if not passed: raise RuntimeError(f'Throw parity or balance failed: {work}')
print(f'THROW-NOCATCH-PASS observations={receipt["observations"]} seconds={seconds:.3f} evidence={work}')
PY
