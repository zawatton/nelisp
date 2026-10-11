#!/usr/bin/env bash
# Each native process has a 290 s deadline, immutable reader and receipt.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    --eval '(setq load-prefer-newer t)' -l nelisp-native-frame-u7a-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  "$0" "${2:-target/nelisp-static}" in-house || status=1
  "$0" "${3:-target/nelisp-dyn}" gccjit || status=1
  exit "$status"
fi
if [[ ${1:-} == --backend ]]; then
  selected_backend=${2:?missing backend}; shift 2
  selected_binary=${1:-target/nelisp-static}; shift "$(( $# > 0 ? 1 : 0 ))"
  set -- "$selected_binary" "$selected_backend" "$@"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
case "$backend" in in-house|gccjit|template) ;; *) exit 2;; esac
work=$(mktemp -d "$root/target/frame-u7a-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U7A_BACKEND="$backend"
python3 - "$work" "${3:-callback alias constant nested implicit set zero 1 2 3 4 5 buffer-local watcher}" <<'PY'
import concurrent.futures,hashlib,json,os,subprocess,sys,time
from pathlib import Path
work,selection=sys.argv[1:]; directory=Path(work); binary=directory/'reader'; cold=directory/'reader.cold'
identity=dict(binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
              observer_sha256=hashlib.sha256(Path('test/support/native-entry-observer.el').read_bytes()).hexdigest(),
              driver_sha256=hashlib.sha256(Path('test/standalone-native-frame-u7a-driver.el').read_bytes()).hexdigest(),
              startup_sha256=hashlib.sha256(Path(str(binary)+'.native-startup.el').read_bytes()).hexdigest() if Path(str(binary)+'.native-startup.el').is_file() else None,
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None)
cases=selection.split()
if not cases or len(set(cases)) != len(cases) or any(c not in {'callback','alias','constant','nested','implicit','set','zero','1','2','3','4','5','buffer-local','watcher'} for c in cases):
    raise SystemExit('Invalid U7a fixture selection')
jobs=int(os.environ.get('U7A_JOBS','4'))
if not 1 <= jobs <= 4: raise SystemExit('U7A_JOBS must be 1..4')
def run(case):
    cache=directory/('cache-'+case); cache.mkdir(mode=0o700)
    logs=[]
    for phase in ['compile','load']:
        env=os.environ.copy(); env.update(U7A_CASE=case,U7A_PHASE=phase,NELISP_NATIVE_CACHE=str(cache))
        command=['timeout','-k','5','290',str(binary)]
        if env['U7A_BACKEND'] != 'gccjit' and cold.is_file(): command+=['--cold-load-from',str(cold)]
        command+=['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                  '--load','test/standalone-native-frame-u7a-driver.el']
        prefix=case+'-'+phase
        start=time.monotonic()
        with (directory/(prefix+'.out')).open('w') as out,(directory/(prefix+'.err')).open('w') as err:
            result=subprocess.run(command,env=env,stdout=out,stderr=err)
        elapsed=time.monotonic()-start
        output=(directory/(prefix+'.out')).read_text(); errors=(directory/(prefix+'.err')).read_text()
        ok=result.returncode==0 and elapsed<300 and not errors and 'U7A-NATIVE-PASS' in output and 'phase='+phase in output
        if case=='callback': ok &= 'U7A-AUTH-PASS' in output
        (directory/(prefix+'.json')).write_text(json.dumps(dict(backend=env['U7A_BACKEND'],fixture=case,phase=phase,seconds=elapsed,
            rc=result.returncode,passed=ok,**identity),indent=2))
        logs.append(output+errors[-3000:])
        if not ok: return False,''.join(logs)
    return True,''.join(logs)
with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
    results=list(pool.map(run,cases))
for ok,output in results: print(output,end='')
print('U7A-EVIDENCE='+work)
if not all(ok for ok,_ in results): raise SystemExit(1)
PY
