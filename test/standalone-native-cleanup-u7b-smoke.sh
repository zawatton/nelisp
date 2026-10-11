#!/usr/bin/env bash
# Bounded fixture/mode cohorts retain cold/warm evidence below 300 seconds.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-cleanup-u7b-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  "$0" "${2:-target/nelisp-static}" in-house "${U7B_SELECTION:-97 114 138 140 ordered implicit forms}" || status=1
  "$0" "${3:-target/nelisp-dyn}" gccjit "${U7B_SELECTION:-97 114 138 140 ordered implicit forms}" || status=1
  exit "$status"
fi
if [[ ${1:-} == --backend ]]; then
  selected_backend=${2:?missing backend}; shift 2
  selected_binary=${1:-target/nelisp-static}; shift "$(( $# > 0 ? 1 : 0 ))"
  set -- "$selected_binary" "$selected_backend" "$@"
fi
binary=${1:-target/nelisp-static} backend=${2:-in-house}
case "$backend" in in-house|gccjit|template) ;; *) exit 2;; esac
work=$(mktemp -d "$root/target/cleanup-u7b-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U7B_BACKEND="$backend"
python3 - "$work" "${3:-97 114 138 140 ordered implicit forms}" <<'PY'
import hashlib,json,os,subprocess,sys,time
from pathlib import Path
directory=Path(sys.argv[1]); cases=sys.argv[2].split()
if not cases or len(set(cases))!=len(cases) or not set(cases)<=set('97 114 138 140 ordered implicit forms'.split()):
    raise SystemExit('Invalid U7b selection')
batch=int(os.environ.get('U7B_BATCH_SIZE','1' if os.environ['U7B_BACKEND']=='in-house' else '2'))
if batch not in (1,2): raise SystemExit('U7B_BATCH_SIZE must be 1 or 2')
binary=directory/'reader'; cold=directory/'reader.cold'
identity=dict(binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
              startup_sha256=hashlib.sha256(Path(str(binary)+'.native-startup.el').read_bytes()).hexdigest() if Path(str(binary)+'.native-startup.el').is_file() else None,
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None,
              source_sha256={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in map(Path,[
                  'lisp/nelisp-bytecode-cleanup.el','lisp/nelisp-native-frame-v2.el',
                  'lisp/nelisp-bytecode-frame-ir.el','lisp/nelisp-bytecode-native-rooted-cfg-plan.el',
                  'lisp/nelisp-bytecode-native-rooted-cfg-shared-emit.el','scripts/nelisp-standalone-build.el',
                  'test/support/native-cleanup-u7b-fixtures.el','test/standalone-native-cleanup-u7b-driver.el',
                  'test/support/native-entry-observer.el'])})
for index,case in enumerate(cases):
    # Large cleanup bodies keep every old mode in BOTH phases, split across
    # fresh processes. Only the first compile cohort needs to emit a unit;
    # subsequent compile cohorts may use the same authenticated cache file.
    modes=('normal body-error body-throw caller-normal caller-throw cleanup-error cleanup-throw last-error last-throw reenter'.split()
           if case in ('ordered','implicit') else None)
    width=batch
    cohorts=[modes[i:i+width] for i in range(0,len(modes),width)] if modes else [None]
    cache=directory/('cache-'+str(index));cache.mkdir(mode=0o700)
    reused=False
    if os.environ.get('U7B_REUSE_WORK'):
        previous=Path(os.environ['U7B_REUSE_WORK']).resolve()
        matches=[p for p in previous.glob('*.json')
                 if json.loads(p.read_text()).get('fixtures')==[case]
                 and json.loads(p.read_text()).get('phase')=='compile']
        if len(matches)!=1:raise SystemExit('Reuse requires one retained compiler receipt per fixture')
        receipt=json.loads(matches[0].read_text())
        for key in ('binary_sha256','cold_sha256'):
            if receipt[key]!=identity[key]:raise SystemExit('Reuse binary/cold identity mismatch')
        # The driver was refactored only to select the same parity modes.
        # Target bytecode, compiler/runtime and fixture source must match.
        for source,digest in receipt['source_sha256'].items():
            if source.endswith('standalone-native-cleanup-u7b-driver.el'):continue
            if identity['source_sha256'][source]!=digest:raise SystemExit('Reuse source identity mismatch')
        output=matches[0].with_suffix('.out').read_text()
        if not receipt['passed'] and not (receipt['rc']==124 and 'U7B-PARITY' in output):
            raise SystemExit('Reuse has no completed machine parity case after install')
        cache=previous/('cache-'+matches[0].name.split('-')[0]);reused=True
        if not list(cache.rglob('*.nelr')) and not list(cache.rglob('*.so')):
            raise SystemExit('Reuse compiled artifact missing')
    for cohort_index,cohort in enumerate(cohorts):
      for phase in ('compile','load'):
        env=os.environ.copy();env.update(U7B_CASES=case,U7B_PHASE=phase,NELISP_NATIVE_CACHE=str(cache))
        if cohort:env['U7B_MODES']=' '.join(cohort)
        command=['timeout','-k','5','290',str(binary)]
        if cold.is_file():command+=['--cold-load-from',str(cold)]
        command+=['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                  '--load','test/standalone-native-cleanup-u7b-driver.el']
        prefix=str(index)+'-'+str(cohort_index)+'-'+phase;start=time.monotonic()
        with (directory/(prefix+'.out')).open('w') as out,(directory/(prefix+'.err')).open('w') as err:
            result=subprocess.run(command,env=env,stdout=out,stderr=err)
        elapsed=time.monotonic()-start
        output=(directory/(prefix+'.out')).read_text();errors=(directory/(prefix+'.err')).read_text()
        ok=result.returncode==0 and elapsed<300 and not errors and 'U7B-NATIVE-PASS' in output and 'phase='+phase in output
        receipt=dict(backend=env['U7B_BACKEND'],fixtures=[case],modes=cohort,phase=phase,seconds=elapsed,rc=result.returncode,passed=ok,reused=reused,cache=str(cache),**identity)
        (directory/(prefix+'.json')).write_text(json.dumps(receipt,indent=2))
        print(output+errors[-3000:],end='',flush=True)
        if not ok:print('U7B-EVIDENCE='+str(directory));raise SystemExit(1)
print('U7B-EVIDENCE='+str(directory))
PY
