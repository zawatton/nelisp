#!/usr/bin/env bash
# Preserve executable identity and require every native run to finish below 300 s.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-list-u3a-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  "$0" "${2:-target/nelisp-static}" in-house & first=$!
  "$0" "${3:-target/nelisp-dyn}" gccjit & second=$!
  failed=0
  wait "$first" || failed=1
  wait "$second" || failed=1
  exit "$failed"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
case "$backend" in in-house|gccjit) ;; *) exit 2;; esac
work=$(mktemp -d "$root/target/list-u3a-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U3A_BACKEND="$backend"
python3 - "$work/reader" "$work" "${3:-0 1 2 3 4 5 6 7 8}" <<'PY'
import hashlib,json,os,subprocess,sys,time
from pathlib import Path
binary,work,selection=sys.argv[1:]; directory=Path(work)
identity=dict(binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest())
cold=Path(binary+'.cold')
identity['cold_sha256']=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None
sources=['lisp/nelisp-bytecode-ir.el','lisp/nelisp-bytecode-frame-ir.el','lisp/nelisp-native-funcall-v2.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-plan.el','lisp/nelisp-bytecode-native-rooted-cfg-emit.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-shared-emit.el','test/standalone-native-list-u3a-driver.el',
         'test/support/native-list-u3a-fixtures.el','test/standalone-native-list-u3a-smoke.sh']
identity['source_sha256']={path:hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in sources}
identity['compiler_cache_sha256']=hashlib.sha256(Path('target/nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
indices=selection.split()
if not indices or len(set(indices)) != len(indices) or any(int(x) not in range(9) for x in indices):
    raise SystemExit('Expected unique fixture indices 0..8')
batch=int(os.environ.get('U3A_BATCH_SIZE','2'))
if not 1 <= batch <= 3: raise SystemExit('U3A_BATCH_SIZE must be 1..3')
# Long allocations remain isolated. Additional compiles for error and join
# controls run in separate processes, preserving the 290-second deadline.
# Amortize initialization over small fixtures without relaxing deadlines.
cohorts=[]; group=[]
for index in indices:
    if index in ('7', '8'):
        if group: cohorts.append(group); group=[]
        cohorts.append([index])
    else:
        group.append(index)
        if len(group) == batch: cohorts.append(group); group=[]
if group: cohorts.append(group)
runs=[('main',cohort) for cohort in cohorts]
if '3' in indices: runs.append(('error',['3']))
if '7' in indices: runs.append(('join',['7']))
passed=True
for phase,cohort in runs:
    index='-'.join(cohort)+'-'+phase
    env=os.environ.copy(); env['U3A_CASE']=' '.join(cohort); env['U3A_PHASE']=phase
    cache=directory/('cache-'+index); cache.mkdir(mode=0o700); env['NELISP_NATIVE_CACHE']=str(cache)
    command=['timeout','-k','5','290',binary]
    if env['U3A_BACKEND'] != 'gccjit' and cold.is_file():
        command += ['--cold-load-from',str(cold.resolve())]
    command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                '--load','test/standalone-native-list-u3a-driver.el']
    start=time.monotonic()
    with directory.joinpath(index+'.out').open('w') as out,directory.joinpath(index+'.err').open('w') as err:
        result=subprocess.run(command,env=env,stdout=out,stderr=err)
    elapsed=time.monotonic()-start
    output=directory.joinpath(index+'.out').read_text(); errors=directory.joinpath(index+'.err').read_text()
    ok=(result.returncode == 0 and elapsed < 300 and not errors and
        output.count(f"U3A-NATIVE-PASS backend={env['U3A_BACKEND']} ") == (len(cohort) if phase == 'main' else 0) and
        (phase != 'error' or 'U3A-ERROR-PASS native=1 prior-mutation=1 later-effect=0' in output) and
        (phase != 'join' or f"U3A-JOIN-PASS backend={env['U3A_BACKEND']} cases=2 native=2" in output))
    directory.joinpath(index+'.json').write_text(json.dumps(dict(seconds=elapsed,rc=result.returncode,
        backend=env['U3A_BACKEND'],phase=phase,fixtures=list(map(int,cohort)),passed=ok,**identity)))
    print(output,end=''); print(errors[-2000:],end=''); passed &= ok
    if not ok: break
print('U3A-EVIDENCE='+work)
if not passed: sys.exit(1)
PY
