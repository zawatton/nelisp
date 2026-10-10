#!/usr/bin/env bash
# Preserve executable identity and require every native run to finish below 300 s.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-concat-u3b-test -f ert-run-tests-batch-and-exit
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
work=$(mktemp -d "$root/target/concat-u3b-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U3B_BACKEND="$backend"
python3 - "$work/reader" "$work" "${3:-0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15}" <<'PY'
import concurrent.futures,hashlib,json,os,subprocess,sys,time
from pathlib import Path
binary,work,selection=sys.argv[1:]; directory=Path(work)
identity=dict(binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest())
cold=Path(binary+'.cold')
identity['cold_sha256']=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None
sources=['lisp/nelisp-bytecode-ir.el','lisp/nelisp-bytecode-frame-ir.el','lisp/nelisp-native-funcall-v2.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-plan.el','lisp/nelisp-bytecode-native-rooted-cfg-emit.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-shared-emit.el','scripts/nelisp-standalone-build.el',
         'scripts/nelisp-stdlib-prelude.el','test/standalone-native-concat-u3b-driver.el',
         'test/support/native-concat-u3b-fixtures.el','test/standalone-native-concat-u3b-smoke.sh']
identity['source_sha256']={path:hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in sources}
identity['compiler_cache_sha256']=hashlib.sha256(Path('target/nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
indices=selection.split()
if not indices or len(set(indices)) != len(indices) or any(int(x) not in range(16) for x in indices):
    raise SystemExit('Expected unique fixture indices 0..15')
batch=int(os.environ.get('U3B_BATCH_SIZE','2'))
if not 1 <= batch <= 3: raise SystemExit('U3B_BATCH_SIZE must be 1..3')
# Long sequence allocation/GC and the semantic controls remain isolated.
# Amortize initialization over small fixtures without relaxing deadlines.
cohorts=[]; group=[]
for index in indices:
    if index in ('6', '7', '11', '12', '13', '14', '15'):
        if group: cohorts.append(group); group=[]
        cohorts.append([index])
    else:
        group.append(index)
        if len(group) == batch: cohorts.append(group); group=[]
if group: cohorts.append(group)
jobs=int(os.environ.get('U3B_JOBS','2'))
if not 1 <= jobs <= 4: raise SystemExit('U3B_JOBS must be 1..4')
# Preserve both mutation/error checks while bounding each compile process.
runs=[]
for cohort in cohorts:
    runs.extend([(cohort,'80'),(cohort,'177')] if cohort == ['14'] else [(cohort,None)])
def run(item):
    cohort,error_opcode=item
    index='-'.join(cohort)+('-'+error_opcode if error_opcode else '')
    env=os.environ.copy(); env['U3B_CASE']=' '.join(cohort)
    if error_opcode: env['U3B_ERROR_OPCODE']=error_opcode
    else: env.pop('U3B_ERROR_OPCODE',None)
    cache=directory/('cache-'+index); cache.mkdir(mode=0o700); env['NELISP_NATIVE_CACHE']=str(cache)
    command=['timeout','-k','5','290',binary]
    if env['U3B_BACKEND'] != 'gccjit' and cold.is_file():
        command += ['--cold-load-from',str(cold.resolve())]
    command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                '--load','test/standalone-native-concat-u3b-driver.el']
    start=time.monotonic()
    with directory.joinpath(index+'.out').open('w') as out,directory.joinpath(index+'.err').open('w') as err:
        result=subprocess.run(command,env=env,stdout=out,stderr=err)
    elapsed=time.monotonic()-start
    output=directory.joinpath(index+'.out').read_text(); errors=directory.joinpath(index+'.err').read_text()
    ok=(result.returncode == 0 and elapsed < 300 and not errors and
        all(f"backend={env['U3B_BACKEND']} case={item} " in output and
            any(f"U3B-{kind}-PASS backend={env['U3B_BACKEND']} case={item} " in output
                for kind in ('NATIVE', 'TYPES', 'ERROR', 'GC')) for item in cohort) and
        ('14' not in cohort or f"U3B-ERROR-PASS backend={env['U3B_BACKEND']} case=14 cases=1 native=1 opcodes=({error_opcode})" in output) and
        ('13' not in cohort or f"U3B-TYPES-PASS backend={env['U3B_BACKEND']} case=13 cases=6 native=6" in output) and
        ('15' not in cohort or 'allocations=2 gc=1 native=1' in output))
    directory.joinpath(index+'.json').write_text(json.dumps(dict(seconds=elapsed,rc=result.returncode,
        backend=env['U3B_BACKEND'],fixtures=list(map(int,cohort)),error_opcode=error_opcode,passed=ok,**identity)))
    return ok,output+errors[-2000:]
start=time.monotonic()
with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
    results=list(pool.map(run,runs))
for ok,output in results: print(output,end='')
passed=all(ok for ok,_ in results)
directory.joinpath('suite.json').write_text(json.dumps(dict(seconds=time.monotonic()-start,
    passed=passed,jobs=jobs,batch_size=batch,fixtures=list(map(int,indices)),**identity)))
print('U3B-EVIDENCE='+work)
if not passed: sys.exit(1)
PY
