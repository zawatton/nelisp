#!/usr/bin/env bash
# Cohort startup is amortized; every native process retains a strict deadline.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-buffer-u4c-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  "$0" "${2:-target/nelisp-static}" in-house "${U4C_CASES:-119 120 121 122 123 124 125 126 127}" & first=$!
  "$0" "${3:-target/nelisp-dyn}" gccjit "${U4C_CASES:-119 120 121 122 123 124 125 126 127}" & second=$!
  failed=0
  wait "$first" || failed=1
  wait "$second" || failed=1
  exit "$failed"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
case "$backend" in in-house|gccjit) ;; *) exit 2;; esac
work=$(mktemp -d "$root/target/buffer-u4c-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U4C_BACKEND="$backend" U4C_ORACLE="$work/gnu-oracle.el"
"${EMACS:-emacs}" -Q --batch -l test/support/native-buffer-u4c-fixtures.el -f native-buffer-u4c-oracle
python3 - "$work/reader" "$work" "${3:-119 120 121 122 123 124 125 126 127}" <<'PYCODE'
import hashlib,json,os,re,subprocess,sys,time
from pathlib import Path
binary,work,selection=sys.argv[1:]; directory=Path(work)
errors_only=selection=="errors"
opcodes=[] if errors_only else list(map(int,selection.split()))
family=set(range(119,128))
if not errors_only and (not opcodes or len(set(opcodes)) != len(opcodes) or not set(opcodes) <= family):
    raise SystemExit('Expected unique U4c opcode selection')
batch=int(os.environ.get('U4C_BATCH_SIZE','2'))
if batch not in (1,2): raise SystemExit('U4C_BATCH_SIZE must be 1 or 2')
cold=Path(binary+'.cold')
identity=dict(binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None)
sources=['lisp/nelisp-bytecode-ir.el','lisp/nelisp-bytecode-frame-ir.el','lisp/nelisp-native-funcall-v2.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-plan.el','scripts/nelisp-stdlib-prelude.el',
         'scripts/nelisp-standalone-build.el','test/standalone-native-buffer-u4c-driver.el',
         'test/support/native-buffer-u4c-fixtures.el','test/standalone-native-buffer-u4c-smoke.sh']
identity['source_sha256']={path:hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in sources}
identity['compiler_cache_sha256']=hashlib.sha256(Path('target/nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
cohorts=[opcodes[i:i+batch] for i in range(0,len(opcodes),batch)]
# Ordered-effect bodies run separately, amortized across three opcodes.
if errors_only or set(opcodes)==family: cohorts.extend((123,124,125))
passed=True
for selection in cohorts:
    error_opcode=selection if isinstance(selection,int) else None
    cohort=[] if error_opcode else selection
    label='-'.join(map(str,cohort)) if cohort else 'errors-'+str(error_opcode)
    env=os.environ.copy(); env['U4C_CASE']=' '.join(map(str,cohort))
    if not cohort: env.update(U4C_ERRORS='1',U4C_ERROR_CASES=str(error_opcode))
    cache=directory/('cache-'+label); cache.mkdir(mode=0o700); env['NELISP_NATIVE_CACHE']=str(cache)
    command=['timeout','-k','5','290',binary]
    if env['U4C_BACKEND'] != 'gccjit' and cold.is_file():
        command += ['--cold-load-from',str(cold.resolve())]
    command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                '--load','test/standalone-native-buffer-u4c-driver.el']
    start=time.monotonic()
    with directory.joinpath(label+'.out').open('w') as out,directory.joinpath(label+'.err').open('w') as err:
        result=subprocess.run(command,env=env,stdout=out,stderr=err)
    elapsed=time.monotonic()-start
    output=directory.joinpath(label+'.out').read_text(); errors=directory.joinpath(label+'.err').read_text()
    markers=re.findall(r'^U4C-NATIVE-PASS backend=(in-house|gccjit) opcode=(\d+) cases=(\d+) native=(\d+) rebound=1 gc=1$',output,re.M)
    ok=(result.returncode == 0 and elapsed < 300 and not errors and 'U4C-DONE' in output
        and [(x[0],int(x[1])) for x in markers] == [(env['U4C_BACKEND'],x) for x in cohort]
        and all(int(x[2]) > 0 and x[2] == x[3] for x in markers)
        and (bool(cohort) or re.findall(r'^U4C-ERROR-PASS opcode=(123|124|125) native=2 prior-insertion=1 later-insertion=0$',output,re.M)==[str(error_opcode)]))
    directory.joinpath(label+'.json').write_text(json.dumps(dict(rc=result.returncode,seconds=elapsed,
        backend=env['U4C_BACKEND'],opcodes=cohort,error_opcode=error_opcode,passed=ok,**identity)))
    print(output+errors[-4000:],end='',flush=True)
    print(f'U4C-COHORT seconds={elapsed:.3f} passed={ok}',flush=True)
    passed &= ok
print('U4C-EVIDENCE='+work,flush=True)
if not passed: sys.exit(1)
PYCODE
