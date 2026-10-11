#!/usr/bin/env bash
# Cohort startup is amortized; every native process retains a strict deadline.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-buffer-u4a-test -f ert-run-tests-batch-and-exit
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
work=$(mktemp -d "$root/target/buffer-u4a-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U4A_BACKEND="$backend"
python3 - "$work/reader" "$work" "${3:-96 98 99 100 101 102 103 104 105 106}" <<'PYCODE'
import hashlib,json,os,re,subprocess,sys,time
from pathlib import Path
binary,work,selection=sys.argv[1:]; directory=Path(work)
opcodes=list(map(int,selection.split()))
family={96,98,99,100,101,102,103,104,105,106}
if not opcodes or len(set(opcodes)) != len(opcodes) or not set(opcodes) <= family:
    raise SystemExit('Expected unique U4a opcode selection')
batch=int(os.environ.get('U4A_BATCH_SIZE','2'))
if batch not in (1,2): raise SystemExit('U4A_BATCH_SIZE must be 1 or 2')
cold=Path(binary+'.cold')
identity=dict(binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None)
sources=['lisp/nelisp-bytecode-ir.el','lisp/nelisp-bytecode-frame-ir.el','lisp/nelisp-native-funcall-v2.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-plan.el','scripts/nelisp-stdlib-prelude.el',
         'scripts/nelisp-standalone-build.el','test/standalone-native-buffer-u4a-driver.el',
         'test/support/native-buffer-u4a-fixtures.el','test/standalone-native-buffer-u4a-smoke.sh']
identity['source_sha256']={path:hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in sources}
identity['compiler_cache_sha256']=hashlib.sha256(Path('target/nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
cohorts=[]; group=[]
for opcode in opcodes:
    # INSERT also compiles the combined error body; isolate it to retain the
    # same deadline rather than weakening the native process limit.
    if opcode == 99:
        if group: cohorts.append(group); group=[]
        cohorts.append([opcode])
    else:
        group.append(opcode)
        if len(group) == batch: cohorts.append(group); group=[]
if group: cohorts.append(group)
passed=True
for cohort in cohorts:
    label='-'.join(map(str,cohort))
    env=os.environ.copy(); env['U4A_CASE']=' '.join(map(str,cohort))
    cache=directory/('cache-'+label); cache.mkdir(mode=0o700); env['NELISP_NATIVE_CACHE']=str(cache)
    command=['timeout','-k','5','290',binary]
    if env['U4A_BACKEND'] != 'gccjit' and cold.is_file():
        command += ['--cold-load-from',str(cold.resolve())]
    command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                '--load','test/standalone-native-buffer-u4a-driver.el']
    start=time.monotonic()
    with directory.joinpath(label+'.out').open('w') as out,directory.joinpath(label+'.err').open('w') as err:
        result=subprocess.run(command,env=env,stdout=out,stderr=err)
    elapsed=time.monotonic()-start
    output=directory.joinpath(label+'.out').read_text(); errors=directory.joinpath(label+'.err').read_text()
    markers=re.findall(r'^U4A-NATIVE-PASS backend=(in-house|gccjit) opcode=(\d+) cases=(\d+) native=(\d+) rebound=1$',output,re.M)
    ok=(result.returncode == 0 and elapsed < 300 and not errors and 'U4A-DONE' in output
        and [(x[0],int(x[1])) for x in markers] == [(env['U4A_BACKEND'],x) for x in cohort]
        and all(int(x[2]) > 0 and x[2] == x[3] for x in markers)
        and (99 not in cohort or 'U4A-ERROR-PASS prior-insertion=1 later-insertion=0' in output))
    directory.joinpath(label+'.json').write_text(json.dumps(dict(rc=result.returncode,seconds=elapsed,
        backend=env['U4A_BACKEND'],opcodes=cohort,passed=ok,**identity)))
    print(output+errors[-4000:],end='',flush=True)
    print(f'U4A-COHORT seconds={elapsed:.3f} passed={ok}',flush=True)
    passed &= ok
print('U4A-EVIDENCE='+work,flush=True)
if not passed: sys.exit(1)
PYCODE
