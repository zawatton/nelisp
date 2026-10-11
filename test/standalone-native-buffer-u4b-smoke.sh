#!/usr/bin/env bash
# Cohort startup is amortized; every native process retains a strict deadline.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-buffer-u4b-test -f ert-run-tests-batch-and-exit
fi
# The standalone source reader tolerates a trailing unmatched delimiter.
# Reject it before any expensive native startup or compilation.
"${EMACS:-emacs}" -Q --batch --eval '
  (dolist (file (quote ("test/standalone-native-buffer-u4b-driver.el"
                       "test/support/native-buffer-u4b-fixtures.el")))
    (with-temp-buffer
      (insert-file-contents file) (emacs-lisp-mode) (check-parens)))'
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
work=$(mktemp -d "$root/target/buffer-u4b-XXXXXX")
chmod 700 "$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U4B_BACKEND="$backend"
python3 - "$work/reader" "$work" "${3:-108 109 110 111 112 113 116 117 118}" <<'PYCODE'
import hashlib,json,os,re,subprocess,sys,time
from pathlib import Path
binary,work,selection=sys.argv[1:]; directory=Path(work)
opcodes=list(map(int,selection.split()))
family={108,109,110,111,112,113,116,117,118}
if not opcodes or len(set(opcodes)) != len(opcodes) or not set(opcodes) <= family:
    raise SystemExit('Expected unique U4b opcode selection')
batch=int(os.environ.get('U4B_BATCH_SIZE','2'))
if batch not in (1,2): raise SystemExit('U4B_BATCH_SIZE must be 1 or 2')
cold=Path(binary+'.cold')
identity=dict(binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None)
sources=['lisp/nelisp-bytecode-ir.el','lisp/nelisp-bytecode-frame-ir.el','lisp/nelisp-native-funcall-v2.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-plan.el','scripts/nelisp-stdlib-prelude.el',
         'scripts/nelisp-standalone-build.el','test/standalone-native-buffer-u4b-driver.el',
         'test/support/native-buffer-u4b-fixtures.el','test/standalone-native-buffer-u4b-smoke.sh']
identity['source_sha256']={path:hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in sources}
identity['compiler_cache_sha256']=hashlib.sha256(Path('target/nelisp-artifact-runtime.el.nelc').read_bytes()).hexdigest()
cohorts=[]; group=[]
for opcode in opcodes:
    # FORWARD-CHAR also compiles the ordered-error body; isolate it to retain the
    # same deadline rather than weakening the native process limit.
    if opcode == 117:
        if group: cohorts.append(group); group=[]
        cohorts.append([opcode])
    else:
        group.append(opcode)
        if len(group) == batch: cohorts.append(group); group=[]
if group: cohorts.append(group)
passed=True
for cohort in cohorts:
    label='-'.join(map(str,cohort))
    env=os.environ.copy(); env['U4B_CASE']=' '.join(map(str,cohort))
    cache=directory/('cache-'+label); cache.mkdir(mode=0o700); env['NELISP_NATIVE_CACHE']=str(cache)
    command=['timeout','-k','5','290',binary]
    if env['U4B_BACKEND'] != 'gccjit' and cold.is_file():
        command += ['--cold-load-from',str(cold.resolve())]
    command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                '--load','test/standalone-native-buffer-u4b-driver.el']
    start=time.monotonic()
    with directory.joinpath(label+'.out').open('w') as out,directory.joinpath(label+'.err').open('w') as err:
        result=subprocess.run(command,env=env,stdout=out,stderr=err)
    elapsed=time.monotonic()-start
    output=directory.joinpath(label+'.out').read_text(); errors=directory.joinpath(label+'.err').read_text()
    markers=re.findall(r'^U4B-NATIVE-PASS backend=(in-house|gccjit) opcode=(\d+) cases=(\d+) native=(\d+) rebound=1$',output,re.M)
    ok=(result.returncode == 0 and elapsed < 300 and not errors and 'U4B-DONE' in output
        and [(x[0],int(x[1])) for x in markers] == [(env['U4B_BACKEND'],x) for x in cohort]
        and all(int(x[2]) > 0 and x[2] == x[3] for x in markers)
        and (117 not in cohort or 'U4B-ERROR-PASS prior-insertion=1 later-insertion=0' in output))
    directory.joinpath(label+'.json').write_text(json.dumps(dict(rc=result.returncode,seconds=elapsed,
        backend=env['U4B_BACKEND'],opcodes=cohort,passed=ok,**identity)))
    print(output+errors[-4000:],end='',flush=True)
    print(f'U4B-COHORT seconds={elapsed:.3f} passed={ok}',flush=True)
    passed &= ok
print('U4B-EVIDENCE='+work,flush=True)
if not passed: sys.exit(1)
PYCODE
