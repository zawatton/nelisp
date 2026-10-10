#!/usr/bin/env bash
# Targetable cohorts retain GNU oracle, binary identity and cold/warm receipts.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    --eval '(setq load-prefer-newer t)' -l nelisp-native-legacy-u9-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  static_options=(); dyn_options=()
  [[ -z ${U9_STATIC_REUSE:-} ]] || static_options=(--reuse "$U9_STATIC_REUSE")
  [[ -z ${U9_GCCJIT_REUSE:-} ]] || dyn_options=(--reuse "$U9_GCCJIT_REUSE")
  "$0" "${static_options[@]}" "${2:-target/nelisp-static}" in-house || status=1
  "$0" "${dyn_options[@]}" "${3:-target/nelisp-dyn}" gccjit || status=1
  exit "$status"
fi
reuse=""
if [[ ${1:-} == --reuse ]]; then reuse=${2:?retained evidence directory required}; shift 2; fi
binary=${1:-target/nelisp-static} backend=${2:-in-house}
case "$backend" in in-house|gccjit) ;; *) exit 2;; esac
if [[ -n $reuse ]]; then
  work=$(cd -- "$reuse" && pwd)
else
  work=$(mktemp -d "$root/target/legacy-u9-XXXXXX")
  chmod 700 "$work"
  cp -- "$binary" "$work/reader"; chmod 500 "$work/reader"
  if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
fi
export U9_BACKEND="$backend" U9_ORACLE="$work/oracle.el"
"${EMACS:-emacs}" -Q --batch -l test/support/native-legacy-u9-fixtures.el -f native-legacy-u9-oracle
python3 - "$work" "${3:-139 141 143 144 145}" "$reuse" "$binary" <<'PY'
import hashlib,json,os,re,subprocess,sys,time
from pathlib import Path
directory=Path(sys.argv[1]); cases=sys.argv[2].split()
if not cases or len(set(cases))!=len(cases) or not set(cases)<=set('139 141 143 144 145'.split()):
    raise SystemExit('Expected unique U9 opcode selection')
binary=directory/'reader'; cold=directory/'reader.cold'
sources=['lisp/nelisp-bytecode-cleanup.el','lisp/nelisp-native-funcall-v2.el',
         'lisp/nelisp-bytecode-ir.el','lisp/nelisp-bytecode-frame-ir.el',
         'lisp/nelisp-bytecode-native-rooted-cfg-plan.el','lisp/nelisp-bytecode-native-rooted-cfg-shared-emit.el',
         'scripts/nelisp-standalone-build.el','test/support/native-legacy-u9-fixtures.el',
         'test/standalone-native-legacy-u9-driver.el']
identity=dict(binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None,
              oracle_sha256=hashlib.sha256((directory/'oracle.el').read_bytes()).hexdigest(),
              source_sha256={s:hashlib.sha256(Path(s).read_bytes()).hexdigest() for s in sources})
phases=('compile','load')
if sys.argv[3]:
    if hashlib.sha256(Path(sys.argv[4]).read_bytes()).hexdigest()!=identity['binary_sha256']:
        raise SystemExit('Reuse executable identity mismatch')
    for case in cases:
        receipt=json.loads((directory/(case+'-compile.json')).read_text())
        if not receipt['passed'] or receipt['backend']!=os.environ['U9_BACKEND'] or any(receipt[k]!=v for k,v in identity.items()):
            raise SystemExit('Reuse source/oracle/backend identity mismatch')
    phases=('load',)
# One fixture per process bounds large frame contracts and keeps retries narrow.
for case in cases:
    cache=directory/('cache-'+case)
    if not sys.argv[3]:cache.mkdir(mode=0o700)
    for phase in phases:
        env=os.environ.copy();env.update(U9_CASES=case,U9_PHASE=phase,NELISP_NATIVE_CACHE=str(cache))
        command=['timeout','-k','5','290',str(binary)]
        if cold.is_file():command+=['--cold-load-from',str(cold)]
        command+=['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                  '--load','test/standalone-native-legacy-u9-driver.el']
        label=case+'-'+phase+('-reuse-'+str(time.time_ns()) if sys.argv[3] else '');start=time.monotonic()
        with (directory/(label+'.out')).open('w') as out,(directory/(label+'.err')).open('w') as err:
            result=subprocess.run(command,env=env,stdout=out,stderr=err)
        elapsed=time.monotonic()-start
        output=(directory/(label+'.out')).read_text();errors=(directory/(label+'.err')).read_text()
        markers=re.findall(r'^U9-NATIVE-PASS backend=(in-house|gccjit) cases=(\d+) entries=(\d+) phase=(compile|load)$',output,re.M)
        ok=(result.returncode==0 and elapsed<300 and not errors and len(markers)==1
            and markers[0][0]==env['U9_BACKEND'] and markers[0][3]==phase
            and int(markers[0][1])>0 and markers[0][1]==markers[0][2])
        (directory/(label+'.json')).write_text(json.dumps(dict(opcode=int(case),backend=env['U9_BACKEND'],
            phase=phase,seconds=elapsed,rc=result.returncode,passed=ok,**identity),indent=2))
        print(output+errors[-4000:],end='',flush=True)
        print(f'U9-RUN seconds={elapsed:.3f} passed={ok}',flush=True)
        if not ok:print('U9-EVIDENCE='+str(directory));raise SystemExit(1)
print('U9-EVIDENCE='+str(directory))
PY
