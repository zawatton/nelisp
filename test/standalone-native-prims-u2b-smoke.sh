#!/usr/bin/env bash
# Retain stable executable/image identities and bound every native process below 300 s.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L . -L lisp -L src -L scripts -L test \
    -l nelisp-native-prims-u2b-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  "$0" "${2:-target/nelisp-static}" in-house || status=1
  "$0" "${3:-target/nelisp-dyn}" gccjit || status=1
  exit "$status"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/prims-u2b-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U2B_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
# Independent fresh processes may run together; no compiler state is shared.
python3 - "$work/reader" "$work" "${3:-147 148 149 150 151 152 153 154 155 156 157 158 159 160 161}" <<'PYCODE'
import concurrent.futures,hashlib,json,os,subprocess,sys,time
from pathlib import Path
binary,work,selection=sys.argv[1:]; directory=Path(work)
opcodes=selection.split(); jobs=int(os.environ.get('U2B_JOBS','1'))
if not 1 <= jobs <= 8 or not opcodes or len(set(opcodes)) != len(opcodes):
    raise SystemExit('U2B_JOBS must be 1..8; opcode selection must be nonempty and unique')
if any(int(opcode) not in range(147,162) for opcode in opcodes):
    raise SystemExit('Unknown U2b opcode')
cold=Path(binary+'.cold')
identity=dict(binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
              cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None)

def run(opcode):
    env=os.environ.copy(); env['U2B_OPCODE']=opcode
    cache=directory/('cache-'+opcode); cache.mkdir(mode=0o700)
    env['NELISP_NATIVE_CACHE']=str(cache)
    command=['timeout','-k','5','290',binary]
    if env['U2B_BACKEND'] != 'gccjit' and cold.is_file():
        command += ['--cold-load-from',str(cold.resolve())]
    command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
                '--load','test/standalone-native-prims-u2b-driver.el']
    start=time.monotonic()
    with directory.joinpath(opcode+'.out').open('w') as out,directory.joinpath(opcode+'.err').open('w') as err:
        result=subprocess.run(command,env=env,stdout=out,stderr=err)
    elapsed=time.monotonic()-start
    directory.joinpath(opcode+'.json').write_text(json.dumps(dict(seconds=elapsed,rc=result.returncode,
        backend=env['U2B_BACKEND'],opcode=int(opcode),**identity)))
    output=directory.joinpath(opcode+'.out').read_text(); errors=directory.joinpath(opcode+'.err').read_text()
    passed=(result.returncode == 0 and elapsed < 300 and not errors and
            f"U2B-NATIVE-PASS backend={env['U2B_BACKEND']} opcode={opcode} " in output)
    return passed,output+errors[-4000:]

start=time.monotonic()
with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
    results=list(pool.map(run,opcodes))
for passed,output in results:
    print(output,end='')
print('U2B-EVIDENCE='+work)
directory.joinpath('suite.json').write_text(json.dumps(dict(seconds=time.monotonic()-start,jobs=jobs,
    opcodes=list(map(int,opcodes)),passed=all(passed for passed,_ in results),**identity)))
if not all(passed for passed,_ in results): sys.exit(1)
PYCODE
