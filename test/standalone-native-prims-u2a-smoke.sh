#!/usr/bin/env bash
# Retain stable executable/image identities and bound every native process below 300 s.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L . -L lisp -L src -L scripts -L test \
    -l nelisp-native-prims-u2a-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  "$0" "${2:-target/nelisp-static}" in-house || status=1
  "$0" "${3:-target/nelisp-dyn}" gccjit || status=1
  exit "$status"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/prims-u2a-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
export U2A_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
status=0
for opcode in ${3:-56 62 71 72 73 74 75 76 77 78 79}; do
  export U2A_OPCODE=$opcode
  python3 - "$work/reader" "$work" <<'PY' || status=1
import hashlib,json,os,resource,subprocess,sys,time
from pathlib import Path
binary,work=sys.argv[1:]; directory=Path(work); opcode=os.environ['U2A_OPCODE']
command=['timeout','-k','5','290',binary]
cold=Path(binary+'.cold')
if os.environ['U2A_BACKEND'] != 'gccjit' and cold.is_file():
    command += ['--cold-load-from',str(cold.resolve())]
command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
            '--load','test/standalone-native-prims-u2a-driver.el']
start=time.monotonic()
with directory.joinpath(opcode+'.out').open('w') as out,directory.joinpath(opcode+'.err').open('w') as err:
    result=subprocess.run(command,stdout=out,stderr=err)
directory.joinpath(opcode+'.json').write_text(json.dumps(dict(seconds=time.monotonic()-start,rc=result.returncode,
    binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
    cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None,
    peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))
output=directory.joinpath(opcode+'.out').read_text()
print(output,end=''); print('U2A-EVIDENCE='+work)
if result.returncode or directory.joinpath(opcode+'.err').stat().st_size:
    print(directory.joinpath(opcode+'.err').read_text()[-4000:]); sys.exit(result.returncode or 1)
if 'U2A-NATIVE-PASS' not in output: sys.exit(1)
PY
done
exit "$status"
