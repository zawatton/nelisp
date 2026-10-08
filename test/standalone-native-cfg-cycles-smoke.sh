#!/usr/bin/env bash
# Each native invocation has its own 290-second deadline and evidence files.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-cfg-cycles-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  "$0" "${2:-target/nelisp-static}" in-house
  "$0" "${3:-target/nelisp-dyn}" gccjit
  exit
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/cfg-cycles-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
# Keep the measured executable and image stable if another build publishes target/.
export CYCLES_SOURCE_BINARY="$binary"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
binary="$work/reader"
export CYCLES_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
for fixture in ${3:-entry counted swap ref6 ref7 irreducible closed}; do
  export CYCLES_CASE=$fixture CYCLES_STAGE="$work/$fixture.stage" NELISP_ROOTED_CFG_STAGE_LOG="$work/$fixture.producer"
  python3 - "$binary" "$work" <<'PY'
import hashlib,json,os,resource,subprocess,sys,time
from pathlib import Path
binary,work=sys.argv[1:]; directory=Path(work); case=os.environ['CYCLES_CASE']
command=['timeout','290',binary]
if os.environ['CYCLES_BACKEND'] != 'gccjit' and Path(binary+'.cold').is_file():
    command += ['--cold-load-from',str(Path(binary+'.cold').resolve())]
command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
            '--load','test/standalone-native-cfg-cycles-driver.el']
start=time.monotonic()
with directory.joinpath(case+'.out').open('w') as out,directory.joinpath(case+'.err').open('w') as err:
    result=subprocess.run(command,stdout=out,stderr=err)
directory.joinpath(case+'.json').write_text(json.dumps(dict(seconds=time.monotonic()-start,rc=result.returncode,
    binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
    source_binary=os.environ['CYCLES_SOURCE_BINARY'],
    peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))
print(directory.joinpath(case+'.out').read_text(),end=''); print('CYCLES-EVIDENCE='+work)
if result.returncode or directory.joinpath(case+'.err').stat().st_size:
    print(directory.joinpath(case+'.err').read_text()[-4000:]); sys.exit(result.returncode or 1)
if 'CYCLES-NATIVE-PASS' not in directory.joinpath(case+'.out').read_text(): sys.exit(1)
PY
done
