#!/usr/bin/env bash
# Each native invocation has its own 290-second deadline and evidence files.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-switch-u6-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  "$0" "${2:-target/nelisp-static}" in-house || status=1
  "$0" "${3:-target/nelisp-dyn}" gccjit || status=1
  exit "$status"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/switch-u6-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
# Keep the measured executable and image stable if another build publishes target/.
export U6_SOURCE_BINARY="$binary"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
binary="$work/reader"
export U6_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
for fixture in ${3:-eq eql equal custom dynamic backedge}; do
  export U6_CASE=$fixture U6_STAGE="$work/$fixture.stage" NELISP_ROOTED_CFG_STAGE_LOG="$work/$fixture.producer"
  python3 - "$binary" "$work" <<'PY'
import hashlib,json,os,re,resource,subprocess,sys,time
from pathlib import Path
binary,work=sys.argv[1:]; directory=Path(work); case=os.environ['U6_CASE']
command=['timeout','-k','5','290',binary]
if os.environ['U6_BACKEND'] != 'gccjit' and Path(binary+'.cold').is_file():
    command += ['--cold-load-from',str(Path(binary+'.cold').resolve())]
command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
            '--load','test/standalone-native-switch-u6-driver.el']
start=time.monotonic()
with directory.joinpath(case+'.out').open('w') as out,directory.joinpath(case+'.err').open('w') as err:
    result=subprocess.run(command,stdout=out,stderr=err)
elapsed=time.monotonic()-start
output=directory.joinpath(case+'.out').read_text()
markers=re.findall(r'^U6-NATIVE-PASS backend=(\S+) fixture=(\S+) cases=(\d+) native-entries=(\d+) compiles=(\d+) seconds=([\d.]+)$',output,re.M)
passed=(result.returncode == 0 and elapsed < 300 and not directory.joinpath(case+'.err').stat().st_size and
        len(markers) == 1 and markers[0][0] == os.environ['U6_BACKEND'] and markers[0][1] == case and
        int(markers[0][2]) > 0 and markers[0][2] == markers[0][3])
directory.joinpath(case+'.json').write_text(json.dumps(dict(seconds=elapsed,rc=result.returncode,passed=passed,
    backend=os.environ['U6_BACKEND'],fixture=case,native_metrics=markers,
    binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
    cold_sha256=hashlib.sha256(Path(binary+'.cold').read_bytes()).hexdigest() if Path(binary+'.cold').is_file() else None,
    source_binary=os.environ['U6_SOURCE_BINARY'],
    peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))
print(directory.joinpath(case+'.out').read_text(),end=''); print('U6-EVIDENCE='+work)
if not passed:
    print(directory.joinpath(case+'.err').read_text()[-4000:]); sys.exit(result.returncode or 1)
PY
done
