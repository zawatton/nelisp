#!/usr/bin/env bash
# Batch inline fixtures per backend; every process has a deadline below 300 s.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L . -L lisp -L src -L scripts -L test \
    -l nelisp-native-stackset-u5-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  status=0
  "$0" "${2:-target/nelisp-static}" in-house & first=$!
  "$0" "${3:-target/nelisp-dyn}" gccjit & second=$!
  wait "$first" || status=1
  wait "$second" || status=1
  exit "$status"
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/stackset-u5-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
# Optional replay uses the production ABI/input-key checks and private loader;
# default runs always start with an empty cache and compile every body once.
seed=""
case "$backend" in
  in-house) seed=${U5_IN_HOUSE_CACHE_SEED:-};;
  gccjit) seed=${U5_GCCJIT_CACHE_SEED:-};;
  *) exit 2;;
esac
if [[ -n $seed ]]; then
  [[ -d $seed && ! -L $seed ]]
  cp -a -- "$seed/." "$work/cache/"
fi
export U5_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
# Amortize startup over bounded pairs, keeping the deep-offset bodies alone.
if [[ -n ${3:-} ]]; then
  cohorts=("$3")
else
  cohorts=("set0 set1" "set2zero set2one" "set255" "set256" \
           "drop0 drop1" "keep0 keep1" "keep127" "swap" "swap2")
fi
for cohort in "${!cohorts[@]}"; do
export U5_CASES="${cohorts[$cohort]}" U5_COHORT="$cohort"
python3 - "$work/reader" "$work" <<'PY'
import hashlib,json,os,resource,subprocess,sys,time
from pathlib import Path
binary,work=sys.argv[1:]; directory=Path(work)/os.environ['U5_COHORT']; directory.mkdir()
command=['timeout','-k','5','290',binary]
cold=Path(binary+'.cold')
if os.environ['U5_BACKEND'] != 'gccjit' and cold.is_file():
    command += ['--cold-load-from',str(cold.resolve())]
command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
            '--load','test/standalone-native-stackset-u5-driver.el']
start=time.monotonic()
with directory.joinpath('run.out').open('w') as out,directory.joinpath('run.err').open('w') as err:
    result=subprocess.run(command,stdout=out,stderr=err)
directory.joinpath('run.json').write_text(json.dumps(dict(seconds=time.monotonic()-start,rc=result.returncode,
    binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
    cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if cold.is_file() else None,
    peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))
output=directory.joinpath('run.out').read_text()
print(output,end=''); print('U5-EVIDENCE='+str(directory))
if result.returncode or directory.joinpath('run.err').stat().st_size:
    print(directory.joinpath('run.err').read_text()[-4000:]); sys.exit(result.returncode or 1)
if 'U5-NATIVE-PASS' not in output: sys.exit(1)
PY
done
