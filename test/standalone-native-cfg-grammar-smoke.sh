#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --host ]]; then
  exec "${EMACS:-emacs}" -Q --batch -L lisp -L src -L scripts -L test \
    -l nelisp-native-cfg-grammar-test -f ert-run-tests-batch-and-exit
fi
if [[ ${1:-} == --both ]]; then
  "$0" "${2:-target/nelisp-static}" in-house
  "$0" "${3:-target/nelisp-dyn}" gccjit
  exit
fi
binary=${1:-target/nelisp-static}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/cfg-grammar-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
cat > "$work/fixture.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(defun cfg-boxed (x) (cfg-id "boxed constant"))
(defun cfg-diamond (x) (cfg-id (if x x 'absent)))
(defun cfg-shared (x y) (cfg-id (cons (if x x y) (if y y x))))
EL
"${EMACS:-emacs}" -Q --batch --eval "(byte-compile-file \"$work/fixture.el\")" > "$work/host.out" 2>&1
export CFG_FIXTURE="$work/fixture.elc" CFG_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
fixtures=(cfg-boxed cfg-diamond cfg-shared)
if [[ -n ${3:-} ]]; then
  case $3 in cfg-boxed|cfg-diamond|cfg-shared) fixtures=("$3");; *) exit 2;; esac
fi
for fixture in "${fixtures[@]}"; do
export CFG_CASE=$fixture
python3 - "$binary" "$work" <<'PY'
import hashlib,json,os,resource,subprocess,sys,time
from pathlib import Path
binary,work=sys.argv[1:]; directory=Path(work)
case=os.environ['CFG_CASE']
command=['timeout','290',binary]
if os.environ['CFG_BACKEND'] != 'gccjit' and Path(binary+'.cold').is_file():
    command += ['--cold-load-from',str(Path(binary+'.cold').resolve())]
command += ['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src',
            '--load','test/standalone-native-cfg-grammar-driver.el']
start=time.monotonic()
with directory.joinpath(case+'.out').open('w') as out,directory.joinpath(case+'.err').open('w') as err:
    result=subprocess.run(command,stdout=out,stderr=err)
directory.joinpath(case+'.json').write_text(json.dumps(dict(seconds=time.monotonic()-start,rc=result.returncode,
    binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
    peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))
print(directory.joinpath(case+'.out').read_text(),end='')
print('CFG-EVIDENCE='+work)
if result.returncode or directory.joinpath(case+'.err').stat().st_size:
    print(directory.joinpath(case+'.err').read_text()[-4000:]); sys.exit(result.returncode or 1)
if 'CFG-NATIVE-PASS' not in directory.joinpath(case+'.out').read_text(): sys.exit(1)
PY
done
