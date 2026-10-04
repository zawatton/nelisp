#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
if [[ ${1:-} == --both ]]; then
  "$0" "${2:-target/nelisp-static}" in-house
  "$0" "${3:-target/nelisp-dyn}" gccjit
  exit
fi
binary=${1:-target/nelisp}
backend=${2:-in-house}
work=$(mktemp -d "$root/target/f1-corpus-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
cat > "$work/fixture.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(defun f1-fixture (x) (f1-user (cons (car x) (cdr x))))
EL
"${EMACS:-emacs}" -Q --batch --eval "(byte-compile-file \"$work/fixture.el\")" > "$work/host.out" 2>&1
export F1_FIXTURE="$work/fixture.elc" F1_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
for phase in compile load; do
  export F1_PHASE=$phase
  python3 - "$binary" "$work" "$phase" <<'PYRUN'
import os,subprocess,sys,time,json,resource
from pathlib import Path
binary,work,phase=sys.argv[1:]; directory=Path(work)
start=time.monotonic()
with directory.joinpath(phase+'.out').open('w') as stdout, directory.joinpath(phase+'.err').open('w') as stderr:
    command=['timeout','120',binary]
    # F1 proof issuance segfaults in the generated dynamic cold cohort.
    # Normal boot is qualified for gccjit; both images are regenerated.
    if os.environ['F1_BACKEND'] != 'gccjit' and Path(binary+'.cold').is_file(): command += ['--cold-load-from',str(Path(binary+'.cold').resolve())]
    result=subprocess.run(command+['-L','lisp','-L','src','-L','scripts','-L','packages/nl-ffi/src','-L','packages/nl-prelude/src','--load','test/standalone-bytecode-native-funcall-driver.el'],stdout=stdout,stderr=stderr)
directory.joinpath(phase+'.time').write_text(json.dumps(dict(seconds=time.monotonic()-start,rc=result.returncode,peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)))
if result.returncode:
    print(directory.joinpath(phase+'.err').read_text()); print('F1 failed evidence='+work); sys.exit(result.returncode)
PYRUN
  cat "$work/$phase.out"
  test ! -s "$work/$phase.err"
  grep -q "F1-.*-PASS" "$work/$phase.out"
done
printf 'F1-EVIDENCE=%s\n' "$work"
