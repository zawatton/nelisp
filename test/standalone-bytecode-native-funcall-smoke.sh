#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
both=0 cold=0
backend_option=""
while [[ ${1:-} == --* ]]; do
  case $1 in --backend) backend_option=${2:?missing backend}; shift;; --both) both=1;; --cold) cold=1;; *) echo "Unknown option: $1" >&2; exit 2;; esac
  shift
done
if (( both )); then
  [[ -z $backend_option ]] || { echo "--backend and --both are exclusive" >&2; exit 2; }
  options=(); (( cold == 0 )) || options+=(--cold)
  "$0" "${options[@]}" "${1:-target/nelisp-static}" in-house
  "$0" "${options[@]}" "${2:-target/nelisp-dyn}" gccjit
  exit
fi
binary=${1:-target/nelisp}
backend=${backend_option:-${2:-in-house}}
case "$backend" in in-house|gccjit|template) ;; *) echo "Unknown backend: $backend" >&2; exit 2;; esac
work=$(mktemp -d "$root/target/f1-corpus-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
cat > "$work/fixture.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(defun f1-fixture (x) (f1-user (cons (car x) (cdr x))))
EL
"${EMACS:-emacs}" -Q --batch --eval "(byte-compile-file \"$work/fixture.el\")" > "$work/host.out" 2>&1
export F1_FIXTURE="$work/fixture.elc" F1_BACKEND="$backend" NELISP_NATIVE_CACHE="$work/cache"
export F1B_COLD=$cold
if [[ $backend != gccjit && -f $binary.cold ]]; then export F1B_COLD=1; fi
phases=(compile load)
[[ $backend != template ]] || phases+=(reload)
for phase in "${phases[@]}"; do
  export F1_PHASE=$phase
  [[ $phase != reload ]] || export F1_PHASE=load
  python3 test/support/run-native-funcall-v2.py "$binary" "$work" "$phase" \
    test/standalone-bytecode-native-funcall-driver.el
  cat "$work/$phase.out"
  test ! -s "$work/$phase.err"
  grep -q "F1-.*-PASS" "$work/$phase.out"
done
grep -qx 'F1-CORPUS-DIGEST=52c26b63098a83d62c044908b231afd5cce3a85da3b559147e6dc4810eeac2ef' "$work/load.out"
printf 'F1-EVIDENCE=%s\n' "$work"
