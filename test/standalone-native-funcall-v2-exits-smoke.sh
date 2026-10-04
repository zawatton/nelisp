#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
both=0 cold=0 reuse=""
digest=ebe5cd1249025f15a6245e00f493cf7a5a9f1765bb4e07a5295836ad5a5b67d0
while [[ ${1:-} == --* ]]; do
  case $1 in --both) both=1;; --cold) cold=1;; --reuse) reuse=${2:?missing retained work directory}; shift;; *) echo "Unknown option: $1" >&2; exit 2;; esac
  shift
done
if (( both )); then
  [[ -z $reuse ]] || { echo '--reuse selects one retained backend cohort' >&2; exit 2; }
  options=(); (( cold == 0 )) || options+=(--cold)
  work=$(mktemp -d "$root/target/f1b-both-XXXXXX")
  "$0" "${options[@]}" "${1:-target/nelisp-static}" in-house | tee "$work/in-house.out"
  "$0" "${options[@]}" "${2:-target/nelisp-dyn}" gccjit | tee "$work/gccjit.out"
  first=$(sed -n 's/^F1B-CORPUS-DIGEST=//p' "$work/in-house.out")
  second=$(sed -n 's/^F1B-CORPUS-DIGEST=//p' "$work/gccjit.out")
  [[ $first == "$digest" && $second == "$digest" ]]
  printf 'F1B-BOTH-PASS digest=%s cold=%s evidence=%s\n' "$first" "$cold" "$work"
  exit
fi
binary=${1:-target/nelisp-static} backend=${2:-in-house}
work=$(mktemp -d "$root/target/f1b-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
phases=(compile load)
if [[ -n $reuse ]]; then
  reuse=$(cd -- "$reuse" && pwd)
  test -s "$reuse/fixture.elc"
  test -d "$reuse/cache"
  fixture="$reuse/fixture.elc" cache="$reuse/cache"
  phases=(load)
else
  cat > "$work/fixture.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(defun f1b-one (f x) (f1b-tick) (funcall f x))
(defun f1b-zero (f) (funcall f))
(defun f1b-six (f x) (funcall f x x x x x x))
EL
  "${EMACS:-emacs}" -Q --batch --eval "(byte-compile-file \"$work/fixture.el\")" > "$work/host.out" 2>&1
  fixture="$work/fixture.elc" cache="$work/cache"
fi
export F1B_FIXTURE="$fixture" F1B_BACKEND="$backend" F1B_COLD="$cold" NELISP_NATIVE_CACHE="$cache"
for phase in "${phases[@]}"; do
  export F1B_PHASE=$phase
  python3 test/support/run-native-funcall-v2.py "$binary" "$work" "$phase" \
    test/standalone-native-funcall-v2-exits-driver.el
  cat "$work/$phase.out"
  test ! -s "$work/$phase.err"
  grep -q "F1B-${phase^^}-PASS" "$work/$phase.out"
done
test "$(sed -n 's/^F1B-CORPUS-DIGEST=//p' "$work/load.out")" = "$digest"
printf 'F1B-EVIDENCE=%s\n' "$work"
