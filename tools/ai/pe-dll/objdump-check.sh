#!/usr/bin/env bash
# Build the S1 sample DLL and assert `objdump -p` sees the export names and a
# base relocation directory.  Exit 0 on success, 1 otherwise.
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/pe-dll-objdump.XXXXXX")" || exit 1
trap 'rm -rf "$WORK"' EXIT
(ulimit -v 8388608; timeout 300 emacs -Q --batch -L "$ROOT/lisp" \
   -l "$HERE/pe-dll-sample.el" \
   --eval "(nelisp-pe-dll-sample-build-all \"$WORK\")") || { echo "FAIL: build"; exit 1; }
OUT="$(timeout 60 objdump -p "$WORK/pedllsample.dll" 2>&1)" || { echo "FAIL: objdump"; exit 1; }
fail=0
for sym in add2 answer via_ptr pedllsample.dll; do
  echo "$OUT" | grep -q "$sym" || { echo "FAIL: export '$sym' not found"; fail=1; }
done
echo "$OUT" | grep -E '^Entry 5 ' | grep -Eq 'Base Relocation' \
  && ! echo "$OUT" | grep -E '^Entry 5 ' | grep -Eq '^Entry 5 [0-9a-f]+ 00000000 ' \
  || { echo "FAIL: no non-empty base relocation directory"; fail=1; }
echo "$OUT" | grep -q 'DIR64' || { echo "FAIL: no DIR64 fixups"; fail=1; }
for flag in DYNAMIC_BASE NX_COMPAT HIGH_ENTROPY_VA; do
  echo "$OUT" | grep -q "$flag" || { echo "FAIL: $flag missing"; fail=1; }
done
[ "$fail" = 0 ] && echo "PASS: objdump sees exports, DIR64 base relocations and DLL flags"
exit "$fail"
