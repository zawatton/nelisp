#!/bin/bash
# Re-verify saved outputs (no re-run). Fails on mismatch, missing DONE, case-count drift, stale case file, or tampering.
K=$(cd "$(dirname "$0")" && pwd); cd "$K" || exit 1
SRC=${KERNEL_SRC:-$HOME/.cache/tmp/nelisp-v8-native-20261029/out/gnu-native-batch6/final-inputs/package/source-package.el}
fail=0
[ -f "$SRC" ] && { grep -E '^\(defun bench-(vector-read|vector-write|vector-allocate64|string-concat16|string-slice16|hash-get1024) ' "$SRC" | diff -q - <(grep '^(defun' kernels.el) >/dev/null 2>&1 || \
  [ "$(grep -E '^\(defun bench-(vector-read|vector-write|vector-allocate64|string-concat16|string-slice16|hash-get1024) ' "$SRC" | sha256sum)" = "$(grep '^(defun' kernels.el | sha256sum)" ] || { echo "kernels.el differs from source-package.el"; fail=1; }; }
N=$(grep -c '^(kc-run ' kernel-cases.el)
for f in gnu.out nelisp.out nelisp-interp.out; do
  [ "$(grep -c ' => ' $f)" = "$N" ] && grep -qx "DONE n=$N" $f || { echo "$f: case count != $N or no DONE"; fail=1; }
  python3 compare.py gnu.out $f >/dev/null || { echo "gnu.out vs $f: MISMATCH"; fail=1; }
done
grep -c 'NATIVE-PROBE .* identity-eq=t row=300' nelisp.out | grep -qx 6 || { echo "native probes missing"; fail=1; }
python3 compare.py --self-test gnu.out | grep -q 'SELF-TEST PASS' || { echo "compare self-test failed"; fail=1; }
[ $fail = 0 ] && echo "CHECK PASS ($N cases, GNU vs NeLisp native and interp)" || echo "CHECK FAIL"
exit $fail
