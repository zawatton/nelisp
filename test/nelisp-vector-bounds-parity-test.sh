#!/usr/bin/env bash
# Native `aset' bounds and `make-vector'/`make-record' length validation:
# exact GNU Emacs 31.1 comparison of test/nelisp-vector-bounds-parity-cases.el.
#
# Usage: nelisp-vector-bounds-parity-test.sh [--expect-defect SHA256]
#   --expect-defect SHA  pass only when the standalone transcript differs from
#                        GNU and has exactly this SHA256 (predecessor control)
# Environment: NELISP_BIN (standalone binary), EMACS (GNU Emacs 31.1 host).
set -u
here=$(cd "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$here/target/nelisp}
host=${EMACS:-emacs}
cases="$here/test/nelisp-vector-bounds-parity-cases.el"
defect=""
fail() { printf 'vector-bounds-parity: FAIL: %s\n' "$*" >&2; exit 1; }
[ "${1:-}" != --expect-defect ] || { [ $# -eq 2 ] || fail "--expect-defect needs a value"; defect=$2; }
[ -x "$bin" ] || fail "standalone binary missing: $bin"
"$host" --version 2>/dev/null | head -1 | grep -q '^GNU Emacs 31\.1' || fail "GNU baseline must be Emacs 31.1"
work=$(mktemp -d) || fail "mktemp"
trap 'rm -rf "$work"' EXIT
count=$(grep -c '^(' "$cases")
{
  printf ';;; -*- lexical-binding: t; -*-\n'
  grep '^(' "$cases" | while IFS= read -r form; do
    printf "(prin1 (condition-case err %s (error (cons 'ERR err)))) (terpri)\n" "$form"
  done
  printf '(princ "P-DONE") (terpri)\n'
} > "$work/driver.el"
timeout 30 "$host" -Q --batch -l "$work/driver.el" > "$work/host.out" 2> "$work/host.err" || fail "host run failed"
[ "$(wc -l < "$work/host.out")" -eq $((count + 1)) ] || fail "host transcript incomplete"
timeout 30 "$bin" --load "$work/driver.el" --eval nil > "$work/native.out" 2> "$work/native.err"
rc=$?
sum=$(sha256sum < "$work/native.out" | awk '{print $1}')
if [ -n "$defect" ]; then
  cmp -s "$work/host.out" "$work/native.out" && fail "predecessor unexpectedly matches GNU"
  [ "$sum" = "$defect" ] || fail "predecessor transcript $sum is not the recorded defect"
  printf 'vector-bounds-parity: PASS (recorded defect reproduced)\n'
  exit 0
fi
if [ "$rc" -ne 0 ] || ! cmp -s "$work/host.out" "$work/native.out"; then
  diff "$work/host.out" "$work/native.out" | head -12 >&2
  fail "transcripts differ (rc=$rc, standalone sha256=$sum)"
fi
printf 'vector-bounds-parity: PASS (%s rows match GNU)\n' "$count"
