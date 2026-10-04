#!/usr/bin/env bash
# Shortest round-trip float printing: exact GNU Emacs 31.1 comparison of
# `number-to-string' over a deterministic corpus of 6000 doubles
# (test/nelisp-float-print-corpus.el: 53-bit mantissas scaled by 2^-60..2^60).
#
# Usage: nelisp-float-print-parity-test.sh [--expect-defect SHA256]
#   --expect-defect SHA  pass only when the standalone transcript differs from
#                        GNU and has exactly this SHA256 (predecessor control)
# Environment: NELISP_BIN (standalone binary), EMACS (GNU Emacs 31.1 host).
set -u
here=$(cd "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$here/target/nelisp}
host=${EMACS:-emacs}
corpus="$here/test/nelisp-float-print-corpus.el"
defect=""
fail() { printf 'float-print-parity: FAIL: %s\n' "$*" >&2; exit 1; }
[ "${1:-}" != --expect-defect ] || { [ $# -eq 2 ] || fail "--expect-defect needs a value"; defect=$2; }
[ -x "$bin" ] || fail "standalone binary missing: $bin"
"$host" --version 2>/dev/null | head -1 | grep -q '^GNU Emacs 31\.1' || fail "GNU baseline must be Emacs 31.1"
work=$(mktemp -d) || fail "mktemp"
trap 'rm -rf "$work"' EXIT
timeout 40 "$host" -Q --batch -l "$corpus" > "$work/host.out" 2> "$work/host.err" || fail "host run failed"
[ "$(wc -l < "$work/host.out")" -eq 6000 ] || fail "host transcript incomplete"
timeout 40 "$bin" --load "$corpus" --eval nil > "$work/native.out" 2> "$work/native.err"
rc=$?
[ "$(wc -l < "$work/native.out")" -eq 6000 ] || fail "standalone transcript incomplete (rc=$rc)"
sum=$(sha256sum < "$work/native.out" | awk '{print $1}')
differing=$(diff "$work/host.out" "$work/native.out" | grep -c '^<')
if [ -n "$defect" ]; then
  [ "$differing" -gt 0 ] || fail "predecessor unexpectedly matches GNU"
  [ "$sum" = "$defect" ] || fail "predecessor transcript $sum is not the recorded defect"
  printf 'float-print-parity: PASS (recorded defect reproduced, %s of 6000 differ)\n' "$differing"
  exit 0
fi
if [ "$rc" -ne 0 ] || [ "$differing" -ne 0 ]; then
  diff "$work/host.out" "$work/native.out" | head -8 >&2
  fail "$differing of 6000 printed floats differ (standalone sha256=$sum)"
fi
printf 'float-print-parity: PASS (6000 printed floats match GNU)\n'
