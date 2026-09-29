#!/bin/sh
# nelisp-evalperf-parity-smoke.sh --- host GNU Emacs vs standalone parity for
# the behaviour touched by the eval-performance lane: native fboundp for
# nil/t, hash lookups missing on vector/record/buffer/cons keys, and the
# ^ / \` fast paths of the regexp scanner, plus two timing bounds that fail
# on a binary without the fixes (negative control: run it on an older binary).
#
# Usage: NELISP_BIN=target/nelisp-ev sh test/nelisp-evalperf-parity-smoke.sh
# Optional: EMACS_BIN (host Emacs; default "emacs").
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi
tmp=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-ep-parity.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
probe=$script_dir/nelisp-evalperf-parity-smoke.el
"$emacs_bin" --batch -Q -l "$probe" >"$tmp/host.out" 2>"$tmp/host.err" || {
    echo "FAIL host emacs errored"; head -5 "$tmp/host.err" >&2; exit 1; }
(cd "$tmp" && "$binary" --load "$probe" -- >"$tmp/sa.raw" 2>"$tmp/sa.err") || {
    echo "FAIL standalone errored"; head -5 "$tmp/sa.err" >&2; exit 1; }
# The standalone echoes the last form's value after the load; keep probe lines only.
grep ' => ' "$tmp/sa.raw" >"$tmp/sa.out" || true
grep ' => ' "$tmp/host.out" >"$tmp/host.lines" || true
checked=$(wc -l <"$tmp/host.lines" | tr -d ' ')
if diff "$tmp/host.lines" "$tmp/sa.out" >"$tmp/diff.txt"; then
    echo "GATE-COUNT checked=$checked findings=0"
    echo "PASS nelisp-evalperf-parity-smoke ($checked probes)"
else
    findings=$(grep -c '^<' "$tmp/diff.txt" || true)
    echo "GATE-COUNT checked=$checked findings=$findings"
    head -40 "$tmp/diff.txt"
    echo "FAIL nelisp-evalperf-parity-smoke"
    exit 1
fi
