#!/bin/sh
# nelisp-buf-read-parity-smoke.sh --- host GNU Emacs vs standalone parity for
# the reader: buffer/marker `read' (fast path, narrowing, EOF and stray
# brackets), `read-from-string' (dotted/incomplete/invalid forms, `1.',
# `?\N{U+XXXX}', `#@N', `#(..)' text properties), and list/vector scaling
# (iterative list reading, right-sized parse pool).  Expected output is always
# regenerated with the host Emacs at run time.
#
# Usage: NELISP_BIN=target/nelisp-rp sh test/nelisp-buf-read-parity-smoke.sh
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
tmp=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bufread-parity.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
probe=$script_dir/nelisp-buf-read-parity-smoke.el
LC_ALL=C.UTF-8 "$emacs_bin" --batch -Q -l "$probe" >"$tmp/host.out" 2>"$tmp/host.err" || {
    echo "FAIL host emacs errored"; head -5 "$tmp/host.err" >&2; exit 1; }
(cd "$tmp" && "$binary" --load "$probe" -- >"$tmp/sa.raw" 2>"$tmp/sa.err") || {
    echo "FAIL standalone errored"; head -5 "$tmp/sa.err" >&2; exit 1; }
if [ -s "$tmp/sa.err" ]; then
    echo "FAIL standalone wrote to stderr"; head -5 "$tmp/sa.err" >&2; exit 1
fi
grep -a ' => ' "$tmp/sa.raw" >"$tmp/sa.out" || true
grep -a ' => ' "$tmp/host.out" >"$tmp/host.lines" || true
checked=$(wc -l <"$tmp/host.lines" | tr -d ' ')
if [ "$checked" -lt 100 ]; then
    echo "FAIL only $checked probes ran"; exit 1
fi
if diff "$tmp/host.lines" "$tmp/sa.out" >"$tmp/diff.txt"; then
    echo "GATE-COUNT checked=$checked findings=0"
    echo "PASS nelisp-buf-read-parity-smoke ($checked probes)"
else
    findings=$(grep -c '^<' "$tmp/diff.txt" || true)
    echo "GATE-COUNT checked=$checked findings=$findings"
    head -40 "$tmp/diff.txt" | cut -c1-300
    echo "FAIL nelisp-buf-read-parity-smoke"
    exit 1
fi
