#!/bin/sh
# nelisp-nadvice-parity-smoke.sh --- host/VM parity for GNU nadvice.el, oclosure.el
# and gv.el places on the standalone binary -- JIT ledger S6.14.
#
# Usage: NELISP_BIN=target/nelisp-s614 sh test/nelisp-nadvice-parity-smoke.sh
# Optional: EMACS_BIN (host Emacs to compare against; default "emacs").
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
probe=$script_dir/nelisp-nadvice-parity-smoke.el
if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
"$emacs_bin" --batch -Q -l "$probe" >"$tmp/host.out" 2>"$tmp/host.err" || {
    echo "FAIL host emacs errored"; head -5 "$tmp/host.err" >&2; exit 1; }
(cd "$tmp" && "$binary" --load "$probe" -- >"$tmp/vm.out" 2>"$tmp/vm.err") || {
    echo "FAIL standalone errored"; head -5 "$tmp/vm.err" >&2; exit 1; }
total=$(wc -l <"$tmp/host.out")
head -n "$total" "$tmp/vm.out" >"$tmp/vm.rows"
if diff "$tmp/host.out" "$tmp/vm.rows" >"$tmp/diff.txt"; then
    echo "nadvice/oclosure parity smoke: PASS checked=$total findings=0"
else
    n=$(grep -c '^<' "$tmp/diff.txt" || true)
    echo "nadvice/oclosure parity smoke: FAIL checked=$total findings=$n"
    head -40 "$tmp/diff.txt"
    exit 1
fi
