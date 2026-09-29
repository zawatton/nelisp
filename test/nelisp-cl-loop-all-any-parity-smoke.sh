#!/bin/sh
# nelisp-cl-loop-all-any-parity-smoke.sh --- host/VM parity for GNU 31 `all'/`any'
# and the cl-loop shapes byte-opt.el uses (`for VAR in-ref LIST',
# `finally return FORM' / `finally [do] FORMS') -- JIT ledger S6.9 / S6.11.
#
# Usage: NELISP_BIN=target/nelisp-cloop sh test/nelisp-cl-loop-all-any-parity-smoke.sh
# Optional: EMACS_BIN (host Emacs to compare against; default "emacs").
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
probe=$script_dir/nelisp-cl-loop-all-any-parity-smoke.el
if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
"$emacs_bin" --batch -Q --eval '(require (quote cl-lib))' -l "$probe" >"$tmp/host.out" 2>"$tmp/host.err" || {
    echo "FAIL host emacs errored"; head -5 "$tmp/host.err" >&2; exit 1; }
(cd "$tmp" && "$binary" --load "$probe" -- >"$tmp/vm.out" 2>"$tmp/vm.err") || {
    echo "FAIL standalone errored"; head -5 "$tmp/vm.err" >&2; exit 1; }
total=$(wc -l <"$tmp/host.out")
# The standalone binary may echo a trailing value line; compare the probe rows only.
head -n "$total" "$tmp/vm.out" >"$tmp/vm.rows"
if diff "$tmp/host.out" "$tmp/vm.rows" >"$tmp/diff.txt"; then
    echo "cl-loop/all/any parity smoke: PASS checked=$total findings=0"
else
    n=$(grep -c '^<' "$tmp/diff.txt" || true)
    echo "cl-loop/all/any parity smoke: FAIL checked=$total findings=$n"
    head -40 "$tmp/diff.txt"
    exit 1
fi
