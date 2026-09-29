#!/bin/sh
# nelisp-primspeed-parity-smoke.sh --- host GNU Emacs vs standalone parity for
# the primitives given fast paths by the primitive-speed lane: expand-file-name,
# string-search, equal/string=, mapconcat, file-name-directory/-nondirectory,
# secure-hash (sha256, in-process) and func-arity on byte-code objects.
#
# Usage: NELISP_BIN=target/nelisp-ps sh test/nelisp-primspeed-parity-smoke.sh
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
tmp=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-ps-parity.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
probe=$script_dir/nelisp-primspeed-parity-smoke.el
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
    echo "PASS nelisp-primspeed-parity-smoke ($checked probes)"
else
    findings=$(grep -c '^<' "$tmp/diff.txt" || true)
    echo "GATE-COUNT checked=$checked findings=$findings"
    head -40 "$tmp/diff.txt"
    echo "FAIL nelisp-primspeed-parity-smoke"
    exit 1
fi
