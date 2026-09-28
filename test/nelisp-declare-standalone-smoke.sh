#!/bin/sh
# nelisp-declare-standalone-smoke.sh --- `declare' / redefined-definer parity.
#
# Runs test/nelisp-declare-standalone-probe.el on stock Emacs (--batch -Q)
# and on the standalone binary and requires identical stdout and an empty
# standalone stderr.  See the probe's commentary for what each line pins.
#
# Usage: NELISP_BIN=target/nelisp-decl test/nelisp-declare-standalone-smoke.sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
bin=${NELISP_BIN:-$repo/target/nelisp}
emacs=${EMACS:-emacs}
probe=$script_dir/nelisp-declare-standalone-probe.el
work=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-declare.XXXXXX")
trap 'rm -rf "$work"' EXIT

"$emacs" --batch -Q -l "$probe" > "$work/host.out" 2> "$work/host.err" || {
  echo "FAIL host run"; cat "$work/host.err"; exit 1; }
"$bin" -l "$probe" --eval t > "$work/nelisp.out" 2> "$work/nelisp.err" || {
  echo "FAIL standalone run"; head -20 "$work/nelisp.err"; exit 1; }

status=0
if ! cmp -s "$work/host.out" "$work/nelisp.out"; then
  echo "FAIL output differs from stock Emacs:"
  diff "$work/host.out" "$work/nelisp.out" || true
  status=1
fi
if [ -s "$work/nelisp.err" ]; then
  echo "FAIL standalone stderr not empty:"
  head -20 "$work/nelisp.err"
  status=1
fi
[ "$status" -eq 0 ] && echo "PASS nelisp-declare-standalone-smoke ($(wc -l < "$work/host.out") lines match)"
exit "$status"
