#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
emacs_bin=${EMACS:-emacs}
nelisp_bin=${NELISP_BIN:-"$root/target/nelisp"}
test_file=$root/test/nelisp-marker-parity-test.el
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

"$emacs_bin" -Q --batch -l "$test_file" >"$tmp/host"
"$nelisp_bin" --eval "(load \"$test_file\")" >"$tmp/nelisp.raw"
# The standalone CLI prints the --eval return value after the file's own output.
sed '$d' "$tmp/nelisp.raw" >"$tmp/nelisp"
if ! cmp -s "$tmp/host" "$tmp/nelisp"; then
  printf 'FAIL parity\nHost:   '; cat "$tmp/host"
  printf 'NeLisp: '; cat "$tmp/nelisp"
  exit 1
fi
printf 'PASS Host=NeLisp: '
cat "$tmp/host"

# Negative control: a pre-change binary (no marker API at all) must NOT
# match host output, so this parity check is provably not vacuous.
pre_bin=${NELISP_BIN_PRE:-}
if [ -n "$pre_bin" ] && [ -x "$pre_bin" ]; then
  "$pre_bin" --eval "(load \"$test_file\")" >"$tmp/pre.raw" 2>"$tmp/pre.err" || true
  sed '$d' "$tmp/pre.raw" >"$tmp/pre" 2>/dev/null || true
  if cmp -s "$tmp/host" "$tmp/pre"; then
    echo 'FAIL negative control: pre-change binary unexpectedly matched host' >&2
    exit 1
  fi
  echo 'PASS negative control (pre-change binary diverges from host)'
else
  printf '41\n' >"$tmp/negative"
  if cmp -s "$tmp/host" "$tmp/negative"; then
    echo 'FAIL negative control did not detect mismatch' >&2
    exit 1
  fi
  echo 'PASS negative control (decoy)'
fi
