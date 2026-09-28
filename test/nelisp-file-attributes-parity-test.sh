#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
emacs_bin=${EMACS:-emacs}
nelisp_bin=${NELISP_BIN:-"$root/target/nelisp"}
test_file=$root/test/nelisp-file-attributes-parity-test.el

if [ ! -x "$nelisp_bin" ]; then
  echo "nelisp-file-attributes-parity-test: missing executable: $nelisp_bin" >&2
  exit 2
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

# Fixture is created once, up front, and never touched again -- both
# the host run and the standalone run stat these exact same bytes, so
# mtime/ctime are safe to compare byte-for-byte (see the .el file's
# own comment on why atime is excluded regardless).
fixture="$tmp/fixture"
mkdir -p "$fixture/subdir"
printf 'hi' >"$fixture/regular.txt"
ln -s regular.txt "$fixture/link.txt"
ln -s /no/such/nelisp-file-attrs-parity-target "$fixture/dangling.txt"
# "$fixture/missing.txt" is intentionally never created.

NELISP_FILE_ATTRS_FIXTURE_DIR="$fixture" "$emacs_bin" -Q --batch -l "$test_file" >"$tmp/host"
NELISP_FILE_ATTRS_FIXTURE_DIR="$fixture" "$nelisp_bin" --eval "(load \"$test_file\")" >"$tmp/nelisp.raw"
# The standalone CLI prints the --eval return value after the file's own output.
sed '$d' "$tmp/nelisp.raw" >"$tmp/nelisp"

if ! cmp -s "$tmp/host" "$tmp/nelisp"; then
  printf 'FAIL parity\nHost:   '; cat "$tmp/host"
  printf 'NeLisp: '; cat "$tmp/nelisp"
  exit 1
fi
printf 'PASS Host=NeLisp: '
cat "$tmp/host"

# verify-the-verifier: confirm cmp actually distinguishes a mismatch,
# so a silently-broken comparison cannot masquerade as a pass.
printf ':mismatch\n' >"$tmp/negative"
if cmp -s "$tmp/host" "$tmp/negative"; then
  echo 'FAIL negative control did not detect mismatch' >&2
  exit 1
fi
echo 'PASS negative control'
