#!/bin/sh
# nelisp-native-normal-boot-smoke.sh --- harvested native code runs in a normal boot (F43/F50).
# Usage: NELISP_BIN=target/nelisp test/nelisp-native-normal-boot-smoke.sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
bin=${NELISP_BIN:-$repo/target/nelisp}
work=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-nnb.XXXXXX")
trap 'rm -rf "$work"' EXIT
"$bin" dump-runtime-image "$work/empty.nlri" '(progn nil)' < /dev/null
"$bin" exec-runtime-image "$work/empty.nlri" "(load \"$script_dir/nelisp-native-normal-boot-standalone.el\" nil t t)" \
  < /dev/null > "$work/out" 2> "$work/err" || true
cat "$work/out"
if grep -q '^NNB-DONE fail=0' "$work/out" && ! grep -q '^FAIL' "$work/out"; then
  echo "PASS nelisp-native-normal-boot-smoke"; exit 0
fi
echo "FAIL nelisp-native-normal-boot-smoke"; head -5 "$work/err"; exit 1
