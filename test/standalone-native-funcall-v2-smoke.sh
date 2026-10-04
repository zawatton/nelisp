#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
binary=${1:-target/nelisp}
work=$(mktemp -d "$root/target/f1-roots-XXXXXX")
chmod 700 "$work"
mkdir -m 700 "$work/cache"
NELISP_NATIVE_CACHE="$work/cache" timeout 120 "$binary" -L lisp -L src -L scripts \
  --load test/standalone-native-funcall-v2-driver.el > "$work/run.out" 2> "$work/run.err"
cat "$work/run.out"
test ! -s "$work/run.err"
grep -qx 'F1-ROOTS-PASS cases=6' "$work/run.out"
printf 'F1-ROOTS-EVIDENCE=%s\n' "$work"
