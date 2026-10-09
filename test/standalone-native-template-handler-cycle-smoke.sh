#!/usr/bin/env bash
# Force GC and catch real poll exits in cycles with no branch opcode.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
work=$(mktemp -d "$root/target/template-handler-cycle-XXXXXX")
mkdir -m 700 "$work/cache"
TEMPLATE_CYCLE_HOST=1 "${EMACS:-emacs}" -Q --batch -l test/standalone-native-template-handler-cycle.el > "$work/gnu.out" 2> "$work/gnu.err"
grep -qx 'TEMPLATE-HANDLER-CYCLE-GNU-PASS calls=3' "$work/gnu.out"
test ! -s "$work/gnu.err"
binary=${1:-target/nelisp}
sha256sum "$binary" "$binary.cold" > "$work/identity.sha256"
NELISP_NATIVE_CACHE="$work/cache" timeout -k 5 290 "$binary" --cold-load-from "$binary.cold" \
  -L lisp -L src -L scripts -L packages/nl-ffi/src --load test/standalone-native-template-handler-cycle.el > "$work/native.out" 2> "$work/native.err"
cat "$work/native.out"
test ! -s "$work/native.err"
test "$(grep -c '^TEMPLATE-HANDLER-CYCLE-PASS ' "$work/native.out")" = 2
printf 'TEMPLATE-HANDLER-CYCLE-EVIDENCE=%s\n' "$work"
