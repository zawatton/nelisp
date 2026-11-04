#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"
driver="${2:-$repo_root/test/standalone-native-pin-copy-v2-driver.el}"

actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$driver" \
  --eval '(princ (prin1-to-string (nelisp-test-native-pin-copy-v2-smoke)))')

if [[ "$actual" != t ]]; then
  printf 'native pin-copy v2 smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi

printf 'native-pin-copy-v2: PASS (identity across GC, mutation visibility, stale ticket/index refusal, output unchanged)\n'
