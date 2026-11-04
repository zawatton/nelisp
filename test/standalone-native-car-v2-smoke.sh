#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"

actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-native-car-v2-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-native-car-v2-smoke)))')

if [[ "$actual" != t ]]; then
  printf 'native CAR v2 smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi

printf 'native-car-v2: PASS (nil, mutable cons, GC identity, refusal statuses)\n'
