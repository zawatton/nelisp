#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"

actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-native-cons-v2-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-native-cons-v2-smoke)))')

if [[ "$actual" != t ]]; then
  printf 'native CONS v2 smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi

printf 'native-cons-v2: PASS (nested construction, GC identity/mutation, refusals)\n'
