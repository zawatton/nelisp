#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"

actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-native-load-cdr-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-native-load-cdr-smoke)))')

if [[ "$actual" != t ]]; then
  printf 'checked native CDR wrapper smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi

printf 'native-load-cdr: PASS (nil, proper-list tails, nested identity, conditions, cleanup)\n'
