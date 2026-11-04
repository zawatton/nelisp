#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"

actual=$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-root-pin-v2-ticket-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-root-pin-v2-ticket-smoke)))')

if [[ "$actual" != t ]]; then
  printf 'root pin v2 ticket smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi

printf 'root-pin-v2-ticket: PASS (stale generation, nested LIFO, cross-mode, GC, unwind cleanup)\n'
