#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
fixture_dir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-locate-library.XXXXXX")"
trap 'rm -rf "$fixture_dir"' EXIT

mkdir "$fixture_dir/shadow.elc"
printf "(provide 'shadow)\n" > "$fixture_dir/shadow.el"
printf 'not source bytecode\n' > "$fixture_dir/ordered.elc"
printf "(provide 'ordered)\n" > "$fixture_dir/ordered.el"

actual="$(LOCATE_TMP="$fixture_dir" "$binary" -L "$repo_root/lisp" -L "$repo_root/scripts" \
  --load "$repo_root/lisp/nelisp-stdlib-misc.el" \
  --eval '(progn (setq load-path (list (getenv "LOCATE_TMP"))) (princ (list (locate-library "shadow") (locate-library "ordered") (require (quote shadow)) (require (quote ordered)))))')"
expected="($fixture_dir/shadow.el $fixture_dir/ordered.elc shadow ordered)"
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-locate-library-smoke: expected %s, got %s\n' "$expected" "$actual" >&2
  exit 1
fi

echo "standalone-locate-library-smoke: PASS (regular-file filtering, .elc lookup order, source require)"
