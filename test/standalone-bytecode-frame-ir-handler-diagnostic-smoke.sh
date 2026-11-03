#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$(realpath "$repo_root/../standalone-reader-fix/target/nelisp")"}
if [[ ! -x "$binary" ]]; then
  echo "handler-diagnostic: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" "$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-bytecode-frame-ir-handler-diagnostic-driver.el" \
  --eval '(prin1 (nelisp-test-handler-diagnostic))')

if [[ "$actual" != t ]]; then
  printf 'handler-diagnostic: expected t, got %s\n' "$actual" >&2
  exit 1
fi
echo "handler-diagnostic: PASS (GNU 31.1 literal bytes, PC12/depth diagnostics, unsupported/no blocks, malformed target refused)"
