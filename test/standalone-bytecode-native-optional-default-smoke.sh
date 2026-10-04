#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-optional-default.XXXXXX")
artifact="$artifact_dir/optional-default.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "boxed-optional-default: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" NELISP_OPTIONAL_DEFAULT_ARTIFACT="$artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --eval '(require (quote nelisp-bytecode-native-compiler))' \
  --load "$repo_root/test/standalone-bytecode-native-optional-default-driver.el" \
  --eval '(nelisp-test-optional-default-compile)' \
  --eval '(require (quote nelisp-native-boxed-unit))' \
  --eval '(prin1 (nelisp-test-optional-default-native-call))')

if [[ "$actual" != t ]]; then
  printf 'boxed-optional-default: expected t, got %s\n' "$actual" >&2
  exit 1
fi
echo "boxed-optional-default: PASS (GNU bytecode 513, native both paths, nil fallback, VM/native identity across GC, strict refusals)"
