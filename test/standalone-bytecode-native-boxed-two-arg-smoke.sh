#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-boxed-two-arg.XXXXXX")
artifact="$artifact_dir/runtime-boxed-two-arg.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-boxed-two-arg: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" NELISP_BOXED_TWO_ARG_ARTIFACT="$artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$repo_root/test/standalone-bytecode-native-boxed-two-arg-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-bytecode-native-boxed-two-arg-run)))')
expected='(t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-boxed-two-arg: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-boxed-two-arg: PASS (packed514 ABI, VM/native boxed identity, GC, mutation)"
