#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-native-internal.XXXXXX")
artifact="$artifact_dir/constant-return.neln"
wrong_abi="$artifact_dir/wrong-abi.neln"
malformed="$artifact_dir/malformed.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-constant-internal: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_NATIVE_ARTIFACT="$artifact" \
  NELISP_BC_NATIVE_WRONG_ABI="$wrong_abi" \
  NELISP_BC_NATIVE_MALFORMED="$malformed" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-constant-internal.el" \
    --eval '(princ (prin1-to-string (nelisp-test-bytecode-native-constant-internal-run)))')
expected='(t t t t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-constant-internal: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-constant-internal: PASS (in-runtime compile/load/call, cons identity across GC, malformed and wrong-ABI refusals)"
