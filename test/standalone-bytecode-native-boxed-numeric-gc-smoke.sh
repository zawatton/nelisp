#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
api_root=${NELISP_ARTIFACT_API_ROOT:-"$repo_root"}
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-boxed-numeric-gc.XXXXXX")
artifact="$artifact_dir/materialized-identity.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "boxed-numeric-gc: missing executable: $binary" >&2
  exit 2
fi

controls=$(NELISP_REPO_ROOT="$repo_root" "$host" -Q --batch \
    -L "$api_root/lisp" -L "$repo_root/lisp" \
    -L "$api_root/src" -L "$repo_root/src" \
    -L "$api_root/scripts" -L "$repo_root/scripts" \
    -L "$api_root/packages/nl-prelude/src" \
    -L "$repo_root/packages/nl-prelude/src" \
    -l "$repo_root/test/standalone-bytecode-native-boxed-numeric-gc-driver.el" \
    --eval '(nelisp-test-bytecode-native-boxed-numeric-gc-negative-controls)')
if [[ "$controls" != *"decoder controls PASS"* ]]; then
  printf 'boxed-numeric-gc: decoder controls failed: %s\n' "$controls" >&2
  exit 1
fi

if ! actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_ENTRY_ARTIFACT="$artifact" \
  "$binary" -L "$api_root/lisp" -L "$repo_root/lisp" \
    -L "$api_root/src" -L "$repo_root/src" \
    -L "$api_root/scripts" -L "$repo_root/scripts" \
    -L "$api_root/packages/nl-prelude/src" \
    -L "$repo_root/packages/nl-prelude/src" \
    -l "$repo_root/test/standalone-bytecode-native-boxed-numeric-gc-driver.el" \
    --eval '(nelisp-test-bytecode-native-boxed-numeric-gc-suite)' 2>&1); then
  printf 'boxed-numeric-gc: standalone FAIL:\n%s\n' "$actual" >&2
  exit 1
fi
if [[ "$actual" != *"NELISP_BC_BOXED_NUMERIC_GC_PASS"* ]]; then
  printf 'boxed-numeric-gc: expected E2E PASS, got %s\n' "$actual" >&2
  exit 1
fi
echo "boxed-numeric-gc: PASS (fresh binary compile/load/call, fixnum bounds, bignum, float, negative zero, cons identity/mutation across forced GC)"
