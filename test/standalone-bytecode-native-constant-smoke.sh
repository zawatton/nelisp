#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
api_root=${NELISP_ARTIFACT_API_ROOT:-"$repo_root"}
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-native.XXXXXX")
artifact="$artifact_dir/constant-return.neln"
wrong_abi="$artifact_dir/wrong-abi.neln"
malformed="$artifact_dir/malformed.neln"
branch="$artifact_dir/branch.neln"
call="$artifact_dir/call.neln"
maximum="$artifact_dir/maximum.neln"
too_many="$artifact_dir/too-many.neln"
zero_args="$artifact_dir/zero-args.neln"
unknown_var="$artifact_dir/unknown-var.neln"
special_var="$artifact_dir/special-var.neln"
duplicate_args="$artifact_dir/duplicate-args.neln"
optional_args="$artifact_dir/optional-args.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-constant-smoke: missing executable: $binary" >&2
  exit 2
fi

NELISP_REPO_ROOT="$repo_root" \
NELISP_ARTIFACT_API_ROOT="$api_root" \
NELISP_BC_NATIVE_ARTIFACT="$artifact" \
NELISP_BC_NATIVE_WRONG_ABI="$wrong_abi" \
NELISP_BC_NATIVE_MALFORMED="$malformed" \
NELISP_BC_NATIVE_BRANCH="$branch" \
NELISP_BC_NATIVE_CALL="$call" \
NELISP_BC_NATIVE_MAXIMUM="$maximum" \
NELISP_BC_NATIVE_TOO_MANY="$too_many" \
NELISP_BC_NATIVE_ZERO_ARGS="$zero_args" \
NELISP_BC_NATIVE_UNKNOWN_VAR="$unknown_var" \
NELISP_BC_NATIVE_SPECIAL_VAR="$special_var" \
NELISP_BC_NATIVE_DUPLICATE_ARGS="$duplicate_args" \
NELISP_BC_NATIVE_OPTIONAL_ARGS="$optional_args" \
  "$host" -Q --batch \
    -L "$api_root/lisp" -L "$repo_root/lisp" \
    -L "$api_root/src" -L "$repo_root/src" \
    -L "$api_root/scripts" -L "$repo_root/scripts" \
    -L "$api_root/packages/nl-prelude/src" \
    -L "$repo_root/packages/nl-prelude/src" \
    -l "$repo_root/test/standalone-bytecode-native-constant-driver.el" \
    --eval '(nelisp-test-bytecode-native-constant-build)'

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_NATIVE_ARTIFACT="$artifact" \
  NELISP_BC_NATIVE_WRONG_ABI="$wrong_abi" \
  NELISP_BC_NATIVE_MAXIMUM="$maximum" \
  "$binary" --eval \
    "(progn (load (expand-file-name \"test/standalone-bytecode-native-constant-driver.el\" (getenv \"NELISP_REPO_ROOT\")) nil nil t) (nelisp-test-bytecode-native-constant-run))")
expected='(t t t t t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-constant-smoke: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-constant-smoke: PASS (verified straight-line CFG slots, source-free ELF/.neln, hidden/user args, VM identity across GC, malformed, control-flow, call and wrong-ABI refusals)"
