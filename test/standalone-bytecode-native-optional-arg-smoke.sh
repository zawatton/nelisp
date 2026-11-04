#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-optional-arg.XXXXXX")
artifact="$artifact_dir/optional-arg.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-optional-arg: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_OPTIONAL_ARG_ARTIFACT="$artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-optional-arg-driver.el" \
    --eval '(princ (if (nelisp-test-bytecode-native-optional-arg-run)
                       "boxed-optional-arg: PASS (missing nil, supplied object, VM/native identity across GC, arity and unsupported-effect refusals)"
                     "boxed-optional-arg: FAIL"))')
expected='boxed-optional-arg: PASS (missing nil, supplied object, VM/native identity across GC, arity and unsupported-effect refusals)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-optional-arg: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "$actual"
