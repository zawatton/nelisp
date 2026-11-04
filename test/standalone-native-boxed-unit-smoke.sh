#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-boxed-unit.XXXXXX")
artifact="$artifact_dir/boxed-values.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-native-boxed-unit-smoke: missing executable: $binary" >&2
  exit 2
fi

if ! NELISP_REPO_ROOT="$repo_root" NELISP_BOXED_ARTIFACT="$artifact" \
    "$host" -Q --batch -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -l "$repo_root/test/standalone-native-boxed-unit-driver.el" \
    --eval '(nelisp-test-native-boxed-unit-build)'; then
  echo "standalone-native-boxed-unit-smoke: AOT build failed" >&2
  exit 1
fi

actual=$(NELISP_REPO_ROOT="$repo_root" NELISP_BOXED_ARTIFACT="$artifact" \
  "$binary" --eval \
  "(progn (load (expand-file-name \"test/standalone-native-boxed-unit-driver.el\" (getenv \"NELISP_REPO_ROOT\")) nil nil t) (nelisp-test-native-boxed-unit-run))")
expected='(t t t t t t t t t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-native-boxed-unit-smoke: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "standalone-native-boxed-unit-smoke: PASS (hidden rooted constant, identity, mutation, GC, ABI/arity/close negatives)"
