#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
api_root=${NELISP_ARTIFACT_API_ROOT:-"$repo_root"}
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-entry.XXXXXX")
artifact="$artifact_dir/materialized-argument.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-compiler-smoke: missing executable: $binary" >&2
  exit 2
fi

NELISP_REPO_ROOT="$repo_root" \
NELISP_ARTIFACT_API_ROOT="$api_root" \
NELISP_BC_ENTRY_ARTIFACT="$artifact" \
  "$host" -Q --batch \
    -L "$api_root/lisp" -L "$repo_root/lisp" \
    -L "$api_root/src" -L "$repo_root/src" \
    -L "$api_root/scripts" -L "$repo_root/scripts" \
    -L "$api_root/packages/nl-prelude/src" \
    -L "$repo_root/packages/nl-prelude/src" \
    -l "$repo_root/test/standalone-bytecode-native-compiler-driver.el" \
    --eval '(nelisp-test-bytecode-native-compiler-build)'

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_ENTRY_ARTIFACT="$artifact" \
  "$binary" --eval \
    "(progn (load (expand-file-name \"test/standalone-bytecode-native-compiler-driver.el\" (getenv \"NELISP_REPO_ROOT\")) nil nil t) (nelisp-test-bytecode-native-compiler-run))")
if [[ "$actual" != t ]]; then
  printf 'standalone-bytecode-native-compiler-smoke: expected t, got %s\n' \
    "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-compiler-smoke: PASS (materialized byte-code, required arg descriptor, source-free boxed entry, VM/native eq identity, GC and mutation)"
