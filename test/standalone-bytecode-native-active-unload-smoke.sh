#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-active-unload.XXXXXX")
artifact="$artifact_dir/active-unload.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-active-unload: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_ACTIVE_UNLOAD_ARTIFACT="$artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-active-unload-driver.el" \
    --eval '(princ (if (nelisp-test-bytecode-native-active-unload-run)
                       "active-unload: PASS (close refused during protected call extent, unit remained callable, post-return close released roots and code)"
                     "active-unload: FAIL"))')
expected='active-unload: PASS (close refused during protected call extent, unit remained callable, post-return close released roots and code)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-active-unload: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "$actual"
