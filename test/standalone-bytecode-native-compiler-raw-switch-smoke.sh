#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-raw-bswitch.XXXXXX")
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "raw-public-bswitch: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_RAW_BC_ARTIFACT_DIR="$artifact_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-compiler-raw-switch.el" \
    --eval '(princ (if (nelisp-test-public-raw-bytecode-bswitch) "PASS" "FAIL"))')

if [[ "$actual" != PASS ]]; then
  printf 'raw-public-bswitch: expected PASS, got %s\n' "$actual" >&2
  exit 1
fi
echo "raw-public-bswitch: PASS (public compile/stage/publish/call matches GNU VM selectors 1/2/7)"
