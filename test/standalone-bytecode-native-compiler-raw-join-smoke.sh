#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
expected_sha256=${NELISP_EXPECTED_BINARY_SHA256:-}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-raw-public.XXXXXX")
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "raw-public-join: missing executable: $binary" >&2
  exit 2
fi
actual_sha256=$(sha256sum "$binary" | cut -d ' ' -f 1)
if [[ -n "$expected_sha256" && "$actual_sha256" != "$expected_sha256" ]]; then
  printf 'raw-public-join: binary SHA-256 mismatch: expected %s, got %s\n' \
    "$expected_sha256" "$actual_sha256" >&2
  exit 1
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_RAW_BC_ARTIFACT_DIR="$artifact_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-compiler-raw-join.el" \
    --eval '(princ (if (nelisp-test-public-raw-bytecode-diamond) "PASS" "FAIL"))')

if [[ "$actual" != PASS ]]; then
  printf 'raw-public-join: expected PASS, got %s\n' "$actual" >&2
  exit 1
fi
echo "raw-public-join: PASS (SHA-256 $actual_sha256; public compile/stage/publish/call matches GNU VM nil/t goldens; nested-loop VM/native parity; ungrounded cycle rejected)"
