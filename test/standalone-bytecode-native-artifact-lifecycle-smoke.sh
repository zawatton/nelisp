#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-artifact-lifecycle.XXXXXX")
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "artifact-lifecycle: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_ARTIFACT_LIFECYCLE_DIR="$artifact_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-artifact-lifecycle.el" \
    --eval '(nelisp-test-bytecode-native-artifact-lifecycle)')

if [[ "$actual" != *"NATIVE-BYTECODE-ARTIFACT-LIFECYCLE: PASS"* ]]; then
  printf 'artifact-lifecycle: completion marker missing; output was %s\n' "$actual" >&2
  exit 1
fi
echo "artifact-lifecycle: PASS (reproducible manifests, byte-code/constants and ABI/SHA invalidation, native generations, rooted object identity across GC)"
