#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-s25-corpus.XXXXXX")
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "s2.5-corpus: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_S25_ARTIFACT_DIR="$artifact_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-s25-corpus.el" \
    --eval '(nelisp-test-native-s25-corpus)')

if [[ "$actual" != *"S2.5-CONTROL-FLOW-CORPUS: PASS"* ]]; then
  printf 's2.5-corpus: completion marker missing; output was %s\n' "$actual" >&2
  exit 1
fi
echo "s2.5-corpus: PASS (boxed branches/joins, raw diamond, Bswitch, nested loop, GC identity, strict refusals)"
