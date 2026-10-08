#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$(realpath "$repo_root/../standalone-reader-fix/target/nelisp")"}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-optional-shortcircuit.XXXXXX")
or_artifact="$artifact_dir/or.neln"
and_artifact="$artifact_dir/and.neln"
cleanup() {
  find "$artifact_dir" -type f -delete
  rmdir "$artifact_dir"
}
trap cleanup EXIT

if [[ ! -x "$binary" ]]; then
  echo "boxed-optional-shortcircuit: missing executable: $binary" >&2
  exit 2
fi

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_OPTIONAL_OR_ARTIFACT="$or_artifact" \
  NELISP_OPTIONAL_AND_ARTIFACT="$and_artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --eval '(require (quote nelisp-bytecode-native-compiler))' \
  --load "$repo_root/test/standalone-bytecode-native-optional-shortcircuit-driver.el" \
  --eval '(nelisp-test-optional-shortcircuit-compile)' \
  --eval '(require (quote nelisp-native-boxed-unit))' \
  --eval '(prin1 (nelisp-test-optional-shortcircuit-native-call))')

if [[ "$actual" != t ]]; then
  printf 'boxed-optional-shortcircuit: expected t, got %s\n' "$actual" >&2
  exit 1
fi
echo "boxed-optional-shortcircuit: PASS (or/and GNU bytecode 513, native both paths, GC identity, strict refusals)"
