#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="$(realpath "${NELISP_BINARY:?Set NELISP_BINARY}")"
expected="${NELISP_EXPECTED_SHA256:?Set NELISP_EXPECTED_SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-rooted-stack.XXXXXX")"
trap 'rm -rf -- "$tmpdir"' EXIT
source="$repo_root/test/fixtures/native-bytecode/rooted-stack.el"
compile_source="$tmpdir/rooted-stack.el"
elc="$tmpdir/rooted-stack.elc"
artifact_dir="$tmpdir/artifacts"
mkdir -p "$artifact_dir"
actual="$(sha256sum "$binary" | cut -d' ' -f1)"
[[ "$actual" == "$expected" ]]
[[ -x "$binary" ]]
[[ "$(emacs --batch -Q --eval '(princ emacs-version)')" == "31.1" ]]
cp "$source" "$compile_source"
emacs --batch -Q -f batch-byte-compile "$compile_source" 2>"$tmpdir/compile.stderr"
[[ -f "$elc" ]]
rm -f -- "$compile_source"
[[ ! -e "$compile_source" ]]
NELISP_ROOTED_SOURCE="$compile_source" NELISP_ROOTED_ELC="$elc" \
  NELISP_ROOTED_ARTIFACT_DIR="$artifact_dir" timeout 240 "$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-rooted-stack-driver.el" \
  --eval '(progn (nelisp-test-rooted-stack-source-free-smoke) (princ "rooted-stack: PASS (source-free GNU bytecode, authenticated public producer/caller, composed CAR/CDR/CONS, VM identity, GC, mutation, error payload parity)"))'
