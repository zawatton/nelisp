#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="$(realpath "${NELISP_BINARY:?Set NELISP_BINARY}")"
expected="${NELISP_EXPECTED_SHA256:?Set NELISP_EXPECTED_SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-rooted-branch-join.XXXXXX")"
trap 'rm -rf -- "$tmpdir"' EXIT
compile_source="$tmpdir/rooted-branch-join.el"
elc="$compile_source"c
artifact_dir="$tmpdir/artifacts"
mkdir -p "$artifact_dir"
[[ "$(sha256sum "$binary" | cut -d' ' -f1)" == "$expected" ]]
[[ "$(emacs --batch -Q --eval '(princ emacs-version)')" == "31.1" ]]
cp "$script_dir/fixtures/native-bytecode/gnu-31.1-rooted-branch-join.el" "$compile_source"
emacs --batch -Q -f batch-byte-compile "$compile_source" 2>"$tmpdir/compile.stderr"
[[ -f "$elc" ]]
rm -f -- "$compile_source"
[[ ! -e "$compile_source" ]]
stage_log="${NELISP_BRANCH_JOIN_STAGE_LOG:-$PWD/target/progress/gnu-branch-join/native-stage.log}"
mkdir -p -- "$(dirname -- "$stage_log")"
rm -f -- "$stage_log"
NELISP_BRANCH_JOIN_SOURCE="$compile_source" NELISP_BRANCH_JOIN_ELC="$elc" \
NELISP_BRANCH_JOIN_ARTIFACT_DIR="$artifact_dir" NELISP_BRANCH_JOIN_STAGE_LOG="$stage_log" timeout 240 "$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-rooted-branch-join-driver.el" \
  --eval '(progn (nelisp-test-rooted-branch-join-smoke) (princ "rooted-branch-join: PASS (source-free GNU ELC, four-block branch join, CAR/CDR, both arms, identity/GC, wrong-type parity, cleanup/recovery)"))'
