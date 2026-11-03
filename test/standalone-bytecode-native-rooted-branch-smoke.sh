#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${NELISP_REPO_ROOT:?Set NELISP_REPO_ROOT to the NeLisp checkout}"
binary="$(realpath "${NELISP_BINARY:?Set NELISP_BINARY}")"
expected="${NELISP_EXPECTED_SHA256:?Set NELISP_EXPECTED_SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-rooted-branch.XXXXXX")"
trap 'rm -rf -- "$tmpdir"' EXIT
source="$script_dir/fixtures/native-bytecode/gnu-31.1-rooted-branch.el"
compile_source="$tmpdir/rooted-branch.el"
elc="$compile_source"c
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
NELISP_BRANCH_SOURCE="$compile_source" NELISP_BRANCH_ELC="$elc" \
NELISP_BRANCH_ARTIFACT_DIR="$artifact_dir" timeout 240 "$binary" \
  -L "$script_dir/../lisp" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-rooted-branch-driver.el" \
  --eval '(progn (nelisp-test-rooted-branch-source-free-smoke) (princ "rooted-branch: PASS (source-free GNU ELC, authenticated three-block branch, CAR/CDR, GC identity, wrong-type parity, cleanup/recovery)"))'
