#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="$(realpath "${NELISP_BINARY:?Set NELISP_BINARY}")"
expected="${NELISP_EXPECTED_SHA256:?Set NELISP_EXPECTED_SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-unary-chain.XXXXXX")"
trap 'rm -rf -- "$tmpdir"' EXIT
source="$repo_root/test/fixtures/native-bytecode/unary-chain.el"
compile_source="$tmpdir/unary-chain.el"
elc="$tmpdir/unary-chain.elc"
actual="$(sha256sum "$binary" | cut -d' ' -f1)"
[[ "$actual" == "$expected" ]]
[[ -x "$binary" ]]
[[ "$(emacs --batch -Q --eval '(princ emacs-version)')" == "31.1" ]]
cp "$source" "$compile_source"
emacs --batch -Q -f batch-byte-compile "$compile_source" 2>"$tmpdir/compile.stderr"
[[ -f "$elc" ]]
rm -f -- "$compile_source"
[[ ! -e "$compile_source" ]]
NELISP_CHAIN_SOURCE="$compile_source" NELISP_CHAIN_ELC="$elc" \
  NELISP_CHAIN_EVEN="$tmpdir/even.nelr" NELISP_CHAIN_ODD="$tmpdir/odd.nelr" \
  NELISP_CHAIN_ERROR="$tmpdir/error.nelr" timeout 150 "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
  -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-unary-chain-driver.el" \
  --eval '(progn (nelisp-test-native-unary-chain-smoke) (princ "unary-chain: PASS (GNU 31.1 literal attestation, source-free even/odd compiler-dispatched chains, checked rooted caller, VM parity, GC identity, first and intermediate error stop)"))'
