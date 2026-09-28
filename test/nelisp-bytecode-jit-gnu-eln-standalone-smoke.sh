#!/usr/bin/env bash
set -euo pipefail

test_dir=$(cd "$(dirname "$0")" && pwd)
overlay=$(cd "$test_dir/.." && pwd)
source_root=${NELISP_TEST_SOURCE_ROOT:?set NELISP_TEST_SOURCE_ROOT}
binary=${NELISP_BIN:-"$source_root/target/nelisp"}
jit_source=${NELISP_JIT_ADAPTER_SOURCE:-"$overlay/lisp/nelisp-bytecode-jit.el"}
tmp_dir=$(mktemp -d)
trap 'result=$?; if [ "$result" -eq 0 ]; then rm -rf "$tmp_dir"; else printf "Smoke artifacts retained at %s\n" "$tmp_dir" >&2; fi' EXIT

cd "$source_root"
if ! NELISP_ROOT="$source_root" NELISP_JIT_ADAPTER_SOURCE="$jit_source" \
  "$binary" \
  --load "$test_dir/nelisp-bytecode-jit-gnu-eln-standalone-smoke.el" \
  --eval '(progn (unless (= (funcall nelisp-test-f) 17) (error "cold VM result was not 17")) (princ "cold-user-result=17\n"))' \
  --eval '(nelisp-bytecode-jit-drain-pending)' \
  --eval '(let* ((status (nelisp-bytecode-jit-status)) (record (gethash nelisp-test-f nelisp-bytecode-jit--handles)) (handle (plist-get record :handle)) (artifact (plist-get handle :artifact))) (unless (and (= (plist-get status :compiled-functions) 1) (= (plist-get status :native-calls) 0) (eq (plist-get handle :backend) (quote gnu-eln-native-subr)) (stringp artifact) (string-suffix-p ".eln" artifact)) (error "safe-boundary publication mismatch: %S %S failure=%S" status handle (gethash nelisp-test-f nelisp-bytecode-jit--compile-failures))))' \
  --eval '(progn (unless (= (funcall nelisp-test-f) 17) (error "warm user result was not 17")) (unless (= (plist-get (nelisp-bytecode-jit-status) :native-calls) 1) (error "warm call did not enter native backend")) (princ "warm-user-result=17 backend=gnu-eln-native-subr\n"))' \
  >"$tmp_dir/stdout" 2>"$tmp_dir/stderr"; then
  cat "$tmp_dir/stdout"
  cat "$tmp_dir/stderr" >&2
  exit 1
fi

if [ -s "$tmp_dir/stderr" ]; then
  cat "$tmp_dir/stderr" >&2
  exit 1
fi
if ! grep -q '^cold-user-result=17$' "$tmp_dir/stdout" \
  || ! grep -q '^warm-user-result=17 backend=gnu-eln-native-subr$' "$tmp_dir/stdout"; then
  cat "$tmp_dir/stdout"
  exit 1
fi
cat "$tmp_dir/stdout"
