#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-jit-loop-smoke: missing executable: $binary" >&2
  exit 2
fi

result=$("$binary" --eval "(let* ((fn (make-byte-code 257 (unibyte-string 192 137 2 87 131 11 0 84 130 1 0 135) [0] 4))) (load-file \"$repo_root/lisp/nelisp-bytecode-ir.el\") (load-file \"$repo_root/lisp/nelisp-bytecode-jit.el\") (let* ((nelisp-bytecode-jit-threshold 99) (nelisp-bytecode-jit--hot-counts (make-hash-table :test (quote eq))) (nelisp-bytecode-jit--native-call-count 0) (vm-results (mapcar (lambda (n) (funcall fn n)) (quote (-4 0 1 9)))) (native-results nil)) (setq nelisp-bytecode-jit-threshold 1 nelisp-bytecode-jit--hot-counts (make-hash-table :test (quote eq))) (dolist (n (quote (-4 0 1 9))) (push (nelisp-bytecode-jit--runtime-dispatch fn n) native-results)) (list :vm-results vm-results :native-results (nreverse native-results) :native-calls nelisp-bytecode-jit--native-call-count)))" | tail -n 1)
expected='(:vm-results (0 0 1 9) :native-results ([t 0] [t 0] [t 1] [t 9]) :native-calls 4)'
if [[ "$result" != "$expected" ]]; then
  echo "standalone-bytecode-jit-loop-smoke: unexpected result: $result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-loop-smoke: PASS (GNU Emacs 31.1 backward-loop bytecode -> standalone VM/native parity; native calls=4)"
