#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-jit-branch-smoke: missing executable: $binary" >&2
  exit 2
fi

result=$("$binary" --eval "(let* ((fn (make-byte-code 257 (unibyte-string 137 192 85 131 8 0 193 135 194 135) [0 17 23] 3))) (load \"$repo_root/lisp/nelisp-bytecode-jit.el\" nil nil t) (list (nelisp-bytecode-jit--decode-branch fn) (nelisp-bytecode-jit-call fn 0) (nelisp-bytecode-jit-call fn 1) (nelisp-bytecode-jit-call fn -9)))" | tail -n 1)
expected='((branch-eq-zero 17 23) 17 23 23)'
if [[ "$result" != "$expected" ]]; then
  echo "standalone-bytecode-jit-branch-smoke: unexpected result: $result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-branch-smoke: PASS (GNU Emacs 31.1 branch byte stream -> native results)"
