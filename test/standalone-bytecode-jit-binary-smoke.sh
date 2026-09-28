#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-jit-binary-smoke: missing executable: $binary" >&2
  exit 2
fi

result=$("$binary" --eval "(progn
  (load \"$repo_root/lisp/nelisp-bytecode-jit.el\" nil nil t)
  (setq nelisp-bytecode-jit-threshold 3)
  (let* ((add (make-byte-code 514 (unibyte-string 1 1 92 135) [] 4))
         (near-miss (make-byte-code 514 (unibyte-string 1 1 84 92 135) [] 4))
         (cold-1 (funcall add 19 23))
         (cold-count (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (cold-2 (funcall add 20 22))
         (cold-count-2 (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (hot (funcall add 18 24))
         (hot-count (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (original-plus (symbol-function '+))
         (override-value
          (unwind-protect
              (progn (fset '+ (lambda (&rest _args) 999))
                     (funcall add 18 24))
            (fset '+ original-plus)))
         (near-value (funcall near-miss 50 7))
         (near-count (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (float-count (progn (funcall add 1.5 2.25)
                             (plist-get (nelisp-bytecode-jit-status) :native-calls)))
         (bignum-count (progn (funcall add (ash 1 100) 1)
                              (plist-get (nelisp-bytecode-jit-status) :native-calls)))
         (overflow-count (progn (funcall add most-positive-fixnum 1)
                                (plist-get (nelisp-bytecode-jit-status) :native-calls)))
         (status (nelisp-bytecode-jit-status))
         (fallback-count (plist-get status :native-calls))
         (rx-entry-p (> (plist-get status :mapped-entry) 4096)))
    (list cold-1 cold-count cold-2 cold-count-2 hot hot-count override-value
          near-value near-count float-count bignum-count overflow-count fallback-count
          rx-entry-p)))" | tail -n 1)
expected='(42 0 42 0 42 1 42 58 1 1 1 1 1 t)'
if [[ "$result" != "$expected" ]]; then
  echo "standalone-bytecode-jit-binary-smoke: unexpected result: $result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-binary-smoke: PASS (cold VM -> hot RX ADD, near-miss VM fallback)"
