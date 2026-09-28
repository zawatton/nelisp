#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-jit-predicate-smoke: missing executable: $binary" >&2
  exit 2
fi

result=$(timeout "${NELISP_TIMEOUT_SECONDS:-15}s" env NELISP_REPO_ROOT="$repo_root" "$binary" --eval '(progn
  (load (concat (getenv "NELISP_REPO_ROOT") "/lisp/nelisp-bytecode-jit.el") nil nil t)
  (setq nelisp-bytecode-jit-threshold 1)
  (let* ((symbolp-fn (make-byte-code 257 (unibyte-string 57 135) [] 2))
         (consp-fn (make-byte-code 257 (unibyte-string 58 135) [] 2))
         (stringp-fn (make-byte-code 257 (unibyte-string 59 135) [] 2))
         (listp-fn (make-byte-code 257 (unibyte-string 60 135) [] 2))
         (decoded (list
                   (plist-get (nelisp-bytecode-jit--decode-ir symbolp-fn) :expression)
                   (plist-get (nelisp-bytecode-jit--decode-ir consp-fn) :expression)
                   (plist-get (nelisp-bytecode-jit--decode-ir stringp-fn) :expression)
                   (plist-get (nelisp-bytecode-jit--decode-ir listp-fn) :expression)))
         (validator-stack
          (plist-get
           (nelisp-bytecode-ir-validate (aref symbolp-fn 1) (aref symbolp-fn 2) 1)
           :stack-analysis))
         (values (list (funcall symbolp-fn (quote name))
                       (funcall symbolp-fn 42)
                       (funcall consp-fn (quote (a)))
                       (funcall consp-fn nil)
                       (funcall stringp-fn "text")
                       (funcall stringp-fn (quote name))
                       (funcall listp-fn nil)
                       (funcall listp-fn 42)))
         (status (nelisp-bytecode-jit-status)))
    (list decoded values (plist-get status :native-calls)
          (plist-get validator-stack :status))))' | tail -n 1)
expected='((symbolp x0) (consp x0) (stringp x0) (listp x0))'
expected_values='(t nil t nil t nil t nil)'
if [[ "$result" != *"$expected"* || "$result" != *"$expected_values"* || "$result" != *' 0 complete)'* ]]; then
  echo "standalone-bytecode-jit-predicate-smoke: unexpected result: $result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-predicate-smoke: PASS (predicate IR decoded; exact VM booleans; native calls=0)"
