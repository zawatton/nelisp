#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
timeout_seconds=${NELISP_TIMEOUT_SECONDS:-45}
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-jit-native-predicate-smoke: missing executable: $binary" >&2
  exit 2
fi

result=$(timeout "${timeout_seconds}s" env NELISP_REPO_ROOT="$repo_root" "$binary" --eval '(progn
  (load (concat (getenv "NELISP_REPO_ROOT") "/lisp/nelisp-bytecode-jit.el") nil nil t)
  (setq nelisp-bytecode-jit-threshold 1)
  (let* ((symbolp-fn (make-byte-code 257 (unibyte-string 57 135) [] 2))
         (consp-fn (make-byte-code 257 (unibyte-string 58 135) [] 2))
         (listp-fn (make-byte-code 257 (unibyte-string 60 135) [] 2))
         (symbol-value (quote jit-bridge-symbol))
         (list-value (quote (a b)))
         (nested-value (quote (a (b c))))
         (vm-values (list (funcall symbolp-fn symbol-value)
                          (funcall consp-fn list-value)
                          (funcall listp-fn list-value)
                          (funcall consp-fn nested-value)
                          (funcall listp-fn nested-value)))
         (_symbol-handle (nelisp-bytecode-jit-prepare symbolp-fn))
         (_cons-handle (nelisp-bytecode-jit-prepare consp-fn))
         (_list-handle (nelisp-bytecode-jit-prepare listp-fn))
         (before (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (native-values (list (funcall symbolp-fn symbol-value)
                              (funcall consp-fn list-value)
                              (funcall listp-fn list-value)))
         (after-native (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (nested-dispatch
          (list (nelisp-bytecode-jit--runtime-dispatch consp-fn nested-value)
                (nelisp-bytecode-jit--runtime-dispatch listp-fn nested-value)))
         (after-nested (plist-get (nelisp-bytecode-jit-status) :native-calls))
         (cycle (list (quote a)))
         (uninterned (make-symbol "jit-uninterned"))
         (over-budget (make-list 257 (quote a)))
         (negative-values nil))
    (setcdr cycle cycle)
    (setq negative-values
          (list (funcall symbolp-fn uninterned)
                (funcall consp-fn (quote (a . b)))
                (funcall listp-fn (quote (a . b)))
                (funcall consp-fn cycle)
                (funcall listp-fn cycle)
                (funcall symbolp-fn [a])
                (funcall consp-fn (list uninterned))
                (funcall consp-fn over-budget)))
    (let* ((status (nelisp-bytecode-jit-status))
           (final-calls (plist-get status :native-calls)))
      (list vm-values native-values (equal (cl-subseq vm-values 0 3)
                                           native-values)
          (= (- after-native before) 3)
          (and (equal nested-dispatch (list nil nil))
               (= after-nested after-native))
          (= final-calls after-native)
          negative-values
          (plist-get status :compiled-functions)
          (plist-get status :interpreter-fallbacks)))))')

expected='((t t t t t) (t t t) t t t t (t t t t t nil t t) 3 0)'
if [[ "$result" != "$expected" ]]; then
  echo "standalone-bytecode-jit-native-predicate-smoke: unexpected result: $result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-native-predicate-smoke: PASS (interned symbol and flat proper-list predicates; nested/unsupported inputs stayed in VM)"
