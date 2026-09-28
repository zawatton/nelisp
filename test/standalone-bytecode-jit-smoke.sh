#!/usr/bin/env bash
set -euo pipefail

binary=${1:-target/nelisp}
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

output=$("$binary" --eval '(let* ((control (make-byte-code 257 (unibyte-string 84 135) [] 2)) (before-require (funcall control 5))) (require (quote nelisp-bytecode-jit)) (let ((initial (nelisp-bytecode-jit-status))) (setq nelisp-bytecode-jit-threshold 3) (let* ((add (make-byte-code 257 (unibyte-string 84 135) [] 2)) (sub (make-byte-code 257 (unibyte-string 83 135) [] 2)) (unsupported (make-byte-code 257 (unibyte-string 135) [] 2)) (noninteger-refused (null (nelisp-bytecode-jit--runtime-dispatch add 2.5))) (bignum-refused (null (nelisp-bytecode-jit--runtime-dispatch add 1267650600228229401496703205376))) (add-boundary-refused (null (nelisp-bytecode-jit--runtime-dispatch add most-positive-fixnum))) (sub-boundary-refused (null (nelisp-bytecode-jit--runtime-dispatch sub most-negative-fixnum))) (boundary-fallback (= (plist-get (nelisp-bytecode-jit-status) :native-calls) 0)) (add-cold-1 (funcall add 41)) (add-cold-2 (funcall add -41)) (add-hot (funcall add 0)) (add-hot-2 (funcall add -2)) (sub-cold-1 (funcall sub 41)) (sub-cold-2 (funcall sub -41)) (sub-hot (funcall sub 0)) (sub-hot-2 (funcall sub -2)) (unsupported-result (funcall unsupported (quote (a b)))) (status (nelisp-bytecode-jit-status))) (list before-require (= (plist-get initial :dispatch-attempts) 0) (= (plist-get initial :native-calls) 0) noninteger-refused bignum-refused add-boundary-refused sub-boundary-refused boundary-fallback add-cold-1 add-cold-2 add-hot add-hot-2 sub-cold-1 sub-cold-2 sub-hot sub-hot-2 unsupported-result (= (plist-get status :native-calls) 4) (= (plist-get status :interpreter-fallbacks) 4) (= (hash-table-count nelisp-bytecode-jit--handles) 2) (= (plist-get status :compiled-functions) 2) (> (plist-get status :dispatch-attempts) 12) (> (plist-get status :mapped-entry) 4096)))))')
result=${output##*$'\n'}
if [[ "$result" != '(6 t t t t t t t 42 -40 1 -1 40 -42 -1 -3 (a b) t t t t t t)' ]]; then
  echo "standalone-bytecode-jit-smoke: unexpected result: $result" >&2
  exit 1
fi

nil_output=$("$binary" --eval '(let* ((function (make-byte-code 0 (unibyte-string 192 135) [nil] 1))) (require (quote nelisp-bytecode-jit)) (setq nelisp-bytecode-jit-threshold 2) (let* ((vm-result (let ((nelisp-bytecode-jit--dispatch-active t)) (funcall function))) (cold-result (funcall function)) (cold-count (plist-get (nelisp-bytecode-jit-status) :native-calls)) (hot-result (funcall function)) (hot-count (plist-get (nelisp-bytecode-jit-status) :native-calls)) (arity-error (condition-case error-data (progn (funcall function 7) nil) (wrong-number-of-arguments (eq (car error-data) (quote wrong-number-of-arguments))))) (reentrant-result (let ((nelisp-bytecode-jit--dispatch-active t)) (nelisp-bytecode-jit--runtime-dispatch function)))) (list (null vm-result) (null cold-result) (= cold-count 0) (null hot-result) (= hot-count 1) arity-error (null reentrant-result) (= (plist-get (nelisp-bytecode-jit-status) :native-calls) 1))))')
nil_result=${nil_output##*$'\n'}
if [[ "$nil_result" != '(t t t t t t t t)' ]]; then
  echo "standalone-bytecode-jit-smoke: zero-argument nil byte-code dispatch failed: $nil_result" >&2
  exit 1
fi

# Emacs 31.1 vendor/staged-emacs-lisp/subr.el caar at line 612 has body
# (car (car x)); byte-compile emits [137 64 64 135], constants [], depth 2.
car_output=$("$binary" --eval '(progn (require (quote nelisp-bytecode-jit)) (setq nelisp-bytecode-jit-threshold 2) (let* ((function (make-byte-code 257 (unibyte-string 137 64 64 135) [] 2)) (marker '\''nelisp-caar-marker) (input (list (list marker))) (vm-result (let ((nelisp-bytecode-jit--dispatch-active t)) (funcall function input))) (cold-result (funcall function input)) (cold-count (plist-get (nelisp-bytecode-jit-status) :native-calls))) (garbage-collect) (let* ((hot-result (funcall function input)) (hot-count (plist-get (nelisp-bytecode-jit-status) :native-calls)) (cons-input (list (list (list marker)))) (cons-result (funcall function cons-input)) (cons-count (plist-get (nelisp-bytecode-jit-status) :native-calls)) (invalid (condition-case error-data (progn (funcall function 1) '\''missed) (wrong-type-argument (list (car error-data) (cdr error-data))))) (expected-cons (car (car cons-input)))) (list (eq vm-result marker) (eq cold-result marker) (= cold-count 0) (eq hot-result marker) (= hot-count 1) (eq cons-result expected-cons) (= cons-count (1+ hot-count)) (and (eq (car invalid) '\''wrong-type-argument) (eq (car (cadr invalid)) '\''listp) (= (cadr (cadr invalid)) 1))))))')
car_result=${car_output##*$'\n'}
if [[ "$car_result" != '(t t t t t t t t)' ]]; then
  echo "standalone-bytecode-jit-smoke: boxed byte-car parity failed: $car_result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-smoke: PASS (RX ADD1/SUB1, boxed nil and byte-car; unsafe results fall back)"
