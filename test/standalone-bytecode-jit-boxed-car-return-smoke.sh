#!/usr/bin/env bash
set -euo pipefail

binary=${1:-target/nelisp}
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

# GNU Emacs 31.1 caar body: descriptor 257, bytes 137 64 64 135,
# constants [], stack depth 2; coverage fingerprint (SHA256 of printed
# (descriptor code constants depth)) = 54657f49c4d902c5a7c19d4a30e977cb6c49f11c8add17ae6ab0166f845f2643.
output=$("$binary" --eval '(progn (require (quote nelisp-bytecode-jit)) (setq nelisp-bytecode-jit-threshold 2) (let* ((function (make-byte-code 257 (unibyte-string 137 64 64 135) [] 2)) (cons-child (list (quote marker))) (cons-input (list (list cons-child))) (vm-cons (let ((nelisp-bytecode-jit--dispatch-active t)) (funcall function cons-input))) (cold-cons (funcall function cons-input)) (cons-count-before (plist-get (nelisp-bytecode-jit-status) :native-calls))) (garbage-collect) (let* ((hot-cons (funcall function cons-input)) (cons-count-after (plist-get (nelisp-bytecode-jit-status) :native-calls)) (string-function (make-byte-code 257 (unibyte-string 137 64 64 135) [] 2)) (shared-string (copy-sequence "payload")) (string-input (list (list shared-string))) (vm-string (let ((nelisp-bytecode-jit--dispatch-active t)) (funcall string-function string-input))) (cold-string (funcall string-function string-input))) (garbage-collect) (let* ((hot-string (funcall string-function string-input)) (string-count-after (plist-get (nelisp-bytecode-jit-status) :native-calls)) (nested-child (list (list (quote marker)))) (nested-input (list (list nested-child))) (nested-result (funcall function nested-input)) (nested-count (plist-get (nelisp-bytecode-jit-status) :native-calls)) (invalid (condition-case error-data (progn (funcall function 1) (quote missed)) (wrong-type-argument (list (car error-data) (cdr error-data))))) (final-count (plist-get (nelisp-bytecode-jit-status) :native-calls))) (list (= (aref function 0) 257) (equal (string-to-list (aref function 1)) (quote (137 64 64 135))) (equal (aref function 2) []) (= (aref function 3) 2) (equal (secure-hash (quote sha256) (prin1-to-string (list (aref function 0) (aref function 1) (aref function 2) (aref function 3)))) "54657f49c4d902c5a7c19d4a30e977cb6c49f11c8add17ae6ab0166f845f2643") (eq vm-cons cons-child) (eq cold-cons cons-child) (= cons-count-before 0) (eq hot-cons cons-child) (= cons-count-after 1) (eq vm-string shared-string) (eq cold-string shared-string) (eq hot-string shared-string) (= string-count-after 2) (eq nested-result nested-child) (= nested-count 3) (and (eq (car invalid) (quote wrong-type-argument)) (eq (car (cdr invalid)) (quote listp)) (= (cadr (cdr invalid)) 1)) (= final-count 3))))))')
result=${output##*$'\n'}
if [[ "$result" != '(t t t t t t t t t t t t t t t t nil t)' ]]; then
  echo "standalone-bytecode-jit-boxed-car-return-smoke: unexpected result: $result" >&2
  exit 1
fi

signal_output=$("$binary" --eval '(let ((function (make-byte-code 257 (unibyte-string 137 64 64 135) [] 2))) (require (quote nelisp-bytecode-jit)) (condition-case error-data (progn (funcall function 1) (quote missed)) (wrong-type-argument (equal error-data (quote (wrong-type-argument listp 1))))) )')
signal_result=${signal_output##*$'\n'}
if [[ "$signal_result" != 't' ]]; then
  echo "standalone-bytecode-jit-boxed-car-return-smoke: signal parity failed: $signal_result" >&2
  exit 1
fi

host_output=$(emacs -Q --batch --eval '
(let* ((function (make-byte-code 257 (unibyte-string 137 64 64 135) [] 2))
       (cons-child (list (quote marker)))
       (cons-result (funcall function (list (list cons-child))))
       (string-child (copy-sequence "payload"))
       (string-result (funcall function (list (list string-child))))
       (nested-child (list (list (quote marker))))
       (nested-result (funcall function (list (list nested-child))))
       (signal (condition-case error-data
                   (progn (funcall function 1) (quote missed))
                 (wrong-type-argument
                  (list (car error-data) (cdr error-data)))))
       (fingerprint
        (secure-hash (quote sha256)
                     (prin1-to-string
                      (list (aref function 0) (aref function 1)
                            (aref function 2) (aref function 3))))))
  (prin1
   (list (string-match-p "\\`31\\.1" emacs-version)
         (equal fingerprint
                "54657f49c4d902c5a7c19d4a30e977cb6c49f11c8add17ae6ab0166f845f2643")
         (eq cons-result cons-child)
         (eq string-result string-child)
         (eq nested-result nested-child)
         (and (eq (car signal) (quote wrong-type-argument))
              (eq (car (cadr signal)) (quote listp))
              (= (cadr (cadr signal)) 1)))))')
host_result=${host_output##*$'\n'}
if [[ "$host_result" != '(0 t t t t t)' && "$host_result" != '(1 t t t t t)' ]]; then
  echo "standalone-bytecode-jit-boxed-car-return-smoke: host parity failed: $host_result" >&2
  exit 1
fi
echo "standalone-bytecode-jit-boxed-car-return-smoke: PASS (boxed cons/string identity, forced GC, VM fallback, original CAR signal)"
