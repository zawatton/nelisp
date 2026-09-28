#!/bin/sh
set -eu

binary=${1:-target/nelisp}
export TMPDIR=${TMPDIR:-/tmp/nelisp-byte-eq-smoke}
mkdir -p "$TMPDIR"

vm_output=$("$binary" --eval '(progn (require (quote nelisp-bytecode-jit)) (let ((nelisp-bytecode-jit--dispatch-active t)) (let* ((fn (make-byte-code 514 (unibyte-string 1 1 61 135) [] 4)) (same-cons (cons (quote a) nil)) (other-cons (cons (quote a) nil)) (same-string (copy-sequence "text")) (other-string (copy-sequence "text")) (uninterned (make-symbol "foo"))) (list (funcall fn (quote foo) (quote foo)) (funcall fn (quote foo) (quote bar)) (funcall fn nil nil) (funcall fn 19 19) (funcall fn 19 20) (funcall fn same-cons same-cons) (funcall fn same-cons other-cons) (funcall fn same-string same-string) (funcall fn same-string other-string) (funcall fn uninterned (quote foo))))))')
vm_expected='(t nil t t nil t nil t nil nil)'
if [ "$vm_output" != "$vm_expected" ]; then
    printf 'VM parity mismatch: expected %s, got %s\n' "$vm_expected" "$vm_output" >&2
    exit 1
fi

jit_output=$("$binary" --eval '(progn (require (quote nelisp-bytecode-jit)) (setq nelisp-bytecode-jit-threshold 2) (let* ((fn (make-byte-code 514 (unibyte-string 1 1 61 135) [] 4)) (same-cons (cons (quote a) nil)) (other-cons (cons (quote a) nil)) (same-string (copy-sequence "text")) (other-string (copy-sequence "text")) (uninterned (make-symbol "foo")) (cold (funcall fn (quote foo) (quote foo))) (hot (funcall fn (quote foo) (quote foo))) (sym-diff (funcall fn (quote foo) (quote bar))) (nil-same (funcall fn nil nil)) (fix-same (funcall fn 19 19)) (fix-diff (funcall fn 19 20)) (gc (garbage-collect)) (gc-same (funcall fn (quote stable) (quote stable))) (cons-same (funcall fn same-cons same-cons)) (cons-diff (funcall fn same-cons other-cons)) (string-same (funcall fn same-string same-string)) (string-diff (funcall fn same-string other-string)) (unsupported (funcall fn uninterned (quote foo))) (status (nelisp-bytecode-jit-status))) (list cold hot sym-diff nil-same fix-same fix-diff gc-same cons-same cons-diff string-same string-diff unsupported (plist-get status :native-calls) (plist-get status :interpreter-fallbacks))))')
jit_expected='(t t nil t t nil t t nil t nil nil 10 1)'
if [ "$jit_output" != "$jit_expected" ]; then
    printf 'JIT parity/native/fallback mismatch: expected %s, got %s\n' "$jit_expected" "$jit_output" >&2
    exit 1
fi

printf 'standalone-bytecode-eq-jit-smoke: PASS (opcode 61; native identity for symbols, nil, fixnums, conses, strings; unsupported symbol fallback)\n'
