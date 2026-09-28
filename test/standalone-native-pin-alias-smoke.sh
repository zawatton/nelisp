#!/bin/sh
set -eu

binary=${1:-target/nelisp}

identity=$($binary --eval '(progn (require (quote nelisp-native-load)) (let* ((env (nelisp--native-env)) (marker (nelisp-native-load--pin-begin env)) (same (cons (quote a) nil)) (other (cons (quote a) nil)) (a (nelisp--native-pin-copy env marker same)) (b (nelisp--native-pin-copy env marker same)) (c (nelisp--native-pin-copy env marker other)) (before (list (nelisp--native-pin-eq-slots a b) (nelisp--native-pin-eq-slots a c) (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) (/= (ptr-read-u64 a 8) (ptr-read-u64 c 8)))) (gc (garbage-collect)) (after (list (nelisp--native-pin-eq-slots a b) (nelisp--native-pin-eq-slots a c) (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) (/= (ptr-read-u64 a 8) (ptr-read-u64 c 8))))) (unwind-protect (list before after) (nelisp-native-load--pin-end env marker))))')
identity_expected='((t nil t t) (t nil t t))'
if [ "$identity" != "$identity_expected" ]; then
    printf 'native slot identity mismatch: expected %s, got %s\n' "$identity_expected" "$identity" >&2
    exit 1
fi

vm=$("$binary" --eval '(let ((a (cons (quote a) nil)) (b (cons (quote a) nil))) (list (eq a a) (eq a b)))')
if [ "$vm" != '(t nil)' ]; then
    printf 'standalone VM eq mismatch: expected (t nil), got %s\n' "$vm" >&2
    exit 1
fi

reserve_failure=$("$binary" --eval '(progn (require (quote nelisp-native-load)) (let* ((env (nelisp--native-env)) (marker (nelisp-native-load--pin-begin env)) (failed-slot (nelisp--native-pin-copy env (+ marker 1) 17)) (released (condition-case nil (progn (nelisp-native-load--pin-end env marker) t) (error nil)))) (list failed-slot released)))')
if [ "$reserve_failure" != '(0 t)' ]; then
    printf 'invalid pin marker was not rejected and released: %s\n' "$reserve_failure" >&2
    exit 1
fi

cleanup=$($binary --eval '(progn (require (quote nelisp-native-load)) (let* ((env (nelisp--native-env)) (obj (cons (quote a) nil)) (handle (quote (:abi boxed :param-repr sexp-ptr :return-repr raw-i64 :arity 1 :name "forced-error" :codepage nil :body-entry nil :trampoline-bytes "x" :boundary-imm64-offsets nil :trampoline-entry-imm64-offset 0))) (failed (condition-case nil (progn (nelisp-native-load-call handle (list obj)) nil) (error t))) (marker (nelisp-native-load--pin-begin env)) (released (and (integerp marker) (> marker 0))) (done (when released (nelisp-native-load--pin-end env marker) (quote ended)))) (list failed released done)))')
cleanup_expected='(t t ended)'
if [ "$cleanup" != "$cleanup_expected" ]; then
    printf 'pinned-root cleanup mismatch: expected %s, got %s\n' "$cleanup_expected" "$cleanup" >&2
    exit 1
fi

host=$(emacs --batch -Q --eval '(let ((a (cons (quote a) nil)) (b (cons (quote a) nil))) (princ (list (eq a a) (eq a b))))')
host_expected='(t nil)'
if [ "$host" != "$host_expected" ]; then
    printf 'Host Emacs eq mismatch: expected %s, got %s\n' "$host_expected" "$host" >&2
    exit 1
fi

printf 'standalone-native-pin-alias-smoke: PASS (distinct rooted slots preserve cons identity across GC; loader error releases pin frame; Host=%s)\n' "$host"
