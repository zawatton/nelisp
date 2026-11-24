;;; nelisp-runtime-prims-decl.el --- Compile-time declarations of runtime primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NeLisp contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Host Emacs does not define the primitives of the standalone NeLisp
;; runtime (`ptr-*', `syscall-direct', `alloc-bytes', `nl-jit-call-*' and
;; the `nelisp--*' natives).  Files under lisp/ call them behind the
;; runtime's own dispatch, so `batch-byte-compile' on host Emacs reports
;; "the function `X' is not known to be defined" for every call site.
;;
;; A file that calls such a primitive adds
;;
;;   (eval-when-compile (require 'nelisp-runtime-prims-decl nil t))
;;
;; near its top.  The table below is then replayed into the byte
;; compiler's function environment, exactly what a `declare-function'
;; form does while compiling the file, so the calls are known (and, where
;; the arglist is listed, arity-checked).  `declare-function' itself cannot
;; be used from a required file: it only takes effect when the compiler
;; sees the form in the file being compiled.
;;
;; No runtime effect.  The replay runs only while `byte-compile-current-file'
;; is non-nil, nothing is defined or bound, and the NOERROR argument keeps a
;; runtime that has no lisp/ on its load path (the standalone image) loading
;; the requiring file unchanged.
;;
;; Arglist provenance (an entry with no arglist is declared without
;; arity checking because no authoritative arity is available):
;;   - `ptr-call' (7), `nelisp--native-pin-copy-v2' (4) and
;;     `nelisp--native-unbox-reference' (3): `nelisp--builtin-fixed-arities'
;;     in scripts/nelisp-stdlib-prelude.el.
;;   - `ptr-read-uN' / `ptr-write-uN' / `ptr-read-bytes' / `alloc-bytes' /
;;     `syscall-direct': the reader builtin documentation in
;;     docs/design/138-windows-x86_64-standalone-port.org,
;;     docs/design/122-phase47-grammar-extensions.org and
;;     docs/design/199-true-parallelism-frontier.org.
;;   - `nelisp--ADD2' style binary wrappers and `nelisp--syscall-nr-resolve',
;;     `nelisp--char-table-aref', `nelisp--mut-str-len': their defuns in the
;;     design documents / prelude.
;;   - `nl-jit-call-*' and the remaining natives: the call sites compiled in
;;     lisp/ agree on one argument count (checked by the compiler itself).

;;; Code:

(eval-when-compile
  (defvar byte-compile-function-environment)
  (defvar byte-compile-current-file)
  (when (bound-and-true-p byte-compile-current-file)
    (dolist (d '(;; Raw memory and system call primitives.
                 (ptr-read-u8 (base offset))
                 (ptr-read-u16 (base offset))
                 (ptr-read-u32 (base offset))
                 (ptr-read-u64 (base offset))
                 (ptr-write-u8 (base offset value))
                 (ptr-write-u16 (base offset value))
                 (ptr-write-u32 (base offset value))
                 (ptr-write-u64 (base offset value))
                 (ptr-read-bytes (addr count))
                 (ptr-write-bytes (addr string))
                 (ptr-call (fn a1 a2 a3 a4 a5 a6))
                 (alloc-bytes (size align))
                 (syscall-direct (nr a1 a2 a3 a4 a5 a6))
                 ;; JIT bridge primitives.
                 (nl-jit-call-out-1 (symbol x))
                 (nl-jit-call-out-2 (symbol x y))
                 (nl-jit-call-out-1i (symbol x y))
                 (nl-jit-call-out-2i (symbol x y z))
                 (nl-jit-call-i64-i64 (symbol a b))
                 (nl-jit-call-float-float (symbol a b))
                 (nl-jit-call-float-unary (symbol x))
                 (nl-jit-call-float-cmp)
                 (nl-jit-call-ptr-ptr (symbol a b))
                 (nl-jit-call-syscall)
                 (nl-jit-call-format-float (symbol a b c))
                 ;; Arithmetic wrappers.
                 (nelisp--add2 (a b))
                 (nelisp--sub2 (a b))
                 (nelisp--mul2 (a b))
                 (nelisp--num-eq2 (a b))
                 (nelisp--num-lt2 (a b))
                 (nelisp--num-gt2 (a b))
                 (nelisp--num-le2 (a b))
                 (nelisp--num-ge2 (a b))
                 (nelisp--logand2 (a b))
                 (nelisp--logior2 (a b))
                 (nelisp--logxor2 (a b))
                 (nelisp--add2-float (a b))
                 (nelisp--sub2-float (a b))
                 (nelisp--mul2-float (a b))
                 (nelisp--num-eq2-float (a b))
                 (nelisp--num-lt2-float (a b))
                 (nelisp--num-gt2-float (a b))
                 (nelisp--num-le2-float (a b))
                 (nelisp--num-ge2-float (a b))
                 ;; Native object, environment and target queries.
                 (nelisp--native-unbox-reference (a b c))
                 (nelisp--native-pin-copy-v2 (a b c d))
                 (nelisp--native-env ())
                 (nelisp--native-symbol-addr (symbol))
                 (nelisp--target-arch-code ())
                 (nelisp--target-os-code ())
                 (nelisp--arena-stats ())
                 (nl-unix-time-usec ())
                 (nelisp--syscall-nr-resolve (name))
                 (nelisp--char-table-aref (table idx))
                 (nelisp--mut-str-len (string))
                 (nelisp--write-stderr-line (line))
                 (nelisp--write-stdout-bytes)
                 (nelisp--env-globals-op)
                 (nelisp--debug-switch)
                 (nelisp--syscall-stat (path))
                 (nelisp--syscall-readdir (path))
                 (nelisp--intern-lookup (name))
                 (nelisp--ref-eq (a b))
                 (nelisp--int-eq-zero (x))
                 (nelisp--length-cons-cc (x))
                 (nelisp--bool-vector-len (x))
                 (nelisp--str-codepoint-at (string idx))
                 (nelisp--mut-str-set-codepoint (string idx codepoint))
                 (nelisp--recordp-cc (x))
                 (nelisp--apply-lambda-inner (captured formals body args))
                 (nelisp--apply-builtin-dispatch)
                 (nelisp--casify-fast)
                 (nelisp--format-float-body)
                 (nelisp--concat-ints)
                 (nelisp--make-mut-string)
                 (nelisp--rx-exec)
                 (nelisp--string-search-chars)
                 (nelisp--f64-trunc)
                 (nelisp--native-subr-create)
                 (nelisp--symbol-global-cell-p)
                 (nl-make-directory)))
      (push (cons (car d) (if (cdr d) (list 'declared (cadr d)) t))
            byte-compile-function-environment))))

(provide 'nelisp-runtime-prims-decl)

;;; nelisp-runtime-prims-decl.el ends here
