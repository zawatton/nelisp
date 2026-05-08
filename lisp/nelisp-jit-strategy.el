;;; nelisp-jit-strategy.el --- JIT lowering strategy wrappers (Doc 77b Stage b.4)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 77b Stage b.4 — `lowered_X' Rust strategy fns migrated to elisp
;; wrappers on top of the `nl-jit-call-*' bridge primitives shipped in
;; Stage b.2 / b.2.5.
;;
;; 24 wrappers cover the full pre-b.4 JIT registry surface:
;;
;;   12 arith    (nelisp--add2 / -sub2 / -mul2 / -num-{eq,lt,gt,le,ge}2 /
;;                -logior2 / -logand2 / -logxor2 / ash)
;;    5 cons     (car / cdr / cons / setcar / setcdr)
;;    4 access   (length / aref / aset / elt)
;;    1 predicate (eq)
;;    2 syscall  (nelisp--syscall / nelisp--syscall-supported-p)
;;
;; This file is loaded FIRST in the STDLIB image chain so it can
;; install fcell overrides BEFORE `nelisp-stdlib-eval-special.el' /
;; `nelisp-stdlib.el' run any of these names.  After installation, the
;; function cell for each wrapped name points at an elisp closure
;; instead of the Rust `(builtin NAME)' sentinel — `apply_builtin'
;; (which used to consult the JIT registry's `lowered_X') is no longer
;; reached for these names, so deleting `lowered_X' + their
;; `register(map)' insert calls is safe.
;;
;; Constraints on wrapper bodies:
;; - Only Tier 1 special forms (`if', `quote', `lambda', `let',
;;   `let*') are used — `cond' / `and' / `when' aren't defined yet at
;;   this point in the bootstrap.
;; - Pre-eq-wrapper code uses `nelisp--ref-eq' (= a Rust builtin
;;   exposed for cycle-safe `equal') for symbol equality;
;;   `nelisp--ref-eq' falls through to `sexp_eq' for `(Symbol, Symbol)'
;;   which compares by name — same semantics as `eq' for symbols.
;; - For arith Float fallback (= mixed Int/Float), wrappers call
;;   `nelisp--*-float' Rust primitives.  Calling host `+' here would
;;   recurse via `nelisp-stdlib.el' `+' → `nelisp--add2' → `+'.
;; - For length/aref/aset/elt the multi-variant fall-through (=
;;   MutStr / CharTable / BoolVector / Cons-walk + canonical
;;   ArithError / WrongType shapes) is delegated to a Rust helper
;;   (`nelisp--{length,aref,aset,elt}-impl').  These helpers carry the
;;   exact body of the pre-b.4 `lowered_X' fn — same JIT call + same
;;   match-on-variant fall-through.  The elisp wrapper exists so the
;;   public name's function cell becomes a closure instead of a
;;   `(builtin NAME)' sentinel; the heavy lifting stays Rust because
;;   variant dispatching with proper error shapes (ArithError vs
;;   WrongType, message formatting) is not expressible cleanly in
;;   pre-stdlib elisp.

;;; Code:

;; ---------- predicate (eq) ----------------------------------------
;;
;; eq is installed FIRST so subsequent wrappers can use it for
;; `(eq (type-of x) 'integer)' style checks.  The wrapper body uses
;; `nelisp--int-eq-zero' (= Rust primitive returning T/Nil) to convert
;; the bridge's i64 result (1 / 0) to the elisp boolean.

(fset 'eq
      (lambda (a b)
        (if (nelisp--int-eq-zero
             (nl-jit-call-ptr-ptr "nelisp_jit_eq_inline" a b))
            nil t)))

;; ---------- cons (car / cdr / cons / setcar / setcdr) -------------
;;
;; The bridge trampolines (`nl_jit_cons_*') handle the full Cons + Nil
;; contract (cars/cdrs of Nil = Nil; setcar/setcdr of non-Cons → ERR
;; → bridge raises generic WrongType).  No multi-variant fall-through
;; is needed — the bridge ERR shape matches the old `lowered_X'
;; canonical error 1:1 for these 5.

(fset 'cons
      (lambda (a b)
        (nl-jit-call-out-2 "nelisp_jit_cons" a b)))

(fset 'car
      (lambda (x)
        (nl-jit-call-out-1 "nelisp_jit_car" x)))

(fset 'cdr
      (lambda (x)
        (nl-jit-call-out-1 "nelisp_jit_cdr" x)))

(fset 'setcar
      (lambda (cell val)
        (nl-jit-call-out-2 "nelisp_jit_setcar" cell val)))

(fset 'setcdr
      (lambda (cell val)
        (nl-jit-call-out-2 "nelisp_jit_setcdr" cell val)))

;; ---------- access (length / aref / aset / elt) -------------------
;;
;; Each wrapper trampolines into the Rust `*-impl' helper which does
;; JIT call + multi-variant fall-through (= verbatim body of the
;; pre-b.4 `lowered_X' fn).  Reason for keeping the body in Rust: the
;; ERR branch needs ArithError vs WrongType discrimination + multi-
;; line format strings, not a clean fit for pre-stdlib elisp.

(fset 'length
      (lambda (x) (nelisp--length-impl x)))

(fset 'aref
      (lambda (arr idx) (nelisp--aref-impl arr idx)))

(fset 'aset
      (lambda (arr idx val) (nelisp--aset-impl arr idx val)))

(fset 'elt
      (lambda (seq idx) (nelisp--elt-impl seq idx)))

;; ---------- arith (12) --------------------------------------------
;;
;; Int+Int → JIT i64 fast path; Float involvement → Rust
;; `nelisp--*-float' primitive (which uses `num_pair' for f64
;; promotion + canonical wrong-type error).  Bitwise ops have no
;; float path — they route Float via `nelisp--*-int' (= as_int cast
;; + canonical WrongType for non-numeric).
;;
;; Cmp wrappers convert the JIT's i64 (1 / 0) result to T/Nil via
;; `nelisp--int-eq-zero'.

(fset 'nelisp--add2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (nl-jit-call-i64-i64 "nelisp_jit_add2" a b)
              (nelisp--add2-float a b))
          (nelisp--add2-float a b))))

(fset 'nelisp--sub2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (nl-jit-call-i64-i64 "nelisp_jit_sub2" a b)
              (nelisp--sub2-float a b))
          (nelisp--sub2-float a b))))

(fset 'nelisp--mul2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (nl-jit-call-i64-i64 "nelisp_jit_mul2" a b)
              (nelisp--mul2-float a b))
          (nelisp--mul2-float a b))))

(fset 'nelisp--num-eq2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (if (nelisp--int-eq-zero
                     (nl-jit-call-i64-i64 "nelisp_jit_eq2" a b))
                    nil t)
              (nelisp--num-eq2-float a b))
          (nelisp--num-eq2-float a b))))

(fset 'nelisp--num-lt2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (if (nelisp--int-eq-zero
                     (nl-jit-call-i64-i64 "nelisp_jit_lt2" a b))
                    nil t)
              (nelisp--num-lt2-float a b))
          (nelisp--num-lt2-float a b))))

(fset 'nelisp--num-gt2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (if (nelisp--int-eq-zero
                     (nl-jit-call-i64-i64 "nelisp_jit_gt2" a b))
                    nil t)
              (nelisp--num-gt2-float a b))
          (nelisp--num-gt2-float a b))))

(fset 'nelisp--num-le2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (if (nelisp--int-eq-zero
                     (nl-jit-call-i64-i64 "nelisp_jit_le2" a b))
                    nil t)
              (nelisp--num-le2-float a b))
          (nelisp--num-le2-float a b))))

(fset 'nelisp--num-ge2
      (lambda (a b)
        (if (nelisp--ref-eq (type-of a) 'integer)
            (if (nelisp--ref-eq (type-of b) 'integer)
                (if (nelisp--int-eq-zero
                     (nl-jit-call-i64-i64 "nelisp_jit_ge2" a b))
                    nil t)
              (nelisp--num-ge2-float a b))
          (nelisp--num-ge2-float a b))))

(fset 'nelisp--logior2
      (lambda (a b) (nelisp--logior2-impl a b)))

(fset 'nelisp--logand2
      (lambda (a b) (nelisp--logand2-impl a b)))

(fset 'nelisp--logxor2
      (lambda (a b) (nelisp--logxor2-impl a b)))

(fset 'ash
      (lambda (n count) (nelisp--ash-impl n count)))

;; ---------- syscall (2) -------------------------------------------
;;
;; The bridge primitive `nl-jit-call-syscall' takes (NAME NR A0 A1 A2
;; A3 A4 A5) — 8 args.  The elisp wrapper handles arity normalization
;; (= elisp `nelisp--syscall NR &rest ARGS') by padding short calls
;; with 0.  syscall_nr resolution (= symbol → libc::SYS_* int) stays
;; in Rust via `nelisp--syscall-nr-resolve' to keep the symbol map
;; there.

(fset 'nelisp--syscall-supported-p
      (lambda ()
        ;; nelisp_jit_syscall_supported_p is a 0-arg constant fn; route
        ;; through `nl-jit-call-i64-i64' with dummy args (the entry's
        ;; declared sig has 0 params; Cranelift's host C ABI ignores
        ;; the extra register-passed args for a 0-arg fn).
        (if (nelisp--int-eq-zero
             (nl-jit-call-i64-i64 "nelisp_jit_syscall_supported_p" 0 0))
            nil t)))

(fset 'nelisp--syscall
      (lambda (nr &rest args)
        (let* ((nr-int (nelisp--syscall-nr-resolve nr))
               (a0 (if args (car args) 0))
               (r1 (if args (cdr args) nil))
               (a1 (if r1 (car r1) 0))
               (r2 (if r1 (cdr r1) nil))
               (a2 (if r2 (car r2) 0))
               (r3 (if r2 (cdr r2) nil))
               (a3 (if r3 (car r3) 0))
               (r4 (if r3 (cdr r3) nil))
               (a4 (if r4 (car r4) 0))
               (r5 (if r4 (cdr r4) nil))
               (a5 (if r5 (car r5) 0)))
          (nl-jit-call-syscall "nelisp_jit_syscall" nr-int
                                a0 a1 a2 a3 a4 a5))))

;; (provide 'nelisp-jit-strategy) — omitted: this file is loaded
;; FIRST in the bootstrap chain, before `provide' is installed by
;; `nelisp-stdlib.el'.  Inclusion would error out at image-eval time.
;;; nelisp-jit-strategy.el ends here
