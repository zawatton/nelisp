;;; nelisp-jit-strategy.el --- JIT lowering strategy wrappers (Doc 77b Stage b.4 + Doc 80)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 77b Stage b.4 — `lowered_X' Rust strategy fns migrated to elisp
;; wrappers on top of the `nl-jit-call-*' bridge primitives shipped in
;; Stage b.2 / b.2.5.
;;
;; Doc 80 Stage 80.3〜80.5 (2026-05-09) — `length' / `aref' / `aset' /
;; `elt' fall-through dispatch lifted from Rust (`bi_length_impl' /
;; `aref_helper' / `bi_aref_impl' / `bi_aset_impl' / `bi_elt_impl', =
;; ~255 LOC) into elisp on top of the `lisp/nelisp-jit-substrate.el'
;; expression substrate (`cond' / `signal' / signal-helpers).  The
;; remaining narrow per-variant operations (= MutStr char count /
;; codepoint mutation / BoolVector length / CharTable get/set) live as
;; slim Rust primitives in `jit/strategy.rs'.
;;
;; 24 wrappers cover the full pre-b.4 JIT registry surface:
;;
;;   12 arith    (nelisp--add2 / -sub2 / -mul2 / -num-{eq,lt,gt,le,ge}2 /
;;                -logior2 / -logand2 / -logxor2 / ash)
;;    5 cons     (car / cdr / cons / setcar / setcdr)  ← pre-installed
;;                  by Doc 80 substrate; this file's re-`fset' is a
;;                  redundant idempotent restoration left in place for
;;                  self-documentation.
;;    4 access   (length / aref / aset / elt)
;;    1 predicate (eq)
;;    2 syscall  (nelisp--syscall / nelisp--syscall-supported-p)
;;
;; Constraints on wrapper bodies (Doc 80-relaxed):
;; - Only Tier 1 special forms (`if', `quote', `lambda', `let',
;;   `let*', `progn', `while', `setq', `condition-case') and Rust
;;   builtins are used PLUS the macros / helpers installed by
;;   `lisp/nelisp-jit-substrate.el' (= `cond' / `when' / `unless' /
;;   `null' / `not' / `nelisp--signal-{wrong-type,arith}').
;; - For arith Float fallback (= mixed Int/Float), wrappers call
;;   `nelisp--*-float' Rust primitives.  Calling host `+' here would
;;   recurse via `nelisp-stdlib.el' `+' → `nelisp--add2' → `+'.
;; - For `length' / `aref' / `aset' / `elt' the variant dispatch is
;;   pure elisp (= `cond' on `type-of') with slim Rust primitives for
;;   MutStr / BoolVector / CharTable Sexp-internal access.

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

;; ---------- access (length / aref / aset / elt) — Doc 80 elisp dispatch ----
;;
;; Pure-elisp variant dispatch on top of the JIT trampoline (= JIT
;; covers Nil / Vector / Str / BoolVector-in-range / Cons-elt-in-range
;; fast paths) + slim Rust primitives for MutStr / BoolVector /
;; CharTable Sexp-internal access.  See Doc 80 §3 for the bootstrap
;; expressibility audit.
;;
;; Each wrapper:
;;   1. Calls the `nl-jit-call-out-N' bridge to invoke the JIT entry.
;;      On `TRAMPOLINE_OK' the bridge returns the result.
;;   2. On `TRAMPOLINE_ERR' the bridge raises a generic `WrongType'
;;      error.  The wrapper catches it via `condition-case' and
;;      dispatches by type-tag to the appropriate slim primitive or
;;      raises the canonical `arith-error' / `wrong-type-argument'
;;      via `nelisp--signal-{arith,wrong-type}'.

(fset 'length
      (lambda (x)
        (condition-case _err
            (nl-jit-call-out-1 "nelisp_jit_length" x)
          (error
           (let ((tag (type-of x)))
             (cond
              ;; MutStr arm (= JIT length only handles immutable Str).
              ((eq tag 'string) (nelisp--mut-str-len x))
              ;; BoolVector arm.
              ((eq tag 'bool-vector) (nelisp--bool-vector-len x))
              ;; Cons-walk via `cdr' chain — Tier-1 `while' loop.
              ((eq tag 'cons)
               (let ((n 0) (p x))
                 (while (eq (type-of p) 'cons)
                   (setq n (nelisp--add2 n 1))
                   (setq p (cdr p)))
                 (if (eq p nil)
                     n
                   (nelisp--signal-wrong-type 'sequencep x))))
              (t (nelisp--signal-wrong-type 'sequencep x)))))))) 

(fset 'aref
      (lambda (arr idx)
        ;; Negative index check before dispatch (= matches pre-Doc-80
        ;; `aref_helper' arith-error shape).  `nelisp--num-lt2' returns
        ;; t/nil — the substrate `if' tests Elisp truthiness directly,
        ;; no `nelisp--int-eq-zero' bridge needed.
        (if (nelisp--ref-eq (type-of idx) 'integer)
            (if (nelisp--num-lt2 idx 0)
                (nelisp--signal-arith (cons "negative index" nil))
              ;; idx >= 0 → JIT call + fall-through dispatch.
              (condition-case _err
                  (nl-jit-call-out-1i "nelisp_jit_aref" arr idx)
                (error
                 (let ((tag (type-of arr)))
                   (cond
                    ;; Str / MutStr — char-indexed codepoint via
                    ;; slim primitive (handles both variants).
                    ((eq tag 'string)
                     (nelisp--str-codepoint-at arr idx))
                    ;; CharTable — slim primitive.
                    ((eq tag 'char-table)
                     (nelisp--char-table-aref arr idx))
                    ;; Vector / BoolVector hit ERR only on out-of-
                    ;; range; the `bool-vector' arm needs a
                    ;; dedicated arith-error (the JIT trampoline
                    ;; doesn't know to disambiguate).
                    ((eq tag 'bool-vector)
                     (nelisp--signal-arith
                      (cons "bool-vector index out of range" nil)))
                    ((eq tag 'vector)
                     (nelisp--signal-arith
                      (cons "vector index out of range" nil)))
                    (t (nelisp--signal-wrong-type 'arrayp arr)))))))
          (nelisp--signal-wrong-type 'integerp idx))))

(fset 'aset
      (lambda (arr idx val)
        (if (nelisp--ref-eq (type-of idx) 'integer)
            (if (nelisp--num-lt2 idx 0)
                (nelisp--signal-arith (cons "negative index" nil))
              (condition-case _err
                  (nl-jit-call-out-2i "nelisp_jit_aset" arr idx val)
                (error
                 (let ((tag (type-of arr)))
                   (cond
                    ;; MutStr — codepoint mutation via slim primitive.
                    ;; Note: type-of also returns 'string for Str
                    ;; (immutable); the slim primitive raises
                    ;; wrong-type-argument internally for Str so the
                    ;; canonical "mutable-array" error shape surfaces.
                    ((eq tag 'string)
                     (nelisp--mut-str-set-codepoint arr idx val))
                    ;; CharTable — slim primitive.
                    ((eq tag 'char-table)
                     (nelisp--char-table-aset arr idx val))
                    ;; Vector / BoolVector → only OOR arrives here.
                    ((eq tag 'vector)
                     (nelisp--signal-arith
                      (cons "vector index out of range" nil)))
                    ((eq tag 'bool-vector)
                     (nelisp--signal-arith
                      (cons "bool-vector index out of range" nil)))
                    (t (nelisp--signal-wrong-type 'arrayp arr)))))))
          (nelisp--signal-wrong-type 'integerp idx))))

(fset 'elt
      (lambda (seq idx)
        (if (nelisp--ref-eq (type-of idx) 'integer)
            (if (nelisp--num-lt2 idx 0)
                (nelisp--signal-arith (cons "negative index" nil))
              (condition-case _err
                  (nl-jit-call-out-1i "nelisp_jit_elt" seq idx)
                (error
                 (let ((tag (type-of seq)))
                   (cond
                    ;; Empty list / cons-OOR → arith-error.
                    ((eq tag 'symbol)
                     (if (eq seq nil)
                         (nelisp--signal-arith
                          (cons "elt: empty sequence" nil))
                       (nelisp--signal-wrong-type 'sequencep seq)))
                    ((eq tag 'cons)
                     (nelisp--signal-arith
                      (cons "elt: index out of range for list" nil)))
                    ;; Strings / char-tables / bool-vectors / vectors
                    ;; all delegate to `aref' (= same fall-through
                    ;; arms; the elisp `aref' wrapper handles them).
                    ((eq tag 'string) (aref seq idx))
                    ((eq tag 'char-table) (aref seq idx))
                    ((eq tag 'bool-vector) (aref seq idx))
                    ((eq tag 'vector) (aref seq idx))
                    (t (nelisp--signal-wrong-type 'sequencep seq)))))))
          (nelisp--signal-wrong-type 'integerp idx))))

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
