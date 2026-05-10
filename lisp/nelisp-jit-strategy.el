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

;; ---------- type-of ------------------------------------------------
;;
;; Doc 86 §86.1.a (2026-05-10) — `type-of' migrated from Rust to elisp
;; on top of the new `nl_jit_type_of' trampoline (= same Sexp variant
;; → tag-symbol mapping that lived in `bi_type_of', plus the Record
;; type_tag verbatim special case).  Installed FIRST so every
;; subsequent wrapper below (= `nelisp--int-eq-zero', `length' /
;; `aref' / `aset' / `elt' fall-through dispatch) sees the elisp
;; version.  The result is `eq'-comparable to existing tag symbols
;; (`integer' / `cons' / `string' / etc.) because the trampoline
;; constructs each via `Sexp::Symbol("integer".into())' — same
;; underlying interned representation as the literal quoted symbol on
;; the elisp side.

(fset 'type-of
      (lambda (x)
        (nl-jit-call-out-1 "nelisp_jit_type_of" x)))

;; ---------- predicate (eq) ----------------------------------------
;;
;; eq is installed FIRST so subsequent wrappers can use it for
;; `(eq (type-of x) 'integer)' style checks.  The wrapper body uses
;; `nelisp--int-eq-zero' (= elisp helper installed below per Phase
;; 7.1.7.a.1) to convert the bridge's i64 result (1 / 0) to the elisp
;; boolean.

;; Phase 7.1.7.a.1 (Doc 28 §3.7.a.1, 2026-05-10) — `nelisp--int-eq-zero'
;; ported from Rust `bi_int_eq_zero' (= strategy.rs).  Strict-integer
;; predicate: returns T for Int(0), Nil for any other Int, signals
;; `wrong-type-argument' for non-Int.  Installed FIRST so all wrappers
;; below (eq / arith cmp / syscall) can use it as a Rust-builtin
;; replacement.
;;
;; CRITICAL: the body uses `nelisp--ref-eq' (= Rc::ptr_eq Rust builtin,
;; never overridden) instead of `eq', because `eq' is fset to the
;; bridge wrapper just below which itself calls `nelisp--int-eq-zero'.
;; Using `eq' in this body would create infinite recursion.  Likewise
;; we hand-build the `wrong-type-argument' signal data via raw `cons'
;; (which IS overridden by the substrate file but to a Rust bridge
;; primitive that doesn't recurse through eq).
(fset 'nelisp--int-eq-zero
      (lambda (x)
        (if (nelisp--ref-eq (type-of x) 'integer)
            (if (nelisp--ref-eq x 0) t nil)
          (signal 'wrong-type-argument (cons 'integer (cons x nil))))))

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

;; ---------- box accessors (Doc 84 §84.3) --------------------------
;;
;; Six wrappers for the `length' / `aref' / `aset' fall-through arms
;; over `MutStr' / `BoolVector' / `CharTable' Sexp variants.  Each
;; calls the matching `nl_jit_*' trampoline shipped in
;; `build-tool/src/jit/box_accessor.rs' through the existing
;; `nl-jit-call-out-{1,1i,2i}' bridge primitives — no new ABI mode.
;; ERR from the bridge surfaces as a generic `error' which we
;; re-signal as `arith-error' (out-of-range) or `wrong-type-argument'
;; (wrong tag / invalid codepoint) per the pre-Doc-84 contract.
;;
;; Replaces the deleted Rust `bi_*' fns in `jit/strategy.rs' (= 6
;; entries removed from `eval::builtins::dispatch' in the same commit).

(fset 'nelisp--mut-str-len
      (lambda (s)
        (condition-case _err
            (nl-jit-call-out-1 "nl_jit_mut_str_len" s)
          (error (nelisp--signal-wrong-type 'mut-string s)))))

(fset 'nelisp--bool-vector-len
      (lambda (v)
        (condition-case _err
            (nl-jit-call-out-1 "nl_jit_bool_vector_len" v)
          (error (nelisp--signal-wrong-type 'bool-vector v)))))

;; `nelisp--str-codepoint-at': handles both Str + MutStr in the
;; trampoline.  ERR means out-of-range (elisp `aref' wrapper has
;; already gated `idx < 0' / non-string tag).
(fset 'nelisp--str-codepoint-at
      (lambda (s idx)
        (condition-case _err
            (nl-jit-call-out-1i "nl_jit_str_codepoint_at" s idx)
          (error
           (nelisp--signal-arith
            (cons "string index out of range" nil))))))

;; `nelisp--mut-str-set-codepoint': in-place codepoint mutation.
;; ERR collapses to either wrong tag (= immutable Str / non-string),
;; non-Int codepoint, invalid codepoint, or OOR idx.  Pre-Doc-84
;; `slim!' macro signalled `wrong-type-argument' for the immutable-Str
;; case and `arith-error' for OOR; we cannot reliably distinguish in
;; elisp (= no MutStr-specific predicate today) so we surface
;; `wrong-type-argument' uniformly — matches the Str-arm fast path
;; (= the dominant aset-on-string error in user code) and falls back
;; to `arith-error'-like behaviour only via the slow `condition-case'.
(fset 'nelisp--mut-str-set-codepoint
      (lambda (s idx val)
        (condition-case _err
            (nl-jit-call-out-2i "nl_jit_mut_str_set_codepoint" s idx val)
          (error (nelisp--signal-wrong-type 'mut-string s)))))

(fset 'nelisp--char-table-aref
      (lambda (table idx)
        (condition-case _err
            (nl-jit-call-out-1i "nl_jit_char_table_aref" table idx)
          (error (nelisp--signal-wrong-type 'char-table table)))))

(fset 'nelisp--char-table-aset
      (lambda (table idx val)
        (condition-case _err
            (nl-jit-call-out-2i "nl_jit_char_table_aset" table idx val)
          (error (nelisp--signal-wrong-type 'char-table table)))))

;; ---------- record family (Doc 86 §86.1.c) -------------------------
;;
;; Five wrappers replacing the deleted Rust `bi_record_*' helpers:
;;   `nelisp--record-type' / `-record-length' / `-record-ref' /
;;   `-record-set' / `-make-record'.
;;
;; Each calls the matching `nl_jit_record_*' trampoline shipped in
;; `build-tool/src/jit/box_accessor.rs' through the existing
;; `nl-jit-call-out-{1,1i,2i,2}' bridge primitive — Tier 1.5 per Doc 87
;; §1.2.3 (= no new ABI mode, same shape as Doc 84 §84.3 box accessor
;; precedents above).  Installed BEFORE `nelisp-stdlib*.el' so all
;; downstream consumers (= `cl-defstruct' macro, `nelisp-stdlib-hash.el'
;; / `-equal.el' / `-prn.el') see the elisp version.
;;
;; Error-signal contract preserved bit-for-bit:
;; - `make-record' non-symbol tag        → wrong-type-argument 'symbolp
;; - `record-ref' / `record-set' non-rec → wrong-type-argument 'recordp
;; - `record-ref' / `record-set' OOR     → arith-error "out-of-range..."
;; - `record-length' / `record-type' non-rec → wrong-type-argument 'recordp
;;
;; Disambiguation of OOR vs non-Record for `record-ref' / `record-set'
;; uses an inline `record-tag-check' via `nl_jit_record_type': success
;; means the input IS a record so the primary ERR must be OOR.

(fset 'nelisp--record-type
      (lambda (rec)
        (condition-case _err
            (nl-jit-call-out-1 "nl_jit_record_type" rec)
          (error (nelisp--signal-wrong-type 'recordp rec)))))

(fset 'nelisp--record-length
      (lambda (rec)
        (condition-case _err
            (nl-jit-call-out-1 "nl_jit_record_len" rec)
          (error (nelisp--signal-wrong-type 'recordp rec)))))

(fset 'nelisp--record-ref
      (lambda (rec idx)
        (condition-case _err
            (nl-jit-call-out-1i "nl_jit_record_ref" rec idx)
          (error
           ;; ERR = non-Record OR out-of-range.  Probe via inner
           ;; `condition-case' returning a boolean (= no nested signal
           ;; in the protected form so the outer handler does not
           ;; re-catch the dispatched signal below).
           (let ((is-rec (condition-case _err2
                             (progn (nl-jit-call-out-1
                                     "nl_jit_record_type" rec)
                                    t)
                           (error nil))))
             (if is-rec
                 (nelisp--signal-arith
                  (cons "out-of-range-args" nil))
               (nelisp--signal-wrong-type 'recordp rec)))))))

(fset 'nelisp--record-set
      (lambda (rec idx val)
        (condition-case _err
            (nl-jit-call-out-2i "nl_jit_record_set" rec idx val)
          (error
           (let ((is-rec (condition-case _err2
                             (progn (nl-jit-call-out-1
                                     "nl_jit_record_type" rec)
                                    t)
                           (error nil))))
             (if is-rec
                 (nelisp--signal-arith
                  (cons "out-of-range-args" nil))
               (nelisp--signal-wrong-type 'recordp rec)))))))

;; `nelisp--make-record' takes (TAG &rest SLOTS).  We build the slots
;; into a Cons list and call the 2-arg trampoline through
;; `nl-jit-call-out-2'.  ERR from the trampoline means non-symbol/non-nil
;; tag (= only failure mode for proper-list slots).
(fset 'nelisp--make-record
      (lambda (tag &rest slots)
        (condition-case _err
            (nl-jit-call-out-2 "nl_jit_record_alloc" tag slots)
          (error (nelisp--signal-wrong-type 'symbolp tag)))))

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

;; Doc 84 §84.1 (2026-05-10) — Float-family fall-through wrappers
;; ported from Rust `bi_{add,sub,mul}2_float' / `bi_num_{eq,lt,gt,le,ge}2_float'
;; (= 8 fns deleted from `build-tool/src/jit/strategy.rs').  Each
;; wrapper calls the new `nl-jit-call-float-{float,cmp}' bridge
;; primitives which resolve `nl_jit_float_*' xmm-register trampolines
;; via `unified_fn_ptr' and perform the f64 promotion + canonical
;; WrongType error via the existing `num_pair' helper.  Cmp wrappers
;; convert the bridge's i64 (1 / 0) result to T/Nil via
;; `nelisp--int-eq-zero'.

(fset 'nelisp--add2-float
      (lambda (a b)
        (nl-jit-call-float-float "nl_jit_float_add" a b)))

(fset 'nelisp--sub2-float
      (lambda (a b)
        (nl-jit-call-float-float "nl_jit_float_sub" a b)))

(fset 'nelisp--mul2-float
      (lambda (a b)
        (nl-jit-call-float-float "nl_jit_float_mul" a b)))

(fset 'nelisp--num-eq2-float
      (lambda (a b)
        (if (nelisp--int-eq-zero
             (nl-jit-call-float-cmp "nl_jit_float_eq_eps" a b))
            nil t)))

(fset 'nelisp--num-lt2-float
      (lambda (a b)
        (if (nelisp--int-eq-zero
             (nl-jit-call-float-cmp "nl_jit_float_lt" a b))
            nil t)))

(fset 'nelisp--num-gt2-float
      (lambda (a b)
        (if (nelisp--int-eq-zero
             (nl-jit-call-float-cmp "nl_jit_float_gt" a b))
            nil t)))

(fset 'nelisp--num-le2-float
      (lambda (a b)
        (if (nelisp--int-eq-zero
             (nl-jit-call-float-cmp "nl_jit_float_le" a b))
            nil t)))

(fset 'nelisp--num-ge2-float
      (lambda (a b)
        (if (nelisp--int-eq-zero
             (nl-jit-call-float-cmp "nl_jit_float_ge" a b))
            nil t)))

;; Phase 7.1.7.a.1 (Doc 28 §3.7.a.1, 2026-05-10) — bitwise + ash impls
;; ported from Rust `bi_logior2_impl' / `bi_logand2_impl' / `bi_logxor2_impl'
;; / `bi_ash_impl' (= strategy.rs).  Each wrapper calls the
;; `nl-jit-call-i64-i64' bridge directly which performs the `as_int' cast
;; (Int passthrough / Float→i64 truncation / WrongType for non-numeric)
;; before invoking the corresponding `nl_jit_arith_*' #[no_mangle]
;; trampoline and wrapping the i64 result as Sexp::Int.  Eliminates the
;; `nelisp--{logior2,logand2,logxor2,ash}-impl' Rust dispatch arms.

(fset 'nelisp--logior2
      (lambda (a b)
        (nl-jit-call-i64-i64 "nelisp_jit_logior2" a b)))

(fset 'nelisp--logand2
      (lambda (a b)
        (nl-jit-call-i64-i64 "nelisp_jit_logand2" a b)))

(fset 'nelisp--logxor2
      (lambda (a b)
        (nl-jit-call-i64-i64 "nelisp_jit_logxor2" a b)))

;; `ash' covers count ∈ [-62, +62] via the JIT trampoline; outside
;; that range the elisp wrapper applies the same explicit clamping
;; semantics as the deleted Rust `bi_ash_impl' (count >= 63 → 0;
;; count <= -63 → 0 if n >= 0 else -1).
;;
;; The argument-coercion idiom `(nl-jit-call-i64-i64 "nelisp_jit_add2"
;; 0 X)' coerces a Sexp::Int / Sexp::Float to i64 (= identical to the
;; deleted Rust `as_int' helper) and signals `wrong-type-argument' for
;; non-numeric, matching the canonical error shape bit-for-bit.
(fset 'ash
      (lambda (n count)
        (let ((ni (nl-jit-call-i64-i64 "nelisp_jit_add2" 0 n))
              (ci (nl-jit-call-i64-i64 "nelisp_jit_add2" 0 count)))
          (if (nelisp--int-eq-zero
               (nl-jit-call-i64-i64 "nelisp_jit_lt2" ci -62))
              ;; ci >= -62
              (if (nelisp--int-eq-zero
                   (nl-jit-call-i64-i64 "nelisp_jit_gt2" ci 62))
                  ;; -62 <= ci <= 62 → JIT fast path
                  (nl-jit-call-i64-i64 "nelisp_jit_ash" ni ci)
                ;; ci > 62 (= count >= 63) → 0
                0)
            ;; ci < -62 (= count <= -63) → if n < 0 then -1 else 0
            (if (nelisp--int-eq-zero
                 (nl-jit-call-i64-i64 "nelisp_jit_lt2" ni 0))
                0
              -1)))))

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
