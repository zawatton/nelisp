;;; nelisp-cc-jit-type-of.el --- Doc 120 §120.A.2 type-of trampoline swap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.A.2 — Phase-47-compiled replacement for the Rust
;; `nl_jit_type_of' trampoline in `build-tool/src/jit/predicate.rs'.
;; Same `(*const Sexp, *mut Sexp) -> i64' contract:
;;
;;   1. Cell-unwrap loop via `sexp-payload-ptr' shortcut:
;;      `NlCell.value' lives at offset 0 of the NlCell heap block, so
;;      `sexp-payload-ptr(cell_ptr)' IS a valid `*const Sexp' to the
;;      inner value without any explicit copy.  Recurse until non-Cell.
;;   2. Record special case: if type_tag is a Symbol, clone it into *out
;;      via `nl_record_type_tag_ptr' + `nl_sexp_clone_into'.  If the
;;      type_tag is not Symbol, write the literal symbol `record'.
;;   3. All other variants: allocate a byte buffer, write the ASCII bytes
;;      of the type name, call `sexp-write-symbol', then dealloc.
;;
;; Tag constants (= `SEXP_TAG_*' from `build-tool/src/eval/sexp.rs'):
;;   0=Nil, 1=T, 2=Int, 3=Float, 4=Symbol, 5=Str, 6=MutStr, 7=Cons,
;;   8=Vector, 9=CharTable, 10=BoolVector, 11=Cell, 12=Record.
;;
;; Phase 47 grammar pieces used:
;;   `(sexp-tag PTR)'            — read tag byte at offset 0 via movzx.
;;   `(sexp-payload-ptr PTR)'    — read box pointer at offset 8 (= NlCellRef
;;                                 payload; NlCell.value is at box+0).
;;   `(extern-call NAME ...)'    — call into Rust externs for type_tag
;;                                 pointer + refcount-aware clone.
;;   `(alloc-bytes SIZE ALIGN)'  — heap allocate SIZE bytes, align ALIGN.
;;   `(ptr-write-u8 PTR OFF V)'  — write one byte at PTR+OFF.
;;   `(sexp-write-symbol S P L)' — allocate Sexp::Symbol from P[0..L].
;;   `(dealloc-bytes PTR S A)'   — free heap buffer.
;;
;; Refcount note: cloning the record type_tag uses `nl_sexp_clone_into'
;; (same helper `nelisp_jit_record_type' uses) — refcount-safe.  The
;; `sexp-payload-ptr' Cell shortcut does NOT clone the inner Sexp; it is
;; safe only because we only read the tag of the unwrapped value and then
;; write a freshly-allocated type-name symbol to *out — we never retain a
;; naked pointer to the inner value past the trampoline return.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; `build-tool/build.rs::link_elisp_cc_spike' source list.  The emitted
;; `nl_jit_type_of' STT_FUNC symbol is resolved by
;; `build-tool/src/jit/bridge.rs::alias' under the
;; `"nelisp_jit_type_of"' arm.

;;; Code:

;; ---------------------------------------------------------------------------
;; Per-variant symbol-write helpers.
;;
;; Each helper:
;;   1. Allocates a raw byte buffer of the exact name length.
;;   2. Fills each byte with the ASCII code of the name character via
;;      `ptr-write-u8'.  All offsets and character codes are compile-time
;;      integer constants; only `out' and `buf' are runtime values.
;;   3. Calls `sexp-write-symbol' to allocate a fresh Sexp::Symbol and
;;      write it into *out.
;;   4. Deallocates the raw buffer.
;;   5. Returns 0 (= TRAMPOLINE_OK).
;;
;; The 2-level split (nl_jit_to_write_VARIANT + nl_jit_to_fill_VARIANT)
;; is required because Phase 47's `let' form only accepts compile-time
;; integer constants as values; the `alloc-bytes' return value is runtime
;; so it must be threaded as a function parameter.
;; ---------------------------------------------------------------------------

(defconst nelisp-cc-jit-type-of--source
  '(seq
    ;; "cons" (4 bytes: 99 111 110 115)
    (defun nl_jit_to_fill_cons (out buf)
      (and (ptr-write-u8 buf 0 99)
           (ptr-write-u8 buf 1 111)
           (ptr-write-u8 buf 2 110)
           (ptr-write-u8 buf 3 115)
           (sexp-write-symbol out buf 4)
           (dealloc-bytes buf 4 1)
           0))
    (defun nl_jit_to_write_cons (out)
      (nl_jit_to_fill_cons out (alloc-bytes 4 1)))

    ;; "symbol" (6 bytes: 115 121 109 98 111 108)
    (defun nl_jit_to_fill_symbol (out buf)
      (and (ptr-write-u8 buf 0 115)
           (ptr-write-u8 buf 1 121)
           (ptr-write-u8 buf 2 109)
           (ptr-write-u8 buf 3 98)
           (ptr-write-u8 buf 4 111)
           (ptr-write-u8 buf 5 108)
           (sexp-write-symbol out buf 6)
           (dealloc-bytes buf 6 1)
           0))
    (defun nl_jit_to_write_symbol (out)
      (nl_jit_to_fill_symbol out (alloc-bytes 6 1)))

    ;; "integer" (7 bytes: 105 110 116 101 103 101 114)
    ;; Two-pass fill: fill6 writes bytes 0-5, then nl_jit_to_fill_integer
    ;; writes byte 6 and completes.
    (defun nl_jit_to_fill6_integer (buf)
      (and (ptr-write-u8 buf 0 105)
           (ptr-write-u8 buf 1 110)
           (ptr-write-u8 buf 2 116)
           (ptr-write-u8 buf 3 101)
           (ptr-write-u8 buf 4 103)
           (ptr-write-u8 buf 5 101)))
    (defun nl_jit_to_fill_integer (out buf)
      (and (nl_jit_to_fill6_integer buf)
           (ptr-write-u8 buf 6 114)
           (sexp-write-symbol out buf 7)
           (dealloc-bytes buf 7 1)
           0))
    (defun nl_jit_to_write_integer (out)
      (nl_jit_to_fill_integer out (alloc-bytes 7 1)))

    ;; "float" (5 bytes: 102 108 111 97 116)
    (defun nl_jit_to_fill_float (out buf)
      (and (ptr-write-u8 buf 0 102)
           (ptr-write-u8 buf 1 108)
           (ptr-write-u8 buf 2 111)
           (ptr-write-u8 buf 3 97)
           (ptr-write-u8 buf 4 116)
           (sexp-write-symbol out buf 5)
           (dealloc-bytes buf 5 1)
           0))
    (defun nl_jit_to_write_float (out)
      (nl_jit_to_fill_float out (alloc-bytes 5 1)))

    ;; "string" (6 bytes: 115 116 114 105 110 103)
    (defun nl_jit_to_fill_string (out buf)
      (and (ptr-write-u8 buf 0 115)
           (ptr-write-u8 buf 1 116)
           (ptr-write-u8 buf 2 114)
           (ptr-write-u8 buf 3 105)
           (ptr-write-u8 buf 4 110)
           (ptr-write-u8 buf 5 103)
           (sexp-write-symbol out buf 6)
           (dealloc-bytes buf 6 1)
           0))
    (defun nl_jit_to_write_string (out)
      (nl_jit_to_fill_string out (alloc-bytes 6 1)))

    ;; "vector" (6 bytes: 118 101 99 116 111 114)
    (defun nl_jit_to_fill_vector (out buf)
      (and (ptr-write-u8 buf 0 118)
           (ptr-write-u8 buf 1 101)
           (ptr-write-u8 buf 2 99)
           (ptr-write-u8 buf 3 116)
           (ptr-write-u8 buf 4 111)
           (ptr-write-u8 buf 5 114)
           (sexp-write-symbol out buf 6)
           (dealloc-bytes buf 6 1)
           0))
    (defun nl_jit_to_write_vector (out)
      (nl_jit_to_fill_vector out (alloc-bytes 6 1)))

    ;; "char-table" (10 bytes: 99 104 97 114 45 116 97 98 108 101)
    ;; Two helpers: fill6 writes bytes 0-5, fill_char_table writes 6-9.
    (defun nl_jit_to_fill6_char_table (buf)
      (and (ptr-write-u8 buf 0 99)
           (ptr-write-u8 buf 1 104)
           (ptr-write-u8 buf 2 97)
           (ptr-write-u8 buf 3 114)
           (ptr-write-u8 buf 4 45)
           (ptr-write-u8 buf 5 116)))
    (defun nl_jit_to_fill_char_table (out buf)
      (and (nl_jit_to_fill6_char_table buf)
           (ptr-write-u8 buf 6 97)
           (ptr-write-u8 buf 7 98)
           (ptr-write-u8 buf 8 108)
           (ptr-write-u8 buf 9 101)
           (sexp-write-symbol out buf 10)
           (dealloc-bytes buf 10 1)
           0))
    (defun nl_jit_to_write_char_table (out)
      (nl_jit_to_fill_char_table out (alloc-bytes 10 1)))

    ;; "bool-vector" (11 bytes: 98 111 111 108 45 118 101 99 116 111 114)
    ;; Three helpers: fill6 writes 0-5, fill_bool_vector writes 6-10.
    (defun nl_jit_to_fill6_bool_vec (buf)
      (and (ptr-write-u8 buf 0 98)
           (ptr-write-u8 buf 1 111)
           (ptr-write-u8 buf 2 111)
           (ptr-write-u8 buf 3 108)
           (ptr-write-u8 buf 4 45)
           (ptr-write-u8 buf 5 118)))
    (defun nl_jit_to_fill_bool_vec (out buf)
      (and (nl_jit_to_fill6_bool_vec buf)
           (ptr-write-u8 buf 6 101)
           (ptr-write-u8 buf 7 99)
           (ptr-write-u8 buf 8 116)
           (ptr-write-u8 buf 9 111)
           (ptr-write-u8 buf 10 114)
           (sexp-write-symbol out buf 11)
           (dealloc-bytes buf 11 1)
           0))
    (defun nl_jit_to_write_bool_vector (out)
      (nl_jit_to_fill_bool_vec out (alloc-bytes 11 1)))

    ;; "record" (6 bytes: 114 101 99 111 114 100)
    (defun nl_jit_to_fill_record (out buf)
      (and (ptr-write-u8 buf 0 114)
           (ptr-write-u8 buf 1 101)
           (ptr-write-u8 buf 2 99)
           (ptr-write-u8 buf 3 111)
           (ptr-write-u8 buf 4 114)
           (ptr-write-u8 buf 5 100)
           (sexp-write-symbol out buf 6)
           (dealloc-bytes buf 6 1)
           0))
    (defun nl_jit_to_write_record (out)
      (nl_jit_to_fill_record out (alloc-bytes 6 1)))

    ;; ---------------------------------------------------------------------------
    ;; Core dispatch helpers.
    ;; ---------------------------------------------------------------------------

    ;; Dispatch tag → write the appropriate type-name symbol to *out.
    ;; Called only after Cell unwrapping and Record special-casing are done.
    (defun nl_jit_type_of_tag (tag out)
      ;; tag: i64 (= sexp-tag result), out: *mut Sexp.
      ;; Returns: 0 on OK.
      (cond
       ;; Cons (7)
       ((= tag 7) (nl_jit_to_write_cons out))
       ;; Nil (0), T (1), Symbol (4) → "symbol"
       ((= tag 0) (nl_jit_to_write_symbol out))
       ((= tag 1) (nl_jit_to_write_symbol out))
       ((= tag 4) (nl_jit_to_write_symbol out))
       ;; Int (2) → "integer"
       ((= tag 2) (nl_jit_to_write_integer out))
       ;; Float (3) → "float"
       ((= tag 3) (nl_jit_to_write_float out))
       ;; Str (5), MutStr (6) → "string"
       ((= tag 5) (nl_jit_to_write_string out))
       ((= tag 6) (nl_jit_to_write_string out))
       ;; Vector (8) → "vector"
       ((= tag 8) (nl_jit_to_write_vector out))
       ;; CharTable (9) → "char-table"
       ((= tag 9) (nl_jit_to_write_char_table out))
       ;; BoolVector (10) → "bool-vector"
       ((= tag 10) (nl_jit_to_write_bool_vector out))
       ;; Fallback (should not be reached for valid Sexp)
       (t 1)))

    ;; Main Cell-unwrap + dispatch loop.
    ;; Uses `sexp-payload-ptr' shortcut: NlCell.value is at offset 0
    ;; of the NlCell heap block, so payload-ptr of a Cell Sexp IS a
    ;; valid *const Sexp for the inner value without copying.
    (defun nl_jit_type_of_inner (arg out)
      ;; arg: *const Sexp.  out: *mut Sexp.
      ;; Returns: 0 on OK.
      (if (= (sexp-tag arg) 11)  ; Cell (11)
          ;; Unwrap: inner value is at NlCell+0 = sexp-payload-ptr(arg)
          (nl_jit_type_of_inner (sexp-payload-ptr arg) out)
        ;; Non-Cell: check for Record special case
        (if (= (sexp-tag arg) 12)  ; Record (12)
            ;; Record: use type_tag if it's a Symbol, else write "record"
            (if (= (sexp-tag (extern-call nl_record_type_tag_ptr arg)) 4)  ; Symbol
                (and
                 (extern-call
                  nl_sexp_clone_into
                  (extern-call nl_record_type_tag_ptr arg)
                  out)
                 0)
              ;; type_tag is not Symbol → write literal "record"
              (nl_jit_to_write_record out))
          ;; Non-Cell, Non-Record: dispatch on tag
          (nl_jit_type_of_tag (sexp-tag arg) out))))

    ;; Public entry point — matches `nl_jit_type_of' Rust trampoline ABI.
    (defun nl_jit_type_of (arg out)
      ;; arg: *const Sexp.  out: *mut Sexp.
      ;; Returns: i64 = 0 always (no error path for valid Sexp input).
      (nl_jit_type_of_inner arg out)))
  "Phase 47 source for the Doc 120 §120.A.2 `nl_jit_type_of' swap.

Implements the `type-of' variant dispatch (Cell unwrap + Record type_tag
verbatim + tag-to-symbol mapping) using `sexp-payload-ptr' for zero-copy
Cell traversal and per-variant alloc+write+dealloc sequences for constant
type-name symbols.

Tag constants: Nil=0, T=1, Int=2, Float=3, Symbol=4, Str=5, MutStr=6,
Cons=7, Vector=8, CharTable=9, BoolVector=10, Cell=11, Record=12.

Emits `nl_jit_type_of' STT_FUNC symbol resolved by
`build-tool/src/jit/bridge.rs::alias' under `\"nelisp_jit_type_of\"'.")

(provide 'nelisp-cc-jit-type-of)

;;; nelisp-cc-jit-type-of.el ends here
