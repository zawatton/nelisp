;;; nelisp-cc-jit-record.el --- Doc 120 §120.B record family swap sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.B — AOT-compiled replacements for 5 of 11
;; `jit/box_accessor.rs' trampolines, covering the record family:
;;
;;   - `nl_jit_record_type'  → `nelisp_jit_record_type'
;;   - `nl_jit_record_len'   → `nelisp_jit_record_len'
;;   - `nl_jit_record_ref'   → `nelisp_jit_record_ref'
;;   - `nl_jit_record_set'   → `nelisp_jit_record_set'
;;   - `nl_jit_record_alloc' → `nl_jit_record_alloc'
;;
;; ABI shapes (matching `nl-jit-call-out-{1,1i,2i}' bridge primitives):
;;
;;   record_type/record_len: `(*const Sexp, *mut Sexp) -> i64'
;;   record_ref:             `(*const Sexp, i64, *mut Sexp) -> i64'
;;   record_set:             `(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64'
;;
;; Return value: TRAMPOLINE_OK=0 / TRAMPOLINE_ERR=1.
;;
;; AOT grammar pieces used:
;;   `(sexp-tag PTR)'          — read tag byte at offset 0 (= 12 for Record).
;;   `(record-type-tag P S)'   — copy record.type_tag Sexp into slot.
;;   `(record-slot-count P)'   — read record.slots.len() into rax.
;;   `(record-slot-ref P I S)' — refcount-aware copy of slots[i] into slot.
;;   `(record-slot-set P I V)' — refcount-safe overwrite slots[i] = *V.
;;   `(sexp-int-make S V)'     — write Sexp::Int(V) into slot S.
;;   `(extern-call NAME ...)'  — call into Rust extern for slot val copy.
;;
;; The tag-byte constant `12' is `nelisp-sexp--tag-record' from
;; `lisp/nelisp-sexp-layout.el', pinned by the §100.B ABI assert tests.
;;
;; `nl_jit_record_alloc' now uses that 2-pass walk explicitly:
;; first count a proper cons list, then `record-make' with that slot
;; count, then fill each slot via `record-slot-set'.  Improper lists
;; still return TRAMPOLINE_ERR.

;;; Code:

(defconst nelisp-cc-jit-record-type--source
  '(defun nelisp_jit_record_type (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK (= Record input, type_tag written to out),
     ;; 1 on ERR (= non-Record input).
     ;;
     ;; Refcount discipline: `record-type-tag' grammar op does a raw
     ;; SIMD 32-byte copy (= NOT refcount-aware), which causes a
     ;; double-free when the inline `type_tag' is a box-tagged variant
     ;; (Symbol / Cons / etc.).  Instead, route through the
     ;; `nl_record_type_tag_ptr' extern (= returns `*const Sexp' for
     ;; the type_tag field) composed with `nl_sexp_clone_into' (=
     ;; refcount-aware copy into out), mirroring the discipline
     ;; `record-slot-ref' uses internally.
     (if (= (sexp-tag arg) 12)
         (and
          (extern-call
           nl_sexp_clone_into
           (extern-call nl_record_type_tag_ptr arg)
           out)
          0)
       1))
  "AOT source for the §120.B `nl_jit_record_type' swap.

Tag-checks `arg' against `nelisp-sexp--tag-record' (= 12) inline;
on match obtains `*const Sexp' for the inline type_tag via the
`nl_record_type_tag_ptr' Rust extern, then copies into `*out' via
`nl_sexp_clone_into' (= refcount-aware, same helper `record-slot-
ref' uses).  Returns 0 (= TRAMPOLINE_OK) on Record input, 1 (=
TRAMPOLINE_ERR) on any other variant.

NOTE: the existing `record-type-tag' grammar op does a raw 32-
byte SIMD copy without refcount adjustment which is unsafe for
box-tagged type tags; the explicit `extern-call' pair above
threads through `nl_sexp_clone_into' for refcount-safe semantics.
The `record-type-tag' grammar form remains useful for ABI assert
tests where the type_tag is statically known to be a primitive
variant (Symbol / Nil only).  The `(and ... 0)' idiom threads
the side effect through to a stable 0 return per §120.A
`nl_jit_ref_eq' convention.")

(defconst nelisp-cc-jit-record-len--source
  '(defun nelisp_jit_record_len (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK (= Record input, Int(slots.len) → out),
     ;; 1 on ERR (= non-Record).
     (if (= (sexp-tag arg) 12)
         (and (sexp-int-make out (record-slot-count arg)) 0)
       1))
  "AOT source for the §120.B `nl_jit_record_len' swap.

Tag-checks `arg' against `nelisp-sexp--tag-record' (= 12) inline;
on match reads `arg.slots.len()' (= i64 at NlRecord offset 48) via
`record-slot-count' and writes `Sexp::Int(len)' into `*out' via
`sexp-int-make'.  Returns 0 (= TRAMPOLINE_OK) on Record input, 1
(= TRAMPOLINE_ERR) on any other variant.")

(defconst nelisp-cc-jit-record-ref--source
  '(defun nelisp_jit_record_ref (arg idx out)
     ;; arg: *const Sexp.  idx: i64.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK (= Record + idx in [0, slots.len)),
     ;; 1 on ERR (= non-Record OR idx < 0 OR idx >= slots.len).
     (if (= (sexp-tag arg) 12)
         (if (< idx 0)
             1
           (if (< idx (record-slot-count arg))
               (and (record-slot-ref arg idx out) 0)
             1))
       1))
  "AOT source for the §120.B `nl_jit_record_ref' swap.

Three-arm guard: (1) tag check vs `nelisp-sexp--tag-record', (2)
`idx >= 0' lower bound, (3) `idx < slots.len' upper bound.  On all
three passing, copies `arg.slots[idx]' into `*out' via the
refcount-aware `record-slot-ref' grammar op (= delegates to
`nl_sexp_clone_into' under the hood).  Returns 0 / 1 per the
TRAMPOLINE_{OK,ERR} contract; the elisp wrapper distinguishes
non-Record from OOR by re-probing with `nl_jit_record_type'.")

(defconst nelisp-cc-jit-record-set--source
  '(defun nelisp_jit_record_set (arg idx val out)
     ;; arg: *const Sexp.  idx: i64.  val: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK (= Record + idx in [0, slots.len),
     ;; slots[idx] overwritten with clone(val), *out = clone(val)),
     ;; 1 on ERR (= non-Record OR OOR).
     (if (= (sexp-tag arg) 12)
         (if (< idx 0)
             1
           (if (< idx (record-slot-count arg))
               (and
                (record-slot-set arg idx val)
                (extern-call nl_sexp_clone_into val out)
                0)
             1))
       1))
  "AOT source for the §120.B `nl_jit_record_set' swap.

Three-arm guard identical to `nl_jit_record_ref'.  On OK arm:
(1) `record-slot-set' refcount-safely overwrites `slots[idx]' with
`clone(*val)' via the `nl_record_set_slot' Rust extern; (2)
`nl_sexp_clone_into' writes `clone(*val)' into `*out' via
`extern-call' (= same helper `record-slot-ref' uses internally).
Matches the Rust trampoline's `*out = (*val).clone()' contract
bit-for-bit.  Returns 0 / 1 per TRAMPOLINE_{OK,ERR}.")

(defconst nelisp-cc-jit-record-alloc--source
  '(seq
    (defun nl_jit_record_alloc_count (cur-box n)
      (if (= (sexp-tag (+ cur-box 32)) 0)
          (+ n 1)
        (if (= (sexp-tag (+ cur-box 32)) 7)
            (nl_jit_record_alloc_count
             (sexp-payload-ptr (+ cur-box 32))
             (+ n 1))
          -1)))
    (defun nl_jit_record_alloc_fill (cur-box idx out)
      (and
       (record-slot-set out idx cur-box)
       (if (= (sexp-tag (+ cur-box 32)) 0)
           1
         (nl_jit_record_alloc_fill
          (sexp-payload-ptr (+ cur-box 32))
          (+ idx 1)
          out))))
    (defun nl_jit_record_alloc (tag list out)
      ;; tag: *const Sexp.  list: *const Sexp.  out: *mut Sexp.
      ;; Returns: i64 = 0 on OK, 1 on invalid tag / improper list.
      (if (= (sexp-tag tag) 0)
          (if (= (sexp-tag list) 0)
              (and (record-make tag 0 out) 0)
            (if (= (sexp-tag list) 7)
                (if (< (nl_jit_record_alloc_count (sexp-payload-ptr list) 0) 0)
                    1
                  (and
                   (record-make tag
                                (nl_jit_record_alloc_count (sexp-payload-ptr list) 0)
                                out)
                   (nl_jit_record_alloc_fill (sexp-payload-ptr list) 0 out)
                   0))
              1))
        (if (= (sexp-tag tag) 4)
            (if (= (sexp-tag list) 0)
                (and (record-make tag 0 out) 0)
              (if (= (sexp-tag list) 7)
                  (if (< (nl_jit_record_alloc_count (sexp-payload-ptr list) 0) 0)
                      1
                    (and
                     (record-make tag
                                  (nl_jit_record_alloc_count (sexp-payload-ptr list) 0)
                                  out)
                     (nl_jit_record_alloc_fill (sexp-payload-ptr list) 0 out)
                     0))
                1))
          1))))
  "AOT source for the `nl_jit_record_alloc' trampoline.

Uses a 2-pass proper-list walk: count first, allocate the record
with `record-make', then fill slots through refcount-safe
`record-slot-set'.  A non-Symbol/non-Nil tag or any improper list
tail still returns TRAMPOLINE_ERR, matching the removed Rust body.")

(provide 'nelisp-cc-jit-record)

;;; nelisp-cc-jit-record.el ends here
