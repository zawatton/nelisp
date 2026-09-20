;;; nelisp-cc-mirror-is-constant.el --- Doc 111 §111.E #6 mirror_is_constant  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #6 — `mirror_is_constant'.  AOT
;; composition of #1 (`mirror_lookup_entry') + §111.B
;; `record-slot-ref-ptr' (slot 3) + §100 `sexp-tag' equality against
;; `SEXP_TAG_T' (= 1).  Replaces the `Env::mirror_is_constant' Rust
;; impl at `build-tool/src/eval/env_mirror.rs::mirror_is_constant'.
;;
;; Signature:
;;   (nelisp_mirror_is_constant MIRROR-PTR SYM-PTR) -> i64
;;     MIRROR-PTR : *const Sexp — env-mirror Record.
;;     SYM-PTR    : *const Sexp — Sexp::Symbol / Sexp::Str to look up.
;;   Returns: i64.  1 if the entry exists AND slot 3 holds `Sexp::T'
;;   (= `SEXP_TAG_T' / numeric value 1), else 0.  No unbound-marker
;;   parameter — constancy is a single tag-byte check, not a
;;   sentinel-Sexp comparison.

;;; Code:

(defconst nelisp-cc-mirror-is-constant--source
  '(seq
    ;; entry-ptr: result of nelisp_mirror_lookup_entry, pre-fetched as
    ;; register arg 0 (no alignment hazard).
    ;; Arity 4 (even): body-entry rsp ≡ 0 mod 16.
    ;; `record-slot-ref-ptr' and `sexp-tag' are inline ops — no CALL.
    ;; `SEXP_TAG_T' = 1 per `build-tool/src/eval/sexp.rs::SEXP_TAG_T'.
    ;; Guard: if entry-ptr = 0 (symbol not found), return 0 — no slot access.
    ;; `(= entry-ptr 0)' is safe: entry-ptr is a register arg, not an
    ;; extern-call result pushed mid-expression.
    ;;
    ;; perf/mirror-is-constant-raw: entry-ptr's slot 3 (the constant-flag
    ;; cell) is initialised to `Sexp::Nil' and only ever set to
    ;; `Sexp::T' by `intern_constant'/`defconst' (see the Commentary
    ;; above) -- it NEVER holds a pointer WORD.  So every call that
    ;; FINDS an entry (any global/dynamic `setq' on an already-bound
    ;; symbol, constant or not -- the overwhelming common case is "not")
    ;; unconditionally paid `nl_record_slot_ptr''s `(alloc-bytes 32 8)'
    ;; + `nl_val_load' to materialise a Nil (or T) box it discards after
    ;; one `sexp-tag' comparison.  Same fix as A1's
    ;; `nelisp_frame_scope_boundary_p' (lisp/nelisp-cc-frame-stack-
    ;; find.el, commit ab4a72484): read the slot's raw 8-byte tagged
    ;; WORD directly (`nelisp_mirror_is_constant_slot_word' below,
    ;; identical addressing to `nl_record_slot_ptr',
    ;; lisp/nelisp-cc-nlrecord-slot-ptr.el) and compare it to the WORD
    ;; itself instead of the materialised tag.  `T's immediate encoding
    ;; is the literal 7 (`SEXP_TAG_T' = 1, low bit 1 = immediate,
    ;; `nl_val_load''s own table, lisp/nelisp-cc-val-load.el); Nil's is
    ;; 3.  Since this slot provably holds only one of those two WORDs,
    ;; `word == 7' decides the exact same predicate as
    ;; `(sexp-tag (materialised)) == 1' for every value it can hold.
    (defun nelisp_mirror_is_constant_slot_word (entry-ptr)
      (ptr-read-u64
       (+ (ptr-read-u64 (ptr-read-u64 entry-ptr 8) 32) 24)
       0))
    (defun nelisp_mirror_is_constant_check (entry-ptr _mirror-ptr _sym-ptr _pad)
      (if (= entry-ptr 0)
          0
        (if (= (nelisp_mirror_is_constant_slot_word entry-ptr) 7) 1 0)))

    ;; Public entry: nelisp_mirror_is_constant(mirror-ptr, sym-ptr) → i64
    ;; Arity 2 (even): body-entry rsp ≡ 0.
    ;; extern-call `nelisp_mirror_lookup_entry' at position 0 → rsp ≡ 0 ✓.
    (defun nelisp_mirror_is_constant (mirror-ptr sym-ptr)
      (nelisp_mirror_is_constant_check
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       mirror-ptr sym-ptr 0)))
  "AOT source for Doc 111 §111.E #6 `mirror_is_constant'.

Three defuns (seq form).  Alignment-safe CPS structure.

Reads symbol-entry slot 3 (= the constant-flag cell, initialised to
`Sexp::Nil' and set to `Sexp::T' by `intern_constant' / `defconst') as
a raw tagged WORD via `nelisp_mirror_is_constant_slot_word' (perf/
mirror-is-constant-raw, see above) instead of materialising it, and
compares the WORD directly to 7 (`Sexp::T's immediate encoding).
Returns 1 iff the slot holds that WORD, matching the Rust impl's
`matches!(slot, Some(Sexp::T))' classification for every value this
slot can hold (Nil or T only).

Alignment fix: the original single-defun version had
`(= (extern-call nelisp_mirror_lookup_entry ...) 0)' in arity-2
which caused rsp ≡ 8 at the extern-call → SIGSEGV.  The check
helper receives entry-ptr at arg position 0 → safe.")

(provide 'nelisp-cc-mirror-is-constant)

;;; nelisp-cc-mirror-is-constant.el ends here
