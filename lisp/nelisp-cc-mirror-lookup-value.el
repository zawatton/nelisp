;;; nelisp-cc-mirror-lookup-value.el --- Doc 111 §111.E #2 mirror_lookup_value  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #2 — `mirror_lookup_value'.  AOT
;; composition of #1 (`mirror_lookup_entry') + `record-slot-ref' over
;; slot 0 (= the symbol-entry's value cell).  Replaces the
;; `Env::mirror_lookup_value' Rust impl at
;; `build-tool/src/eval/env_mirror.rs::mirror_lookup_value'.
;;
;; Signature:
;;   (nelisp_mirror_lookup_value MIRROR-PTR SYM-PTR RESULT-SLOT)
;;     MIRROR-PTR  : *const Sexp — env-mirror Record (= globals_record).
;;     SYM-PTR     : *const Sexp — Sexp::Symbol / Sexp::Str to look up.
;;     RESULT-SLOT : *mut Sexp   — 32-byte caller-owned slot, will be
;;                                  filled with the value Sexp (= a
;;                                  refcount-aware clone of slot 0) on
;;                                  hit, or `Sexp::Nil' on miss.
;;
;; Sentinel-return semantics: the Rust impl returns
;; `self.unbound_marker.clone()` on miss (= a `Sexp::Symbol("nelisp--unbound-marker")');
;; this AOT helper returns `Sexp::Nil' instead so the dispatcher
;; (= the eventual extern-wrapper in env_mirror.rs) re-introduces the
;; sentinel.  Doc 111 §3.E "Group A helpers all compose on #1, minimal
;; new logic" gate — we deliberately do not bake the unbound marker
;; into this object.

;;; Code:

(defconst nelisp-cc-mirror-lookup-value--source
  '(seq
    ;; Doc 215 word ABI: the slot-0 value WORD of SYM's mirror entry, 2
    ;; when there is no entry.  The adapter keeps writing nil on a miss.
    (defun nelisp_mirror_lookup_value_w (mirror-ptr sym-ptr _pad)
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)))
        (if (= entry 0) 2 (nl_ent_w entry 0))))
    (defun nelisp_mirror_lookup_value_done (w result-slot)
      (nl_abi_word_to_rc (if (= w 2) 3 w) result-slot))
    (defun nelisp_mirror_lookup_value (mirror-ptr sym-ptr result-slot)
      (nelisp_mirror_lookup_value_done
       (nelisp_mirror_lookup_value_w mirror-ptr sym-ptr 0) result-slot)))
  "AOT source for Doc 111 §111.E #2 `mirror_lookup_value'.

Composes §111.E #1 `mirror_lookup_entry' (via `extern-call' into
`nelisp_mirror_lookup_entry') with §111.B `record-slot-ref' to read
slot 0 (the symbol-entry value cell).  On miss the result slot is
written to `Sexp::Nil' via `sexp-write-nil' (= zeroed tag byte).

R11a (Doc 49 Wave 9): `let-rt' CSE hoist of the entry pointer.  The
duplicate extern-call into `nelisp_mirror_lookup_entry' (the
previous compose-on-#1 shape) is replaced by a single call whose
result lives in a frame slot; the `if' test and the hit-branch
`record-slot-ref' both consume the same slot.  Cuts 2 FNV-1a hashes
+ 2 bucket walks down to 1 per call; semantics bit-for-bit
identical (= the second hash always observed the same memory and
returned the same pointer).")

(provide 'nelisp-cc-mirror-lookup-value)

;;; nelisp-cc-mirror-lookup-value.el ends here
