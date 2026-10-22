;;; nelisp-cc-env-lookup-function.el --- Wave a-2: Env::lookup_function AOT .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::lookup_function' body migrated to AOT elisp .o.
;; Replaces the 7-LOC Rust body in
;; `build-tool/src/eval/env_helpers.rs::Env::lookup_function'.
;;
;; Algorithm (= literal transcription of the Rust body):
;;
;;   1. Check mirror entry existence via `nelisp_mirror_lookup_entry'.
;;      If miss (= 0), return 1 (= unbound-fn sentinel).
;;
;;   2. If hit: compare slot 1 with UNBOUND-PTR.  A symbol created by a
;;      variable-only definition has a mirror entry whose function slot is
;;      this sentinel; treat it as a miss instead of exposing it as a
;;      callable function.
;;
;;   3. Otherwise fill out-ptr with the function Sexp (refcount-aware copy
;;      via record-slot-ref, which delegates to `nl_sexp_clone_into' since
;;      Doc 111 §111.C v3).  Return 0 (= found).
;;
;; Signature:
;;   (nelisp_env_lookup_function MIRROR-PTR UNBOUND-PTR NAME-PTR OUT-PTR)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     UNBOUND-PTR : *const Sexp — Env::unbound_marker.
;;     NAME-PTR    : *const Sexp — Sexp::Symbol name to look up.
;;     OUT-PTR     : *mut Sexp   — 32-byte caller-owned result slot.
;;   Returns: i64.  0 = found (function written to *out-ptr),
;;                  1 = unbound (out-ptr unchanged).
;;
;; ABI:
;;   4 args (even) — body-entry rsp ≡ 0 mod 16 ✓.
;;   Each defun has at most one extern-call in any execution path.
;;
;; ABI deps:
;;   nelisp_mirror_lookup_entry    — hit/miss check (0 = miss)
;;   nelisp_mirror_lookup_function — fills out-ptr (refcount-safe)

;;; Code:

(defconst nelisp-cc-env-lookup-function--source
  '(seq
    ;; Doc 215 word ABI: the function-cell WORD (the slot's own child), 2
    ;; when the entry is missing or holds the unbound marker.
    ;; The unbound test reads the slot's own WORD, as GNU tests the symbol's
    ;; function cell in place (lisp.h SYMBOL_FUNCTION): `record-slot-ref-ptr'
    ;; materialised a view of it on every call dispatch.
    ;; (Inline: a helper function would grow the F1 prelink closure.)
    (defun nelisp_env_lookup_function_w (mirror-ptr unbound-ptr name-ptr _pad)
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
        (if (= entry 0)
            2
          ;; The marker is an uninterned symbol (tag 16) compared by identity, not
          ;; by name (gnu-semantics 3b1706d44); the slot WORD is tested in place.
          (let ((w (nl_record_slot_word entry 1)))
            (if (if (= (logand w 1) 1) 0
                  (if (= (nl_val_tag w) 16)
                      (if (= (heap-sexp-payload w) (heap-sexp-payload unbound-ptr)) 1 0)
                    0))
                2 w)))))
    ;; The (rc, OUT) form has its own body (rc 1 on a miss): it is on every
    ;; call dispatch.  It stores the slot WORD into OUT directly
    ;; (nl_abi_word_to_rc: an immediate or a 32-byte copy, 0), without the
    ;; nl_abi_lookup_to_rc layer and without the old refcount-bumping clone
    ;; of `record-slot-ref'.
    (defun nelisp_env_lookup_function (mirror-ptr unbound-ptr name-ptr out-ptr)
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
        (if (= entry 0)
            1
          (let ((w (nl_record_slot_word entry 1)))
            (if (if (= (logand w 1) 1) 0
                  (if (= (nl_val_tag w) 16)
                      (if (= (heap-sexp-payload w) (heap-sexp-payload unbound-ptr)) 1 0)
                    0))
                1
              (nl_abi_word_to_rc w out-ptr)))))))
  "AOT source for Wave a-2 `Env::lookup_function' body.

R11a (Doc 49 Wave 9): collapsed two-defun CPS to a single defun
using `let-rt' CSE hoist of the entry pointer + `record-slot-ref'
direct slot-1 read.  Previous shape paid 2 FNV-1a hashes per call
(`lookup_entry' + `lookup_function'); hoisted shape pays 1.

The `unbound-ptr' parameter prevents a variable-only symbol's slot-1
`nelisp--unbound-marker' from escaping as a callable definition.  The miss
sentinel is 1 (= absent entry or unbound function slot), found is 0 (=
out-ptr filled with function Sexp via the refcount-safe
`nl_sexp_clone_into' invoked by `record-slot-ref').")

(provide 'nelisp-cc-env-lookup-function)

;;; nelisp-cc-env-lookup-function.el ends here
