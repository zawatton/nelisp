;;; nelisp-cc-mirror-lookup-value.el --- Doc 111 §111.E #2 mirror_lookup_value  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #2 — `mirror_lookup_value'.  Phase 47
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
;; this Phase 47 helper returns `Sexp::Nil' instead so the dispatcher
;; (= the eventual extern-wrapper in env_mirror.rs) re-introduces the
;; sentinel.  Doc 111 §3.E "Group A helpers all compose on #1, minimal
;; new logic" gate — we deliberately do not bake the unbound marker
;; into this object.

;;; Code:

(defconst nelisp-cc-mirror-lookup-value--source
  '(defun nelisp_mirror_lookup_value (mirror-ptr sym-ptr result-slot)
     ;; Compose §111.E #1 + §111.B `record-slot-ref' over slot 0.
     ;; The entry pointer returned by `nelisp_mirror_lookup_entry' is
     ;; a `*const Sexp' pointing at the bucket's `Sexp::Record'-shape
     ;; entry slot; `record-slot-ref' then dereferences `.payload' to
     ;; reach the inner `NlRecord' and refcount-aware-clones slot[0]
     ;; into `result-slot' (= `nl_sexp_clone_into' under the hood).
     (if (= (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr) 0)
         (sexp-write-nil result-slot)
       (record-slot-ref
        (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
        0
        result-slot)))
  "Phase 47 source for Doc 111 §111.E #2 `mirror_lookup_value'.

Composes §111.E #1 `mirror_lookup_entry' (via `extern-call' into
`nelisp_mirror_lookup_entry') with §111.B `record-slot-ref' to read
slot 0 (the symbol-entry value cell).  On miss the result slot is
written to `Sexp::Nil' via `sexp-write-nil' (= zeroed tag byte).

The double `extern-call' is the simplest expression of the compose-
on-#1 contract; the second call hits the same bucket and pays the
same FNV-1a hash + walk, but Phase 47 has no `let' binding for i64
intermediates yet, so we eat the duplicate call to keep this helper
under the §3.E Group A \"minimal new logic\" gate.  Doc 111 §111.E
optimisation pass (= post-Group-A) will fold the duplicate via the
Phase 47 grammar `let' that lands with §111.E Group B.")

(provide 'nelisp-cc-mirror-lookup-value)

;;; nelisp-cc-mirror-lookup-value.el ends here
