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
  '(defun nelisp_mirror_lookup_value (mirror-ptr sym-ptr result-slot)
     ;; Compose §111.E #1 + §111.B `record-slot-ref' over slot 0.
     ;; The entry pointer returned by `nelisp_mirror_lookup_entry' is
     ;; a `*const Sexp' pointing at the bucket's `Sexp::Record'-shape
     ;; entry slot; `record-slot-ref' then dereferences `.payload' to
     ;; reach the inner `NlRecord' and refcount-aware-clones slot[0]
     ;; into `result-slot' (= `nl_sexp_clone_into' under the hood).
     ;;
     ;; CSE-hoisted: bind the entry pointer once via `let' (= `let-rt'
     ;; runtime slot) and reuse in both the miss test and the
     ;; record-slot-ref hit path.  Cuts 2 hashes → 1 hash per call.
     (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)))
       (if (= entry 0)
           (sexp-write-nil result-slot)
         (record-slot-ref entry 0 result-slot))))
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
