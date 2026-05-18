;;; nelisp-cc-mirror-install-entry-or-insert.el --- Doc 119 §119.A mirror_install_entry_or_insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 119 §119.A — `mirror_install_entry_or_insert'.  Same structure
;; as `nelisp-cc-mirror-set-value-or-insert.el' (see that file for
;; the scratch-vec layout + algorithm) but the hit-path overwrites
;; all four symbol-entry slots (= value / function / plist / constant)
;; in a single 4-call sequence.  Used by `intern_constant'.
;;
;; For this wrapper: scratch slots 7/8/9/10 hold the four caller-
;; supplied entry slot Sexps (= value / function / plist / constant),
;; pre-resolved by the Rust safe wrapper (= `Option<Sexp>' resolved
;; to the actual value or `unbound_marker' / `Sexp::Nil' default).
;; All four slots are used regardless of hit / miss.
;;
;; ABI deps + outer arity discipline: identical to
;; `set_value_or_insert' (see that file for the audit).

;;; Code:

(defconst nelisp-cc-mirror-install-entry-or-insert--source
  '(seq
    (defun nelisp_mirror_install_entry_or_insert_dispatch
        (entry-ptr mirror-ptr sym-ptr scratch-vec-ptr _pad _pad2)
      ;; Dispatch on ENTRY-PTR; hit-path writes all 4 slots (= same as
      ;; existing helper #12 `mirror_install_entry').
      (if (= entry-ptr 0)
          (and
           (extern-call nelisp_mirror_alloc_entry
                        (vector-ref-ptr scratch-vec-ptr 5)
                        (vector-ref-ptr scratch-vec-ptr 7)
                        (vector-ref-ptr scratch-vec-ptr 8)
                        (vector-ref-ptr scratch-vec-ptr 9)
                        (vector-ref-ptr scratch-vec-ptr 10)
                        (vector-ref-ptr scratch-vec-ptr 6))
           (extern-call nelisp_mirror_bucket_prepend
                        mirror-ptr sym-ptr
                        (vector-ref-ptr scratch-vec-ptr 6)
                        scratch-vec-ptr))
        (and (record-slot-set entry-ptr 0
                              (vector-ref-ptr scratch-vec-ptr 7))
             (record-slot-set entry-ptr 1
                              (vector-ref-ptr scratch-vec-ptr 8))
             (record-slot-set entry-ptr 2
                              (vector-ref-ptr scratch-vec-ptr 9))
             (record-slot-set entry-ptr 3
                              (vector-ref-ptr scratch-vec-ptr 10)))))
    (defun nelisp_mirror_install_entry_or_insert
        (mirror-ptr sym-ptr scratch-vec-ptr _pad)
      ;; See `nelisp_mirror_set_value_or_insert' for ABI / scratch layout.
      ;; Hit-path writes all four slots; slot 7=value / 8=function /
      ;; 9=plist / 10=constant.
      (nelisp_mirror_install_entry_or_insert_dispatch
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       mirror-ptr sym-ptr scratch-vec-ptr 0 0)))
  "Phase 47 source for Doc 119 §119.A `mirror_install_entry_or_insert'.

Full 4-slot install variant of `mirror_set_value_or_insert'.")

(provide 'nelisp-cc-mirror-install-entry-or-insert)

;;; nelisp-cc-mirror-install-entry-or-insert.el ends here
