;;; nelisp-cc-mirror-set-constant-or-insert.el --- Doc 119 §119.A mirror_set_constant_or_insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 119 §119.A — `mirror_set_constant_or_insert'.  Same structure
;; as `nelisp-cc-mirror-set-value-or-insert.el' (see that file for
;; the scratch-vec layout + algorithm) but the hit-path writes the
;; *constant* slot (= slot 3 of the symbol-entry record) instead of
;; the value slot (= slot 0).
;;
;; For this wrapper specifically: scratch slot 7 holds the
;; `unbound_marker' (= value cell default when auto-vivifying),
;; scratch slot 8 holds the `unbound_marker' (= function cell
;; default), slot 9 is `Sexp::Nil' (= no plist), slot 10 holds the
;; new constant flag (= `Sexp::T' for truthy / `Sexp::Nil' for
;; falsy).
;;
;; Mirrors `Env::mirror_set_constant' (~25 LOC) — the
;; `intern_constant' usage pattern (= mark a symbol constant before
;; its value cell is ever set).
;;
;; ABI deps + outer arity discipline: identical to
;; `set_value_or_insert' (see that file for the audit).

;;; Code:

(defconst nelisp-cc-mirror-set-constant-or-insert--source
  '(seq
    (defun nelisp_mirror_set_constant_or_insert_dispatch
        (entry-ptr mirror-ptr sym-ptr scratch-vec-ptr _pad _pad2)
      ;; Dispatch on ENTRY-PTR; hit-path writes slot 3 (= constant
      ;; flag).  See `set_value_or_insert_dispatch' for details.
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
        (and (record-slot-set entry-ptr 3
                              (vector-ref-ptr scratch-vec-ptr 10))
             1)))
    (defun nelisp_mirror_set_constant_or_insert
        (mirror-ptr sym-ptr scratch-vec-ptr _pad)
      ;; See `nelisp_mirror_set_value_or_insert' for ABI / scratch layout.
      ;; Hit-path writes slot 3 (= constant flag); slot 10 holds the
      ;; flag (Sexp::T / Sexp::Nil).
      (nelisp_mirror_set_constant_or_insert_dispatch
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       mirror-ptr sym-ptr scratch-vec-ptr 0 0)))
  "AOT source for Doc 119 §119.A `mirror_set_constant_or_insert'.

Slot-3 (constant flag) variant of `mirror_set_value_or_insert'.")

(provide 'nelisp-cc-mirror-set-constant-or-insert)

;;; nelisp-cc-mirror-set-constant-or-insert.el ends here
