;;; nelisp-heap-types.el --- Phase 7.2/7.3 shared heap-type enums  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.2 (allocator) and Phase 7.3 (gc-inner) historically shipped
;; *two* family-tag enums:
;;
;;   - allocator (`nelisp-allocator--family-tag-alist'): bare symbols
;;     mapped to 1..5 (`#x01'..`#x05'), with `cons-pool' = 1.
;;   - gc-inner  (`nelisp-gc-inner--family-tag-keyword-alist'):
;;     keywords mapped to 0..4, with `:cons-pool' = 0.
;;
;; T51 codex audit (CRITICAL #2) flagged the mismatch:
;;   * allocator emits header.family-tag = 1 for cons-pool;
;;   * gc-inner reads tag = 1 as `:closure-pool' (the slot 1 of *its*
;;     keyword alist) and tag = 5 as unknown (falls back to cons-pool
;;     — silent corruption).
;;
;; This module is the *single source of truth* for the 5-family enum
;; (Doc 29 §2.5) shared between allocator + gc-inner.  Both modules
;; `(require 'nelisp-heap-types)' and consume the same defconst values
;; + bidirectional translation helpers.
;;
;; Wire layout:
;;   tag 0  →  cons-pool     /  :cons-pool
;;   tag 1  →  string-span   /  :string-span
;;   tag 2  →  vector-span   /  :vector-span
;;   tag 3  →  closure-pool  /  :closure-pool
;;   tag 4  →  large-object  /  :large-object
;;
;; The tag ordering is deliberately *gc-inner's* original 0..4 layout
;; (the simulator paths in §10/§12 already encode object headers with
;; `:cons-pool' at tag 0); only allocator-side `--family-tag-alist'
;; rotates from 1..5 to 0..4 and the slot-1 value moves from
;; `closure-pool' to `string-span' to match.  Order swapping at the
;; allocator side is safe because no Phase 7.2 tests pin specific
;; numeric values; they round-trip via `pack-header' / `header-family'.
;;
;; Doc 30 v2 §2.13 still allocates 8 bits to the family-tag field, so
;; this 5-element enum leaves 250 reserved values for future families.
;; Unknown tags MUST be treated as a hard error (Doc 29 §1.4 invariant
;; — silent fallback masks corruption).

;;; Code:

(require 'cl-lib)

;;; Numeric tag constants -------------------------------------------

(defconst nelisp-heap-family-tag-cons-pool    0
  "Family tag for cons-pool (16-byte cons cells, Doc 29 §2.5).")

(defconst nelisp-heap-family-tag-string-span  1
  "Family tag for string-span (size-classed string payloads, Doc 29 §2.5).")

(defconst nelisp-heap-family-tag-vector-span  2
  "Family tag for vector-span (size-classed vector payloads, Doc 29 §2.5).")

(defconst nelisp-heap-family-tag-closure-pool 3
  "Family tag for closure-pool (32-byte closure header, Doc 29 §2.5).")

(defconst nelisp-heap-family-tag-large-object 4
  "Family tag for large-object (>4 KiB dedicated mmap region, Doc 29 §2.5).")

(defconst nelisp-heap-family-tag-count 5
  "Number of distinct family tags currently defined (Doc 29 §2.5).
Bumped in lock-step with the constants above when a new family is
added.  Doc 30 v2 §2.13 reserves 8 bits (256 values) so the ceiling
is far away.")

;;; Bidirectional alists --------------------------------------------

(defconst nelisp-heap-family-tag-bare-alist
  `((cons-pool    . ,nelisp-heap-family-tag-cons-pool)
    (string-span  . ,nelisp-heap-family-tag-string-span)
    (vector-span  . ,nelisp-heap-family-tag-vector-span)
    (closure-pool . ,nelisp-heap-family-tag-closure-pool)
    (large-object . ,nelisp-heap-family-tag-large-object))
  "Bare symbol → numeric tag (allocator-side namespace).
Doc 29 §2.5 family enum.  Allocator pack-header consumers index this
to encode the 8-bit family field of the object header.")

(defconst nelisp-heap-family-tag-keyword-alist
  `((:cons-pool    . ,nelisp-heap-family-tag-cons-pool)
    (:string-span  . ,nelisp-heap-family-tag-string-span)
    (:vector-span  . ,nelisp-heap-family-tag-vector-span)
    (:closure-pool . ,nelisp-heap-family-tag-closure-pool)
    (:large-object . ,nelisp-heap-family-tag-large-object))
  "Keyword → numeric tag (gc-inner-side namespace).
Doc 29 §2.5 family enum, keyword spelling.  Symmetric with
`nelisp-heap-family-tag-bare-alist'; both sides MUST stay in lock-
step or the bridge breaks (T51 codex critical #2 root cause).")

;;; Errors -----------------------------------------------------------

(define-error 'nelisp-heap-unknown-family
  "Unknown heap-family tag or symbol (Doc 29 §2.5)")

;;; Translation helpers ---------------------------------------------

(defun nelisp-heap-family-tag-of-bare (sym)
  "Return the numeric family tag for bare symbol SYM.
Signals `nelisp-heap-unknown-family' rather than returning nil — silent
nil-fallback would corrupt the object header layout downstream."
  (let ((cell (assq sym nelisp-heap-family-tag-bare-alist)))
    (unless cell
      (signal 'nelisp-heap-unknown-family
              (list 'bare sym
                    'known (mapcar #'car nelisp-heap-family-tag-bare-alist))))
    (cdr cell)))

(defun nelisp-heap-family-tag-of-keyword (kw)
  "Return the numeric family tag for keyword KW.
Signals `nelisp-heap-unknown-family' on unknown KW."
  (let ((cell (assq kw nelisp-heap-family-tag-keyword-alist)))
    (unless cell
      (signal 'nelisp-heap-unknown-family
              (list 'keyword kw
                    'known (mapcar #'car
                                   nelisp-heap-family-tag-keyword-alist))))
    (cdr cell)))

(defun nelisp-heap-family-bare-of-tag (tag)
  "Return the bare symbol for numeric family TAG.
Signals `nelisp-heap-unknown-family' on out-of-range TAG.  The hard
fail is intentional — silent fallback is what T51 codex flagged as
the root cause of the schema mismatch."
  (let ((cell (rassq tag nelisp-heap-family-tag-bare-alist)))
    (unless cell
      (signal 'nelisp-heap-unknown-family
              (list 'tag tag
                    'range (cons 0
                                 (1- nelisp-heap-family-tag-count)))))
    (car cell)))

(defun nelisp-heap-family-keyword-of-tag (tag)
  "Return the keyword symbol for numeric family TAG.
Signals `nelisp-heap-unknown-family' on out-of-range TAG."
  (let ((cell (rassq tag nelisp-heap-family-tag-keyword-alist)))
    (unless cell
      (signal 'nelisp-heap-unknown-family
              (list 'tag tag
                    'range (cons 0
                                 (1- nelisp-heap-family-tag-count)))))
    (car cell)))

(defun nelisp-heap-family-bare-to-keyword (sym)
  "Translate bare SYM to its keyword form (no tag round-trip).
Equivalent to `(nelisp-heap-family-keyword-of-tag
                (nelisp-heap-family-tag-of-bare SYM))' but skips the
intermediate numeric tag — useful for callers that only need the
namespace conversion."
  (nelisp-heap-family-keyword-of-tag
   (nelisp-heap-family-tag-of-bare sym)))

(defun nelisp-heap-family-keyword-to-bare (kw)
  "Translate keyword KW to its bare form (no tag round-trip)."
  (nelisp-heap-family-bare-of-tag
   (nelisp-heap-family-tag-of-keyword kw)))

(provide 'nelisp-heap-types)
;;; nelisp-heap-types.el ends here
