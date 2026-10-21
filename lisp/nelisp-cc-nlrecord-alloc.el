;;; nelisp-cc-nlrecord-alloc.el --- nl_alloc_record AOT migration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 216 compact record allocation. One aligned body contains the packed
;; kind/length header at +0, an owned type WORD at +8, and n inline user
;; WORD slots at +16. The canonical record WORD is body|6; no separate Sexp,
;; Vec metadata, slot buffer, or refcount belongs to the record body.
;;
;; `nl_record_type_own' keeps canonical values unchanged, shares an owned
;; interned type symbol per name, and owns other borrowed legacy type views.

;;; Code:

(defconst nelisp-cc-nlrecord-alloc--source
  '(seq
    ;; Recursive Nil-fill loop for the slots buffer.  Doc 147 Phase 2:
    ;; writes the 8-byte Nil immediate WORD (= 3) into each slot at
    ;; data-ptr+i*8 (stride 8, was a 32B stride + 1-byte Nil tag).
    ;; Identical shape to `nl_alloc_vector_fill'; modulo variable names.
    (defun nl_alloc_record_fill (data-ptr i n)
      (if (< i n)
          (and (ptr-write-u64 (+ data-ptr (* i 8)) 0 3)
               (nl_alloc_record_fill data-ptr (+ i 1) n))
        data-ptr))

    ;; Public entry. Type normalization precedes body allocation, so a
    ;; returned record never points into a caller's temporary root view.
    (defun nl_alloc_record (type-tag-ptr slot-count)
      (let* ((n (if (< slot-count 0) 0 slot-count))
             (type-word (nl_record_type_own type-tag-ptr))
             (box (alloc-bytes (+ 16 (* n 8)) 8)))
        (and (ptr-write-u64 box 0 (+ 12 (* n 256)))
             (ptr-write-u64 box 8 type-word)
             (nl_alloc_record_fill (+ box 16) 0 n) box))))
  "AOT source for compact records: a 16-byte prefix and n inline WORDs.
`nl_alloc_record_fill' initializes user slots to nil; `nl_alloc_record'
owns the type WORD and returns the raw aligned body for legacy writers.")

(provide 'nelisp-cc-nlrecord-alloc)

;;; nelisp-cc-nlrecord-alloc.el ends here
