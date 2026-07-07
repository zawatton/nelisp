;;; nelisp-cc-mirror-set-special-or-insert.el --- Doc 164 §2.1 mirror_set_special_or_insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 164 §2.1 — `mirror_set_special_or_insert'.  Clone of
;; `nelisp-cc-mirror-set-constant-or-insert.el' that stores the
;; standalone reader's "declared special" sentinel in symbol-entry
;; slot 2 (= plist) without changing the 4-slot mirror layout.
;;
;; Scratch layout matches the sibling helper except slot 9 carries the
;; plist payload (`Sexp::T') and slot 10 stays `Sexp::Nil' (= constant
;; flag unchanged).

;;; Code:

(defconst nelisp-cc-mirror-set-special-or-insert--source
  '(seq
    (defun nelisp_mirror_set_special_or_insert_dispatch
        (entry-ptr mirror-ptr sym-ptr scratch-vec-ptr _pad _pad2)
      (if (= entry-ptr 0)
          (let ((entry-slot (alloc-bytes 32 8)))
            (and
             (extern-call nelisp_mirror_alloc_entry
                          (vector-ref-ptr scratch-vec-ptr 5)
                          (vector-ref-ptr scratch-vec-ptr 7)
                          (vector-ref-ptr scratch-vec-ptr 8)
                          (vector-ref-ptr scratch-vec-ptr 9)
                          (vector-ref-ptr scratch-vec-ptr 10)
                          entry-slot)
             (extern-call nelisp_mirror_bucket_prepend
                          mirror-ptr sym-ptr
                          entry-slot
                          scratch-vec-ptr)))
        (and (record-slot-set entry-ptr 2
                              (vector-ref-ptr scratch-vec-ptr 9))
             1)))
    (defun nelisp_mirror_set_special_or_insert
        (mirror-ptr sym-ptr scratch-vec-ptr _pad)
      (nelisp_mirror_set_special_or_insert_dispatch
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       mirror-ptr sym-ptr scratch-vec-ptr 0 0))
    (defun nl_mirror_is_special_p (mirror-ptr sym-ptr)
      (let* ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)))
        (if (= entry 0)
            0
          (let* ((plist-slot (alloc-bytes 32 8))
                 (buf (alloc-bytes 8 1))
                 (t_sym_slot (alloc-bytes 32 8))
                 (result_slot (alloc-bytes 32 8)))
            (seq
             (record-slot-ref entry 2 plist-slot)
             (ptr-write-u64 buf 0 116)
             (nl_alloc_symbol buf 1 t_sym_slot)
             (nelisp_eq_symbol plist-slot t_sym_slot result_slot)
             (if (= (sexp-tag result_slot) 1) 1 0)))))))
  "AOT source for Doc 164 §2.1 `mirror_set_special_or_insert'.")

(provide 'nelisp-cc-mirror-set-special-or-insert)

;;; nelisp-cc-mirror-set-special-or-insert.el ends here
