;;; nelisp-cc-recordp.el --- Doc 111 §111.B recordp predicate swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.B swaps `(recordp X)' to a Phase 47-compiled elisp
;; object.  The body checks only the Sexp tag byte and writes `t' /
;; `nil' into a caller-owned result slot.

;;; Code:

(defconst nelisp-cc-recordp--source
  '(defun nelisp_recordp (arg0 result-slot)
     ;; arg0: *const Sexp pointing at any Sexp value.
     ;; result-slot: *mut Sexp 32-byte slot for Sexp::T or Sexp::Nil.
     (if (= (sexp-tag arg0) 12)
         (sexp-write-t result-slot)
       (sexp-write-nil result-slot))))

(provide 'nelisp-cc-recordp)

;;; nelisp-cc-recordp.el ends here
