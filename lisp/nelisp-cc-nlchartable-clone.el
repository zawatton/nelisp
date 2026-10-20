;;; nelisp-cc-nlchartable-clone.el --- GC-owned char-table identity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Doc 216 S4: the packed 80-byte char-table body has no refcount trailer.
;; Tracing GC owns the body and its child buffers. Legacy physical views
;; can share that body, so cloning preserves its address without a write.

;;; Code:
(defconst nelisp-cc-nlchartable-clone--source
  '(seq
    (defun nelisp_nlchartable_clone_prog2 (_eff val) val)
    (defun nelisp_nlchartable_clone (box-ptr) box-ptr))
  "AOT source for identity-preserving, GC-owned char-table cloning.")

(provide 'nelisp-cc-nlchartable-clone)
;;; nelisp-cc-nlchartable-clone.el ends here
