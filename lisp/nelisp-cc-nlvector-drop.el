;;; nelisp-cc-nlvector-drop.el --- GC-owned vector drop boundary -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Doc 216 S4.1: compact vector backing has no refcount or Vec metadata.
;; Keep the established native entry points while tracing GC owns lifetime.
;;; Code:
(defconst nelisp-cc-nlvector-drop--source
  '(seq
    (defun nelisp_nlvector_drop_prog2 (_eff val) val)
    (defun nelisp_nlvector_drop (box-ptr) 0))
  "Compiler source for the GC-owned vector drop compatibility entry.")
(provide 'nelisp-cc-nlvector-drop)
;;; nelisp-cc-nlvector-drop.el ends here
