;;; nelisp-cc-nlvector-clone.el --- GC-owned vector clone boundary -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Doc 216 S4.1: compact vector backing has no refcount or Vec metadata.
;; Keep the established native entry points while tracing GC owns lifetime.
;;; Code:
(defconst nelisp-cc-nlvector-clone--source
  '(seq
    (defun nelisp_nlvector_clone_prog2 (_eff val) val)
    (defun nelisp_nlvector_clone (box-ptr) box-ptr))
  "Compiler source for the GC-owned vector clone compatibility entry.")
(provide 'nelisp-cc-nlvector-clone)
;;; nelisp-cc-nlvector-clone.el ends here
