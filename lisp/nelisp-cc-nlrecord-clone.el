;;; nelisp-cc-nlrecord-clone.el --- GC-owned record clone boundary -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Doc 216 S4.1: compact record backing has no refcount or Vec metadata.
;; Keep the established native entry points while tracing GC owns lifetime.
;;; Code:
(defconst nelisp-cc-nlrecord-clone--source
  '(seq
    (defun nelisp_nlrecord_clone_prog2 (_eff val) val)
    (defun nelisp_nlrecord_clone (box-ptr) box-ptr))
  "Compiler source for the GC-owned record clone compatibility entry.")
(provide 'nelisp-cc-nlrecord-clone)
;;; nelisp-cc-nlrecord-clone.el ends here
