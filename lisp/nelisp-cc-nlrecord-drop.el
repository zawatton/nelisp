;;; nelisp-cc-nlrecord-drop.el --- GC-owned record drop boundary -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Doc 216 S4.1: compact record backing has no refcount or Vec metadata.
;; Keep the established native entry points while tracing GC owns lifetime.
;;; Code:
(defconst nelisp-cc-nlrecord-drop--source
  '(seq
    (defun nelisp_nlrecord_drop_prog2 (_eff val) val)
    (defun nelisp_nlrecord_drop (box-ptr) 0))
  "Compiler source for the GC-owned record drop compatibility entry.")
(provide 'nelisp-cc-nlrecord-drop)
;;; nelisp-cc-nlrecord-drop.el ends here
