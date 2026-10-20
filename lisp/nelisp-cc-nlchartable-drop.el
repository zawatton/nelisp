;;; nelisp-cc-nlchartable-drop.el --- GC-owned char-table lifetime -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Doc 216 S4: neither the 80-byte body nor its child WORDs carry a
;; reference count. Explicit drops preserve the historical success
;; sentinel; tracing GC releases unreachable bodies and child buffers.

;;; Code:
(defconst nelisp-cc-nlchartable-drop--source
  '(seq
    (defun nelisp_nlchartable_drop_prog2 (_eff val) val)
    (defun nelisp_nlchartable_drop (_box-ptr) 1))
  "AOT source for the GC-owned char-table drop compatibility entry.")

(provide 'nelisp-cc-nlchartable-drop)
;;; nelisp-cc-nlchartable-drop.el ends here
