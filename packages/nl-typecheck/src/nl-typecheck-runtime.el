;;; nl-typecheck-runtime.el --- run-time no-ops for type annotations -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 236: annotation forms are no-ops at run time.  Load this file so
;; side files and annotated code that mention `nelisp-declare-type' still
;; load on any Emacs-compatible runtime.

;;; Code:

(defmacro nelisp-declare-type (_symbol _type)
  "Declare TYPE for SYMBOL for the type checker; a no-op at run time."
  (declare (indent 0))
  nil)

(provide 'nl-typecheck-runtime)
;;; nl-typecheck-runtime.el ends here
