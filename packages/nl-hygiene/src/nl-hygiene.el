;;; nl-hygiene.el --- Opt-in hygienic macro composition for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 198 Phase 3.  `nl-defmacro-hygienic' composes two existing L1,
;; opt-in mechanisms:
;;
;; - `nl-defmacro' replaces trailing-# template symbols with a fresh
;;   gensym on every expansion, preventing introduced-binding capture.
;; - `nl-ns-reader' can resolve declared, literal free identifiers while
;;   the macro definition is read, fixing their definition namespace in
;;   the resulting symbols before the macro is installed.
;;
;; The second property is a load-path discipline, not something this macro
;; can add after its BODY has already been read.  Macro-defining source must
;; therefore pass through `nl-ns-read-in', `nl-ns-read-from-string-in', or
;; `nl-ns-read-all-in'.  A plain `require' does not activate resolution;
;; adding such a wrapper is Doc 198 Phase 4 and is intentionally absent.

;;; Code:

(require 'nl-prelude)
(require 'nl-ns-reader)

(defmacro nl-defmacro-hygienic (name arglist &rest body)
  "Define macro NAME with ARGLIST and hygienic-template conventions.
BODY is delegated to `nl-defmacro': each trailing-# symbol in the produced
expansion is replaced by a fresh, expansion-local gensym.  For definition-
environment resolution, the source containing this form must itself have
been read through an `nl-ns-read-*-in' entry point for a namespace whose
`:members' declare the literal free identifiers to qualify.

This is intentionally below syntax-case hygiene: undeclared identifiers,
programmatically constructed symbols, caller forms spliced into the
template, lexical definition bindings, and source read through ordinary
`require'/`eval' retain stock Elisp behavior."
  (declare (indent defun) (doc-string 3))
  `(nl-defmacro ,name ,arglist ,@body))

(provide 'nl-hygiene)

;;; nl-hygiene.el ends here
