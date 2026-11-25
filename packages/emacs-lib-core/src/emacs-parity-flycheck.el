;;; emacs-parity-flycheck.el --- flycheck-def-option-var hard-abort parity fix -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Register the standalone setter for generated Flycheck checker properties.
;; GNU cl-pushnew uses the GV registration.  The legacy setf provider uses
;; the same setter through its cl-simple-setter property.

;;; Code:

(defconst emacs-parity-flycheck--standalone-p
  (fboundp 'nelisp--eval-source-string)
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards activation so host Emacs is left untouched (it keeps the real
`setf' and Flycheck declarations).")

;; --- Piece 2 helper: interpreter-safe checker-property setter ------------
;; Defined unconditionally (inert unless the `cl-simple-setter' property below
;; points at it) so the symbol is always resolvable.
(defun emacs-parity-flycheck--checker-set (checker property value)
  "Set CHECKER's PROPERTY to VALUE; the `setf' setter for `flycheck-checker-get'.
Mirrors `flycheck-checker-get' = `(get CHECKER
\(flycheck--checker-property-name PROPERTY))' with the corresponding `put'."
  (put checker
       (if (fboundp 'flycheck--checker-property-name)
           (flycheck--checker-property-name property)
         (intern (concat "flycheck-" (symbol-name property))))
       value)
  value)

(when emacs-parity-flycheck--standalone-p
  (gv-define-simple-setter flycheck-checker-get
                           emacs-parity-flycheck--checker-set)
  (put 'flycheck-checker-get 'cl-simple-setter
       'emacs-parity-flycheck--checker-set))

(provide 'emacs-parity-flycheck)

;;; emacs-parity-flycheck.el ends here
