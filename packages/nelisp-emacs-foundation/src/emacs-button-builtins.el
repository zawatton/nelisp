;;; emacs-button-builtins.el --- Minimal button compatibility surface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Provide the small part of GNU button.el used by the reusable library
;; stack without making standalone NeLisp depend on the host Emacs Lisp
;; installation.  The current callers need only `buttonize' and
;; `buttonize-region'.  Host Emacs keeps its preloaded implementations;
;; standalone readers install these reduced text-property bridges.

;;; Code:

(unless (boundp 'button-map)
  (defvar button-map
    (if (fboundp 'make-sparse-keymap)
        (make-sparse-keymap)
      '(keymap))
    "Keymap attached to buttons made by the reduced compatibility layer."))

(defun emacs-button-builtins--properties (callback data help-echo)
  "Return reduced button properties for CALLBACK, DATA, and HELP-ECHO."
  (list 'font-lock-face 'button
        'mouse-face 'highlight
        'help-echo help-echo
        'button t
        'follow-link t
        'category t
        'button-data data
        'keymap button-map
        'action callback
        'face 'button))

(defun emacs-button-builtins-buttonize
    (string callback &optional data help-echo)
  "Make STRING into a reduced text button and return the resulting string.
CALLBACK, DATA, and HELP-ECHO become button text properties."
  (apply #'propertize string
         (emacs-button-builtins--properties callback data help-echo)))

(defun emacs-button-builtins-buttonize-region
    (start end callback &optional data help-echo)
  "Make the region from START to END into a reduced text button.
CALLBACK, DATA, and HELP-ECHO become button text properties when the active
buffer substrate supports `add-text-properties'."
  (when (fboundp 'add-text-properties)
    (add-text-properties
     start end
     (emacs-button-builtins--properties callback data help-echo)))
  nil)

(unless (fboundp 'buttonize)
  (defalias 'buttonize #'emacs-button-builtins-buttonize))

(unless (fboundp 'buttonize-region)
  (defalias 'buttonize-region #'emacs-button-builtins-buttonize-region))

;; `button' is preloaded by supported host Emacs versions.  On standalone
;; NeLisp this declaration makes later GNU-library `require' forms resolve to
;; the deliberately small compatibility surface above.
(provide 'button)
(provide 'emacs-button-builtins)

;;; emacs-button-builtins.el ends here
