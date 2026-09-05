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

;; T52: moved here from `src/nelisp-emacs-magit-bridge.el' (originally
;; `nelisp-emacs-magit-bridge--ensure-vendor-preload-globals', reached only
;; when the magit bundle loads), which left it void on the default boot
;; path -- the load matrix showed `(void-variable button-buffer-map)' for 3
;; features that are not magit at all (consult, flycheck,
;; flycheck-posframe).  Real `button.el' defines `button-buffer-map' before
;; `button-map' and parents the latter to it (mode-specific keymaps use
;; `button-buffer-map' as their parent for the `forward-button' /
;; `backward-button' navigation bindings); this reduced compatibility layer
;; does not implement that navigation, so the keymap here stays empty like
;; `button-map' below it -- callers that only need a live keymap object to
;; hang off of (the failure class this addresses) still get one.
(unless (boundp 'button-buffer-map)
  (defvar button-buffer-map
    (if (fboundp 'make-sparse-keymap)
        (make-sparse-keymap)
      '(keymap))
    "Keymap useful for buffers containing buttons.
Mode-specific keymaps may want to use this as their parent keymap.  See
the commentary above for what the reduced compatibility layer omits."))

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
