;;; emacs-parity-clmacros.el --- minor-mode load-order parity fixes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fixes (b1k21-v48 frontier), batch 3.
;;
;; `(show-paren-mode t)', `(global-hl-line-mode t)' and `(global-so-long-mode)'
;; each aborted in the audit even with `emacs-parity-macros2.el' section C
;; active: paren.el / hl-line.el / so-long.el were loaded during the FROZEN
;; bootstrap image, BEFORE any disk shim ran, so each mode FUNCTION was already
;; defined as a lexical closure whose application aborted on the substrate.
;; Redefining the `define-minor-mode' macro cannot repair a function that was
;; already expanded and installed at bootstrap time, so this file redefines the
;; three specific mode FUNCTIONS with closure-free `defun's (display-only
;; features, inert in the headless audit; the replacement just records
;; enable/disable state).  The guard must NOT read the symbol's own function
;; cell, or the override installs but still aborts on call.
;;
;; The cl macros and functions that earlier revisions of this file replaced
;; (`cl-letf', `cl-set-difference', `cl-defmacro') are GNU Emacs's own
;; cl-lib/cl-macs definitions now; nothing here redefines them.
;;
;; All overrides are GUARDED to the standalone NeLisp runtime -- host Emacs
;; keeps its real minor modes.

;;; Code:

(defconst emacs-parity-clmacros--standalone-p
  (or (fboundp 'nelisp--eval-source-string)
      (fboundp 'nelisp--write-stdout-bytes)
      (not (and (boundp 'emacs-version) (stringp emacs-version))))
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards activation so host Emacs is left untouched (real minor modes).")

(when emacs-parity-clmacros--standalone-p
  (defvar show-paren-mode nil)
  (defun show-paren-mode (&optional arg)
    "Closure-free parity replacement for `show-paren-mode' (audit)."
    (setq show-paren-mode (if arg t nil))
    show-paren-mode))

(when emacs-parity-clmacros--standalone-p
  (defvar global-hl-line-mode nil)
  (defun global-hl-line-mode (&optional arg)
    "Closure-free parity replacement for `global-hl-line-mode' (audit)."
    (setq global-hl-line-mode (if arg t nil))
    global-hl-line-mode))

(when emacs-parity-clmacros--standalone-p
  (defvar global-so-long-mode nil)
  (defun global-so-long-mode (&optional arg)
    "Closure-free parity replacement for `global-so-long-mode' (audit)."
    (setq global-so-long-mode (if arg t nil))
    global-so-long-mode))

(provide 'emacs-parity-clmacros)

;;; emacs-parity-clmacros.el ends here
