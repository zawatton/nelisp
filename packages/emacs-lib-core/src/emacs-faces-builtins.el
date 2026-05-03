;;; emacs-faces-builtins.el --- Unprefixed face bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track F (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed face API to the substrate in
;; `emacs-faces.el' (which sits on top of `emacs-redisplay's
;; existing face registry).  Same `unless (fboundp ...)' /
;; `unless (boundp ...)' gating used by every other Track bridge —
;; loading inside a host Emacs is a cheap no-op.
;;
;; Bridged today (γ-stage MVP):
;;
;;   - Functions: facep / make-face / face-attribute /
;;     set-face-attribute / face-foreground / face-background /
;;     set-face-foreground / set-face-background / face-list
;;   - Macro: defface (= delegates to `emacs-faces-defface')
;;
;; Deferred to later γ phases:
;;   - face-spec-set-2 (= display-class precedence resolution)
;;   - face inheritance resolution at attribute-read time
;;   - face-remap, frame-parameter-driven attribute fallback
;;   - X-resource fallback

;;; Code:

(require 'emacs-faces)

;;;; --- predicates / lifecycle ----------------------------------------

(unless (fboundp 'facep)
  (defalias 'facep #'emacs-faces-facep))

(unless (fboundp 'make-face)
  (defalias 'make-face #'emacs-faces-make-face))

;;;; --- attribute accessors -------------------------------------------

(unless (fboundp 'face-attribute)
  (defalias 'face-attribute #'emacs-faces-attribute))

(unless (fboundp 'set-face-attribute)
  (defalias 'set-face-attribute #'emacs-faces-set-attribute))

(unless (fboundp 'face-foreground)
  (defalias 'face-foreground #'emacs-faces-foreground))

(unless (fboundp 'face-background)
  (defalias 'face-background #'emacs-faces-background))

(unless (fboundp 'set-face-foreground)
  (defalias 'set-face-foreground #'emacs-faces-set-foreground))

(unless (fboundp 'set-face-background)
  (defalias 'set-face-background #'emacs-faces-set-background))

;;;; --- enumeration ---------------------------------------------------

(unless (fboundp 'face-list)
  (defalias 'face-list #'emacs-faces-list))

;;;; --- defface macro -------------------------------------------------

(unless (fboundp 'defface)
  (defmacro defface (name spec doc &rest opts)
    "Track F bridge: delegate to `emacs-faces-defface'."
    `(emacs-faces-defface ,name ,spec ,doc ,@opts)))

(provide 'emacs-faces-builtins)

;;; emacs-faces-builtins.el ends here
