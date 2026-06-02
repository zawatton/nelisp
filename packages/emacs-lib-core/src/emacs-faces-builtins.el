;;; emacs-faces-builtins.el --- Unprefixed face bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track F (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed face API to the substrate in
;; `emacs-faces.el' (which sits on top of `emacs-redisplay's
;; existing face registry).  Function definitions use a host-aware
;; install gate: host Emacs keeps its faces.el implementation, while
;; standalone NeLisp overwrites bootstrap stubs with the real face
;; substrate.
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

(defun emacs-faces-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-faces-builtins--install-function-p 'facep)
  (defalias 'facep #'emacs-faces-facep))

(when (emacs-faces-builtins--install-function-p 'make-face)
  (defalias 'make-face #'emacs-faces-make-face))

;;;; --- attribute accessors -------------------------------------------

(when (emacs-faces-builtins--install-function-p 'face-attribute)
  (defalias 'face-attribute #'emacs-faces-attribute))

(when (emacs-faces-builtins--install-function-p 'set-face-attribute)
  (defalias 'set-face-attribute #'emacs-faces-set-attribute))

(when (emacs-faces-builtins--install-function-p 'face-foreground)
  (defalias 'face-foreground #'emacs-faces-foreground))

(when (emacs-faces-builtins--install-function-p 'face-background)
  (defalias 'face-background #'emacs-faces-background))

(when (emacs-faces-builtins--install-function-p 'set-face-foreground)
  (defalias 'set-face-foreground #'emacs-faces-set-foreground))

(when (emacs-faces-builtins--install-function-p 'set-face-background)
  (defalias 'set-face-background #'emacs-faces-set-background))

;;;; --- enumeration ---------------------------------------------------

(when (emacs-faces-builtins--install-function-p 'face-list)
  (defalias 'face-list #'emacs-faces-list))

;;;; --- defface macro -------------------------------------------------

(when (emacs-faces-builtins--install-function-p 'defface)
  (defmacro defface (name spec doc &rest opts)
    "Track F bridge: delegate to `emacs-faces-defface'."
    (if (boundp 'nelisp-emacs-vendor-root)
        `(progn
           (emacs-faces-make-face ',name)
           ',name)
      `(emacs-faces-defface ,name ,spec ,doc ,@opts))))

(provide 'emacs-faces-builtins)

;;; emacs-faces-builtins.el ends here
