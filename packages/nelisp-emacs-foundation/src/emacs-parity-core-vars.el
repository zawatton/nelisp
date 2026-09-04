;;; emacs-parity-core-vars.el --- core var fills -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k19: the marker-leak fix made declared-but-unbound core vars signal
;; void-variable (instead of leaking the sentinel as a value).  Fill the safe
;; core ones with real stock values, guarded with `unless boundp'.
;;
;; DEFERRED (empty shim would shadow the package's real map): `isearch-mode-map',
;; `grep-mode-map', `minibuffer-local-completion-map', `mode-line-major-mode-keymap',
;; `vc-hg-log-view-mode-map', `vc-git-log-view-mode-map', `org-org-menu',
;; `evil-command-window-mode-map', `diff-hl-stage-diff-mode-map',
;; `compilation-minor-mode-map', `treemacs-header-projects-button',
;; `sh-mode-syntax-table' -- read while unbound but defined by their package.

;;; Code:

;; --- version (kills the ~141 `emacs-version' cascade) ---
(unless (boundp 'emacs-version) (defvar emacs-version "30.1"))
(unless (boundp 'emacs-major-version) (defvar emacs-major-version 30))
(unless (boundp 'emacs-minor-version) (defvar emacs-minor-version 1))
(unless (boundp 'emacs-build-time) (defvar emacs-build-time nil))
(unless (boundp 'emacs-build-system) (defvar emacs-build-system "nemacs"))

;; --- batch/display context ---
(unless (boundp 'window-system) (defvar window-system nil))
(unless (boundp 'invocation-directory) (defvar invocation-directory "/usr/bin/"))
(unless (boundp 'invocation-name) (defvar invocation-name "emacs"))
(unless (boundp 'user-full-name)
  (defvar user-full-name (or (getenv "USER") "user")))
(unless (boundp 'enable-multibyte-characters) (defvar enable-multibyte-characters t))

;; --- cc-mode char classes / features (stock cc-defs values; used inside [...]) ---
(unless (boundp 'c-alpha) (defvar c-alpha "[:alpha:]"))
(unless (boundp 'c-alnum) (defvar c-alnum "[:alnum:]"))
(unless (boundp 'c-upper) (defvar c-upper "[:upper:]"))
(unless (boundp 'c-lower) (defvar c-lower "[:lower:]"))
(unless (boundp 'c-use-category) (defvar c-use-category nil))
(unless (boundp 'c-emacs-features)
  (defvar c-emacs-features '(pps-extended-state category-properties syntax-properties)))
(unless (boundp 'objc-font-lock-extra-types) (defvar objc-font-lock-extra-types nil))
(unless (boundp 'cc-imenu-c++-generic-expression) (defvar cc-imenu-c++-generic-expression nil))
(unless (boundp 'cc-imenu-java-type-spec-regexp) (defvar cc-imenu-java-type-spec-regexp nil))

;; --- misc core ---
(unless (boundp 'stipple-pixmap) (defvar stipple-pixmap nil))
(unless (boundp 'lisp-mode-symbol-regexp)
  (defvar lisp-mode-symbol-regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"))
(unless (boundp 'completion-styles-alist) (defvar completion-styles-alist nil))
(unless (boundp 'completion-ignored-extensions)
  (defvar completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".obj")))
(unless (boundp 'face-attribute-name-alist) (defvar face-attribute-name-alist nil))
(unless (boundp 'frameset-filter-alist) (defvar frameset-filter-alist nil))
(unless (boundp 'emacs-easy-mmode--standalone-p) (defvar emacs-easy-mmode--standalone-p t))
(unless (boundp 'flycheck-this-emacs-executable) (defvar flycheck-this-emacs-executable nil))

;; --- mode-line value var (stock default is nil; a VALUE var, not a keymap,
;; so filling it cannot shadow a package's real map like the DEFERRED entries
;; above).  `global-mode-string' is read by mode-line construction in many
;; packages and signalled void-variable in the full-init audit. ---
(unless (boundp 'global-mode-string) (defvar global-mode-string nil))

;; --- minor-mode state vars (read before their define-minor-mode sets them) ---
(unless (boundp 'evil-mode) (defvar evil-mode nil))
(unless (boundp 'mouse-wheel-mode) (defvar mouse-wheel-mode nil))
(unless (boundp 'auto-compression-mode) (defvar auto-compression-mode nil))

;; --- input method state (headless: no input method active) ---
(unless (boundp 'current-input-method)
  (defvar current-input-method nil
    "The current input method for reading text, or nil for none."))
(unless (boundp 'current-input-method-title)
  (defvar current-input-method-title nil
    "Mode-line title of the current input method, or nil."))

;; --- headless display-hint functions -----------------------------------
;; The batch substrate has no frame/redisplay, so these C display primitives
;; are correctly inert here: `force-window-update' only schedules a redisplay
;; and `set-window-fringes' sets display-only geometry; both return nil like
;; the C originals do for a non-interactive frame.  They surfaced as
;; void-function in the full-init audit (mode/ui setup calling them at load).
;; Guarded with `fboundp' so a real GUI build keeps its native primitives.
(unless (fboundp 'force-window-update)
  (defun force-window-update (&optional _object)
    "Headless no-op: no frame redisplay to force in the batch substrate."
    nil))
(unless (fboundp 'set-window-fringes)
  (defun set-window-fringes (&rest _args)
    "Headless no-op: fringe geometry is display-only in the batch substrate."
    nil))

(provide 'emacs-parity-core-vars)

;;; emacs-parity-core-vars.el ends here
