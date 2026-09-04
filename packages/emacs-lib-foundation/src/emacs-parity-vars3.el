;;; emacs-parity-vars3.el --- real-elisp parity variables (round 3) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k19+ parity round 3 (variables only): REAL stock values for the core /
;; data / defconst-owned variables the user's real init references that
;; `emacs-parity-shims.el', `emacs-parity-vars2.el', and
;; `emacs-parity-core-vars.el' did NOT already provide.  Loaded EARLY (before
;; user init) by the same `nemacs-next-session.el' loader.
;;
;; Every binding is guarded with `unless boundp' so a package (or later real
;; preload) that provides the genuine value still wins, and so the file is a
;; no-op on host Emacs.
;;
;; SHADOWING DISCIPLINE (same rules as emacs-parity-vars2.el):
;;
;;   1. `vc-mode' -- core buffer-local var (bindings.el / vc-hooks.el), stock
;;      default nil.  A plain `defvar nil' is the exact stock default;
;;      vc-hooks.el makes it buffer-local and fills it when a file is under
;;      version control, so pre-binding nil can never shadow a real value.
;;
;;   2. `lisp-imenu-generic-expression' -- DATA var (lisp-mode.el `defvar').
;;      Pre-bound to the EXACT stock Emacs 30.1 value (machine-copied via
;;      `prin1', not hand-transcribed).  If lisp-mode.el later loads and its
;;      `defvar' does not override (var already bound), the value is byte-for-
;;      byte identical to stock -- a harmless "shadow".
;;
;;   3. `treemacs-header-projects-button' -- `defconst'-owned (OVERRIDE-SAFE),
;;      verified at treemacs/src/elisp/treemacs-header-line.el:47 (sibling of
;;      `treemacs-header-close-button' already shimmed in vars2.el:99).  The
;;      real value is a propertized glyph carrying a mouse `local-map' built
;;      with `x-popup-menu'/`easy-menu-create-menu', which cannot be
;;      reproduced standalone; the visible glyph + face is faithful and the
;;      treemacs `defconst' restores the full value (with map) on real load.
;;
;; DEFERRED here (NOT provided -- pre-binding would shadow the owner's real
;; definition, matching the vars2/core-vars deferral discipline):
;;   * Functional keymaps whose owner defines them with value/`define-derived-
;;     mode' (an empty sparse map would erase every real binding):
;;       mode-line-major-mode-keymap (bindings.el), isearch-mode-map,
;;       grep-mode-map, minibuffer-local-completion-map,
;;       vc-git-log-view-mode-map, vc-hg-log-view-mode-map,
;;       evil-command-window-mode-map.
;;   * `sh-mode-syntax-table' -- syntax table built by sh-script.el; an empty
;;     table would break sh-mode syntax parsing.
;;   * `skk-auto-paren-string-alist' -- owned by ddskk; user init MODIFIES it,
;;     so a pre-bound value would shadow ddskk's default (see vars2.el).
;;   * `var', `num', `it', `and$' -- NOT stock Emacs variables; leaked
;;     lexical bindings from macro-expansion defects (see triage handoff),
;;     shimming them would mask the real root cause.

;;; Code:

;;;; --- vc-hooks.el / bindings.el: vc-mode (stock default nil) --------
(unless (boundp 'vc-mode)
  (defvar vc-mode nil
    "String for the current buffer's version control status in the mode line.
nil means the buffer is not under version control."))

;;;; --- lisp-mode.el: lisp-imenu-generic-expression (exact stock) -----
;; Verbatim Emacs 30.1 value; `[[:space:]\n]' preserves the literal newline
;; inside the `defvar' matcher's whitespace class.
(unless (boundp 'lisp-imenu-generic-expression)
  (defvar lisp-imenu-generic-expression
    '((nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 2)
      (nil "^\\s-*(\\(def\\(?:\\(?:ine-obsolete-function-\\)?alias\\)\\)\\s-+'\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 2)
      ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var-keymap\\)\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 2)
      ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
      ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 2))
    "Imenu generic expression for Lisp mode.  See `imenu-generic-expression'."))

;;;; --- treemacs-header-line.el: treemacs-header-projects-button ------
;; defconst-owned (override-safe); faithful visible glyph + face fallback.
(unless (boundp 'treemacs-header-projects-button)
  (defvar treemacs-header-projects-button
    (propertize "(P)" 'face 'treemacs-header-button-face)
    "Header button to manage treemacs projects (parity fallback)."))

(provide 'emacs-parity-vars3)

;;; emacs-parity-vars3.el ends here
