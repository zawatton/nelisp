;;; emacs-parity-vars2.el --- more real-elisp parity variables -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; b1k13 parity round 2 (variables only): REAL stock values for the
;; non-`org-*' void-variables the user's real init references that
;; `emacs-parity-shims.el' (round 1) did NOT already provide.  Loaded EARLY
;; (before the user init) by the same loader as the round-1 shims.
;;
;; Every binding is guarded with `unless boundp' so a package that has
;; already provided the real definition still wins.
;;
;; SHADOWING DISCIPLINE (why each var below is safe to pre-bind):
;;
;;   1. `defconst'-owned vars are OVERRIDE-SAFE.  `defconst' reassigns
;;      unconditionally when its owner file finally loads, so a pre-bound
;;      value here is replaced by the real one -- it can never shadow.
;;      Verified from the vendored sources:
;;        - c/c++/objc/java/idl/pike -font-lock-keywords-3
;;          -> defconst in vendor/emacs-lisp/progmodes/cc-fonts.el
;;        - transient-font-lock-keywords
;;          -> defconst in vendor/transient/lisp/transient.el:5710
;;        - treemacs-header-close-button
;;          -> defconst in treemacs/src/elisp/treemacs-header-line.el:40
;;
;;   2. `defvar'-with-value vars are pre-bound to their EXACT stock value
;;      (copied verbatim from the vendored source), so even if the owner's
;;      later `defvar' does not override (var already bound), the value is
;;      identical to stock -- a harmless "shadow".
;;        - file-name-version-regexp -> vendor/emacs-lisp/files.el:5344
;;        - compilation-info-face    -> vendor/emacs-lisp/progmodes/compile.el:1061
;;        - custom-current-group-alist -> vendor/emacs-lisp/custom.el:47
;;
;; DEFERRED (NOT provided here -- pre-binding would risk shadowing a real,
;; functional definition, or the var belongs to the core/interpreter layer):
;;
;;   * Functional keymaps whose owner defines them with `defvar'+value or
;;     `define-derived-mode' (an empty sparse map here would erase every
;;     real binding of that mode):
;;       isearch-mode-map, grep-mode-map, compilation-minor-mode-map,
;;       minibuffer-local-completion-map, menu-bar-bookmark-map,
;;       evil-command-window-mode-map, eat-char-mode-map,
;;       eat-eshell-char-mode-map, diff-hl-stage-diff-mode-map.
;;   * skk-auto-paren-string-alist -- owned by ddskk (skk-vars); the audit
;;     goal is a REAL ddskk load, and user init MODIFIES this alist
;;     (init.el:8858).  A pre-bound value would shadow ddskk's default
;;     auto-paren table and break the feature.
;;   * completion-styles-alist, frameset-filter-alist -- large
;;     `defvar'+value data owned by minibuffer.el / frameset.el; cannot be
;;     reproduced exactly and would shadow the real (richer) value.
;;   * completion-ignored-extensions -- canonical owner not present in the
;;     vendored lisp tree (C/loadup); exact stock list unverifiable here, so
;;     a guessed value could shadow.
;;   * enable-multibyte-characters, current-input-method -- core, permanent
;;     buffer-local variables.  A plain global `defvar' would give wrong
;;     per-buffer answers and interfere with input-method state (relevant to
;;     the ddskk real-load goal); these need core support.
;;   * nelisp--unbound-marker, num, it -- not stock Emacs variables.  These
;;     are an internal runtime marker leak / leaked lexical bindings from a
;;     macro-expansion defect; shimming them would mask the real root cause.

;;; Code:

;;;; --- CC Mode gaudy font-lock keyword lists (defconst-owned) ---------
;; nil is a valid (empty) font-lock-keywords list.  cc-fonts.el rebinds
;; each of these with `(defconst X (c-lang-const c-matchers-3 LANG))' when
;; the mode is used, unconditionally overriding this placeholder.

(unless (boundp 'c-font-lock-keywords-3)    (defvar c-font-lock-keywords-3 nil))
(unless (boundp 'c++-font-lock-keywords-3)  (defvar c++-font-lock-keywords-3 nil))
(unless (boundp 'objc-font-lock-keywords-3) (defvar objc-font-lock-keywords-3 nil))
(unless (boundp 'java-font-lock-keywords-3) (defvar java-font-lock-keywords-3 nil))
(unless (boundp 'idl-font-lock-keywords-3)  (defvar idl-font-lock-keywords-3 nil))
(unless (boundp 'pike-font-lock-keywords-3) (defvar pike-font-lock-keywords-3 nil))

;;;; --- transient font-lock keywords (defconst-owned) -----------------
;; Faithful reconstruction of transient.el's `transient-font-lock-keywords'
;; (defconst, override-safe).  The regexp is the pre-computed
;; `regexp-opt' output for the five `transient-define-*' macro names, so no
;; run-time dependency on `regexp-opt'.  `\t' in the char class is a literal
;; TAB, matching the stock source.

(unless (boundp 'transient-font-lock-keywords)
  (defvar transient-font-lock-keywords
    '(("(\\(transient-define-\\(?:argument\\|group\\|infix\\|prefix\\|suffix\\)\\)\\_>[ \t'(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))
    "Font-lock keywords for `transient-define-*' forms (parity fallback)."))

;;;; --- treemacs header close button (defconst-owned) -----------------
;; Real value is a propertized glyph carrying a mouse local-map built inside
;; a cl-macrolet; the local-map cannot be reproduced standalone, but the
;; visible glyph + face is faithful and treemacs-header-line.el's defconst
;; restores the full value (with map) on real load.

(unless (boundp 'treemacs-header-close-button)
  (defvar treemacs-header-close-button
    (propertize "(❌)" 'face 'treemacs-header-button-face)
    "Header button to close the treemacs window (parity fallback)."))

;;;; --- defvar-owned scalars, pre-bound to EXACT stock value ----------

;; files.el: `defvar file-name-version-regexp' (exact stock regexp).
(unless (boundp 'file-name-version-regexp)
  (defvar file-name-version-regexp
    "\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)"
    "Regexp matching the backup/version part of a file name."))

;; compile.el: `(defvar compilation-info-face 'compilation-info ...)'
;; (obsolete face-holding variable; also consumed by grep.el's grep-hit-face).
(unless (boundp 'compilation-info-face)
  (defvar compilation-info-face 'compilation-info
    "Face name used for grep hits and informational compilation messages."))

;; custom.el: `(defvar custom-current-group-alist nil)' (stock default nil).
(unless (boundp 'custom-current-group-alist)
  (defvar custom-current-group-alist nil
    "Alist of (SYMBOL . COMMENT) for the group being customized."))

(provide 'emacs-parity-vars2)

;;; emacs-parity-vars2.el ends here
