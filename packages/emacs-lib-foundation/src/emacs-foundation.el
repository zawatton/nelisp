;;; emacs-foundation.el --- Reusable foundation layer loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for the FND package group.  It preserves the
;; load order previously encoded directly in `emacs-init.el', but can be
;; required by libraries that need only the reusable primitive substrate
;; rather than the full nemacs application bootstrap.

;;; Code:

;; When this file is loaded from an installed package archive, the sibling
;; feature files are not guaranteed to be on `load-path' yet.  Load them from
;; the same directory as this loader so package activation stays robust.
(defconst emacs-foundation--load-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that contains the foundation feature files.")

(defun emacs-foundation--load-feature (feature)
  "Load FEATURE from the foundation package directory."
  (let ((file (expand-file-name (concat (symbol-name feature) ".el")
                                emacs-foundation--load-directory)))
    (unless (load file nil t)
      (require feature))))

;; Order matters: emacs-eval (defalias) before emacs-list (uses defalias);
;; emacs-fns (plist-get) before emacs-symbol (uses plist-get + plist-put);
;; emacs-list (nreverse, copy-sequence) before emacs-hash (uses both).
(defconst emacs-foundation-features
  '(emacs-fns
    emacs-eval
    emacs-list
    emacs-hash
    emacs-symbol
    emacs-callproc
    emacs-vars
    emacs-char-table
    emacs-backquote
    emacs-error
    emacs-string
    emacs-pcase
    cl-lib
    subr-x
    emacs-cl-macros
    emacs-stub-bulk
    emacs-stub
    emacs-os-detect
    emacs-easy-mmode
    emacs-time
    emacs-numeric
    emacs-subr-extras
    emacs-edebug-stubs)
  "Reusable FND package features loaded by `emacs-foundation'.")

(dolist (feature emacs-foundation-features)
  (emacs-foundation--load-feature feature))

;; NeLisp v1.2.0's reader provides the `cl-lib' FEATURE itself (its
;; prelude carries cl-loop / cl-defstruct / cl-case natively) but not the
;; whole surface Layer 2 reaches for: `cl-member-if' is absent, and
;; `emacs-keymap--set-binding' calls it while vendored `pp.el' installs
;; its keymap at load time.  With the feature already provided, the
;; `(require 'cl-lib)' above never reaches `src/cl-lib.el', whose
;; fboundp-gated polyfills close exactly that gap (measured 2026-09-04,
;; windows-x86_64, anvil's MCP driver: void-function cl-member-if).
;; Load the shim by path when the gap is visible; every definition in it
;; is guarded, so on a runtime that has the symbols it is a no-op, and
;; under host Emacs `cl-member-if' is always fboundp so this never fires.
;; Only the sibling `src/cl-lib.el' qualifies -- the vendored upstream
;; copy is the file the shim exists to avoid.
(unless (fboundp 'cl-member-if)
  (let ((shim (and (fboundp 'locate-library)
                   (condition-case nil (locate-library "cl-lib") (error nil)))))
    (when (and (stringp shim)
               (let ((n (length shim)))
                 (and (> n 14)
                      (string= (substring shim (- n 14)) "/src/cl-lib.el"))))
      (load shim nil t))))

(provide 'emacs-foundation)

;;; emacs-foundation.el ends here
