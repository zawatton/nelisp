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
  (let ((source-file
         (or (and (boundp 'load-file-name) load-file-name)
             (and (boundp 'buffer-file-name) buffer-file-name))))
    (cond
     (source-file
      (file-name-directory source-file))
     ((and (boundp 'default-directory)
           (stringp default-directory))
      (let ((src (expand-file-name "src/" default-directory)))
        (if (and (fboundp 'file-directory-p)
                 (file-directory-p src))
            src
          default-directory)))
     (t nil)))
  "Directory that contains the foundation feature files.")

(defun emacs-foundation--load-feature (feature)
  "Load FEATURE from the foundation package directory, unless already loaded.
The `featurep' guard matters when this file's own body is replayed as
part of a pre-concatenated bootstrap bundle (see
`scripts/build-nelisp-bootstrap.el'): by the time this loader runs, the
bundle's dependency-ordered concatenation has already provided every
member of `emacs-foundation-features' except a couple of trailing ones,
so re-`load'ing them here — unconditionally, regardless of `featurep' —
cost a real second read+eval of each already-loaded file, plus whatever
each of THOSE files reloads in turn (measured 2026-09-28: ~11 s of a
~56 s cold bundle replay, and the same duplicated source content baked a
second time into the generated bundle because the builder's host-Emacs
load-history trace records the forced reload as a second load event).
On host Emacs, or when this file is required directly from `src/'
without the bundle, FEATURE is not loaded yet, so the guard is a no-op
and every feature is still loaded exactly as before."
  (unless (featurep feature)
    (let ((file (expand-file-name (concat (symbol-name feature) ".el")
                                  emacs-foundation--load-directory)))
      (unless (load file nil t)
        (require feature)))))

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
    emacs-button-builtins
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
;; the keymap binding setter calls it while vendored `pp.el' installs
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
