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

(provide 'emacs-foundation)

;;; emacs-foundation.el ends here
