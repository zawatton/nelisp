;;; emacs-io.el --- Reusable IO/runtime loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for IO-facing builtins and runtime
;; adapters.  This groups the SQLite facade, file I/O bridges,
;; standalone primitive dispatch registry, and process bridges without
;; forcing optional FFI adapters such as `emacs-sqlite-ffi'.

;;; Code:

;; `emacs-sqlite-ffi' requires nelisp-ffi to be on load-path; it is
;; opt-in (= caller adds it to ANVIL_MODULE_FILES).  Keep this loader on
;; the host-safe facade.
(defconst emacs-io-features
  '(emacs-ffi
    emacs-sqlite
    nelisp-emacs-compat-fileio
    files-runtime
    emacs-fileio-builtins
    emacs-standalone
    emacs-process
    emacs-process-builtins)
  "Reusable IO package features loaded by `emacs-io'.")

(dolist (feature emacs-io-features)
  (require feature))

(provide 'emacs-io)

;;; emacs-io.el ends here
