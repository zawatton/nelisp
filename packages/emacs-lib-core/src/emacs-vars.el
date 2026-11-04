;;; emacs-vars.el --- NeLisp port of Emacs C core globals.c defvars  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Establishes the small set of Emacs C-core global variables that
;; library code routinely references at load time
;; (`user-emacs-directory', `temporary-file-directory', `system-type',
;; `locale-coding-system', `exec-path', `file-name-handler-alist').
;; Phase 1.6 hard-codes sensible defaults
;; rooted at "~/" + "/tmp/" + 'gnu/linux + 'utf-8.  Phase 2 will
;; resolve dynamically once `getenv' is wired through NeLisp's
;; syscall extension and OS introspection lands.

;;; Code:

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Polyfill: NeLisp standalone fallback for Emacs' user-emacs-directory."))

(unless (boundp 'site-run-file)
  (defvar site-run-file "site-start"
    "File containing site-wide run-time initializations (GNU lread.c default).
Tramp's persistent-cache setting reads `(or init-file-user site-run-file)'
at load time, so the variable must exist before net/tramp-cache.el loads."))

(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory "/tmp/"
    "Polyfill: NeLisp standalone fallback for Emacs' temporary-file-directory."))

(unless (boundp 'locale-coding-system)
  (defvar locale-coding-system 'utf-8
    "Polyfill: NeLisp standalone forces utf-8."))

(unless (boundp 'system-type)
  (defvar system-type 'gnu/linux
    "Polyfill: NeLisp standalone defaults to gnu/linux.
Override per-host once `system-type' detection lands."))

(unless (boundp 'path-separator)
  (defvar path-separator ":"
    "Polyfill: Unix path list separator."))

(unless (boundp 'exec-path)
  (defvar exec-path '("/usr/local/bin" "/usr/bin" "/bin")
    "Polyfill: executable search path."))

(unless (boundp 'exec-suffixes)
  (defvar exec-suffixes nil
    "Polyfill: executable filename suffixes.
Unix-like systems do not need additional suffixes."))

(unless (boundp 'file-name-handler-alist)
  (defvar file-name-handler-alist nil
    "Polyfill: file-name handler registry."))

(unless (boundp 'load-in-progress)
  (defvar load-in-progress nil
    "Non-nil while Emacs is loading a Lisp file.
The standalone loader does not yet dynamically bind this C-core variable;
the nil default lets source-only packages distinguish loading from byte
compilation without signaling `void-variable'."))

(unless (boundp 'inhibit-file-name-handlers)
  (defvar inhibit-file-name-handlers nil
    "Polyfill: dynamically inhibited file-name handlers."))

(unless (boundp 'inhibit-file-name-operation)
  (defvar inhibit-file-name-operation nil
    "Polyfill: file operation whose handlers are dynamically inhibited."))

(unless (boundp 'pre-redisplay-function)
  (defvar pre-redisplay-function #'ignore
    "Polyfill: function run just before redisplay.
Standalone NeLisp starts with Emacs' C bootstrap sentinel so
vendor simple.el can replace it with its Elisp dispatcher."))

(unless (boundp 'dnd-protocol-alist)
  (defvar dnd-protocol-alist nil
    "Polyfill: drag-and-drop protocol handlers.
Standalone has no GUI drag-and-drop source; Org/Dired may still append local
handlers during mode activation."))

(unless (boundp 'menu-bar-final-items)
  (defvar menu-bar-final-items nil
    "Polyfill: menu-bar keys that should appear after ordinary menu items.
Emacs preloads this variable; standalone NeLisp starts with no final items."))

(unless (boundp 'gc-cons-threshold)
  (defvar gc-cons-threshold 800000
    "Bytes of consing between garbage collections (Doc 06 A2 compat default).
Settable by callers that tune GC; the standalone runtime collects at form
boundaries."))

(unless (boundp 'gc-cons-percentage)
  (defvar gc-cons-percentage 0.1
    "Portion of heap growth that triggers a GC (Doc 06 A2 compat default)."))

(unless (boundp 'most-positive-fixnum)
  (defvar most-positive-fixnum 2305843009213693951
    "Largest fixnum value exposed to Elisp compatibility code."))

(unless (boundp 'most-negative-fixnum)
  (defvar most-negative-fixnum -2305843009213693952
    "Smallest fixnum value exposed to Elisp compatibility code."))

(provide 'emacs-vars)

;;; emacs-vars.el ends here
