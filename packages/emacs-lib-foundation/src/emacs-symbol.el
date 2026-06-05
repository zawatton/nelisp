;;; emacs-symbol.el --- NeLisp port of Emacs C core symbol property API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2.1 — Layer 2.
;;
;; Ports the symbol property-list accessors (`put', `get',
;; `symbol-plist', `setplist') from Emacs's C core.  These are
;; foundational — many subr.el / cl-lib helpers store metadata via
;; them, and `define-error' (= our own polyfill) needs `put' to land
;; the error-conditions list on the symbol.
;;
;; Polyfill strategy: maintain a Lisp-side hash table mapping
;; (SYMBOL → PLIST).  This avoids depending on a NeLisp builtin that
;; mutates the symbol's intrinsic property cell — bootstrap eval
;; does not yet expose one.  All accessors operate on this table.

;;; Code:

(defvar emacs-symbol--plist-table (make-hash-table :test 'eq)
  "Hash table mapping symbol → property list (plist).
Used by the `put' / `get' polyfills to store symbol metadata when
the underlying NeLisp runtime has no native property-cell.")

(unless (fboundp 'put)
  (defun put (symbol property value)
    "Store VALUE under PROPERTY in SYMBOL's plist.  Returns VALUE."
    (let ((current (gethash symbol emacs-symbol--plist-table)))
      (puthash symbol (plist-put (or current nil) property value)
               emacs-symbol--plist-table)
      value)))

(unless (fboundp 'get)
  (defun get (symbol property)
    "Return the value stored under PROPERTY in SYMBOL's plist, or nil."
    (plist-get (gethash symbol emacs-symbol--plist-table) property)))

(unless (fboundp 'symbol-plist)
  (defun symbol-plist (symbol)
    "Return SYMBOL's full property list, or nil."
    (gethash symbol emacs-symbol--plist-table)))

(unless (fboundp 'setplist)
  (defun setplist (symbol new-plist)
    "Replace SYMBOL's property list with NEW-PLIST."
    (puthash symbol new-plist emacs-symbol--plist-table)
    new-plist))


(unless (fboundp 'intern-soft)
  (defun intern-soft (name &optional _obarray)
    (intern (if (symbolp name) (symbol-name name) name))))

(provide 'emacs-symbol)

;;; emacs-symbol.el ends here
