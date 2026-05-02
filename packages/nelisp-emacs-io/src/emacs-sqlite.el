;;; emacs-sqlite.el --- NeLisp port of Emacs sqlite.c name-bridging  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Emacs 29's C core ships SQLite under the unprefixed `sqlite-*'
;; names.  NeLisp's `nelisp-sqlite-rs' extension crate provides the
;; same functionality under the `nelisp-sqlite-*' prefix to avoid
;; clashing with host-Emacs symbols when NeLisp is loaded inside
;; Emacs.  This file forwards the unprefixed names to the prefixed
;; ones so any anvil module that calls `(sqlite-open PATH)' works
;; unchanged on either runtime.
;;
;; Each forwarder is gated on the unprefixed name being unbound;
;; under regular Emacs the C-core `sqlite-*' wins and the
;; forwarders are no-ops.

;;; Code:

(unless (fboundp 'sqlite-open)
  (defun sqlite-open (path)
    "Forward to `nelisp-sqlite-open'."
    (nelisp-sqlite-open path)))

(unless (fboundp 'sqlite-close)
  (defun sqlite-close (db)
    "Forward to `nelisp-sqlite-close'."
    (nelisp-sqlite-close db)))

(unless (fboundp 'sqlite-execute)
  (defun sqlite-execute (db query &optional values)
    "Forward to `nelisp-sqlite-execute'."
    (nelisp-sqlite-execute db query values)))

(unless (fboundp 'sqlite-select)
  (defun sqlite-select (db query &optional values return-type)
    "Forward to `nelisp-sqlite-select'."
    (nelisp-sqlite-select db query values return-type)))

(unless (fboundp 'sqlitep)
  (defun sqlitep (object)
    "Forward to `nelisp-sqlitep'."
    (nelisp-sqlitep object)))

(unless (fboundp 'sqlite-pragma)
  (defun sqlite-pragma (db pragma-clause)
    "Forward to `nelisp-sqlite-pragma'."
    (nelisp-sqlite-pragma db pragma-clause)))

(unless (fboundp 'sqlite-transaction)
  (defun sqlite-transaction (db)
    "Forward to `nelisp-sqlite-transaction'."
    (nelisp-sqlite-transaction db)))

(unless (fboundp 'sqlite-commit)
  (defun sqlite-commit (db)
    "Forward to `nelisp-sqlite-commit'."
    (nelisp-sqlite-commit db)))

(unless (fboundp 'sqlite-rollback)
  (defun sqlite-rollback (db)
    "Forward to `nelisp-sqlite-rollback'."
    (nelisp-sqlite-rollback db)))

(unless (fboundp 'sqlite-available-p)
  (defun sqlite-available-p ()
    "Return non-nil when the NeLisp SQLite extension is loaded."
    (if (fboundp 'nelisp-sqlite-available-p)
        (nelisp-sqlite-available-p)
      nil)))

(unless (fboundp 'sqlite-supports-trigram-p)
  (defun sqlite-supports-trigram-p (&optional db)
    "NeLisp does not yet expose a trigram-presence probe; return nil so
callers fall back to the unicode61 / porter tokenizer."
    (ignore db)
    nil))


(provide 'emacs-sqlite)

;;; emacs-sqlite.el ends here
