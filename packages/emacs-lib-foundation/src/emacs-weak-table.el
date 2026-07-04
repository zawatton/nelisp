;;; emacs-weak-table.el --- Elisp weak-table approximation (Doc 06 B5) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 06 B5 option (b): an explicit-prune approximation of a weak hash table,
;; for the standalone reader where true GC-backed `:weakness' is unavailable
;; (that needs native GC work — option (a) — and hosted mode keeps the real
;; `:weakness' delegation, option (c)).
;;
;; This is NOT true weakness: entries are not dropped automatically when their
;; key becomes unreachable.  Instead the caller periodically calls
;; `emacs-weak-table-prune' with a liveness predicate; entries whose key fails
;; the predicate are removed.  This unblocks weak-hash callers (finalizer
;; registries, per-object caches) that can tolerate manual pruning.
;;
;; Keys are tracked in a parallel list rather than enumerated via `maphash':
;; the pure-elisp standalone reader's `maphash' does not iterate (see
;; `emacs-process-events--all-fds'), so a hash-only design could neither prune
;; nor enumerate.  The hash gives O(1) get/put; the list gives reliable
;; iteration.

;;; Code:

(require 'cl-lib)

(cl-defstruct (emacs-weak-table
               (:constructor emacs-weak-table--make)
               (:copier nil))
  "A manually-pruned weak-table approximation (Doc 06 B5)."
  (table nil)   ;; hash-table key -> value (strong)
  (keys  nil))  ;; list of live keys, newest first (iteration without maphash)

(defun emacs-weak-table-create (&optional test)
  "Create a weak-table approximation using TEST (default `eq') for keys."
  (emacs-weak-table--make
   :table (make-hash-table :test (or test 'eq))
   :keys nil))

(defun emacs-weak-table-count (wt)
  "Number of live entries in WT."
  (length (emacs-weak-table-keys wt)))

(defun emacs-weak-table-get (wt key &optional default)
  "Value for KEY in WT, or DEFAULT when absent."
  (gethash key (emacs-weak-table-table wt) default))

(defun emacs-weak-table-put (wt key value)
  "Set KEY -> VALUE in WT.  Returns VALUE."
  (let ((tbl (emacs-weak-table-table wt)))
    ;; A sentinel distinguishes \"absent\" from \"present with nil value\" so a
    ;; re-put of an existing key does not duplicate it in the key list.
    (when (eq (gethash key tbl 'emacs-weak-table--absent) 'emacs-weak-table--absent)
      (setf (emacs-weak-table-keys wt) (cons key (emacs-weak-table-keys wt))))
    (puthash key value tbl)
    value))

(defun emacs-weak-table-remove (wt key)
  "Remove KEY from WT.  Returns non-nil if KEY was present."
  (let ((tbl (emacs-weak-table-table wt)))
    (when (not (eq (gethash key tbl 'emacs-weak-table--absent)
                   'emacs-weak-table--absent))
      (remhash key tbl)
      (setf (emacs-weak-table-keys wt)
            (delq key (emacs-weak-table-keys wt)))
      t)))

(defun emacs-weak-table-prune (wt live-predicate)
  "Remove every entry in WT whose key fails LIVE-PREDICATE.
LIVE-PREDICATE is called with each key; entries for which it returns nil are
dropped (the weak-collection step done explicitly).  Returns the number of
entries pruned (Doc 06 B5)."
  (let ((tbl (emacs-weak-table-table wt))
        (kept nil)
        (pruned 0))
    (dolist (key (emacs-weak-table-keys wt))
      (if (funcall live-predicate key)
          (push key kept)
        (remhash key tbl)
        (setq pruned (1+ pruned))))
    ;; `kept' reverses the list; order is irrelevant for a table.
    (setf (emacs-weak-table-keys wt) (nreverse kept))
    pruned))

(provide 'emacs-weak-table)
;;; emacs-weak-table.el ends here
