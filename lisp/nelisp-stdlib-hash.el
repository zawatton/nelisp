;;; nelisp-stdlib-hash.el --- Elisp hash-table on top of Sexp::Record  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 50 stage 4f — re-implements `make-hash-table' / `hash-table-p' /
;; `puthash' / `gethash' / `remhash' / `clrhash' / `nelisp--hash-pairs'
;; in elisp on top of the Stage 4c record primitives.  Replaces the
;; previous Rust `Sexp::HashTable' variant + ~290 LOC of bi_* function
;; bodies; the Rust dispatch arms become unreachable once this file
;; loads (= function-cell override) and are removed in the matching
;; Rust commit.
;;
;; Storage layout (record):
;;   (record 'hash-table TEST ENTRIES)
;;     slot 0 = TEST    : symbol — `eq' / `eql' / `equal' / `string-equal'
;;     slot 1 = ENTRIES : list of (KEY . VALUE) cons cells.  Newest
;;                        entries are at the head of the list (= cheap
;;                        cons-prepend on insert).  `nelisp--hash-pairs'
;;                        reverses to return insertion order, matching
;;                        the prior Rust behaviour.
;;
;; The record's `type_tag' is the symbol `hash-table', so `(type-of x)'
;; on a hash-table returns `'hash-table' just like host Emacs (Doc 52
;; §2.2 — Stage 4a `type-of' special-case returns the type_tag verbatim
;; for symbol-tagged records).

;;; Code:

;;;; --- equality test dispatch -------------------------------------------

(defun nelisp--hash-test-equal (test a b)
  "Return non-nil if A and B compare equal under hash-table TEST.
TEST is the test symbol carried in slot 0.  Falls back to `equal'
for any unrecognised name (= matches host Emacs's `define-hash-table-
test' default for unknown symbols)."
  (cond ((eq test 'eq) (eq a b))
        ((eq test 'eql) (eql a b))
        ((eq test 'equal) (equal a b))
        ((eq test 'string-equal) (string-equal a b))
        (t (equal a b))))

;;;; --- core API ---------------------------------------------------------

(defun make-hash-table (&rest args)
  "Build a fresh hash-table, returning the new record.

Accepted keyword arguments (in any order):
  :test SYMBOL    equality test (default `eql').  Recognised:
                  `eq' / `eql' / `equal' / `string-equal'.
  :size N         size hint, accepted but ignored — linear-scan
                  storage holds tens of entries for substrate use.
  :rehash-size, :rehash-threshold, :weakness, :data
                  accepted but ignored (parity with host Emacs)."
  (let ((test 'eql)
        (rest args))
    (while rest
      (when (eq (car rest) :test)
        (setq test (car (cdr rest))))
      (setq rest (cdr (cdr rest))))
    (nelisp--make-record 'hash-table test nil)))

(defun hash-table-p (obj)
  "Return t if OBJ is a hash-table record, else nil."
  (and (recordp obj)
       (eq (nelisp--record-type obj) 'hash-table)))

(defun puthash (key value table)
  "Set TABLE[KEY] = VALUE.  Existing entries with the same key under
TABLE's equality test are overwritten in-place via `setcdr' — the
internal cons cell identity is preserved.  New entries are prepended
to the storage list.  Returns VALUE."
  (let* ((test (nelisp--record-ref table 0))
         (entries (nelisp--record-ref table 1))
         (cur entries)
         (found nil))
    (while (and cur (not found))
      (when (nelisp--hash-test-equal test (car (car cur)) key)
        (setcdr (car cur) value)
        (setq found t))
      (setq cur (cdr cur)))
    (unless found
      (nelisp--record-set table 1 (cons (cons key value) entries)))
    value))

(defun gethash (key table &optional default)
  "Return TABLE[KEY] or DEFAULT (default nil) when missing."
  (let* ((test (nelisp--record-ref table 0))
         (entries (nelisp--record-ref table 1))
         (cur entries)
         (result default)
         (found nil))
    (while (and cur (not found))
      (when (nelisp--hash-test-equal test (car (car cur)) key)
        (setq result (cdr (car cur))
              found t))
      (setq cur (cdr cur)))
    result))

(defun remhash (key table)
  "Remove the entry for KEY from TABLE.  Returns t if an entry was
removed, nil if KEY was absent.  Iterates the storage list once and
stores back a filtered copy on hit so unrelated entries keep their
identity (= consumers holding a (cons key value) cell from before the
remove see no aliasing)."
  (let* ((test (nelisp--record-ref table 0))
         (entries (nelisp--record-ref table 1))
         (out nil)
         (changed nil)
         (cur entries))
    (while cur
      (if (nelisp--hash-test-equal test (car (car cur)) key)
          (setq changed t)
        (setq out (cons (car cur) out)))
      (setq cur (cdr cur)))
    (when changed
      (nelisp--record-set table 1 (nreverse out)))
    (if changed t nil)))

(defun clrhash (table)
  "Remove every entry from TABLE.  Returns TABLE."
  (nelisp--record-set table 1 nil)
  table)

(defun nelisp--hash-pairs (table)
  "Return a fresh ((KEY . VALUE) ...) list of TABLE's entries in
insertion order.  Each cons pair is freshly allocated so callers may
mutate the spine without affecting TABLE.  Matches the prior
`bi_hash_pairs' contract used by `hash-table-count' / `maphash' /
`hash-table-keys' / `hash-table-values' (lisp/nelisp-stdlib-misc.el)."
  (let ((entries (nelisp--record-ref table 1))
        (out nil))
    (while entries
      (setq out (cons (cons (car (car entries)) (cdr (car entries))) out))
      (setq entries (cdr entries)))
    out))

;; nelisp-stdlib-hash.el ends here
