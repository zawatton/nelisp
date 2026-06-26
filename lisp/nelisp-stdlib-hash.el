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
;;   (record 'hash-table TEST BUCKETS ENTRY-COUNT)
;;     slot 0 = TEST    : symbol — `eq' / `eql' / `equal' / `string-equal'
;;     slot 1 = BUCKETS : vector of bucket alists.  Each bucket stores
;;                        (KEY . VALUE) cons cells.
;;     slot 2 = COUNT   : number of live entries.
;;
;; The record's `type_tag' is the symbol `hash-table', so `(type-of x)'
;; on a hash-table returns `'hash-table' just like host Emacs (Doc 52
;; §2.2 — Stage 4a `type-of' special-case returns the type_tag verbatim
;; for symbol-tagged records).

;;; Code:

;;;; --- equality test dispatch -------------------------------------------

(defconst nelisp--hash-default-size 64
  "Default bucket count for generic NeLisp hash tables.")

(defconst nelisp--hash-mask #xFFFFFFFF
  "Mask used by the small pure-Elisp structural hash.")

(defun nelisp--hash-power-of-two-at-least (n)
  "Return a power-of-two bucket count at least N."
  (let ((size 1)
        (target (if (and (numberp n) (> n 0)) n nelisp--hash-default-size)))
    (while (< size target)
      (setq size (* size 2)))
    (if (< size 8) 8 size)))

(defun nelisp--hash-string (s)
  "Return a bounded hash for string S."
  (let ((h #x811C9DC5)
        (i 0)
        (len (length s)))
    (while (< i len)
      (setq h (logxor h (aref s i)))
      (setq h (logand (* h #x01000193) nelisp--hash-mask))
      (setq i (1+ i)))
    h))

(defun nelisp--hash-atom (key)
  "Return a stable bounded hash for an atom-like KEY."
  (cond
   ((null key) 0)
   ((symbolp key) (nelisp--hash-string (symbol-name key)))
   ((stringp key) (nelisp--hash-string key))
   ((numberp key) (logand key nelisp--hash-mask))
   (t (logand key nelisp--hash-mask))))

(defun nelisp--hash-key (key depth)
  "Return a bounded structural hash for KEY.
DEPTH is currently used as a compatibility guard; cons keys hash their first
pair with atom-level hashing because SMIE hot keys are usually (TOKEN . TOKEN)."
  (cond
   ((consp key)
    (logand (+ #x9E3779B9
               (* 33 (nelisp--hash-atom (car key)))
               (* 65599 (nelisp--hash-atom (cdr key))))
            nelisp--hash-mask))
   (t (nelisp--hash-atom key))))

(defun nelisp--hash-index (key buckets)
  "Return KEY's bucket index in BUCKETS."
  (let* ((count (length buckets))
         (hash (nelisp--hash-key key 6)))
    (if (= (logand count (1- count)) 0)
        (logand hash (1- count))
      (mod hash count))))

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
        (size nelisp--hash-default-size)
        (rest args))
    (while rest
      (when (eq (car rest) :test)
        (setq test (car (cdr rest))))
      (when (eq (car rest) :size)
        (setq size (car (cdr rest))))
      (setq rest (cdr (cdr rest))))
    (nelisp--make-record
     'hash-table
     test
     (make-vector (nelisp--hash-power-of-two-at-least size) nil)
     0)))

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
         (buckets (nelisp--record-ref table 1))
         (index (nelisp--hash-index key buckets))
         (bucket (aref buckets index))
         (cur bucket)
         (found nil)
         (i 0))
    (while (and cur (not found))
      (when (nelisp--hash-test-equal test (car (car cur)) key)
        (setcdr (car cur) value)
        (setq found t))
      (setq cur (cdr cur)))
    ;; Correctness fallback for key classes whose pure-Elisp hash is not stable
    ;; across equal values in the standalone runtime.
    (while (and (not found) (< i (length buckets)))
      (setq cur (aref buckets i))
      (while (and cur (not found))
        (when (nelisp--hash-test-equal test (car (car cur)) key)
          (setcdr (car cur) value)
          (setq found t))
        (setq cur (cdr cur)))
      (setq i (1+ i)))
    (unless found
      (aset buckets index (cons (cons key value) bucket))
      (nelisp--record-set table 2 (1+ (nelisp--record-ref table 2))))
    value))

(defun gethash (key table &optional default)
  "Return TABLE[KEY] or DEFAULT (default nil) when missing."
  (let* ((test (nelisp--record-ref table 0))
         (buckets (nelisp--record-ref table 1))
         (cur (aref buckets (nelisp--hash-index key buckets)))
         (result default)
         (found nil)
         (i 0))
    (while (and cur (not found))
      (when (nelisp--hash-test-equal test (car (car cur)) key)
        (setq result (cdr (car cur))
              found t))
      (setq cur (cdr cur)))
    (while (and (not found) (< i (length buckets)))
      (setq cur (aref buckets i))
      (while (and cur (not found))
        (when (nelisp--hash-test-equal test (car (car cur)) key)
          (setq result (cdr (car cur))
                found t))
        (setq cur (cdr cur)))
      (setq i (1+ i)))
    result))

(defun remhash (key table)
  "Remove the entry for KEY from TABLE.  Returns t if an entry was
removed, nil if KEY was absent.  Iterates the storage list once and
stores back a filtered copy on hit so unrelated entries keep their
identity (= consumers holding a (cons key value) cell from before the
remove see no aliasing)."
  (let* ((test (nelisp--record-ref table 0))
         (buckets (nelisp--record-ref table 1))
         (changed nil)
         (removed 0)
         (i 0))
    (while (< i (length buckets))
      (let ((entries (aref buckets i))
            (out nil)
            (cur nil))
        (setq cur entries)
        (while cur
          (if (nelisp--hash-test-equal test (car (car cur)) key)
              (setq changed t
                    removed (1+ removed))
            (setq out (cons (car cur) out)))
          (setq cur (cdr cur)))
        (when changed
          (aset buckets i (nreverse out))))
      (setq i (1+ i)))
    (when changed
      (nelisp--record-set table 2 (- (nelisp--record-ref table 2) removed)))
    (if changed t nil)))

(defun clrhash (table)
  "Remove every entry from TABLE.  Returns TABLE."
  (let ((buckets (nelisp--record-ref table 1)))
    (nelisp--record-set table 1 (make-vector (length buckets) nil)))
  (nelisp--record-set table 2 0)
  table)

(defun nelisp--hash-pairs (table)
  "Return a fresh ((KEY . VALUE) ...) list of TABLE's entries.
Each cons pair is freshly allocated so callers may mutate the spine without
affecting TABLE.  Order is bucket order and intentionally unspecified."
  (let ((buckets (nelisp--record-ref table 1))
        (i 0)
        (out nil))
    (while (< i (length buckets))
      (let ((entries (aref buckets i)))
        (while entries
          (setq out (cons (cons (car (car entries)) (cdr (car entries))) out))
          (setq entries (cdr entries))))
      (setq i (1+ i)))
    out))

;;;; --- Doc 87 §86.1.f Tier 2 wrapper: nl-secure-hash ---------------

;; Doc 87 §86.1.f Tier 2 wrapper.  2-arg out-Sexp primitive call to
;; the `nl_jit_secure_hash' trampoline in `build-tool/src/jit/hash.rs'.
;; Replaces the deleted `bi_nl_secure_hash' helper in
;; `eval/builtins.rs'.  ALGO is a symbol or string in `(sha1 sha224
;; sha256 sha384 sha512 md5)'; STRING is a string.  Returns the
;; lowercase hex digest as a string.
(fset 'nl-secure-hash
      (lambda (algo string)
        (condition-case _err
            (nl-jit-call-out-2 "nl_jit_secure_hash" algo string)
          (error
           (nelisp--signal-wrong-type
            'symbol-or-string (cons algo string))))))

;; nelisp-stdlib-hash.el ends here
