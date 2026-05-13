;;; nelisp-stdlib-fast-hash.el --- O(1) bucket-array hashtable  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 102 Phase 2 (= env.rs → elisp migration foundation).
;;
;; The existing `nelisp-stdlib-hash.el' (Doc 50 stage 4f) uses
;; linear-scan alist storage suitable for "tens of entries" — the
;; user-visible `make-hash-table' / `puthash' / `gethash' API for
;; substrate-level use.  This file ships a *high-throughput*
;; alternative for hot-path tables (= the env.rs globals table that
;; the Doc 102 Phase 2-8 migration moves into elisp).
;;
;; Storage layout (`Sexp::Record'):
;;   (record 'fast-hash-table BUCKET-COUNT BUCKETS ENTRY-COUNT)
;;     slot 0 = BUCKET-COUNT : positive integer (= power-of-2)
;;     slot 1 = BUCKETS      : `Sexp::Vector' length BUCKET-COUNT;
;;                             each element is an alist of
;;                             (KEY . VALUE) cells; nil = empty bucket
;;     slot 2 = ENTRY-COUNT  : positive integer — total live
;;                             entries across all buckets
;;
;; Performance characteristics (= §2.2 of Doc 102):
;;   With BUCKET-COUNT 1024 and 2k entries the avg lookup chain
;;   length is 2 cons cells (= 100x faster than the existing
;;   nelisp-stdlib-hash.el linear scan).  Worst case (= all keys
;;   hash to one bucket) degrades to O(N) but is adversarial.
;;
;; Hash function: FNV-1a 32-bit, implemented in pure elisp.  Phase
;; 47 promotion (= compile the hash inner loop to a `.o') is
;; deferred to Doc 103+ optimisation work — Phase 2 ships only the
;; elisp surface so a future ABI swap can refactor freely.
;;
;; Keys: strings (= symbol names in the env table use case).  The
;; hash function iterates characters via `aref'; multi-byte / wide
;; codepoints fold into the FNV state via the same fold-32-bit
;; loop and don't need separate encoding.  Equality uses `string='
;; — same convention as the Rust env table's `HashMap<String, T>'.
;;
;; Doc 102 sentinel: `nelisp--unbound-marker' is a fresh symbol
;; that no user code interns; it serves as the `None' equivalent
;; for the env table's Option<Sexp> cells.  Defined here so
;; downstream `lisp/nelisp-env.el' can share the marker without a
;; circular dependency.

;;; Code:

;; FNV-1a 32-bit hash constants.  Standard values, public-domain.
(defconst nelisp--fast-hash--fnv-offset-basis #x811C9DC5
  "FNV-1a 32-bit offset basis (= initial hash state).")

(defconst nelisp--fast-hash--fnv-prime #x01000193
  "FNV-1a 32-bit prime multiplier.")

(defconst nelisp--fast-hash--32bit-mask #xFFFFFFFF
  "Mask to keep the hash state within 32 bits across multiplies.
Elisp integers are at least 60 bits so the intermediate
multiply can grow large; mask after each iteration.")

;; Sentinel for "absent" / "Option::None" cells.  Single shared
;; symbol (= same `eq' identity throughout the env table's life).
;; Re-exported by `lisp/nelisp-env.el' as the canonical None
;; marker.
(defvar nelisp--unbound-marker (make-symbol "nelisp--unbound-marker")
  "Sentinel value for absent SymbolEntry cells in the env table.
A fresh uninterned symbol, so user code that interns the name
\"nelisp--unbound-marker\" via `(intern ...)' yields a different
symbol and can't collide with the sentinel.  See Doc 102 §2.1 +
§5.3.")

(defun nelisp--fast-hash--hash (str bucket-count)
  "Hash STR via FNV-1a 32-bit; modulo BUCKET-COUNT.
BUCKET-COUNT should be a power of 2 so the modulo collapses to a
fast `logand'.  Doc 102 §2.2 hash function spec."
  (let ((h nelisp--fast-hash--fnv-offset-basis)
        (i 0)
        (len (length str)))
    (while (< i len)
      (setq h (logxor h (aref str i)))
      (setq h (logand (* h nelisp--fast-hash--fnv-prime)
                      nelisp--fast-hash--32bit-mask))
      (setq i (1+ i)))
    ;; Power-of-2 fast-path: BUCKET-COUNT & (BUCKET-COUNT - 1) == 0
    ;; means we can `logand H (1- bucket-count)' instead of `mod'.
    ;; `mod' works for the general case (= callers may pass a
    ;; non-power-of-2 bucket count for testing).
    (if (zerop (logand bucket-count (1- bucket-count)))
        (logand h (1- bucket-count))
      (mod h bucket-count))))

(defun nelisp--fast-hash-make (&optional bucket-count)
  "Build a fresh empty fast-hash-table with BUCKET-COUNT buckets.
BUCKET-COUNT defaults to 1024; non-power-of-2 values are
accepted but lose the hash-modulo fast path.  Returns the new
`Sexp::Record' with type `fast-hash-table'."
  (let* ((bc (or bucket-count 1024))
         (buckets (make-vector bc nil)))
    (nelisp--make-record 'fast-hash-table bc buckets 0)))

(defun nelisp--fast-hash-p (obj)
  "Return non-nil if OBJ is a fast-hash-table record."
  (and (recordp obj)
       (eq (nelisp--record-type obj) 'fast-hash-table)))

(defun nelisp--fast-hash-count (ht)
  "Return the live entry count of fast-hash-table HT."
  (nelisp--record-ref ht 2))

(defun nelisp--fast-hash-bucket-count (ht)
  "Return the bucket array size of fast-hash-table HT."
  (nelisp--record-ref ht 0))

(defun nelisp--fast-hash-get (ht key &optional default)
  "Return HT[KEY] or DEFAULT (default nil) when KEY is absent.
O(1) avg via bucket-array + within-bucket linear scan.  Doc 102
§2.2 lookup body."
  (let* ((bc (nelisp--record-ref ht 0))
         (buckets (nelisp--record-ref ht 1))
         (idx (nelisp--fast-hash--hash key bc))
         (bucket (aref buckets idx))
         (cur bucket)
         (found-val default)
         (found nil))
    (while (and cur (not found))
      (when (string= (car (car cur)) key)
        (setq found-val (cdr (car cur))
              found t))
      (setq cur (cdr cur)))
    found-val))

(defun nelisp--fast-hash-contains-p (ht key)
  "Return non-nil iff KEY has a value in HT."
  ;; Distinct sentinel so `get' returning nil for a present-but-nil
  ;; entry is disambiguated from absence.  Allocates one cons per
  ;; call; cheap enough for substrate use.
  (let ((probe (cons nil nil)))
    (not (eq (nelisp--fast-hash-get ht key probe) probe))))

(defun nelisp--fast-hash-put (ht key value)
  "Set HT[KEY] = VALUE.  Returns VALUE.
If KEY exists, mutates the existing cell via `setcdr' (= the
inner cons cell identity is preserved, so consumers holding a
reference see the new value).  Otherwise prepends a new
(KEY . VALUE) cell to the bucket's list and bumps the entry
count."
  (let* ((bc (nelisp--record-ref ht 0))
         (buckets (nelisp--record-ref ht 1))
         (idx (nelisp--fast-hash--hash key bc))
         (bucket (aref buckets idx))
         (cur bucket)
         (found nil))
    (while (and cur (not found))
      (when (string= (car (car cur)) key)
        (setcdr (car cur) value)
        (setq found t))
      (setq cur (cdr cur)))
    (unless found
      (aset buckets idx (cons (cons key value) bucket))
      (nelisp--record-set
       ht 2 (1+ (nelisp--record-ref ht 2))))
    value))

(defun nelisp--fast-hash-remove (ht key)
  "Remove the entry for KEY from HT.  Returns t if an entry was
removed, nil if KEY was absent.  Iterates the bucket once and
stores back a filtered copy on hit so unrelated entries keep
their cons identity (= consumers holding a (KEY . VALUE) cell
from before the remove see no aliasing)."
  (let* ((bc (nelisp--record-ref ht 0))
         (buckets (nelisp--record-ref ht 1))
         (idx (nelisp--fast-hash--hash key bc))
         (bucket (aref buckets idx))
         (out nil)
         (changed nil)
         (cur bucket))
    (while cur
      (if (string= (car (car cur)) key)
          (setq changed t)
        (setq out (cons (car cur) out)))
      (setq cur (cdr cur)))
    (when changed
      (aset buckets idx (nreverse out))
      (nelisp--record-set
       ht 2 (1- (nelisp--record-ref ht 2))))
    changed))

(defun nelisp--fast-hash-clear (ht)
  "Drop every entry from HT.  Reinitialises the buckets vector +
zeroes the entry count.  Bucket count is preserved."
  (let* ((bc (nelisp--record-ref ht 0))
         (buckets (make-vector bc nil)))
    (nelisp--record-set ht 1 buckets)
    (nelisp--record-set ht 2 0)
    ht))

(defun nelisp--fast-hash-iter (ht fn)
  "Call FN with (KEY VALUE) for every entry in HT.  Iteration
order is bucket-ascending + within-bucket prepend-order (= newest
entries first).  Returns nil; FN's return values are discarded."
  (let* ((bc (nelisp--record-ref ht 0))
         (buckets (nelisp--record-ref ht 1))
         (i 0))
    (while (< i bc)
      (let ((cur (aref buckets i)))
        (while cur
          (funcall fn (car (car cur)) (cdr (car cur)))
          (setq cur (cdr cur))))
      (setq i (1+ i)))
    nil))

(defun nelisp--fast-hash-keys (ht)
  "Return a list of every key in HT.  Order matches
`nelisp--fast-hash-iter'."
  (let ((acc nil))
    (nelisp--fast-hash-iter
     ht (lambda (k _v) (setq acc (cons k acc))))
    (nreverse acc)))

(provide 'nelisp-stdlib-fast-hash)

;;; nelisp-stdlib-fast-hash.el ends here
