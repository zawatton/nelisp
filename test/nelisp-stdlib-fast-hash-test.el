;;; nelisp-stdlib-fast-hash-test.el --- ERT for Doc 102 Phase 2 fast hashtable  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 102 Phase 2 — pure-elisp ert tests for
;; `lisp/nelisp-stdlib-fast-hash.el'.  Validates:
;;
;;   1. FNV-1a hash function — deterministic + within-bucket-count
;;   2. Bucket-array storage layout (= power-of-2 modulo fast-path)
;;   3. get / put / remove / contains-p / count / clear semantics
;;   4. iter ordering invariants
;;   5. Sentinel `unbound-marker' identity stability
;;
;; Mock record primitives: `nelisp--make-record' / `--record-ref' /
;; `--record-set' / `--record-type' / `recordp' are normally
;; provided by `lisp/nelisp-jit-strategy.el' + the nelisp runtime.
;; In standalone Emacs (= `make test' environment) we provide a
;; tiny vector-based shim so the ert runs without invoking the
;; nelisp binary.  Storage shape: record = `(vector TAG SLOT0
;; SLOT1 ...)'; `--record-ref' uses 0-based indexing that excludes
;; the tag (= matches the runtime API from `lisp/nelisp-cl-
;; macros.el' line 371).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

;; --- mock record primitives (= match nelisp runtime semantics) ---
;;
;; Use Emacs' built-in `record' / `recordp' so the existing `aref'
;; semantics + the real `recordp' predicate apply.  Record tag
;; lives at index 0 of the underlying record object; 0-based slot
;; indexing excludes the tag (= matches the runtime API from
;; `lisp/nelisp-cl-macros.el' line 371).

(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (tag &rest slots)
    "Mock: build a record with TAG at index 0 and SLOTS following."
    (apply #'record tag slots)))

(unless (fboundp 'nelisp--record-ref)
  (defun nelisp--record-ref (rec idx)
    "Mock: 0-based slot read, excluding the tag at record index 0."
    (aref rec (1+ idx))))

(unless (fboundp 'nelisp--record-set)
  (defun nelisp--record-set (rec idx val)
    "Mock: 0-based slot write, excluding the tag."
    (aset rec (1+ idx) val)
    val))

(unless (fboundp 'nelisp--record-type)
  (defun nelisp--record-type (rec)
    "Mock: read the tag at record index 0."
    (aref rec 0)))

(require 'nelisp-stdlib-fast-hash)

;; ---- (1) hash function ----

(ert-deftest nelisp-fast-hash/hash-deterministic ()
  (should (= (nelisp--fast-hash--hash "foo" 1024)
             (nelisp--fast-hash--hash "foo" 1024)))
  (should (= (nelisp--fast-hash--hash "bar" 1024)
             (nelisp--fast-hash--hash "bar" 1024))))

(ert-deftest nelisp-fast-hash/hash-different-keys ()
  ;; Different keys should hash differently in the common case.
  ;; (Not strictly required by FNV-1a — collisions are possible
  ;; — but should hold for these short distinct names.)
  (should-not (= (nelisp--fast-hash--hash "foo" 1024)
                 (nelisp--fast-hash--hash "bar" 1024))))

(ert-deftest nelisp-fast-hash/hash-within-bucket-count ()
  (let ((bc 256))
    (dolist (s '("a" "abc" "hello" "world" "x" "y" "longerstring"))
      (let ((h (nelisp--fast-hash--hash s bc)))
        (should (>= h 0))
        (should (< h bc))))))

(ert-deftest nelisp-fast-hash/hash-non-power-of-2 ()
  ;; `mod' branch — works for arbitrary bucket counts.
  (let ((bc 100))
    (dolist (s '("aa" "bb" "cc"))
      (let ((h (nelisp--fast-hash--hash s bc)))
        (should (>= h 0))
        (should (< h bc))))))

;; ---- (2) make / introspection ----

(ert-deftest nelisp-fast-hash/make-default-bucket-count ()
  (let ((ht (nelisp--fast-hash-make)))
    (should (nelisp--fast-hash-p ht))
    (should (= (nelisp--fast-hash-bucket-count ht) 1024))
    (should (= (nelisp--fast-hash-count ht) 0))))

(ert-deftest nelisp-fast-hash/make-custom-bucket-count ()
  (let ((ht (nelisp--fast-hash-make 64)))
    (should (= (nelisp--fast-hash-bucket-count ht) 64))
    (should (= (nelisp--fast-hash-count ht) 0))))

(ert-deftest nelisp-fast-hash/p-rejects-non-record ()
  (should-not (nelisp--fast-hash-p nil))
  (should-not (nelisp--fast-hash-p 'symbol))
  (should-not (nelisp--fast-hash-p "string"))
  (should-not (nelisp--fast-hash-p 42)))

;; ---- (3) get / put basics ----

(ert-deftest nelisp-fast-hash/put-and-get ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "foo" 42)
    (should (= (nelisp--fast-hash-get ht "foo") 42))
    (should (= (nelisp--fast-hash-count ht) 1))))

(ert-deftest nelisp-fast-hash/get-missing-returns-default ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (should (eq (nelisp--fast-hash-get ht "absent") nil))
    (should (eq (nelisp--fast-hash-get ht "absent" 'sentinel)
                'sentinel))))

(ert-deftest nelisp-fast-hash/put-twice-overwrites ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "k" 1)
    (nelisp--fast-hash-put ht "k" 2)
    (should (= (nelisp--fast-hash-get ht "k") 2))
    ;; Entry count unchanged on update (= overwrites in place).
    (should (= (nelisp--fast-hash-count ht) 1))))

(ert-deftest nelisp-fast-hash/put-returns-value ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (should (= (nelisp--fast-hash-put ht "k" 99) 99))
    (should (equal (nelisp--fast-hash-put ht "k2" "v") "v"))))

(ert-deftest nelisp-fast-hash/many-keys ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (dotimes (i 100)
      (nelisp--fast-hash-put ht (format "key%d" i) i))
    (should (= (nelisp--fast-hash-count ht) 100))
    (dotimes (i 100)
      (should (= (nelisp--fast-hash-get ht (format "key%d" i)) i)))))

;; ---- (4) contains-p ----

(ert-deftest nelisp-fast-hash/contains-p ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "present" 'whatever)
    (should (nelisp--fast-hash-contains-p ht "present"))
    (should-not (nelisp--fast-hash-contains-p ht "absent"))))

(ert-deftest nelisp-fast-hash/contains-p-handles-nil-value ()
  ;; Critical edge case: a key whose value is nil must still be
  ;; reported as present.  `get' returning nil is ambiguous (=
  ;; absent vs present-with-nil); `contains-p' must use a distinct
  ;; sentinel.
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "k" nil)
    (should (nelisp--fast-hash-contains-p ht "k"))
    (should (eq (nelisp--fast-hash-get ht "k") nil))))

;; ---- (5) remove ----

(ert-deftest nelisp-fast-hash/remove-existing ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "k" 1)
    (should (nelisp--fast-hash-remove ht "k"))
    (should-not (nelisp--fast-hash-contains-p ht "k"))
    (should (= (nelisp--fast-hash-count ht) 0))))

(ert-deftest nelisp-fast-hash/remove-absent-returns-nil ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (should-not (nelisp--fast-hash-remove ht "absent"))
    (should (= (nelisp--fast-hash-count ht) 0))))

(ert-deftest nelisp-fast-hash/remove-then-put ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "k" 1)
    (nelisp--fast-hash-remove ht "k")
    (nelisp--fast-hash-put ht "k" 2)
    (should (= (nelisp--fast-hash-get ht "k") 2))
    (should (= (nelisp--fast-hash-count ht) 1))))

(ert-deftest nelisp-fast-hash/remove-preserves-siblings ()
  ;; When multiple keys hash to the same bucket, removing one
  ;; must leave the others intact (= bucket linked-list filtering
  ;; works correctly).
  (let ((ht (nelisp--fast-hash-make 4))) ; small bucket count for collisions
    (dotimes (i 20)
      (nelisp--fast-hash-put ht (format "k%d" i) i))
    (nelisp--fast-hash-remove ht "k7")
    (should-not (nelisp--fast-hash-contains-p ht "k7"))
    (dotimes (i 20)
      (unless (= i 7)
        (should (= (nelisp--fast-hash-get ht (format "k%d" i)) i))))
    (should (= (nelisp--fast-hash-count ht) 19))))

;; ---- (6) clear ----

(ert-deftest nelisp-fast-hash/clear ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "a" 1)
    (nelisp--fast-hash-put ht "b" 2)
    (nelisp--fast-hash-put ht "c" 3)
    (nelisp--fast-hash-clear ht)
    (should (= (nelisp--fast-hash-count ht) 0))
    (should-not (nelisp--fast-hash-contains-p ht "a"))
    (should-not (nelisp--fast-hash-contains-p ht "b"))
    (should-not (nelisp--fast-hash-contains-p ht "c"))
    ;; Bucket count preserved
    (should (= (nelisp--fast-hash-bucket-count ht) 16))))

;; ---- (7) iter / keys ----

(ert-deftest nelisp-fast-hash/iter-visits-all ()
  (let ((ht (nelisp--fast-hash-make 16))
        (collected nil))
    (dotimes (i 10)
      (nelisp--fast-hash-put ht (format "k%d" i) i))
    (nelisp--fast-hash-iter ht (lambda (k v) (push (cons k v) collected)))
    (should (= (length collected) 10))
    (dotimes (i 10)
      (let ((cell (assoc (format "k%d" i) collected)))
        (should cell)
        (should (= (cdr cell) i))))))

(ert-deftest nelisp-fast-hash/iter-empty ()
  (let ((ht (nelisp--fast-hash-make 16))
        (called nil))
    (nelisp--fast-hash-iter ht (lambda (_k _v) (setq called t)))
    (should-not called)))

(ert-deftest nelisp-fast-hash/keys ()
  (let ((ht (nelisp--fast-hash-make 16)))
    (nelisp--fast-hash-put ht "a" 1)
    (nelisp--fast-hash-put ht "b" 2)
    (nelisp--fast-hash-put ht "c" 3)
    (let ((keys (nelisp--fast-hash-keys ht)))
      (should (= (length keys) 3))
      (should (member "a" keys))
      (should (member "b" keys))
      (should (member "c" keys)))))

;; ---- (8) collision stress — many keys, few buckets ----

(ert-deftest nelisp-fast-hash/heavy-collision-stress ()
  ;; 200 keys / 4 buckets → ~50 entries per bucket worst case.
  ;; All operations must still produce correct results.
  (let ((ht (nelisp--fast-hash-make 4)))
    (dotimes (i 200)
      (nelisp--fast-hash-put ht (format "stress-key-%d" i) (* i 7)))
    (should (= (nelisp--fast-hash-count ht) 200))
    (dotimes (i 200)
      (should (= (nelisp--fast-hash-get ht (format "stress-key-%d" i))
                 (* i 7))))
    ;; Remove every other key
    (dotimes (i 100)
      (should (nelisp--fast-hash-remove ht
                                         (format "stress-key-%d" (* i 2)))))
    (should (= (nelisp--fast-hash-count ht) 100))
    (dotimes (i 200)
      (if (zerop (mod i 2))
          (should-not (nelisp--fast-hash-contains-p
                       ht (format "stress-key-%d" i)))
        (should (= (nelisp--fast-hash-get
                    ht (format "stress-key-%d" i))
                   (* i 7)))))))

;; ---- (9) sentinel identity ----

(ert-deftest nelisp-fast-hash/unbound-marker-is-symbol ()
  (should (symbolp nelisp--unbound-marker))
  ;; Uninterned — `(intern "nelisp--unbound-marker")' yields a
  ;; distinct symbol.
  (should-not (eq nelisp--unbound-marker
                  (intern "nelisp--unbound-marker"))))

(ert-deftest nelisp-fast-hash/unbound-marker-stable-identity ()
  ;; The marker is a defvar — repeated reads yield the same `eq'
  ;; symbol, so callers can rely on `(eq cell nelisp--unbound-
  ;; marker)' as the absence check.
  (should (eq nelisp--unbound-marker nelisp--unbound-marker)))

(provide 'nelisp-stdlib-fast-hash-test)

;;; nelisp-stdlib-fast-hash-test.el ends here
