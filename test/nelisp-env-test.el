;;; nelisp-env-test.el --- ERT for Doc 102 Phase 2 nelisp-env  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 102 Phase 2 — pure-elisp ert tests for `lisp/nelisp-env.el'.
;; Validates the 11 elisp-side env operations against the
;; behaviour contract of the matching Rust `env.rs::Env::*'
;; methods.  Phase 2.a tests the elisp surface standalone — no
;; Rust env_shim routing yet.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

;; Same record-primitive mocks as the fast-hash test file (=
;; runs standalone in `make test' without the nelisp binary).

(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (tag &rest slots)
    (apply #'record tag slots)))

(unless (fboundp 'nelisp--record-ref)
  (defun nelisp--record-ref (rec idx)
    (aref rec (1+ idx))))

(unless (fboundp 'nelisp--record-set)
  (defun nelisp--record-set (rec idx val)
    (aset rec (1+ idx) val)
    val))

(unless (fboundp 'nelisp--record-type)
  (defun nelisp--record-type (rec)
    (aref rec 0)))

(require 'nelisp-env)

;; ---- env construction ----

(ert-deftest nelisp-env/make-empty ()
  (let ((env (nelisp-env-make)))
    (should (nelisp-env-p env))
    (should (= (nelisp-env-count env) 0))))

(ert-deftest nelisp-env/p-rejects-non-record ()
  (should-not (nelisp-env-p nil))
  (should-not (nelisp-env-p 'symbol))
  (should-not (nelisp-env-p "string"))
  ;; A bare fast-hash-table is not an env (= different tag).
  (should-not (nelisp-env-p (nelisp--fast-hash-make))))

;; ---- symbol-entry construction + accessors ----

(ert-deftest nelisp-env/symbol-entry-default-is-unbound ()
  (let ((e (nelisp-env--make-symbol-entry)))
    (should (nelisp-env--symbol-entry-p e))
    (should (eq (nelisp-env--symbol-entry-value e)
                nelisp--unbound-marker))
    (should (eq (nelisp-env--symbol-entry-function e)
                nelisp--unbound-marker))
    (should (eq (nelisp-env--symbol-entry-plist e) nil))
    (should-not (nelisp-env--symbol-entry-constant-p e))))

(ert-deftest nelisp-env/symbol-entry-fully-populated ()
  (let ((e (nelisp-env--make-symbol-entry 42 'my-func '(prop1 a) t)))
    (should (= (nelisp-env--symbol-entry-value e) 42))
    (should (eq (nelisp-env--symbol-entry-function e) 'my-func))
    (should (equal (nelisp-env--symbol-entry-plist e) '(prop1 a)))
    (should (nelisp-env--symbol-entry-constant-p e))))

;; ---- value cell ops ----

(ert-deftest nelisp-env/set-value-then-lookup ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-value env "x" 42)
    (should (= (nelisp-env-lookup-value env "x") 42))
    (should (= (nelisp-env-count env) 1))))

(ert-deftest nelisp-env/set-value-twice-overwrites ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-value env "x" 1)
    (nelisp-env-set-value env "x" 2)
    (should (= (nelisp-env-lookup-value env "x") 2))
    (should (= (nelisp-env-count env) 1))))

(ert-deftest nelisp-env/lookup-value-absent-signals ()
  (let ((env (nelisp-env-make)))
    (should-error (nelisp-env-lookup-value env "missing")
                  :type 'void-variable)))

(ert-deftest nelisp-env/clear-value-then-lookup-signals ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-value env "x" 99)
    (nelisp-env-clear-value env "x")
    (should-error (nelisp-env-lookup-value env "x")
                  :type 'void-variable)
    ;; Entry still exists but value cell holds the sentinel.
    (should (= (nelisp-env-count env) 1))))

(ert-deftest nelisp-env/value-of-nil-roundtrips ()
  ;; Critical edge case: setting a value to nil must succeed and
  ;; lookup must return nil (= different from "absent").
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-value env "x" nil)
    (should (eq (nelisp-env-lookup-value env "x") nil))
    (should (nelisp-env-is-bound env "x"))))

;; ---- function cell ops ----

(ert-deftest nelisp-env/set-function-then-lookup ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-function env "f" '(lambda (x) x))
    (should (equal (nelisp-env-lookup-function env "f")
                   '(lambda (x) x)))))

(ert-deftest nelisp-env/lookup-function-absent-signals ()
  (let ((env (nelisp-env-make)))
    (should-error (nelisp-env-lookup-function env "missing")
                  :type 'void-function)))

(ert-deftest nelisp-env/clear-function-then-lookup-signals ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-function env "f" 'placeholder)
    (nelisp-env-clear-function env "f")
    (should-error (nelisp-env-lookup-function env "f")
                  :type 'void-function)))

;; ---- value / function cells are independent ----

(ert-deftest nelisp-env/value-and-function-cells-independent ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-value env "name" 42)
    (should (= (nelisp-env-lookup-value env "name") 42))
    (should-error (nelisp-env-lookup-function env "name")
                  :type 'void-function)
    (nelisp-env-set-function env "name" 'thefn)
    (should (= (nelisp-env-lookup-value env "name") 42))
    (should (eq (nelisp-env-lookup-function env "name") 'thefn))
    ;; Clearing one cell doesn't disturb the other.
    (nelisp-env-clear-value env "name")
    (should-error (nelisp-env-lookup-value env "name")
                  :type 'void-variable)
    (should (eq (nelisp-env-lookup-function env "name") 'thefn))))

;; ---- is-bound / is-fbound predicates ----

(ert-deftest nelisp-env/is-bound ()
  (let ((env (nelisp-env-make)))
    (should-not (nelisp-env-is-bound env "x"))
    (nelisp-env-set-value env "x" 0)
    (should (nelisp-env-is-bound env "x"))
    (nelisp-env-clear-value env "x")
    (should-not (nelisp-env-is-bound env "x"))))

(ert-deftest nelisp-env/is-fbound ()
  (let ((env (nelisp-env-make)))
    (should-not (nelisp-env-is-fbound env "f"))
    (nelisp-env-set-function env "f" 'whatever)
    (should (nelisp-env-is-fbound env "f"))
    (nelisp-env-clear-function env "f")
    (should-not (nelisp-env-is-fbound env "f"))))

(ert-deftest nelisp-env/is-bound-distinguishes-from-fbound ()
  ;; A value-cell binding doesn't imply a function binding.
  (let ((env (nelisp-env-make)))
    (nelisp-env-set-value env "name" 1)
    (should (nelisp-env-is-bound env "name"))
    (should-not (nelisp-env-is-fbound env "name"))))

;; ---- defvar / defconst ----

(ert-deftest nelisp-env/defvar-fresh-symbol ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-defvar env "fresh" 42)
    (should (= (nelisp-env-lookup-value env "fresh") 42))))

(ert-deftest nelisp-env/defvar-idempotent ()
  ;; defvar on an already-bound name preserves the existing value
  ;; (= Elisp `defvar' re-eval semantics).
  (let ((env (nelisp-env-make)))
    (nelisp-env-defvar env "x" 1)
    (nelisp-env-defvar env "x" 999)  ; second call is no-op
    (should (= (nelisp-env-lookup-value env "x") 1))))

(ert-deftest nelisp-env/defvar-after-clear-rebinds ()
  ;; After `clear-value', `defvar' resumes binding (= the cell
  ;; holds the sentinel which `defvar' detects).
  (let ((env (nelisp-env-make)))
    (nelisp-env-defvar env "x" 1)
    (nelisp-env-clear-value env "x")
    (nelisp-env-defvar env "x" 2)
    (should (= (nelisp-env-lookup-value env "x") 2))))

(ert-deftest nelisp-env/defconst-marks-constant ()
  (let ((env (nelisp-env-make)))
    (nelisp-env-defvar env "c" 42 t)
    (should (= (nelisp-env-lookup-value env "c") 42))
    ;; setq on a constant signals.
    (should-error (nelisp-env-set-value env "c" 99)
                  :type 'setting-constant)
    ;; Value preserved after the failed setq attempt.
    (should (= (nelisp-env-lookup-value env "c") 42))))

(ert-deftest nelisp-env/defconst-preserves-constant-flag-on-redefvar ()
  ;; Once a name is `defconst' it stays constant — a subsequent
  ;; `defvar' (without explicit constant flag) does not unset it.
  (let ((env (nelisp-env-make)))
    (nelisp-env-defvar env "c" 1 t)
    ;; idempotent defvar — flag preserved.
    (nelisp-env-defvar env "c" 2)
    (should-error (nelisp-env-set-value env "c" 99)
                  :type 'setting-constant)))

;; ---- env scale stress ----

(ert-deftest nelisp-env/many-symbols ()
  (let ((env (nelisp-env-make 64)))
    (dotimes (i 500)
      (nelisp-env-set-value env (format "sym%d" i) (* i 11)))
    (should (= (nelisp-env-count env) 500))
    (dotimes (i 500)
      (should (= (nelisp-env-lookup-value env (format "sym%d" i))
                 (* i 11))))))

(provide 'nelisp-env-test)

;;; nelisp-env-test.el ends here
