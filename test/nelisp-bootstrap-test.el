;;; nelisp-bootstrap-test.el --- Tests for nelisp-bootstrap convenience entry point  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT for `nelisp-bootstrap'.  All tests gate on `NELISP_SELF_HOST_TESTS=1'
;; same as `test/nelisp-self-host-test.el' since the bootstrap shares the
;; deep-stack requirement (= self-host walk consumes ~1300+ host frames).
;;
;; The "module loads cleanly without env var" property is enforced
;; passively: the surrounding `(require 'nelisp-bootstrap)' at the top
;; of this file runs even without NELISP_SELF_HOST_TESTS set, and CI
;; loads this file as part of `make test' — if the bare require errored
;; the entire run would fail before any `skip-unless' triggered.

;;; Code:

(require 'ert)
(require 'nelisp-bootstrap)

(defun nelisp-bootstrap-test--reset ()
  "Force the bootstrap back to the not-loaded state for the next test.
Tests do their own `nelisp-bootstrap-init' so individual ert-deftests
stay independent."
  (setq nelisp-bootstrap--bootstrapped nil)
  (nelisp--reset))

(ert-deftest nelisp-bootstrap-load-without-env-is-noop ()
  "Loading the module without calling init leaves the predicate nil."
  (nelisp-bootstrap-test--reset)
  (should (null (nelisp-bootstrap-bootstrapped-p))))

(ert-deftest nelisp-bootstrap-init-is-idempotent ()
  "Calling `nelisp-bootstrap-init' twice does not error and stays t."
  (skip-unless (getenv "NELISP_SELF_HOST_TESTS"))
  (nelisp-bootstrap-test--reset)
  (should (eq t (nelisp-bootstrap-init)))
  (should (nelisp-bootstrap-bootstrapped-p))
  ;; Second call short-circuits — must not re-load files, must return t.
  (should (eq t (nelisp-bootstrap-init)))
  (should (nelisp-bootstrap-bootstrapped-p)))

(ert-deftest nelisp-bootstrap-predicate-tracks-state ()
  "`nelisp-bootstrap-bootstrapped-p' reflects init / reset state."
  (skip-unless (getenv "NELISP_SELF_HOST_TESTS"))
  (nelisp-bootstrap-test--reset)
  (should (null (nelisp-bootstrap-bootstrapped-p)))
  (nelisp-bootstrap-init)
  (should (nelisp-bootstrap-bootstrapped-p))
  (nelisp-bootstrap-test--reset)
  (should (null (nelisp-bootstrap-bootstrapped-p))))

(ert-deftest nelisp-bootstrap-experiment-2-forms-evaluate ()
  "All six experiment-2 demo forms evaluate to expected values via the public API."
  (skip-unless (getenv "NELISP_SELF_HOST_TESTS"))
  (nelisp-bootstrap-test--reset)
  (nelisp-bootstrap-init)
  ;; Partial-bootstrap-only forms (= already worked pre-bootstrap-module).
  (should (equal 6 (nelisp-eval '(+ 1 2 3))))
  (should (equal 56 (nelisp-eval '(* 7 8))))
  (should (equal 25 (nelisp-eval '(let ((x 5)) (* x x)))))
  (should (equal 'yes (nelisp-eval '(if (< 3 5) 'yes 'no))))
  ;; Full-bootstrap-required forms (= the regression target).
  (should (equal 142
                 (nelisp-eval '(funcall (lambda (n) (+ n 100)) 42))))
  (should (equal '(1 4 9 16 25)
                 (nelisp-eval '(mapcar (lambda (x) (* x x))
                                       '(1 2 3 4 5))))))

(ert-deftest nelisp-bootstrap-eval-wrapper-auto-inits ()
  "`nelisp-bootstrap-eval' bootstraps lazily on first call."
  (skip-unless (getenv "NELISP_SELF_HOST_TESTS"))
  (nelisp-bootstrap-test--reset)
  (should (null (nelisp-bootstrap-bootstrapped-p)))
  (should (equal 49 (nelisp-bootstrap-eval '(funcall (lambda (n) (* n n)) 7))))
  (should (nelisp-bootstrap-bootstrapped-p)))

(provide 'nelisp-bootstrap-test)

;;; nelisp-bootstrap-test.el ends here
