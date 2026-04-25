;;; nelisp-macro-ns-test.el --- Phase 7+D nelisp-macro- namespace ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT pin for T149 / Doc 40 §3.D Phase 7+D macro expansion.
;;
;; Covers the new `nelisp-macro-' namespace surface (Doc 40 §4.4) —
;; cl-defstruct based, layered on top of `nelisp-closure', distinct
;; from the legacy `nelisp-macroexpand-*' / `nelisp--macros' APIs
;; which remain in `nelisp-macro-test.el' (Phase 1 anchor).
;;
;; Tests exercise:
;;   - struct construction / predicate / accessors
;;   - registry CRUD (define / lookup / undefine / clear)
;;   - declare form skipping at definition time
;;   - expand-1 single-step + memoize cache hit / miss accounting
;;   - expand fixpoint over chains
;;   - expand-all recursive walk into let / lambda / setq / cond
;;     / condition-case / quote (no-recur) / function (lambda only)
;;   - env (cl-macrolet) local-macro shadowing
;;   - &optional / &rest argument destructuring
;;   - pcase MVP (literal / cons / guard / unsupported error)
;;
;; Aim: 30+ tests, byte-compile clean, regression-free.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-macro-ns)
(require 'nelisp-closure)
(require 'nelisp-special-forms)

(defmacro nelisp-mns--with-fresh (&rest body)
  "Run BODY after wiping the Phase 7+D registry + cache.
Built-in macros (currently `pcase') are re-installed by
`nelisp-macro-registry-clear', so dispatch remains usable."
  (declare (indent 0))
  `(progn
     (nelisp-macro-registry-clear)
     (unwind-protect
         (progn ,@body)
       (nelisp-macro-registry-clear))))


;;; -----------------------------------------------------------------
;;; Struct construction / predicate / accessors (5 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-struct-define-returns-struct ()
  (nelisp-mns--with-fresh
    (let ((m (nelisp-macro-define 'm '(x) '(x))))
      (should (nelisp-macro-p m))
      (should (eq (nelisp-macro-name m) 'm))
      (should (equal (nelisp-macro-arglist m) '(x)))
      (should (equal (nelisp-macro-body m) '(x))))))

(ert-deftest nelisp-mns-predicate-rejects-non-struct ()
  (should-not (nelisp-macro-p nil))
  (should-not (nelisp-macro-p 42))
  (should-not (nelisp-macro-p "string"))
  (should-not (nelisp-macro-p (lambda () nil)))
  (should-not (nelisp-macro-p '(nelisp-macro foo)))
  ;; A nelisp-closure must NOT also satisfy the macro predicate.
  (should-not (nelisp-macro-p (nelisp-closure-make nil '(x) '(x)))))

(ert-deftest nelisp-mns-define-rejects-non-symbol-name ()
  (nelisp-mns--with-fresh
    (should-error (nelisp-macro-define 42 '(x) '(x))
                  :type 'nelisp-macro-error)))

(ert-deftest nelisp-mns-define-rejects-malformed-arglist ()
  (nelisp-mns--with-fresh
    (should-error (nelisp-macro-define 'm '(&rest) '(x))
                  :type 'nelisp-closure-malformed-arglist)))

(ert-deftest nelisp-mns-define-strips-leading-declare ()
  "`declare' forms at the top of the body are no-op'd at definition."
  (nelisp-mns--with-fresh
    (let ((m (nelisp-macro-define
              'm '(x)
              '((declare (indent 0) (debug t))
                (declare (doc-string 2))
                (list 'quote x)))))
      ;; Only the (list 'quote x) form remains.
      (should (equal (nelisp-macro-body m)
                     '((list 'quote x)))))))


;;; -----------------------------------------------------------------
;;; Registry CRUD (5 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-lookup-returns-nil-for-unknown ()
  (nelisp-mns--with-fresh
    (should-not (nelisp-macro-lookup 'never-defined))))

(ert-deftest nelisp-mns-lookup-after-define ()
  (nelisp-mns--with-fresh
    (let ((m (nelisp-macro-define 'm '(x) '(x))))
      (should (eq (nelisp-macro-lookup 'm) m)))))

(ert-deftest nelisp-mns-undefine-removes-entry ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'm '(x) '(x))
    (should (eq (nelisp-macro-undefine 'm) t))
    (should-not (nelisp-macro-lookup 'm))))

(ert-deftest nelisp-mns-undefine-nonexistent-returns-nil ()
  (nelisp-mns--with-fresh
    (should (eq (nelisp-macro-undefine 'never-defined) nil))))

(ert-deftest nelisp-mns-defmacro-alias-installs-same-struct-shape ()
  "`nelisp-macro-defmacro' is an alias for `nelisp-macro-define'."
  (nelisp-mns--with-fresh
    (let ((m (nelisp-macro-defmacro 'm '(x) '((list '+ x x)))))
      (should (nelisp-macro-p m))
      (should (eq (nelisp-macro-lookup 'm) m)))))

(ert-deftest nelisp-mns-registry-clear-keeps-pcase ()
  "`nelisp-macro-registry-clear' re-installs built-in `pcase'."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'foo '(x) '(x))
    (nelisp-macro-registry-clear)
    (should-not (nelisp-macro-lookup 'foo))
    (should (nelisp-macro-p (nelisp-macro-lookup 'pcase)))))


;;; -----------------------------------------------------------------
;;; expand-1 (5 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-expand-1-atom-unchanged ()
  (nelisp-mns--with-fresh
    (should (eq (nelisp-macro-expand-1 42) 42))
    (should (eq (nelisp-macro-expand-1 'foo) 'foo))
    (should (eq (nelisp-macro-expand-1 nil) nil))))

(ert-deftest nelisp-mns-expand-1-non-macro-call-unchanged ()
  (nelisp-mns--with-fresh
    (let ((form '(+ 1 2)))
      (should (equal (nelisp-macro-expand-1 form) form)))))

(ert-deftest nelisp-mns-expand-1-simple-substitution ()
  "Single-step expansion drives the body once."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'twice '(x) '((list 'progn x x)))
    (should (equal (nelisp-macro-expand-1 '(twice (foo)))
                   '(progn (foo) (foo))))))

(ert-deftest nelisp-mns-expand-1-leaves-inner-macro-call-alone ()
  "Only outermost layer expands; inner macro call survives."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'inner '(x) '(x))
    (nelisp-macro-define 'outer '(x)
                         '((list 'list (list 'quote 'inner) x)))
    (let ((expanded (nelisp-macro-expand-1 '(outer 7))))
      (should (equal expanded '(list 'inner 7))))))

(ert-deftest nelisp-mns-expand-1-with-env-shadows-global ()
  "ENV-supplied local macro takes precedence over global registry."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'm '(x) '((list 'quote 'global)))
    (let* ((local (nelisp-macro--raw-make
                   :name 'm :arglist '(x)
                   :body '((list 'quote 'local))
                   :env nil :source-marker nil))
           (env   (list (cons 'm local))))
      (should (equal (nelisp-macro-expand-1 '(m foo) env)
                     '(quote local))))))


;;; -----------------------------------------------------------------
;;; expand fixpoint (3 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-expand-fixpoint-chain ()
  "Macro A expands to (B X), B expands to (quote X) — driven to fixpoint."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'b '(x) '((list 'quote x)))
    (nelisp-macro-define 'a '(x) '((list 'b x)))
    (should (equal (nelisp-macro-expand '(a 42))
                   '(quote 42)))))

(ert-deftest nelisp-mns-expand-stops-when-head-no-longer-macro ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'm '(x) '((list '+ x 1)))
    (should (equal (nelisp-macro-expand '(m 3))
                   '(+ 3 1)))))

(ert-deftest nelisp-mns-expand-non-macro-returns-form-unchanged ()
  (nelisp-mns--with-fresh
    (let ((form '(plain-fn 1 2)))
      (should (eq (nelisp-macro-expand form) form)))))


;;; -----------------------------------------------------------------
;;; expand-all recursive walk (8 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-expand-all-atom-passthrough ()
  (nelisp-mns--with-fresh
    (should (eq (nelisp-macro-expand-all 42) 42))
    (should (eq (nelisp-macro-expand-all 'sym) 'sym))
    (should (eq (nelisp-macro-expand-all nil) nil))))

(ert-deftest nelisp-mns-expand-all-preserves-quote ()
  "`quote' bodies are literal data — do not expand inside."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'm '(x) '((list '+ x 1)))
    (should (equal (nelisp-macro-expand-all '(quote (m 2)))
                   '(quote (m 2))))))

(ert-deftest nelisp-mns-expand-all-recurs-into-let-init ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all '(let ((x (one))) x))
                   '(let ((x (quote 1))) x)))))

(ert-deftest nelisp-mns-expand-all-recurs-into-let-body ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all '(let ((y 2)) (one)))
                   '(let ((y 2)) (quote 1))))))

(ert-deftest nelisp-mns-expand-all-recurs-into-lambda-body ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all '(lambda (x) (one)))
                   '(lambda (x) (quote 1))))))

(ert-deftest nelisp-mns-expand-all-function-lambda ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all '(function (lambda () (one))))
                   '(function (lambda () (quote 1)))))))

(ert-deftest nelisp-mns-expand-all-setq-values-only ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all '(setq a (one) b 2))
                   '(setq a (quote 1) b 2)))))

(ert-deftest nelisp-mns-expand-all-cond-clauses ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all
                    '(cond ((one) (one))))
                   '(cond ((quote 1) (quote 1)))))))

(ert-deftest nelisp-mns-expand-all-condition-case ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all
                    '(condition-case e (one) (error (one))))
                   '(condition-case e (quote 1) (error (quote 1)))))))

(ert-deftest nelisp-mns-expand-all-recurs-into-call ()
  "Ordinary call form: car untouched, every cdr position walked."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 1)))
    (should (equal (nelisp-macro-expand-all '(+ (one) 2))
                   '(+ (quote 1) 2)))))


;;; -----------------------------------------------------------------
;;; &optional / &rest macros (3 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-macro-optional-default-applies ()
  (nelisp-mns--with-fresh
    ;; (defmacro m (a &optional (b 99)) (list '+ a b))
    (nelisp-macro-define 'm '(a &optional (b 99)) '((list '+ a b)))
    (should (equal (nelisp-macro-expand-1 '(m 1))   '(+ 1 99)))
    (should (equal (nelisp-macro-expand-1 '(m 1 2)) '(+ 1 2)))))

(ert-deftest nelisp-mns-macro-rest-collects-extras ()
  (nelisp-mns--with-fresh
    ;; (defmacro m (a &rest r) (cons 'list (cons a r)))
    (nelisp-macro-define 'm '(a &rest r) '((cons 'list (cons a r))))
    (should (equal (nelisp-macro-expand-1 '(m 1 2 3 4))
                   '(list 1 2 3 4)))))

(ert-deftest nelisp-mns-macro-arity-error-for-too-few-args ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'm '(a b) '((list '+ a b)))
    (should-error (nelisp-macro-expand-1 '(m 1))
                  :type 'nelisp-closure-arity-error)))


;;; -----------------------------------------------------------------
;;; Memoize cache (3 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-cache-miss-then-hit ()
  "Same FORM expanded twice: 1 miss, 1 hit."
  (nelisp-mns--with-fresh
    (nelisp-macro-cache-clear)
    (nelisp-macro-define 'm '(x) '((list 'quote x)))
    (let ((form '(m 7)))
      (nelisp-macro-expand-1 form)
      (let ((stats (nelisp-macro-cache-stats)))
        (should (= (plist-get stats :miss) 1))
        (should (= (plist-get stats :hit) 0)))
      (nelisp-macro-expand-1 form)
      (let ((stats (nelisp-macro-cache-stats)))
        (should (= (plist-get stats :miss) 1))
        (should (= (plist-get stats :hit) 1))))))

(ert-deftest nelisp-mns-cache-flushed-on-redefine ()
  "Re-defining a macro flushes the cache so old expansions vanish."
  (nelisp-mns--with-fresh
    (nelisp-macro-cache-clear)
    (nelisp-macro-define 'm '(x) '((list 'quote 'before)))
    (should (equal (nelisp-macro-expand-1 '(m foo))
                   '(quote before)))
    (nelisp-macro-define 'm '(x) '((list 'quote 'after)))
    (should (equal (nelisp-macro-expand-1 '(m foo))
                   '(quote after)))))

(ert-deftest nelisp-mns-cache-clear-resets-stats ()
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'm '(x) '((list 'quote x)))
    (nelisp-macro-expand-1 '(m foo))
    (nelisp-macro-cache-clear)
    (let ((stats (nelisp-macro-cache-stats)))
      (should (= (plist-get stats :hit) 0))
      (should (= (plist-get stats :miss) 0))
      (should (= (plist-get stats :size) 0)))))


;;; -----------------------------------------------------------------
;;; pcase MVP (5 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-pcase-literal-pattern ()
  "(pcase EXPR (1 ...)) compiles the literal arm to an `equal' test."
  (nelisp-mns--with-fresh
    (let* ((expanded (nelisp-macro-expand-1 '(pcase 1 (1 'one) (_ 'other))))
           ;; Ignore the gensym scrutinee name when comparing — its
           ;; counter is monotonic across the test run.
           (let-binding (cadr expanded))
           (scrut (caar let-binding)))
      ;; Shape: (let ((SCRUT 1)) (cond (...) (...)))
      (should (eq (car expanded) 'let))
      (should (eq (caaadr expanded) scrut))
      (should (eq (caaddr expanded) 'cond)))))

(ert-deftest nelisp-mns-pcase-literal-pattern-evaluates ()
  "End-to-end via host eval — literal pcase yields the matching arm."
  (nelisp-mns--with-fresh
    (let ((expanded (nelisp-macro-expand-1
                     '(pcase 2 (1 'one) (2 'two) (_ 'other)))))
      (should (eq (eval expanded t) 'two)))))

(ert-deftest nelisp-mns-pcase-cons-pattern-evaluates ()
  "(pcase (cons 1 2) (`(cons a b) (+ a b))) — destructured sum."
  (nelisp-mns--with-fresh
    (let ((expanded (nelisp-macro-expand-1
                     '(pcase (cons 1 2)
                        ((cons a b) (+ a b))
                        (_ 0)))))
      (should (= (eval expanded t) 3)))))

(ert-deftest nelisp-mns-pcase-guard-evaluates ()
  "Guard suppresses an otherwise-matching arm."
  (nelisp-mns--with-fresh
    (let ((expanded (nelisp-macro-expand-1
                     '(pcase 5
                        (n (guard (> n 10)) 'big)
                        (_ 'small)))))
      (should (eq (eval expanded t) 'small)))
    (let ((expanded (nelisp-macro-expand-1
                     '(pcase 42
                        (n (guard (> n 10)) 'big)
                        (_ 'small)))))
      (should (eq (eval expanded t) 'big)))))

(ert-deftest nelisp-mns-pcase-unsupported-pattern-signals ()
  "An unknown pcase pattern signals `nelisp-macro-pcase-unsupported'."
  (nelisp-mns--with-fresh
    (should-error
     (nelisp-macro-expand-1
      '(pcase x ((vector a b) 'vec) (_ 'other)))
     :type 'nelisp-macro-pcase-unsupported)))


;;; -----------------------------------------------------------------
;;; cl-macrolet style env shadowing — additional coverage (2 tests)
;;; -----------------------------------------------------------------

(ert-deftest nelisp-mns-env-shadow-with-expand-all ()
  "expand-all also honours ENV for nested macro calls."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 'global)))
    (let* ((local (nelisp-macro--raw-make
                   :name 'one :arglist '()
                   :body '((list 'quote 'local))
                   :env nil :source-marker nil))
           (env   (list (cons 'one local))))
      (should (equal (nelisp-macro-expand-all '(let ((x (one))) x) env)
                     '(let ((x (quote local))) x))))))

(ert-deftest nelisp-mns-env-empty-falls-back-to-global ()
  "Empty ENV: lookup falls back to the global registry."
  (nelisp-mns--with-fresh
    (nelisp-macro-define 'one '() '((list 'quote 'global)))
    (should (equal (nelisp-macro-expand-1 '(one) nil)
                   '(quote global)))))

(provide 'nelisp-macro-ns-test)

;;; nelisp-macro-ns-test.el ends here
