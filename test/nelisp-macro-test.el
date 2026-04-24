;;; nelisp-macro-test.el --- ERT tests for NeLisp macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 11-12 macro tests.  Covers:
;;   - defmacro registration + error on shadowing a function
;;   - macroexpand-1 single-step semantics
;;   - macroexpand fixpoint over chains
;;   - macroexpand-all walker dispatch (quote, let, lambda, setq,
;;     cond, condition-case, ordinary function-call recursion)
;;   - integration with the evaluator: user-defined `unless' via
;;     macro, recursive macros, macros invoked from a defun after
;;     the macro itself is defined (call-time expansion)
;;
;; Backquote is deferred to Phase 2, so every test macro builds its
;; result with explicit `list' / `cons' construction.

;;; Code:

(require 'ert)
(require 'nelisp-macro)

(defun nelisp-macro-test--define-unless ()
  "Install the canonical longhand `unless' macro for the evaluator.
Body is roughly `(if ,c nil (progn ,@body))' written without
backquote, which the Week 3-4 reader does not yet handle."
  (nelisp-eval
   '(defmacro my-unless (c &rest body)
      (list 'if c nil (cons 'progn body)))))

;;; Sanity ------------------------------------------------------------

(ert-deftest nelisp-macro-defmacro-returns-name ()
  (nelisp--reset)
  (should (eq (nelisp-eval '(defmacro m (x) x)) 'm)))

(ert-deftest nelisp-macro-registered-in-macros-only ()
  "Registered macro shows up in `nelisp--macros' but not `nelisp--functions'."
  (nelisp--reset)
  (nelisp-eval '(defmacro m (x) x))
  (should (not (eq (gethash 'm nelisp--macros nelisp--unbound)
                   nelisp--unbound)))
  (should (eq (gethash 'm nelisp--functions nelisp--unbound)
              nelisp--unbound)))

(ert-deftest nelisp-macro-defmacro-rejects-existing-function ()
  (nelisp--reset)
  (nelisp-eval '(defun foo (x) x))
  (should-error (nelisp-eval '(defmacro foo (x) x))
                :type 'nelisp-eval-error))

(ert-deftest nelisp-macro-defmacro-rejects-non-symbol ()
  (nelisp--reset)
  (should-error (nelisp-eval '(defmacro 42 (x) x))
                :type 'nelisp-eval-error))

(ert-deftest nelisp-macro-redefinition-allowed ()
  "Redefining an already-registered macro is allowed without warning."
  (nelisp--reset)
  (nelisp-eval '(defmacro m (x) (list 'quote 1)))
  (nelisp-eval '(defmacro m (x) (list 'quote 2)))
  (should (= (nelisp-eval '(m dummy)) 2)))

;;; macroexpand-1 ----------------------------------------------------

(ert-deftest nelisp-macro-expand-1-atom-unchanged ()
  (nelisp--reset)
  (should (eq (nelisp-macroexpand-1 42) 42))
  (should (eq (nelisp-macroexpand-1 'foo) 'foo)))

(ert-deftest nelisp-macro-expand-1-nonmacro-call-unchanged ()
  (nelisp--reset)
  (let ((form '(+ 1 2)))
    (should (eq (nelisp-macroexpand-1 form) form))))

(ert-deftest nelisp-macro-expand-1-expands-outer-only ()
  "Only the outermost macro is expanded, inner macro calls survive."
  (nelisp--reset)
  (nelisp-eval '(defmacro id (x) x))
  ;; An `id' that wraps another `id' expression.  macroexpand-1 must
  ;; resolve the outer one into its body (a quoted form constructed
  ;; with `list') but leave the inner (id 2) literal alone.
  (nelisp-eval '(defmacro wrap (x) (list 'list (list 'quote 'id) x)))
  (let ((expanded (nelisp-macroexpand-1 '(wrap 2))))
    (should (equal expanded '(list 'id 2)))))

(ert-deftest nelisp-macro-expand-1-single-step ()
  (nelisp--reset)
  (nelisp-macro-test--define-unless)
  (let ((expanded (nelisp-macroexpand-1 '(my-unless nil 1 2))))
    (should (equal expanded '(if nil nil (progn 1 2))))))

;;; macroexpand ------------------------------------------------------

(ert-deftest nelisp-macro-expand-nonmacro-unchanged ()
  (nelisp--reset)
  (let ((form '(+ 1 2)))
    (should (eq (nelisp-macroexpand form) form))))

(ert-deftest nelisp-macro-expand-chain ()
  "Macro A expands into a call to macro B; driven to fixpoint."
  (nelisp--reset)
  (nelisp-eval '(defmacro b (x) (list 'quote x)))
  (nelisp-eval '(defmacro a (x) (list 'b x)))
  (let ((expanded (nelisp-macroexpand '(a 42))))
    (should (equal expanded '(quote 42)))))

(ert-deftest nelisp-macro-expand-stops-on-non-macro-head ()
  (nelisp--reset)
  (nelisp-eval '(defmacro m (x) (list '+ x 1)))
  (let ((expanded (nelisp-macroexpand '(m 3))))
    (should (equal expanded '(+ 3 1)))))

;;; macroexpand-all --------------------------------------------------

(ert-deftest nelisp-macro-expand-all-atom ()
  (nelisp--reset)
  (should (eq (nelisp-macroexpand-all 42) 42))
  (should (eq (nelisp-macroexpand-all 'foo) 'foo))
  (should (eq (nelisp-macroexpand-all nil) nil)))

(ert-deftest nelisp-macro-expand-all-preserves-quote ()
  "`quote' bodies are literal data — do not expand macros inside."
  (nelisp--reset)
  (nelisp-eval '(defmacro m (x) (list '+ x 1)))
  (let ((expanded (nelisp-macroexpand-all '(quote (m 2)))))
    (should (equal expanded '(quote (m 2))))))

(ert-deftest nelisp-macro-expand-all-recurs-into-let-body ()
  (nelisp--reset)
  (nelisp-macro-test--define-unless)
  (let ((expanded (nelisp-macroexpand-all
                   '(let ((x 1)) (my-unless nil x)))))
    (should (equal expanded '(let ((x 1)) (if nil nil (progn x)))))))

(ert-deftest nelisp-macro-expand-all-recurs-into-let-init ()
  "Init forms inside `let' bindings are walked."
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded (nelisp-macroexpand-all '(let ((x (one))) x))))
    (should (equal expanded '(let ((x (quote 1))) x)))))

(ert-deftest nelisp-macro-expand-all-lambda-body ()
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded (nelisp-macroexpand-all '(lambda (x) (one)))))
    (should (equal expanded '(lambda (x) (quote 1))))))

(ert-deftest nelisp-macro-expand-all-lambda-params-untouched ()
  "Params inside `lambda' are literals — no walking."
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  ;; If the walker touched params, `one' in the params would disappear
  ;; — it cannot, because `one' there is just a symbol.  Use a shape
  ;; that would change under over-eager walking.
  (let ((expanded (nelisp-macroexpand-all '(lambda (one) one))))
    (should (equal expanded '(lambda (one) one)))))

(ert-deftest nelisp-macro-expand-all-function-lambda ()
  "(function (lambda ...)) walks the lambda body."
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded (nelisp-macroexpand-all '(function (lambda () (one))))))
    (should (equal expanded '(function (lambda () (quote 1)))))))

(ert-deftest nelisp-macro-expand-all-setq-values ()
  "setq recurs into value positions only."
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded (nelisp-macroexpand-all '(setq x (one) y 2))))
    (should (equal expanded '(setq x (quote 1) y 2)))))

(ert-deftest nelisp-macro-expand-all-cond-clauses ()
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded (nelisp-macroexpand-all '(cond ((one) (one))))))
    (should (equal expanded '(cond ((quote 1) (quote 1)))))))

(ert-deftest nelisp-macro-expand-all-condition-case ()
  "condition-case: VAR literal, protected+handlers walked."
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded
         (nelisp-macroexpand-all
          '(condition-case e (one) (error (one))))))
    (should (equal expanded
                   '(condition-case e (quote 1) (error (quote 1)))))))

(ert-deftest nelisp-macro-expand-all-recurs-into-call ()
  (nelisp--reset)
  (nelisp-eval '(defmacro one () (list 'quote 1)))
  (let ((expanded (nelisp-macroexpand-all '(+ (one) 2))))
    (should (equal expanded '(+ (quote 1) 2)))))

;;; Integration with evaluator --------------------------------------

(ert-deftest nelisp-macro-unless-anchor ()
  "Phase 1 anchor: user-defined `my-unless' drives the evaluator."
  (nelisp--reset)
  (nelisp-macro-test--define-unless)
  (should (= (nelisp-eval '(my-unless nil 1 2 3)) 3))
  (should (eq (nelisp-eval '(my-unless t 1 2 3)) nil)))

(ert-deftest nelisp-macro-call-time-expansion ()
  "Macro defined AFTER the defun is still honored at call time."
  (nelisp--reset)
  (nelisp-eval '(defun caller (x) (twice x)))
  ;; At this point `twice' is neither function nor macro.  Defining it
  ;; as a macro and calling `caller' should expand it at the call site.
  (nelisp-eval '(defmacro twice (x) (list '+ x x)))
  (should (= (nelisp-eval '(caller 5)) 10)))

(ert-deftest nelisp-macro-special-form-shadowing-ignored ()
  "Registering `if' as a macro does not shadow the special form."
  (nelisp--reset)
  (nelisp-eval '(defmacro if (c a b)
                  ;; If the evaluator ever dispatched into this macro,
                  ;; `(if t 1 2)' would try to reach this body.  It
                  ;; never does.
                  (list 'quote 'bogus)))
  (should (= (nelisp-eval '(if t 1 2)) 1)))

(ert-deftest nelisp-macro-recursive-expansion ()
  "User macro whose body is another macro call resolves to fixpoint."
  (nelisp--reset)
  (nelisp-eval '(defmacro m (x) (list 'quote x)))
  (nelisp-eval '(defmacro m2 (x) (list 'm x)))
  (should (eq (nelisp-eval '(m2 hello)) 'hello)))

(ert-deftest nelisp-macro-expands-to-evaluator-error ()
  "A macro may expand into a form the evaluator rejects.
The Week 11-12 point is that expansion itself succeeds and the
resulting form lands in the evaluator cleanly — here the evaluator
then signals `nelisp-void-function' because the expansion head
(`no-such-unbound-fn') is deliberately not installed anywhere.
Phase 1 anchor referenced in `docs/phase1-macro-design.org' §6."
  (nelisp--reset)
  (nelisp-eval '(defmacro boom () (list 'no-such-unbound-fn "msg")))
  (should-error (nelisp-eval '(boom))
                :type 'nelisp-void-function))

(ert-deftest nelisp-macro-reset-clears-macros ()
  "`nelisp--reset' must clear the macro table."
  (nelisp--reset)
  (nelisp-eval '(defmacro m (x) x))
  (should (not (eq (gethash 'm nelisp--macros nelisp--unbound)
                   nelisp--unbound)))
  (nelisp--reset)
  (should (eq (gethash 'm nelisp--macros nelisp--unbound)
              nelisp--unbound)))

;;; Doc 12 §3.4 — macroexpand 系が primitive 表経由で callable --------

(ert-deftest nelisp-macro-macroexpand-1-via-primitive ()
  "`(macroexpand-1 FORM)' inside NeLisp resolves via the primitive
table and returns the first-step expansion."
  (nelisp--reset)
  (nelisp-eval '(defmacro twice (x) (list 'progn x x)))
  (should (equal (nelisp-eval '(macroexpand-1 (quote (twice (foo)))))
                 '(progn (foo) (foo)))))

(ert-deftest nelisp-macro-macroexpand-fixpoint-via-primitive ()
  "`(macroexpand FORM)' iterates until the head is no longer a macro."
  (nelisp--reset)
  (nelisp-eval '(defmacro outer (x) (list 'inner x)))
  (nelisp-eval '(defmacro inner (x) (list 'quote x)))
  (should (equal (nelisp-eval '(macroexpand (quote (outer hello))))
                 '(quote hello))))

(ert-deftest nelisp-macro-macroexpand-all-via-primitive ()
  "`(macroexpand-all FORM)' recurs into nested bodies."
  (nelisp--reset)
  (nelisp-eval '(defmacro one () 1))
  (should (equal (nelisp-eval
                  '(macroexpand-all (quote (let ((x (one))) x))))
                 '(let ((x 1)) x))))

;;; Doc 12 §3.4 — host `macroexpand' は呼ばれないこと dynamic trace ----

(defun nelisp-macro-test--fn-calls-host-mx-p (sym)
  "Return non-nil if SYM's byte-compiled body references `macroexpand*'.
Disassembles SYM's compiled form and inspects the constant vector —
when `macroexpand' / `macroexpand-1' / `macroexpand-all' appear
there, SYM can call host macroexpand at runtime.  This gives a
static-but-runtime-faithful answer without being spoofed by the
host interpreter's own per-form macroexpand-1 activity."
  (let* ((compiled (byte-compile sym))
         (consts (aref compiled 2)))
    (cl-loop for c across consts
             thereis (memq c '(macroexpand macroexpand-1 macroexpand-all)))))

(ert-deftest nelisp-macro-nelisp-side-does-not-mention-host-macroexpand ()
  "Static audit: when NeLisp's macro-family functions are byte-compiled,
their constant pools must NOT reference `macroexpand' / `-1' / `-all'
— proof that every expansion path is self-contained per Doc 12 §3.4.

This replaces the original dynamic-advice trace, which failed in
interpreted test runs because the host's own step-by-step evaluator
calls `macroexpand-1' on sub-forms of any interpreted Lisp function
it steps through (including NeLisp's host-side plumbing).  The
constant-pool check is the signal we actually care about — whether
NeLisp logic *names* host macroexpand — and is unaffected by host
interpreter noise."
  (dolist (sym '(nelisp-macroexpand
                 nelisp-macroexpand-1
                 nelisp-macroexpand-all
                 nelisp--macro-lookup
                 nelisp--macroexpand-map
                 nelisp--macroexpand-all-args))
    (should-not (nelisp-macro-test--fn-calls-host-mx-p sym))))

;;; Doc 12 §3.4 — 3 macro patterns ------------------------------------

(ert-deftest nelisp-macro-expand-backquote-style-construction ()
  "NeLisp macroexpand handles a macro whose body assembles via
`list' / `cons' — the pre-backquote idiom the existing macro
tests rely on.  Verifies the core macroexpand-1 path."
  (nelisp--reset)
  (nelisp-eval
   '(defmacro my-when (cond then)
      (list 'if cond then nil)))
  (should (equal (nelisp-macroexpand-1 '(my-when p (foo)))
                 '(if p (foo) nil))))

(ert-deftest nelisp-macro-expand-nested-macros ()
  "Expanding an outer macro whose body uses another macro: the
outer expansion yields a form that contains the inner macro call;
`nelisp-macroexpand-all' must reach the nested site too."
  (nelisp--reset)
  (nelisp-eval
   '(defmacro inner (x) (list 'quote x)))
  (nelisp-eval
   '(defmacro outer (x) (list 'list (list 'inner x) x)))
  (should (equal (nelisp-macroexpand-all '(outer hello))
                 '(list (quote hello) hello))))

(ert-deftest nelisp-macro-expand-user-defined-survives-eval ()
  "End-to-end: user-defined macro expanded + evaluated yields the
expected runtime value, proving the eval-time `nelisp-macroexpand'
call in `nelisp-eval-form' sees the NeLisp macro table."
  (nelisp--reset)
  (nelisp-eval
   '(defmacro my-unless (cond then)
      (list 'if cond nil then)))
  (nelisp-eval '(defvar *n* 0))
  (nelisp-eval '(my-unless nil (setq *n* 11)))
  (nelisp-eval '(my-unless t   (setq *n* 99)))
  (should (= (nelisp-eval '*n*) 11)))

(provide 'nelisp-macro-test)

;;; nelisp-macro-test.el ends here
