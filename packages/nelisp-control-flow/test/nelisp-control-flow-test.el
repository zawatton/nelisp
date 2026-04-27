;;; nelisp-control-flow-test.el --- T153 Phase 7+E ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; ERT pin for Doc 40 §3.E LOCKED v2 — Phase 7+E control flow + advice.
;; Aim: 30+ tests, byte-compile clean, regression-free.
;;
;; Layout:
;;   - condition-case (8)
;;   - unwind-protect (4)
;;   - catch / throw (5)
;;   - signal (3)
;;   - advice basics (10+)
;;   - advice composite / removal (5+)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-special-forms)
(require 'nelisp-control-flow)

(defmacro nelisp-cf-test--with-fresh-state (&rest body)
  "Run BODY after clearing all Phase 7+B + 7+E globals."
  (declare (indent 0))
  `(progn
     (nelisp-special-forms-reset)
     (nelisp-advice-clear-all)
     (unwind-protect
         (progn ,@body)
       (nelisp-special-forms-reset)
       (nelisp-advice-clear-all))))

(defalias 'nelisp-cf-test--eval 'nelisp-special-forms-eval)


;;; -----------------------------------------------------------------
;;; condition-case
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cf-condition-case-no-error ()
  (nelisp-cf-test--with-fresh-state
    (should (= (nelisp-cf-test--eval
                '(condition-case _e
                     42
                   (error 0)))
               42))))

(ert-deftest nelisp-cf-condition-case-catches-error ()
  (nelisp-cf-test--with-fresh-state
    (should (eq (nelisp-cf-test--eval
                 '(condition-case _e
                      (signal 'wrong-type-argument '(arg))
                    (error 'caught)))
                'caught))))

(ert-deftest nelisp-cf-condition-case-binds-error-data ()
  (nelisp-cf-test--with-fresh-state
    (let ((data (nelisp-cf-test--eval
                 '(condition-case e
                      (signal 'arith-error '(divide-by-zero))
                    (error e)))))
      (should (eq (car data) 'arith-error))
      (should (equal (cdr data) '(divide-by-zero))))))

(ert-deftest nelisp-cf-condition-case-handlers-priority ()
  ;; First matching handler wins, later handlers are not tried.
  (nelisp-cf-test--with-fresh-state
    (should (eq (nelisp-cf-test--eval
                 '(condition-case _e
                      (signal 'wrong-type-argument '(x))
                    (wrong-type-argument 'first)
                    (error 'second)))
                'first))))

(ert-deftest nelisp-cf-condition-case-parent-condition-match ()
  ;; `error' catches every error symbol since they all chain to `error'.
  (nelisp-cf-test--with-fresh-state
    (should (eq (nelisp-cf-test--eval
                 '(condition-case _e
                      (signal 'arith-error nil)
                    (error 'parent-caught)))
                'parent-caught))))

(ert-deftest nelisp-cf-condition-case-list-of-conditions ()
  (nelisp-cf-test--with-fresh-state
    (should (eq (nelisp-cf-test--eval
                 '(condition-case _e
                      (signal 'arith-error nil)
                    ((wrong-type-argument arith-error) 'list-match)))
                'list-match))))

(ert-deftest nelisp-cf-condition-case-rethrows-on-no-match ()
  (nelisp-cf-test--with-fresh-state
    (should-error
     (nelisp-cf-test--eval
      '(condition-case _e
           (signal 'arith-error nil)
         (wrong-type-argument 'nope)))
     :type 'arith-error)))

(ert-deftest nelisp-cf-condition-case-nil-var-no-binding ()
  ;; Handler body still runs, just without a captured variable.
  (nelisp-cf-test--with-fresh-state
    (should (eq (nelisp-cf-test--eval
                 '(condition-case nil
                      (signal 'error '("boom"))
                    (error 'no-var)))
                'no-var))))


;;; -----------------------------------------------------------------
;;; unwind-protect
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cf-unwind-protect-cleanup-runs-on-success ()
  (nelisp-cf-test--with-fresh-state
    (let ((cleaned nil))
      (let ((nelisp-cf-test--cleanup-flag (lambda () (setq cleaned t))))
        ;; Use a closure call from the host side to set a flag.
        (let ((r (unwind-protect
                     (nelisp-cf-test--eval 1)
                   (funcall nelisp-cf-test--cleanup-flag))))
          (should (= r 1))
          (should cleaned))))))

(ert-deftest nelisp-cf-unwind-protect-cleanup-via-special-form ()
  ;; Body returns BODYFORM result; cleanup form is ignored for value.
  (nelisp-cf-test--with-fresh-state
    (should (= (nelisp-cf-test--eval
                '(unwind-protect 7 (quote ignored)))
               7))))

(ert-deftest nelisp-cf-unwind-protect-cleanup-runs-on-error ()
  ;; The cleanup form runs even when the body signals; observe it via
  ;; the Phase 7+B global table after the NeLisp condition-case
  ;; swallows the error.
  (nelisp-cf-test--with-fresh-state
    (puthash 'nelisp-cf-test--counter-a 0
             nelisp-special-forms--globals)
    (nelisp-cf-test--eval
     '(condition-case _e
          (unwind-protect
              (signal 'error '("x"))
            (setq nelisp-cf-test--counter-a
                  (1+ nelisp-cf-test--counter-a)))
        (error nil)))
    (should (= (gethash 'nelisp-cf-test--counter-a
                        nelisp-special-forms--globals)
               1))))

(ert-deftest nelisp-cf-unwind-protect-cleanup-runs-on-throw ()
  (nelisp-cf-test--with-fresh-state
    (puthash 'nelisp-cf-test--counter-b 0
             nelisp-special-forms--globals)
    (let ((r (nelisp-cf-test--eval
              '(catch 'tag
                 (unwind-protect
                     (throw 'tag 99)
                   (setq nelisp-cf-test--counter-b 1))))))
      (should (= r 99))
      (should (= (gethash 'nelisp-cf-test--counter-b
                          nelisp-special-forms--globals)
                 1)))))


;;; -----------------------------------------------------------------
;;; catch / throw
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cf-catch-no-throw-returns-body ()
  (nelisp-cf-test--with-fresh-state
    (should (= (nelisp-cf-test--eval
                '(catch 'tag 1 2 3))
               3))))

(ert-deftest nelisp-cf-catch-throw-roundtrip ()
  (nelisp-cf-test--with-fresh-state
    (should (= (nelisp-cf-test--eval
                '(catch 'foo
                   (throw 'foo 11)
                   99))
               11))))

(ert-deftest nelisp-cf-catch-uncaught-throw-signals-no-catch ()
  (nelisp-cf-test--with-fresh-state
    (should-error
     (nelisp-cf-test--eval
      '(throw 'no-such-tag 0))
     :type 'no-catch)))

(ert-deftest nelisp-cf-nested-catch-inner-tag ()
  (nelisp-cf-test--with-fresh-state
    (should (= (nelisp-cf-test--eval
                '(catch 'outer
                   (catch 'inner
                     (throw 'inner 1))
                   2))
               2))))

(ert-deftest nelisp-cf-nested-catch-outer-tag ()
  (nelisp-cf-test--with-fresh-state
    (should (= (nelisp-cf-test--eval
                '(catch 'outer
                   (catch 'inner
                     (throw 'outer 5))
                   77))
               5))))


;;; -----------------------------------------------------------------
;;; signal
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cf-signal-error-symbol-chain ()
  ;; Signaling `arith-error' is catchable as `error' via parent chain.
  (nelisp-cf-test--with-fresh-state
    (should (eq (nelisp-cf-test--eval
                 '(condition-case _e
                      (signal 'arith-error '(0))
                    (error 'chain-ok)))
                'chain-ok))))

(ert-deftest nelisp-cf-signal-helper-direct ()
  (should-error (nelisp-cf-signal 'wrong-type-argument '(test-data))
                :type 'wrong-type-argument))

(ert-deftest nelisp-cf-signal-with-data-shape ()
  (nelisp-cf-test--with-fresh-state
    (let ((data (nelisp-cf-test--eval
                 '(condition-case e
                      (signal 'wrong-number-of-arguments '(foo 0))
                    (error e)))))
      (should (eq (car data) 'wrong-number-of-arguments))
      (should (equal (cdr data) '(foo 0))))))


;;; -----------------------------------------------------------------
;;; advice — basics
;;; -----------------------------------------------------------------

(defun nelisp-cf-test-advice--double (x) (* x 2))
(defun nelisp-cf-test-advice--target (x) (+ x 1))

(ert-deftest nelisp-cf-advice-add-before-runs-before ()
  (nelisp-cf-test--with-fresh-state
    (defvar nelisp-cf-test--before-flag)
    (setq nelisp-cf-test--before-flag nil)
    (defun nelisp-cf-test--side (_x)
      (setq nelisp-cf-test--before-flag 'before-fired))
    (nelisp-advice-add 'nelisp-cf-test-advice--target
                       :before #'nelisp-cf-test--side)
    (let ((r (nelisp-cf-test-advice--target 5)))
      (should (= r 6))                   ; original ran
      (should (eq nelisp-cf-test--before-flag 'before-fired)))))

(ert-deftest nelisp-cf-advice-add-after-runs-after ()
  (nelisp-cf-test--with-fresh-state
    (defvar nelisp-cf-test--after-trace)
    (setq nelisp-cf-test--after-trace nil)
    (defun nelisp-cf-test--after-fn (_x)
      (push 'after nelisp-cf-test--after-trace))
    (defun nelisp-cf-test--target2 (x)
      (push 'body nelisp-cf-test--after-trace)
      (* x 10))
    (nelisp-advice-add 'nelisp-cf-test--target2
                       :after #'nelisp-cf-test--after-fn)
    (let ((r (nelisp-cf-test--target2 3)))
      (should (= r 30))
      (should (equal (reverse nelisp-cf-test--after-trace)
                     '(body after))))))

(ert-deftest nelisp-cf-advice-around-can-skip-original ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target3 (x) (* x 100))
    (defun nelisp-cf-test--around (_orig _x) 'overridden)
    (nelisp-advice-add 'nelisp-cf-test--target3
                       :around #'nelisp-cf-test--around)
    (should (eq (nelisp-cf-test--target3 4) 'overridden))))

(ert-deftest nelisp-cf-advice-around-can-call-original ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target4 (x) (+ x 1))
    (defun nelisp-cf-test--around2 (orig x)
      (* (funcall orig x) 10))
    (nelisp-advice-add 'nelisp-cf-test--target4
                       :around #'nelisp-cf-test--around2)
    (should (= (nelisp-cf-test--target4 2) 30))))

(ert-deftest nelisp-cf-advice-override ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target5 (_x) 'orig)
    (defun nelisp-cf-test--ovr (_x) 'replaced)
    (nelisp-advice-add 'nelisp-cf-test--target5
                       :override #'nelisp-cf-test--ovr)
    (should (eq (nelisp-cf-test--target5 0) 'replaced))))

(ert-deftest nelisp-cf-advice-before-while-stops-on-nil ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target6 (_x) 'body-ran)
    (defun nelisp-cf-test--gate (_x) nil)
    (nelisp-advice-add 'nelisp-cf-test--target6
                       :before-while #'nelisp-cf-test--gate)
    (should (eq (nelisp-cf-test--target6 1) nil))))

(ert-deftest nelisp-cf-advice-before-while-runs-on-non-nil ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target7 (_x) 'body-ran)
    (defun nelisp-cf-test--gate2 (_x) t)
    (nelisp-advice-add 'nelisp-cf-test--target7
                       :before-while #'nelisp-cf-test--gate2)
    (should (eq (nelisp-cf-test--target7 1) 'body-ran))))

(ert-deftest nelisp-cf-advice-before-until-skips-original ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target8 (_x) 'body)
    (defun nelisp-cf-test--first (_x) 'short-circuit)
    (nelisp-advice-add 'nelisp-cf-test--target8
                       :before-until #'nelisp-cf-test--first)
    (should (eq (nelisp-cf-test--target8 1) 'short-circuit))))

(ert-deftest nelisp-cf-advice-after-while-uses-body-result ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target9 (_x) t)
    (defun nelisp-cf-test--after-w (_x) 'gated-result)
    (nelisp-advice-add 'nelisp-cf-test--target9
                       :after-while #'nelisp-cf-test--after-w)
    (should (eq (nelisp-cf-test--target9 1) 'gated-result))))

(ert-deftest nelisp-cf-advice-after-until-falls-back ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target10 (_x) nil)
    (defun nelisp-cf-test--fallback (_x) 'fallback-value)
    (nelisp-advice-add 'nelisp-cf-test--target10
                       :after-until #'nelisp-cf-test--fallback)
    (should (eq (nelisp-cf-test--target10 1) 'fallback-value))))

(ert-deftest nelisp-cf-advice-filter-args-rewrites ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target11 (x) (* x 10))
    (defun nelisp-cf-test--filter-args (args)
      (list (1+ (car args))))
    (nelisp-advice-add 'nelisp-cf-test--target11
                       :filter-args #'nelisp-cf-test--filter-args)
    (should (= (nelisp-cf-test--target11 1) 20))))

(ert-deftest nelisp-cf-advice-filter-return-rewrites ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target12 (x) (* x 10))
    (defun nelisp-cf-test--filter-ret (r) (1+ r))
    (nelisp-advice-add 'nelisp-cf-test--target12
                       :filter-return #'nelisp-cf-test--filter-ret)
    (should (= (nelisp-cf-test--target12 2) 21))))


;;; -----------------------------------------------------------------
;;; advice — composite / removal
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cf-advice-remove-restores-original ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target13 (x) (+ x 100))
    (defun nelisp-cf-test--ovr2 (_x) 'advised)
    (nelisp-advice-add 'nelisp-cf-test--target13
                       :override #'nelisp-cf-test--ovr2)
    (should (eq (nelisp-cf-test--target13 0) 'advised))
    (nelisp-advice-remove 'nelisp-cf-test--target13
                          #'nelisp-cf-test--ovr2)
    (should (= (nelisp-cf-test--target13 5) 105))))

(ert-deftest nelisp-cf-advice-remove-no-such-is-noop ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target14 (x) x)
    (should-not (nelisp-advice-remove 'nelisp-cf-test--target14
                                      #'no-such-fn))
    ;; Function still callable & unmodified.
    (should (= (nelisp-cf-test--target14 9) 9))))

(ert-deftest nelisp-cf-advice-add-idempotent ()
  ;; Adding the same HOW + FUNCTION twice must not duplicate the cell.
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target15 (x) x)
    (defun nelisp-cf-test--noop-advice (&rest _) nil)
    (nelisp-advice-add 'nelisp-cf-test--target15
                       :before #'nelisp-cf-test--noop-advice)
    (nelisp-advice-add 'nelisp-cf-test--target15
                       :before #'nelisp-cf-test--noop-advice
                       '(:name dup))
    (let ((cells nil))
      (nelisp-advice-mapc (lambda (c) (push c cells))
                          'nelisp-cf-test--target15)
      (should (= (length cells) 1)))))

(ert-deftest nelisp-cf-advice-mapc-iterates-all ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target16 (x) x)
    (defun nelisp-cf-test--a1 (&rest _) nil)
    (defun nelisp-cf-test--a2 (&rest _) nil)
    (nelisp-advice-add 'nelisp-cf-test--target16
                       :before #'nelisp-cf-test--a1)
    (nelisp-advice-add 'nelisp-cf-test--target16
                       :after #'nelisp-cf-test--a2)
    (let ((seen nil))
      (nelisp-advice-mapc (lambda (c) (push (nth 1 c) seen))
                          'nelisp-cf-test--target16)
      (should (equal (sort seen #'string<)
                     (sort (list 'nelisp-cf-test--a1
                                 'nelisp-cf-test--a2)
                           #'string<))))))

(ert-deftest nelisp-cf-advice-member-p ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target17 (x) x)
    (defun nelisp-cf-test--member-fn (&rest _) nil)
    (should-not (nelisp-advice-member-p 'nelisp-cf-test--member-fn
                                        'nelisp-cf-test--target17))
    (nelisp-advice-add 'nelisp-cf-test--target17
                       :before #'nelisp-cf-test--member-fn)
    (should (nelisp-advice-member-p 'nelisp-cf-test--member-fn
                                    'nelisp-cf-test--target17))))

(ert-deftest nelisp-cf-advice-stacked-before-after-order ()
  (nelisp-cf-test--with-fresh-state
    (defvar nelisp-cf-test--stack-trace)
    (setq nelisp-cf-test--stack-trace nil)
    (defun nelisp-cf-test--target18 (_x)
      (push 'body nelisp-cf-test--stack-trace))
    (defun nelisp-cf-test--bx (_x)
      (push 'before nelisp-cf-test--stack-trace))
    (defun nelisp-cf-test--ax (_x)
      (push 'after nelisp-cf-test--stack-trace))
    (nelisp-advice-add 'nelisp-cf-test--target18
                       :before #'nelisp-cf-test--bx)
    (nelisp-advice-add 'nelisp-cf-test--target18
                       :after #'nelisp-cf-test--ax)
    (nelisp-cf-test--target18 0)
    (should (equal (reverse nelisp-cf-test--stack-trace)
                   '(before body after)))))

(ert-deftest nelisp-cf-advice-bad-how-signals ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target19 (x) x)
    (should-error (nelisp-advice-add 'nelisp-cf-test--target19
                                     :not-a-real-how
                                     #'identity)
                  :type 'nelisp-advice-bad-how)))

(ert-deftest nelisp-cf-advice-clear-all-restores-everything ()
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target20 (x) (+ x 1))
    (defun nelisp-cf-test--ovr3 (_x) 'replaced)
    (nelisp-advice-add 'nelisp-cf-test--target20
                       :override #'nelisp-cf-test--ovr3)
    (should (eq (nelisp-cf-test--target20 0) 'replaced))
    (nelisp-advice-clear-all)
    (should (= (nelisp-cf-test--target20 5) 6))))


;;; -----------------------------------------------------------------
;;; integration — control-flow + advice
;;; -----------------------------------------------------------------

(ert-deftest nelisp-cf-condition-case-catches-advised-signal ()
  ;; An advised function that signals must still be catchable through
  ;; the trampoline.
  (nelisp-cf-test--with-fresh-state
    (defun nelisp-cf-test--target21 (_x)
      (signal 'wrong-type-argument '(boom)))
    (defun nelisp-cf-test--noop21 (&rest _) nil)
    (nelisp-advice-add 'nelisp-cf-test--target21
                       :before #'nelisp-cf-test--noop21)
    (should (eq (condition-case _e
                    (nelisp-cf-test--target21 0)
                  (wrong-type-argument 'caught-through-advice))
                'caught-through-advice))))

(provide 'nelisp-control-flow-test)

;;; nelisp-control-flow-test.el ends here
