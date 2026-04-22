;;; nelisp-cond-test.el --- ERT tests for Week 21-24 error / exit forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 21-24: catch / throw / unwind-protect / condition-case.
;; Independent of the Week 11-12 macro spec — every test here runs
;; without touching `nelisp--macros' or the expander.

;;; Code:

(require 'ert)
(require 'nelisp-eval)

;;; catch / throw -----------------------------------------------------

(ert-deftest nelisp-eval-catch-no-throw ()
  "Body runs as progn when no throw happens."
  (should (= (nelisp-eval '(catch (quote foo) 1 2 3)) 3)))

(ert-deftest nelisp-eval-catch-returns-thrown-value ()
  (should (= (nelisp-eval '(catch (quote foo) (throw (quote foo) 42)))
             42)))

(ert-deftest nelisp-eval-catch-throw-skips-trailing-forms ()
  (nelisp--reset)
  (nelisp-eval '(setq x 0))
  (nelisp-eval '(catch (quote foo)
                  (throw (quote foo) :early)
                  (setq x 99)))
  (should (= (nelisp-eval 'x) 0)))

(ert-deftest nelisp-eval-catch-nested-inner-tag-wins ()
  (should (eq (nelisp-eval
               '(catch (quote outer)
                  (catch (quote inner)
                    (throw (quote inner) :inner-value)
                    :never-run)))
              :inner-value)))

(ert-deftest nelisp-eval-catch-nested-outer-tag-bubbles ()
  (should (eq (nelisp-eval
               '(catch (quote outer)
                  (catch (quote inner)
                    (throw (quote outer) :outer-value))
                  :also-never))
              :outer-value)))

(ert-deftest nelisp-eval-throw-without-catch-is-error ()
  (should-error (nelisp-eval '(throw (quote no-catch) 1))))

;;; unwind-protect ----------------------------------------------------

(ert-deftest nelisp-eval-unwind-protect-normal-exit ()
  (nelisp--reset)
  (nelisp-eval '(setq cleanup nil))
  (should (= (nelisp-eval '(unwind-protect 42 (setq cleanup :done)))
             42))
  (should (eq (nelisp-eval 'cleanup) :done)))

(ert-deftest nelisp-eval-unwind-protect-runs-on-throw ()
  "Cleanup block fires when body raises a NeLisp `throw'."
  (nelisp--reset)
  (nelisp-eval '(setq cleanup nil))
  (should (eq (nelisp-eval
               '(catch (quote foo)
                  (unwind-protect
                      (throw (quote foo) :caught)
                    (setq cleanup :ran))))
              :caught))
  (should (eq (nelisp-eval 'cleanup) :ran)))

(ert-deftest nelisp-eval-unwind-protect-runs-on-error ()
  "Cleanup block fires when body signals a NeLisp error."
  (nelisp--reset)
  (nelisp-eval '(setq cleanup nil))
  (should-error
   (nelisp-eval '(unwind-protect
                     (no-such-function)
                   (setq cleanup :ran)))
   :type 'nelisp-void-function)
  (should (eq (nelisp-eval 'cleanup) :ran)))

(ert-deftest nelisp-eval-unwind-protect-preserves-body-value ()
  "Cleanup runs but does not overwrite the value of BODYFORM."
  (nelisp--reset)
  (should (= (nelisp-eval '(unwind-protect 100 200)) 100)))

;;; condition-case ----------------------------------------------------

(ert-deftest nelisp-eval-condition-case-phase-1-anchor ()
  "Canonical Phase 1 success case (docs/03-architecture.org §7.2)."
  (should (eq (nelisp-eval
               '(condition-case err
                    (/ 1 0)
                  (error :caught)))
              :caught)))

(ert-deftest nelisp-eval-condition-case-binds-err-data ()
  "Inside handler, VAR is bound to the signaled error data."
  (should (eq (nelisp-eval
               '(condition-case err
                    (/ 1 0)
                  (error (car err))))
              'arith-error)))

(ert-deftest nelisp-eval-condition-case-no-error-returns-body ()
  (should (= (nelisp-eval
              '(condition-case err 42 (error :never)))
             42)))

(ert-deftest nelisp-eval-condition-case-first-match-wins ()
  (should (eq (nelisp-eval
               '(condition-case err
                    (/ 1 0)
                  (arith-error :arith)
                  (error :general)))
              :arith)))

(ert-deftest nelisp-eval-condition-case-list-of-conditions ()
  "A handler spec may be a list; any element matching wins."
  (should (eq (nelisp-eval
               '(condition-case err
                    (/ 1 0)
                  ((wrong-type-argument arith-error) :combined)))
              :combined)))

(ert-deftest nelisp-eval-condition-case-t-catches-all ()
  (should (eq (nelisp-eval
               '(condition-case err
                    (/ 1 0)
                  (t :anything)))
              :anything)))

(ert-deftest nelisp-eval-condition-case-unmatched-propagates ()
  (should-error
   (nelisp-eval '(condition-case err
                     (no-such-function)
                   (arith-error :not-this)))
   :type 'nelisp-void-function))

(ert-deftest nelisp-eval-condition-case-catches-nelisp-unbound ()
  "NeLisp's own error chains descend from `error'."
  (should (eq (nelisp-eval
               '(condition-case err
                    undefined-variable
                  (error :caught)))
              :caught)))

(ert-deftest nelisp-eval-condition-case-specific-nelisp-type ()
  "Users can target `nelisp-void-function' specifically."
  (should (eq (nelisp-eval
               '(condition-case err
                    (no-such-function)
                  (nelisp-void-function :specific)
                  (error :general)))
              :specific)))

(ert-deftest nelisp-eval-condition-case-var-nil-no-binding ()
  "VAR may be nil — handler still runs, no binding is created."
  (should (eq (nelisp-eval
               '(condition-case nil
                    (/ 1 0)
                  (error :caught)))
              :caught)))

(ert-deftest nelisp-eval-condition-case-malformed-var ()
  (should-error
   (nelisp-eval '(condition-case "not-a-symbol" 1 (error :x)))
   :type 'nelisp-eval-error))

(ert-deftest nelisp-eval-condition-case-malformed-handler ()
  (should-error
   (nelisp-eval '(condition-case err (/ 1 0) bare-symbol))
   :type 'nelisp-eval-error))

;;; Interaction with other forms --------------------------------------

(ert-deftest nelisp-eval-cc-unwind-protect-combination ()
  "unwind-protect runs even when condition-case catches the inner error."
  (nelisp--reset)
  (nelisp-eval '(setq cleanup nil))
  (should (eq (nelisp-eval
               '(condition-case err
                    (unwind-protect
                        (/ 1 0)
                      (setq cleanup :ran))
                  (error :caught)))
              :caught))
  (should (eq (nelisp-eval 'cleanup) :ran)))

(ert-deftest nelisp-eval-cc-in-defun ()
  "condition-case works inside a user-defined function."
  (nelisp--reset)
  (nelisp-eval '(defun safe-div (a b)
                  (condition-case err
                      (/ a b)
                    (arith-error :div-by-zero))))
  (should (= (nelisp-eval '(safe-div 10 2)) 5))
  (should (eq (nelisp-eval '(safe-div 1 0)) :div-by-zero)))

(provide 'nelisp-cond-test)

;;; nelisp-cond-test.el ends here
