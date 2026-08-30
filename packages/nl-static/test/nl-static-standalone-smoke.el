;;; nl-static-standalone-smoke.el --- run nl-static tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance for Doc 197.  The exact ERT bodies in
;; `nl-static-test.el' run through a minimal shim because target/nelisp
;; has no ERT.  Expansion failures and the Tier-3 nl-safe runtime
;; fallthrough are therefore exercised on the target substrate itself.

;;; Code:

(defvar nl-smoke--tests nil
  "Alist of (NAME . BODY-FN) registered by the ERT shim.")

(unless (featurep 'ert)
  (defmacro ert-deftest (name _args &rest body)
    "Register BODY as test NAME in `nl-smoke--tests'."
    (when (and (stringp (car body)) (cdr body))
      (setq body (cdr body)))
    `(setq nl-smoke--tests
           (cons (cons ',name (lambda () ,@body)) nl-smoke--tests)))
  (defmacro should (form)
    `(let ((nl-smoke--value ,form))
       (unless nl-smoke--value
         (error "should failed: %S" ',form))
       nl-smoke--value))
  (defmacro should-not (form)
    `(let ((nl-smoke--value ,form))
       (when nl-smoke--value
         (error "should-not failed: %S" ',form))
       t))
  (defmacro should-error (form &rest keys)
    "Evaluate FORM and expect an error; support KEYS `:type SYMBOL'."
    `(let* ((nl-smoke--expected (plist-get (list ,@keys) :type))
            (nl-smoke--result
             (condition-case nl-smoke--error
                 (progn ,form 'nl-smoke--no-error)
               (error nl-smoke--error))))
       (cond
        ((eq nl-smoke--result 'nl-smoke--no-error)
         (error "should-error: no error signaled by %S" ',form))
        ((and nl-smoke--expected
              (not (memq nl-smoke--expected
                         (get (car nl-smoke--result) 'error-conditions))))
         (error "should-error: expected %S, got %S"
                nl-smoke--expected nl-smoke--result))
        (t nl-smoke--result))))
  (provide 'ert))

(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-safe/src/nl-safe.el")
(load "packages/nl-static/src/nl-static.el")
(load "packages/nl-static/test/nl-static-test.el")

(let ((tests (reverse nl-smoke--tests))
      (ran 0)
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (condition-case err
          (progn
            (funcall (cdr test))
            (setq ran (1+ ran)))
        (error
         (setq failures
               (cons (format "%s: %S" (car test) err) failures)))))
    (setq tests (cdr tests)))
  (princ (format "GATE-COUNT checked=%d findings=%d\n"
                 ran (length failures)))
  (when failures
    (let ((rest failures))
      (while rest
        (princ (format "FAIL %s\n" (car rest)))
        (setq rest (cdr rest))))
    (error "nl-static-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 23)
    (error "nl-static-standalone-smoke: only %d tests ran (expected >= 23)"
           ran))
  (princ (format "nl-static-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-static-standalone-smoke.el ends here
