;;; nl-hygiene-standalone-smoke.el --- Doc 198 smoke on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance for Doc 198 Phases 2-3.  It runs the exact
;; nl-hygiene ERT bodies through a small shim, then drives one million
;; explicit bounces through mutually recursive functions on target/nelisp.
;; Dependencies are loaded by path because standalone `require' is not a
;; trustworthy proof that a file exists.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-hygiene/test/nl-hygiene-standalone-smoke.el

;;; Code:

(defvar nl-smoke--tests nil
  "Alist of (NAME . BODY-FN) registered by the `ert-deftest' shim.")

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
  (provide 'ert))

(load "packages/nl-prelude/src/nl-prelude-trampoline.el")
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-ns/src/nl-ns-in.el")
(load "packages/nl-ns/src/nl-ns-reader.el")
(load "packages/nl-hygiene/src/nl-hygiene.el")
(load "packages/nl-hygiene/test/nl-hygiene-test.el")

(defun nl-hygiene-smoke--even-p (n)
  "Bounce to the odd predicate until N reaches zero."
  (if (zerop n)
      t
    (nl-bounce #'nl-hygiene-smoke--odd-p (1- n))))

(defun nl-hygiene-smoke--odd-p (n)
  "Bounce to the even predicate until N reaches zero."
  (if (zerop n)
      nil
    (nl-bounce #'nl-hygiene-smoke--even-p (1- n))))

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
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-hygiene-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 5)
    (error "nl-hygiene-standalone-smoke: only %d tests ran (expected >= 5)"
           ran))
  (unless (eq (nl-trampoline #'nl-hygiene-smoke--even-p 1000000) t)
    (error "nl-hygiene-standalone-smoke: mutual trampoline result was wrong"))
  (princ (format
          "nl-hygiene-standalone-smoke: PASS (%d hygiene tests, 1000000 mutual bounces)\n"
          ran)))

;;; nl-hygiene-standalone-smoke.el ends here
