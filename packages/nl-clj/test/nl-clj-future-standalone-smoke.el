;;; nl-clj-future-standalone-smoke.el --- Doc 199 Tier 1 smoke on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance for Doc 199 Tier 1 (nl-clj-future).  Unlike
;; nl-clj-async, this tier needs no `generator.el': a future worker never
;; parks, so the library is a plain deferred queue that macroexpands and
;; runs directly on `target/nelisp'.  This smoke replays the exact ERT
;; bodies through a shim, then drives a 10000-element `nl-clj-pmap' to
;; prove no worker is dropped at scale on the standalone substrate.
;; Dependencies are loaded by path because standalone `require' is not a
;; trustworthy proof that a file exists (Doc 195 §2's measured gap).
;;
;; Run from the repository root:
;;   ./target/nelisp --load packages/nl-clj/test/nl-clj-future-standalone-smoke.el

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
    `(let ((nl-smoke--v ,form))
       (unless nl-smoke--v (error "should failed: %S" ',form))
       nl-smoke--v))
  (defmacro should-not (form)
    `(let ((nl-smoke--v ,form))
       (when nl-smoke--v (error "should-not failed: %S" ',form))
       t))
  (defmacro should-error (form &rest keys)
    `(let* ((nl-smoke--expected (plist-get (list ,@keys) :type))
            (nl-smoke--r
             (condition-case nl-smoke--e
                 (progn ,form 'nl-smoke--no-error)
               (error nl-smoke--e))))
       (cond
        ((eq nl-smoke--r 'nl-smoke--no-error)
         (error "should-error: no error signaled by %S" ',form))
        ((and nl-smoke--expected
              (not (memq nl-smoke--expected
                         (get (car nl-smoke--r) 'error-conditions))))
         (error "should-error: expected %S, got %S"
                nl-smoke--expected nl-smoke--r))
        (t nl-smoke--r))))
  (provide 'ert))

(load "packages/nl-prelude/src/nl-prelude-trampoline.el")
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-safe/src/nl-safe.el")
(load "packages/nl-clj/src/nl-clj-core.el")
(load "packages/nl-clj/src/nl-clj-atom.el")
(load "packages/nl-clj/src/nl-clj-vector.el")
(load "packages/nl-clj/src/nl-clj-hash.el")
(load "packages/nl-clj/src/nl-clj-seq.el")
(load "packages/nl-clj/src/nl-clj-future.el")
(load "packages/nl-clj/test/nl-clj-future-test.el")
(load "packages/nl-clj/test/nl-clj-future-lint-test.el")

(let ((tests (reverse nl-smoke--tests))
      (ran 0)
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (condition-case err
          (progn (funcall (cdr test)) (setq ran (1+ ran)))
        (error
         (setq failures (cons (format "%s: %S" (car test) err) failures)))))
    (setq tests (cdr tests)))
  (princ (format "GATE-COUNT checked=%d findings=%d\n" ran (length failures)))
  (when failures
    (let ((all failures))
      (while all (princ (format "FAIL %s\n" (car all))) (setq all (cdr all))))
    (error "nl-clj-future-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 17)
    (error "nl-clj-future-standalone-smoke: only %d tests ran (expected >= 17)"
           ran))
  ;; Scale drive: 10000 workers must all run exactly once.
  (let* ((n 10000)
         (acc (nl-clj-atom 0)))
    (nl-clj-pmap (lambda (_x) (nl-clj-swap! acc #'1+)) (number-sequence 1 n))
    (unless (equal (nl-clj-deref acc) n)
      (error "nl-clj-future-standalone-smoke: pmap dropped a worker (%s of %d)"
             (nl-clj-deref acc) n))
    ;; ordered result alignment at scale
    (unless (equal (nl-clj-pmap #'1+ '(1 2 3 4 5)) '(2 3 4 5 6))
      (error "nl-clj-future-standalone-smoke: pmap lost order")))
  (princ (format "nl-clj-future-standalone-smoke: PASS (%d tests, 10000-worker drive)\n"
                 ran)))

;;; nl-clj-future-standalone-smoke.el ends here
