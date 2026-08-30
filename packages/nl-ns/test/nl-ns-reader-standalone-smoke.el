;;; nl-ns-reader-standalone-smoke.el --- run nl-ns-reader tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 189 Phase 0 (reader-time namespace
;; resolution): runs the exact ERT test bodies from `nl-ns-reader-
;; test.el' on `target/nelisp', which has no `ert'.  Same shim / load-
;; by-path / run-every-registered-test structure as `nl-ns-in-
;; standalone-smoke.el' -- this file differs only in which package it
;; is proving out.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-ns/test/nl-ns-reader-standalone-smoke.el
;;
;; The final line is `nl-ns-reader-standalone-smoke: PASS (N tests)';
;; any failure raises an error so the process exits non-zero.
;;
;; What this proves that the host-Emacs `make test' run of the same
;; file cannot: `nelisp-read-namespace-resolve' is consulted by
;; `nelisp-read--atom' as compiled into `target/nelisp' itself, not
;; only as interpreted by the development host's own Emacs -- i.e. the
;; hook this Phase 0 change added to `src/nelisp-read.el' is real on
;; the substrate the design doc is actually about, not merely on the
;; host tooling used to develop it.
;;
;; Dependencies are loaded explicitly by path: on the standalone,
;; `(require 'nl-ns-in)' would silently "succeed" even with the file
;; absent, so `load' is the only trustworthy dependency check (same
;; pattern as `nl-ns-in-standalone-smoke.el').

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
       (unless nl-smoke--v
         (error "should failed: %S" ',form))
       nl-smoke--v))
  (defmacro should-not (form)
    `(let ((nl-smoke--v ,form))
       (when nl-smoke--v
         (error "should-not failed: %S" ',form))
       t))
  (defmacro should-error (form &rest keys)
    "Evaluate FORM, expect an error; supports `:type'."
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

;; Namespace membership is accumulated in a hash table.  Check that
;; `puthash' overwrites before failures are obscured by a broken runtime.
(let ((probe (make-hash-table)))
  (puthash 'k 1 probe)
  (puthash 'k 2 probe)
  (unless (equal (gethash 'k probe) 2)
    (princ (format "nl-ns-reader-standalone-smoke: FAIL (precondition) -- this runtime's `puthash' does not overwrite an existing key (got %S, want 2). Rebuild the binary at or after 7cd3b827.\n"
                   (gethash 'k probe)))
    (error "nl-ns-reader-standalone-smoke: puthash precondition failed")))

(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-ns/src/nl-ns-in.el")
(load "packages/nl-ns/src/nl-ns-reader.el")
(load "packages/nl-ns/test/nl-ns-reader-test.el")

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
    (error "nl-ns-reader-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 14)
    (error "nl-ns-reader-standalone-smoke: only %d tests ran (expected >= 14)"
           ran))
  (princ (format "nl-ns-reader-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-ns-reader-standalone-smoke.el ends here
