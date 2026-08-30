;;; nl-ns-standalone-smoke.el --- run nl-ns tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 169 defect #6 (namespaces): run the exact ERT
;; test bodies from `nl-ns-test.el' on `target/nelisp', which has
;; no ert.  A minimal ert shim (`ert-deftest' / `should' / `should-not'
;; / `should-error') is installed first, then the real test file is
;; loaded and every registered test body is executed.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-ns/test/nl-ns-standalone-smoke.el
;;
;; The final line is `nl-ns-standalone-smoke: PASS (N tests)'; any
;; failure raises an error so the process exits non-zero.
;;
;; Dependencies are loaded explicitly by path: on the standalone,
;; (require 'nl-safe) would silently "succeed" even with the file
;; absent, so `load' is the only trustworthy path (same pattern as the
;; nl-prelude and nl-safe smoke runners).
;;
;; On host Emacs this file is inert for `make test' (its name does not
;; match the `nl-*-test.el' glob) and the shim only installs when ert
;; is absent.

;;; Code:

(defvar nl-smoke--tests nil
  "Alist of (NAME . BODY-FN) registered by the `ert-deftest' shim.")

(unless (featurep 'ert)
  (defmacro ert-deftest (name _args &rest body)
    "Register BODY as test NAME in `nl-smoke--tests'."
    ;; Drop a leading docstring so it is not the (useless) body value.
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
    "Evaluate FORM, expect an error; return (SYMBOL . DATA).
KEYS supports `:type SYMBOL' for condition matching like ert."
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

;; Precondition.  Namespace inference counts prefixes in a hash table,
;; so it needs `puthash' to overwrite an existing key.  Runtimes built
;; before 7cd3b827 ("let `puthash' overwrite an existing key",
;; 2026-08-08) keep the first value, every counter freezes at 1, and
;; inference returns nil for every file -- which surfaces as five
;; puzzling inference failures rather than one clear message.  Say it
;; plainly instead.
(let ((probe (make-hash-table)))
  (puthash 'k 1 probe)
  (puthash 'k 2 probe)
  (unless (equal (gethash 'k probe) 2)
    ;; `princ' first: this runtime does not render `error' format
    ;; arguments, so the message would be lost inside "error: (1)".
    (princ (format "nl-ns-standalone-smoke: FAIL (precondition) -- this runtime's `puthash' does not overwrite an existing key (got %S, want 2). Rebuild the binary at or after 7cd3b827.\n"
                   (gethash 'k probe)))
    (error "nl-ns-standalone-smoke: puthash precondition failed")))

(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")

(load "packages/nl-ns/src/nl-ns.el")
;; Doc 189 §4 Phase 1: `nl-ns-test.el' now evaluates real `nl-ns-define'/
;; `nl-ns-in' forms (its advisory/enforcement consistency test), so
;; `nl-ns-in.el' has to be a real `load', not the `(require 'nl-ns-in)'
;; at that file's own top -- `require' silently "succeeds" here even
;; with the file absent (same reason every other load in this file is
;; explicit).
(load "packages/nl-ns/src/nl-ns-in.el")
(load "packages/nl-ns/test/nl-ns-test.el")

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
    (error "nl-ns-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 48)
    (error "nl-ns-standalone-smoke: only %d tests ran (expected >= 48)"
           ran))
  (princ (format "nl-ns-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-ns-standalone-smoke.el ends here
