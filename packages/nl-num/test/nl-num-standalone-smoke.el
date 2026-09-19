;;; nl-num-standalone-smoke.el --- run nl-num tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance for Doc 196 Phases 0-4.  target/nelisp has no ERT,
;; so a small shim registers the exact bodies in nl-num-test.el.  In addition
;; to those reference-contract tests, this runner proves that bignum-backed
;; rational and rational-component complex tagged vectors survive the
;; standalone's `prin1-to-string'/`read-from-string' path.  That is the exact
;; substrate boundary where records were measured print/read-asymmetric.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-num/test/nl-num-standalone-smoke.el

;;; Code:

(require 'ert)

;; Capture stock function identities before the opt-in package is loaded.
(setq nl-num-test--stock-plus (symbol-function '+))
(setq nl-num-test--stock-minus (symbol-function '-))
(setq nl-num-test--stock-times (symbol-function '*))
(setq nl-num-test--stock-divide (symbol-function '/))
(setq nl-num-test--stock-less (symbol-function '<))
(setq nl-num-test--stock-equal (symbol-function '=))

;; Explicit paths are intentional: standalone `require' cannot be trusted to
;; locate an absent library, while each source file's own require still checks
;; the already-provided dependency feature.
(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-num/src/nl-num-core.el")
(load "packages/nl-num/src/nl-num-rational.el")
(load "packages/nl-num/src/nl-num-complex.el")
(load "packages/nl-num/src/nl-num.el")
(load "packages/nl-num/test/nl-num-test.el")

(let* ((big 1267650600228229401496703205376)
       (ratio (nl-/ big 3))
       (printed (prin1-to-string ratio))
       (reread (car (read-from-string printed))))
  (unless (and (equal ratio reread)
               (nl-rationalp reread)
               (bignump (nl-numerator reread))
               (equal (nl-num-pr-str reread)
                      "1267650600228229401496703205376/3"))
    (error "nl-num-standalone-smoke: bignum rational round-trip failed: %S -> %S"
           ratio reread)))

(let* ((number (nl-complex (nl-/ 1 3) (nl-/ 2 5)))
       (printed (prin1-to-string number))
       (reread (car (read-from-string printed))))
  (unless (and (equal number reread)
               (nl-complexp reread)
               (nl-num-= (nl-+ reread (nl-complex (nl-/ 2 3) (nl-/ 3 5)))
                         (nl-complex 1 1)))
    (error "nl-num-standalone-smoke: complex tagged-vector round-trip failed: %S -> %S"
           number reread)))

(let ((tests (reverse nl-smoke--tests))
      (ran 0)
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (condition-case error-data
          (progn
            (funcall (cdr test))
            (setq ran (+ ran 1)))
        (error
         (setq failures
               (cons (format "%s: %S" (car test) error-data) failures)))))
    (setq tests (cdr tests)))
  (princ (format "GATE-COUNT checked=%d findings=%d\n"
                 ran (length failures)))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-num-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 51)
    (error "nl-num-standalone-smoke: only %d tests ran (expected >= 51)"
           ran))
  (princ (format "nl-num-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-num-standalone-smoke.el ends here
