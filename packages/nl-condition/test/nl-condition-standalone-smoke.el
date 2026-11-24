;;; nl-condition-standalone-smoke.el --- run nl-condition tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 169 Phase 4: run the exact ERT
;; test bodies from `nl-condition-test.el' on `target/nelisp' (no ert
;; there).  Reuses the mini ert shim from the nl-prelude smoke runner
;; by loading it first; that file also loads and runs the nl-prelude
;; suite, so this smoke gates both packages.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-condition/test/nl-condition-standalone-smoke.el

;;; Code:

;; Installs the shim, loads nl-prelude + its tests, runs them (errors
;; on failure), and leaves the prelude tests registered in ERT.
(load "packages/nl-prelude/test/nl-prelude-standalone-smoke.el")

;; Reset the registry and run only this package's tests.
(dolist (tt (ert-select-tests t t))
  (ert-delete-test (ert-test-name tt)))

(load "packages/nl-condition/src/nl-condition.el")
(load "packages/nl-condition/test/nl-condition-test.el")

;; Doc 169 restart-resume example (examples/nl-condition/restart-resume.el):
;; keeps the demo honest on the substrate its README claims to support.
(load "packages/nl-condition/test/nl-condition-example-test.el")

(let ((tests (mapcar (lambda (tt) (cons (ert-test-name tt) (ert-test-body tt)))
                      (ert-select-tests t t)))
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
  ;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
  ;; report what the gate checked; its absence is itself a hard failure
  ;; there (see tools/ai/nelisp-ai.sh's `cmd_gate').
  (princ (format "GATE-COUNT checked=%d findings=%d\n" ran (length failures)))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-condition-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 33)
    (error "nl-condition-standalone-smoke: only %d tests ran (expected >= 33)"
           ran))
  (princ (format "nl-condition-standalone-smoke: PASS (%d tests)\n" ran)))

;; Doc 244 WP1 end-to-end: a restart chosen by an `nl-handler-bind'
;; handler recovers from a REAL primitive error, (car 1), and the
;; negative control (no restart established) still reaches
;; `condition-case'.  The marker below is printed only if both held.
(let* ((seen-restart 'unset)
       (recovered
        (condition-case err
            (nl-restart-case
                (nl-handler-bind ((error
                                   (lambda (_c)
                                     (nl-invoke-restart 'recover 'ok))))
                  (car 1)
                  'not-reached)
              (recover (v) (list 'recovered v)))
          (error (list 'escaped err))))
       (control
        (condition-case err
            (nl-handler-bind ((error
                               (lambda (_c)
                                 (setq seen-restart
                                       (nl-find-restart 'recover)))))
              (car 1)
              'not-reached)
          (wrong-type-argument (list 'propagated (car err))))))
  (if (and (equal recovered '(recovered ok))
           (equal control '(propagated wrong-type-argument))
           (null seen-restart))
      (princ "WP1-PRIMITIVE-RESTART-PASS\n")
    (princ (format "WP1-PRIMITIVE-RESTART-FAIL recovered=%S control=%S seen=%S\n"
                   recovered control seen-restart))
    (error "nl-condition-standalone-smoke: WP1 primitive restart failed")))

;;; nl-condition-standalone-smoke.el ends here