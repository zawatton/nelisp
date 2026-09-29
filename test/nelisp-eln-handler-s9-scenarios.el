;;; nelisp-eln-handler-s9-scenarios.el --- Doc 210 S9 shared probe scenarios -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Scenarios that exercise the host-compiled condition-case probes
;; (test/fixtures/eln-handler/s9-handler-probes.eln) through the platform
;; function `s9-call' and print a transcript.  The same file is loaded by
;; host GNU Emacs 31.1 (test/nelisp-eln-handler-s9-host.el, where the probes
;; are native code loaded by GNU's own loader) and by the NeLisp driver
;; (test/nelisp-eln-handler-s9-driver.el, where the same native code runs
;; against the shadow handler blocks).  Every transcript line begins with
;; `T ' and the two transcripts must be identical.
;;
;; The platform defines, before calling `s9-run-group':
;;   (s9-call PROBE-SYMBOL ARGUMENT)   call a native probe
;;   (s9-cleanup-around CLEANUP BODY)  run BODY with CLEANUP registered as a
;;                                     native-specpdl cleanup (host: an
;;                                     `unwind-protect')
;;   (s9-gc)                           force a garbage collection

;;; Code:

(defvar s9-probe-dyn 0)
(defvar s9-trace nil)

(defun s9-t (format-string &rest arguments)
  (push (apply #'format format-string arguments) s9-trace))

(defun s9-run (name thunk)
  "Run THUNK as scenario NAME and print its transcript."
  (setq s9-trace nil)
  (setq internal-when-entered-debugger -1)
  (setq s9-probe-dyn 0)
  (let ((outcome
         (condition-case e
             (list 'value (catch 's9-tag (funcall thunk)))
           (error (list 'error e))
           (quit (list 'quit)))))
    (princ (format "T scenario %s\n" name))
    (dolist (line (nreverse s9-trace))
      (princ (format "T   trace: %s\n" line)))
    (princ (format "T   outcome: %S dyn=%S\n" outcome s9-probe-dyn))))

;;; Lisp bodies run inside the guarded region

(defun s9-raise-void ()
  (s9-t "raise void-variable dyn=%S" s9-probe-dyn)
  (signal 'void-variable '(s9-zz)))
(defun s9-raise-arith ()
  (s9-t "raise arith-error dyn=%S" s9-probe-dyn)
  (signal 'arith-error nil))
(defun s9-raise-error ()
  (s9-t "raise error dyn=%S" s9-probe-dyn)
  (signal 'error '("s9 boom")))
(defun s9-raise-quit ()
  (s9-t "raise quit dyn=%S" s9-probe-dyn)
  (signal 'quit nil))
(defun s9-return-fine ()
  (s9-t "f runs dyn=%S" s9-probe-dyn)
  'fine)
(defun s9-throw ()
  (s9-t "throw dyn=%S" s9-probe-dyn)
  (throw 's9-tag 'thrown-value))

(defun s9-f-cleanup-order ()
  (s9-cleanup-around (lambda () (s9-t "cleanup dyn=%S" s9-probe-dyn))
                     #'s9-raise-void))
(defun s9-f-cleanup-raises ()
  (s9-cleanup-around (lambda ()
                       (s9-t "cleanup-begin dyn=%S" s9-probe-dyn)
                       (signal 'arith-error '(from-cleanup)))
                     #'s9-raise-void))
(defun s9-f-cleanup-two ()
  (s9-cleanup-around
   (lambda () (s9-t "outer cleanup dyn=%S" s9-probe-dyn))
   (lambda ()
     (s9-cleanup-around (lambda () (s9-t "inner cleanup dyn=%S" s9-probe-dyn))
                        #'s9-raise-void))))
(defun s9-f-cleanup-fine ()
  (s9-cleanup-around (lambda () (s9-t "cleanup dyn=%S" s9-probe-dyn))
                     #'s9-return-fine))

(defun s9-f-inner-unmatched ()
  (s9-t "inner activation start dyn=%S" s9-probe-dyn)
  (s9-call 's9-probe-arith 's9-raise-void))
(defun s9-f-inner-matched ()
  (s9-t "inner activation start dyn=%S" s9-probe-dyn)
  (s9-call 's9-probe-arith 's9-raise-arith))
(defun s9-f-inner-nested-let ()
  (s9-t "inner activation start dyn=%S" s9-probe-dyn)
  (s9-call 's9-probe-nested 's9-raise-void))
(defun s9-f-inner-inner-error ()
  (s9-t "inner activation start dyn=%S" s9-probe-dyn)
  (s9-call 's9-probe-error 's9-f-inner-unmatched))

(defun s9-f-gc-then-raise ()
  (s9-gc)
  (s9-t "after gc dyn=%S" s9-probe-dyn)
  (s9-raise-void))
(defun s9-f-gc-cleanup ()
  (s9-cleanup-around (lambda () (s9-gc) (s9-t "cleanup after gc dyn=%S" s9-probe-dyn))
                     #'s9-raise-void))
(defun s9-f-gc-fine ()
  (s9-gc)
  (s9-return-fine))

(defun s9-f-hb-observe ()
  (handler-bind ((error (lambda (e) (s9-t "handler-bind saw %S dyn=%S" e s9-probe-dyn))))
    (s9-raise-error)))
(defun s9-f-hb-throw ()
  (handler-bind ((error (lambda (e)
                          (s9-t "handler-bind throwing %S" e)
                          (throw 's9-tag 'hb-thrown))))
    (s9-raise-error)))
(defun s9-f-hb-quit ()
  (handler-bind ((quit (lambda (e) (s9-t "handler-bind saw %S" e))))
    (s9-raise-quit)))

(defun s9-debugger (&rest arguments)
  (s9-t "debugger %S dyn=%S" arguments s9-probe-dyn)
  'debugger-ret)

;;; Scenario groups

(defun s9-scenarios-match ()
  (s9-run "match-error-void" (lambda () (s9-call 's9-probe-error 's9-raise-void)))
  (s9-run "match-error-arith" (lambda () (s9-call 's9-probe-error 's9-raise-arith)))
  (s9-run "match-error-plain" (lambda () (s9-call 's9-probe-error 's9-raise-error)))
  (s9-run "match-error-fine" (lambda () (s9-call 's9-probe-error 's9-return-fine)))
  (s9-run "match-quit-quit" (lambda () (s9-call 's9-probe-quit 's9-raise-quit)))
  (s9-run "nomatch-quit-error" (lambda () (s9-call 's9-probe-quit 's9-raise-error)))
  (s9-run "nomatch-error-quit" (lambda () (s9-call 's9-probe-error 's9-raise-quit)))
  (s9-run "match-t-quit" (lambda () (s9-call 's9-probe-t 's9-raise-quit)))
  (s9-run "match-t-error" (lambda () (s9-call 's9-probe-t 's9-raise-error)))
  (s9-run "match-arith-arith" (lambda () (s9-call 's9-probe-arith 's9-raise-arith)))
  (s9-run "nomatch-arith-void" (lambda () (s9-call 's9-probe-arith 's9-raise-void)))
  (s9-run "nomatch-arith-error" (lambda () (s9-call 's9-probe-arith 's9-raise-error)))
  (s9-run "throw-passes-handler" (lambda () (s9-call 's9-probe-error 's9-throw)))
  (s9-run "nomatch-let-restores" (lambda () (s9-call 's9-probe-let 's9-raise-quit))))

(defun s9-scenarios-unwind ()
  (s9-run "unwind-let-error" (lambda () (s9-call 's9-probe-let 's9-raise-void)))
  (s9-run "unwind-let-fine" (lambda () (s9-call 's9-probe-let 's9-return-fine)))
  (s9-run "unwind-nested-outer" (lambda () (s9-call 's9-probe-nested 's9-raise-void)))
  (s9-run "unwind-nested-inner" (lambda () (s9-call 's9-probe-nested 's9-raise-arith)))
  (s9-run "unwind-nested-quit-passes"
          (lambda () (s9-call 's9-probe-nested 's9-raise-quit)))
  (s9-run "unwind-cleanup-order"
          (lambda () (s9-call 's9-probe-let 's9-f-cleanup-order)))
  (s9-run "unwind-cleanup-raises"
          (lambda () (s9-call 's9-probe-let 's9-f-cleanup-raises)))
  (s9-run "unwind-cleanup-two"
          (lambda () (s9-call 's9-probe-let 's9-f-cleanup-two)))
  (s9-run "unwind-cleanup-fine"
          (lambda () (s9-call 's9-probe-let 's9-f-cleanup-fine)))
  (s9-run "unwind-cleanup-raises-nested"
          (lambda () (s9-call 's9-probe-nested 's9-f-cleanup-raises)))
  (s9-run "nested-activation-unmatched-inner"
          (lambda () (s9-call 's9-probe-error 's9-f-inner-unmatched)))
  (s9-run "nested-activation-matched-inner"
          (lambda () (s9-call 's9-probe-error 's9-f-inner-matched)))
  (s9-run "nested-activation-let"
          (lambda () (s9-call 's9-probe-let 's9-f-inner-nested-let)))
  (s9-run "nested-activation-two-deep"
          (lambda () (s9-call 's9-probe-error 's9-f-inner-inner-error))))

(defun s9-scenarios-gc ()
  (s9-run "gc-guarded-error" (lambda () (s9-call 's9-probe-error 's9-f-gc-then-raise)))
  (s9-run "gc-guarded-fine" (lambda () (s9-call 's9-probe-error 's9-f-gc-fine)))
  (s9-run "gc-let-error" (lambda () (s9-call 's9-probe-let 's9-f-gc-then-raise)))
  (s9-run "gc-in-cleanup" (lambda () (s9-call 's9-probe-let 's9-f-gc-cleanup))))

(defun s9-scenarios-debugger ()
  (s9-run "dbg-hb-observe-declines"
          (lambda () (s9-call 's9-probe-error 's9-f-hb-observe)))
  (s9-run "dbg-hb-throws-skips-handler"
          (lambda () (s9-call 's9-probe-error 's9-f-hb-throw)))
  (s9-run "dbg-hb-quit-observe"
          (lambda () (s9-call 's9-probe-quit 's9-f-hb-quit)))
  (let ((debug-on-error t) (debugger #'s9-debugger))
    (s9-run "dbg-plain-list-no-debugger"
            (lambda () (s9-call 's9-probe-error 's9-raise-error)))
    (s9-run "dbg-debug-tag-calls-debugger"
            (lambda () (s9-call 's9-probe-debug 's9-raise-error)))
    (s9-run "dbg-debug-tag-let-sees-binding"
            (lambda () (s9-call 's9-probe-debug-let 's9-raise-void)))
    (s9-run "dbg-hb-then-debugger"
            (lambda () (s9-call 's9-probe-debug 's9-f-hb-observe)))
    (s9-run "dbg-debug-tag-nonmatching-condition"
            (lambda () (s9-call 's9-probe-debug 's9-raise-quit)))
    (let ((debug-on-signal t))
      (s9-run "dbg-signal-flag-calls-debugger"
              (lambda () (s9-call 's9-probe-error 's9-raise-error))))
    (let ((debug-ignored-errors '(void-variable)))
      (s9-run "dbg-ignored-condition-skips"
              (lambda () (s9-call 's9-probe-debug 's9-raise-void)))
      (s9-run "dbg-ignored-condition-other-still-calls"
              (lambda () (s9-call 's9-probe-debug 's9-raise-error))))
    (let ((debug-ignored-errors '("s9 boom")))
      (s9-run "dbg-ignored-message-skips"
              (lambda () (s9-call 's9-probe-debug 's9-raise-error))))
    (let ((inhibit-debugger t))
      (s9-run "dbg-inhibit-debugger"
              (lambda () (s9-call 's9-probe-debug 's9-raise-error)))))
  (let ((debug-on-error nil) (debugger #'s9-debugger))
    (s9-run "dbg-off-no-debugger"
            (lambda () (s9-call 's9-probe-debug 's9-raise-error))))
  (let ((debug-on-error '(arith-error)) (debugger #'s9-debugger))
    (s9-run "dbg-on-error-list-hit"
            (lambda () (s9-call 's9-probe-debug 's9-raise-arith)))
    (s9-run "dbg-on-error-list-miss"
            (lambda () (s9-call 's9-probe-debug 's9-raise-error))))
  (let ((debug-on-quit t) (debug-on-signal t) (debugger #'s9-debugger))
    (s9-run "dbg-quit-signal-flag"
            (lambda () (s9-call 's9-probe-quit 's9-raise-quit)))))

(defun s9-run-group (group)
  (cond ((equal group "match") (s9-scenarios-match))
        ((equal group "unwind") (s9-scenarios-unwind))
        ((equal group "gc") (s9-scenarios-gc))
        ((equal group "debugger") (s9-scenarios-debugger))
        ((equal group "all")
         (s9-scenarios-match) (s9-scenarios-unwind) (s9-scenarios-gc)
         (s9-scenarios-debugger))
        (t (error "unknown S9 scenario group: %s" group))))

(provide 'nelisp-eln-handler-s9-scenarios)

;;; nelisp-eln-handler-s9-scenarios.el ends here
