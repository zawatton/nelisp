;;; nl-condition-test.el --- ERT tests for nl-condition -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nl-condition.el' (Doc 169 Phase 4): pre-unwind
;; handler execution, the decline/fallback protocol, restart
;; establishment/invocation, and -- per the design doc's explicit risk
;; note -- the interplay with `unwind-protect' and dynamic binding.
;;
;; Written without cl-lib/ert-x so the same bodies run on standalone
;; NeLisp through test/nl-condition-standalone-smoke.el.

;;; Code:

(require 'ert)
(require 'nl-condition)

(define-error 'nlc-test-error "nl-condition test error")
(define-error 'nlc-test-child "nl-condition child error" 'nlc-test-error)

(defvar nlc-test--log nil
  "Event log shared by the tests.")

(defvar nlc-test--dyn 'global
  "Dynamic variable observed from handlers to prove pre-unwind runs.")

;;; nl-signal fallback + condition-case interop -----------------------

(ert-deftest nl-condition-signal-without-handlers-is-plain-signal ()
  (let ((err (should-error (nl-signal 'nlc-test-error '(1 2))
                           :type 'nlc-test-error)))
    (should (equal err '(nlc-test-error 1 2)))))

(ert-deftest nl-condition-signal-falls-back-to-condition-case ()
  (should (equal (condition-case err
                     (nl-signal 'nlc-test-error '(payload))
                   (nlc-test-error (cdr err)))
                 '(payload))))

(ert-deftest nl-condition-declined-handler-falls-through ()
  "A handler that returns declines; the signal continues outward."
  (setq nlc-test--log nil)
  (should (equal (condition-case nil
                     (nl-handler-bind ((nlc-test-error
                                        (lambda (_c)
                                          (setq nlc-test--log
                                                (cons 'saw nlc-test--log))
                                          'declined)))
                       (nl-signal 'nlc-test-error nil))
                   (nlc-test-error 'fell-through))
                 'fell-through))
  (should (equal nlc-test--log '(saw))))

;;; Pre-unwind semantics ----------------------------------------------

(ert-deftest nl-condition-handler-runs-before-unwinding ()
  "The handler sees dynamic bindings of the signaling frame.
`condition-case' would have unwound them before its handler runs."
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c)
                           (setq nlc-test--log
                                 (cons nlc-test--dyn nlc-test--log)))))
        (let ((nlc-test--dyn 'inner))
          (nl-signal 'nlc-test-error nil)))
    (nlc-test-error nil))
  (should (equal nlc-test--log '(inner))))

(ert-deftest nl-condition-condition-case-contrast ()
  "Control experiment: `condition-case' handlers run post-unwind."
  (should (eq (condition-case nil
                  (let ((nlc-test--dyn 'inner))
                    (signal 'nlc-test-error nil))
                (nlc-test-error nlc-test--dyn))
              'global)))

(ert-deftest nl-condition-handler-receives-condition-object ()
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-error
                         (lambda (c) (setq nlc-test--log c))))
        (nl-signal 'nlc-test-error '(a b)))
    (nlc-test-error nil))
  (should (equal nlc-test--log '(nlc-test-error a b))))

(ert-deftest nl-condition-handler-matches-parent-condition ()
  "A handler bound on the parent symbol sees the child error."
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-error
                         (lambda (c) (setq nlc-test--log (car c)))))
        (nl-signal 'nlc-test-child nil))
    (error nil))
  (should (eq nlc-test--log 'nlc-test-child)))

(ert-deftest nl-condition-handler-ignores-unrelated-condition ()
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-child
                         (lambda (_c) (setq nlc-test--log 'wrong))))
        (nl-signal 'nlc-test-error nil))
    (error nil))
  (should-not nlc-test--log))

(ert-deftest nl-condition-inner-handlers-run-first ()
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c)
                           (setq nlc-test--log (cons 'outer nlc-test--log)))))
        (nl-handler-bind ((nlc-test-error
                           (lambda (_c)
                             (setq nlc-test--log (cons 'inner nlc-test--log)))))
          (nl-signal 'nlc-test-error nil)))
    (error nil))
  (should (equal nlc-test--log '(outer inner))))

(ert-deftest nl-condition-handler-sees-only-outer-handlers ()
  "Re-signaling from inside a handler must not re-enter that handler."
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c)
                           (setq nlc-test--log (cons 'outer nlc-test--log)))))
        (nl-handler-bind ((nlc-test-error
                           (lambda (_c)
                             (setq nlc-test--log (cons 'inner nlc-test--log))
                             (when (> (length nlc-test--log) 5)
                               (error "handler loop"))
                             ;; Re-signal: only the OUTER handler may see it.
                             (condition-case nil
                                 (nl-signal 'nlc-test-error nil)
                               (nlc-test-error nil)))))
          (nl-signal 'nlc-test-error nil)))
    (error nil))
  ;; inner ran once; the re-signal reached outer; then the original
  ;; signal continued outward to outer once more.
  (should (equal nlc-test--log '(outer outer inner))))

;;; Restarts ----------------------------------------------------------

(ert-deftest nl-condition-restart-normal-value ()
  "Without an invocation, nl-restart-case returns the form's value."
  (should (equal (nl-restart-case (+ 1 2) (unused () 'nope)) 3)))

(ert-deftest nl-condition-invoke-restart-basic ()
  (should (eq (nl-restart-case
                  (nl-handler-bind ((nlc-test-error
                                     (lambda (_c)
                                       (nl-invoke-restart 'use-default))))
                    (nl-signal 'nlc-test-error nil))
                (use-default () 'default))
              'default)))

(ert-deftest nl-condition-invoke-restart-with-args ()
  (should (equal (nl-restart-case
                     (nl-handler-bind ((nlc-test-error
                                        (lambda (_c)
                                          (nl-invoke-restart 'retry 20 22))))
                       (nl-signal 'nlc-test-error nil))
                   (retry (a b) (+ a b)))
                 42)))

(ert-deftest nl-condition-invoke-restart-directly ()
  "Restarts are invocable outside any signal handling."
  (should (eq (nl-restart-case
                  (nl-invoke-restart 'bail)
                (bail () 'bailed))
              'bailed)))

(ert-deftest nl-condition-missing-restart-is-control-error ()
  (should-error (nl-invoke-restart 'nowhere) :type 'nl-control-error)
  (should-error (nl-restart-case
                    (nl-invoke-restart 'not-this-one)
                  (other () 'other))
                :type 'nl-control-error))

(ert-deftest nl-condition-innermost-restart-wins ()
  (should (eq (nl-restart-case
                  (nl-restart-case
                      (nl-invoke-restart 'r)
                    (r () 'inner))
                (r () 'outer))
              'inner)))

(ert-deftest nl-condition-outer-restart-reachable-by-name ()
  (should (eq (nl-restart-case
                  (nl-restart-case
                      (nl-invoke-restart 'outer-only)
                    (inner-only () 'inner))
                (outer-only () 'outer))
              'outer)))

(ert-deftest nl-condition-find-and-compute-restarts ()
  (setq nlc-test--log nil)
  (nl-restart-case
      (nl-restart-case
          (setq nlc-test--log
                (list (nl-find-restart 'use-default)
                      (nl-find-restart 'nowhere)
                      (nl-compute-restarts)))
        (retry (p) p))
    (use-default () nil))
  (should (equal nlc-test--log
                 '(use-default nil (retry use-default)))))

(ert-deftest nl-condition-restart-frame-popped-after-exit ()
  (nl-restart-case (+ 1 1) (gone () nil))
  (should-not (nl-find-restart 'gone))
  (should (equal (nl-compute-restarts) nil)))

(ert-deftest nl-condition-restart-body-runs-with-frame-popped ()
  "Invoking a restart from within its own body must not loop."
  (should (eq (nl-restart-case
                  (nl-invoke-restart 'once)
                (once ()
                      (if (nl-find-restart 'once)
                          'still-established
                        'popped)))
              'popped)))

;;; unwind-protect interplay (the doc's explicit risk area) ------------

(ert-deftest nl-condition-cleanups-run-between-handler-and-restart ()
  "Order: handler (pre-unwind) -> cleanups -> restart body."
  (setq nlc-test--log nil)
  (nl-restart-case
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c)
                           (setq nlc-test--log (cons 'handler nlc-test--log))
                           (nl-invoke-restart 'recover))))
        (unwind-protect
            (nl-signal 'nlc-test-error nil)
          (setq nlc-test--log (cons 'cleanup nlc-test--log))))
    (recover () (setq nlc-test--log (cons 'restart nlc-test--log))))
  (should (equal nlc-test--log '(restart cleanup handler))))

(ert-deftest nl-condition-handler-runs-before-cleanups ()
  "At handler time the unwind-protect cleanup has NOT run yet."
  (setq nlc-test--log nil)
  (nl-restart-case
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c)
                           (setq nlc-test--log (cons (memq 'cleanup nlc-test--log)
                                                     nlc-test--log))
                           (nl-invoke-restart 'recover))))
        (unwind-protect
            (nl-signal 'nlc-test-error nil)
          (setq nlc-test--log (cons 'cleanup nlc-test--log))))
    (recover () nil))
  ;; The nil recorded first proves cleanup hadn't run at handler time.
  (should (equal nlc-test--log '(cleanup nil))))

(ert-deftest nl-condition-nested-cleanups-all-run-in-order ()
  (setq nlc-test--log nil)
  (nl-restart-case
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c) (nl-invoke-restart 'r))))
        (unwind-protect
            (unwind-protect
                (nl-signal 'nlc-test-error nil)
              (setq nlc-test--log (cons 'inner-cleanup nlc-test--log)))
          (setq nlc-test--log (cons 'outer-cleanup nlc-test--log))))
    (r () nil))
  (should (equal nlc-test--log '(outer-cleanup inner-cleanup))))

(ert-deftest nl-condition-dynamic-binding-restored-in-restart-body ()
  "By restart-body time the signaling frame's dynamic lets are gone."
  (should (eq (nl-restart-case
                  (nl-handler-bind ((nlc-test-error
                                     (lambda (_c) (nl-invoke-restart 'probe))))
                    (let ((nlc-test--dyn 'inner))
                      (nl-signal 'nlc-test-error nil)))
                (probe () nlc-test--dyn))
              'global)))

(ert-deftest nl-condition-handler-stack-restored-after-nonlocal-exit ()
  "Escaping through nl-handler-bind must not leak handler frames."
  (setq nlc-test--log nil)
  (nl-restart-case
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c) (nl-invoke-restart 'out))))
        (nl-signal 'nlc-test-error nil))
    (out () nil))
  ;; A later nl-signal with no handlers must fall straight to signal.
  (should (equal (condition-case nil
                     (nl-signal 'nlc-test-error nil)
                   (nlc-test-error 'clean))
                 'clean)))

;;; Doc 169 section 6.2 example ---------------------------------------

(defvar nlc-test--config-calls nil)

(defun nlc-test--load-config (path)
  (setq nlc-test--config-calls (cons path nlc-test--config-calls))
  (if (equal path "/etc/app.conf")
      (nl-signal 'nlc-test-error (list path))
    (list 'config-from path)))

(ert-deftest nl-condition-doc-example-use-default ()
  (setq nlc-test--config-calls nil)
  (should (eq (nl-restart-case
                  (nl-handler-bind ((nlc-test-error
                                     (lambda (_c)
                                       (nl-invoke-restart 'use-default))))
                    (nlc-test--load-config "/etc/app.conf"))
                (use-default () 'default-config)
                (retry (path) (nlc-test--load-config path)))
              'default-config)))

(ert-deftest nl-condition-doc-example-retry ()
  (setq nlc-test--config-calls nil)
  (should (equal (nl-restart-case
                     (nl-handler-bind ((nlc-test-error
                                        (lambda (_c)
                                          (nl-invoke-restart
                                           'retry "/tmp/app.conf"))))
                       (nlc-test--load-config "/etc/app.conf"))
                   (use-default () 'default-config)
                   (retry (path) (nlc-test--load-config path)))
                 '(config-from "/tmp/app.conf")))
  (should (equal nlc-test--config-calls
                 '("/tmp/app.conf" "/etc/app.conf"))))

;;; Expansion-time validation -----------------------------------------

(ert-deftest nl-condition-bad-handler-binding-is-expansion-error ()
  (should-error (macroexpand '(nl-handler-bind ((err)) 1)))
  (should-error (macroexpand '(nl-handler-bind (err) 1))))

(ert-deftest nl-condition-bad-restart-clause-is-expansion-error ()
  (should-error (macroexpand '(nl-restart-case 1 (r))))
  (should-error (macroexpand '(nl-restart-case 1 ("r" () 1)))))

;;; WP1 (Doc 244): primitive errors reach handlers -------------------
;;
;; Restarts used with PLAIN `signal' / primitive errors are established
;; OUTSIDE `nl-handler-bind': NeLisp standalone's `handler-bind-1' runs
;; such handlers after unwinding to the handler frame (see the file
;; commentary of nl-condition.el).

(ert-deftest nl-condition-wp1-primitive-restart-recovers ()
  "A restart chosen by a handler recovers from the real error (car 1)."
  (should (equal (nl-restart-case
                     (nl-handler-bind ((error
                                        (lambda (_c)
                                          (nl-invoke-restart 'recover 'ok))))
                       (car 1)
                       'not-reached)
                   (recover (v) (list 'recovered v)))
                 '(recovered ok))))

(ert-deftest nl-condition-wp1-primitive-handler-gets-condition-object ()
  (let ((seen nil))
    (should (equal (condition-case e
                       (nl-handler-bind ((wrong-type-argument
                                          (lambda (c) (setq seen c))))
                         (car 1))
                     (wrong-type-argument (cons 'caught (cdr e))))
                   '(caught listp 1)))
    (should (equal seen '(wrong-type-argument listp 1)))))

(ert-deftest nl-condition-wp1-primitive-error-function-and-signal ()
  "`error' and plain `signal' reach handlers, innermost first."
  (setq nlc-test--log nil)
  (should (eq (nl-restart-case
                  (nl-handler-bind ((error
                                     (lambda (_c)
                                       (push 'outer nlc-test--log)
                                       (nl-invoke-restart 'out 'done))))
                    (nl-handler-bind ((nlc-test-error
                                       (lambda (_c)
                                         (push 'inner nlc-test--log))))
                      (signal 'nlc-test-child '("x"))))
                (out (v) v))
              'done))
  (should (equal nlc-test--log '(outer inner)))
  (setq nlc-test--log nil)
  (should (equal (nl-restart-case
                     (nl-handler-bind ((error
                                        (lambda (c)
                                          (nl-invoke-restart 'out c))))
                       (error "boom %d" 3))
                   (out (v) v))
                 '(error "boom 3"))))

(ert-deftest nl-condition-wp1-primitive-no-restart-reaches-condition-case ()
  "Negative control: with no restart the primitive error still propagates."
  (let ((found 'unset))
    (should (equal (condition-case e
                       (nl-handler-bind ((error
                                          (lambda (_c)
                                            (setq found
                                                  (nl-find-restart 'recover)))))
                         (car 1))
                     (wrong-type-argument (list 'caught (car e))))
                   '(caught wrong-type-argument)))
    (should (null found))
    ;; Invoking the missing restart from the handler is a control error.
    (should (eq (condition-case e
                    (nl-handler-bind ((error
                                       (lambda (_c)
                                         (nl-invoke-restart 'recover))))
                      (car 1))
                  (nl-control-error 'control-error))
                'control-error))))

(ert-deftest nl-condition-wp1-primitive-inner-condition-case-wins ()
  "An inner `condition-case' catches a plain `signal' before the handler."
  (let ((fired 0))
    (should (eq (nl-handler-bind ((error (lambda (_c) (setq fired (1+ fired)))))
                  (condition-case nil
                      (signal 'nlc-test-error nil)
                    (nlc-test-error 'inner)))
                'inner))
    (should (= fired 0))))

(ert-deftest nl-condition-wp1-primitive-pre-unwind-on-primitive-runtime ()
  "Where `handler-bind-1' is a true primitive, the handler runs pre-unwind."
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((error (lambda (_c) (push 'handler nlc-test--log))))
        (unwind-protect (car 1)
          (push 'cleanup nlc-test--log)))
    (error nil))
  (should (memq 'handler nlc-test--log))
  (should (memq 'cleanup nlc-test--log))
  (when (and (fboundp 'handler-bind-1)
             (subrp (symbol-function 'handler-bind-1)))
    (should (equal nlc-test--log '(cleanup handler)))))

;;; WP1: exactly-once firing --------------------------------------------

(ert-deftest nl-condition-wp1-once-nl-signal-single-frame ()
  (let ((count 0))
    (should (eq (condition-case nil
                    (nl-handler-bind ((nlc-test-error
                                       (lambda (_c) (setq count (1+ count)))))
                      (nl-signal 'nlc-test-error '(1)))
                  (nlc-test-error 'caught))
                'caught))
    (should (= count 1))))

(ert-deftest nl-condition-wp1-once-nl-signal-nested-frames ()
  (setq nlc-test--log nil)
  (condition-case nil
      (nl-handler-bind ((nlc-test-error
                         (lambda (_c) (push 'outer nlc-test--log))))
        (nl-handler-bind ((nlc-test-child
                           (lambda (_c) (push 'inner nlc-test--log)))
                          (error
                           (lambda (_c) (push 'inner-error nlc-test--log))))
          (nl-signal 'nlc-test-child nil)))
    (nlc-test-error nil))
  (should (equal nlc-test--log '(outer inner-error inner))))

(ert-deftest nl-condition-wp1-once-plain-signal-single-frame ()
  (let ((count 0))
    (condition-case nil
        (nl-handler-bind ((nlc-test-error
                           (lambda (_c) (setq count (1+ count)))))
          (signal 'nlc-test-error '(1)))
      (nlc-test-error nil))
    (should (= count 1))))

(ert-deftest nl-condition-wp1-once-mixed-sequence ()
  "An nl-signal followed by a primitive error fires once each."
  (let ((count 0))
    (condition-case nil
        (nl-handler-bind ((error (lambda (_c) (setq count (1+ count)))))
          (nl-signal 'nlc-test-error nil))
      (error nil))
    (should (= count 1))
    (condition-case nil
        (nl-handler-bind ((error (lambda (_c) (setq count (1+ count)))))
          (car 1))
      (error nil))
    (should (= count 2))))

;;; WP1: restart API -----------------------------------------------------

(ert-deftest nl-condition-wp1-restart-api-simple-restart-normal ()
  (should (equal (nl-with-simple-restart (skip "Skip it")
                   1 2 3)
                 '(3 nil))))

(ert-deftest nl-condition-wp1-restart-api-simple-restart-invoked ()
  (should (equal (nl-with-simple-restart (skip "Skip %s" 'it)
                   (nl-invoke-restart 'skip)
                   'not-reached)
                 '(nil t)))
  (should (null (nl-find-restart 'skip))))

(ert-deftest nl-condition-wp1-restart-api-restart-bind-no-unwind ()
  "The restart function runs on top of the stack, before any cleanup."
  (setq nlc-test--log nil)
  (let ((result
         (nl-restart-bind ((fix (lambda (x)
                                  (push 'fix nlc-test--log)
                                  (* x 2))))
           (unwind-protect
               (list 'returned (nl-invoke-restart 'fix 21))
             (push 'cleanup nlc-test--log)))))
    (should (equal result '(returned 42)))
    (should (equal nlc-test--log '(cleanup fix)))))

(ert-deftest nl-condition-wp1-restart-api-restart-bind-visible-and-popped ()
  (nl-restart-bind ((r (lambda () 1)))
    (should (eq (nl-find-restart 'r) 'r))
    (should (memq 'r (nl-compute-restarts))))
  (should (null (nl-find-restart 'r)))
  (should-error (macroexpand '(nl-restart-bind ((r)) 1))))

(ert-deftest nl-condition-wp1-restart-api-cerror-continue ()
  (setq nlc-test--log nil)
  (should (equal (nl-handler-bind ((nlc-test-error
                                    (lambda (_c) (nl-continue))))
                   (list (nl-cerror "Go on" 'nlc-test-error '(1))
                         'after))
                 '(nil after))))

(ert-deftest nl-condition-wp1-restart-api-cerror-unhandled ()
  "Negative control: no handler, the error propagates."
  (should (eq (condition-case nil
                  (nl-cerror "Go on" 'nlc-test-error '(1))
                (nlc-test-error 'propagated))
              'propagated))
  (should (eq (condition-case nil
                  (nl-handler-bind ((nlc-test-error #'ignore))
                    (nl-cerror "Go on" 'nlc-test-error '(1)))
                (nlc-test-error 'declined))
              'declined)))

;;; WP1: standard restarts -----------------------------------------------

(ert-deftest nl-condition-wp1-standard-abort ()
  (should (eq (nl-restart-case (nl-abort) (abort () 'aborted)) 'aborted))
  (should (eq (condition-case nil (nl-abort) (nl-control-error 'none))
              'none)))

(ert-deftest nl-condition-wp1-standard-continue-and-missing ()
  (should (eq (nl-restart-case (progn (nl-continue) 'no) (continue () 'yes))
              'yes))
  (should (null (nl-continue)))
  (should (null (nl-muffle-warning)))
  (should (null (nl-use-value 1)))
  (should (null (nl-store-value 1))))

(ert-deftest nl-condition-wp1-standard-use-value-and-store-value ()
  (should (equal (nl-restart-case (nl-use-value 'v) (use-value (x) (list x)))
                 '(v)))
  (should (equal (nl-restart-case (nl-store-value 7) (store-value (x) (list x)))
                 '(7))))

(ert-deftest nl-condition-wp1-standard-warn-muffled ()
  (let* ((reports nil)
         (nl-warn-report-function (lambda (text) (push text reports))))
    (should (null (nl-handler-bind ((nl-warning
                                     (lambda (_c) (nl-muffle-warning))))
                    (nl-warn "careful %d" 1))))
    (should (null reports))))

(ert-deftest nl-condition-wp1-standard-warn-reported-and-seen ()
  (let* ((reports nil)
         (seen nil)
         (nl-warn-report-function (lambda (text) (push text reports))))
    (should (null (nl-handler-bind ((nl-warning (lambda (c) (setq seen c))))
                    (nl-warn "careful %d" 2))))
    (should (equal seen '(nl-warning "careful 2")))
    (should (equal reports '("careful 2")))
    ;; A warning is not an `error'.
    (setq reports nil)
    (should (null (nl-handler-bind ((error (lambda (_c) (error "wrong"))))
                    (nl-warn "plain"))))
    (should (equal reports '("plain")))))

;;; WP1: correctable check-type / assert ---------------------------------

(ert-deftest nl-condition-wp1-correctable-check-type-passes ()
  (let ((x 5))
    (should (null (nl-check-type x integer)))
    (should (= x 5))))

(ert-deftest nl-condition-wp1-correctable-check-type-store-value ()
  (let ((x "oops"))
    (nl-handler-bind ((wrong-type-argument
                       (lambda (_c) (nl-store-value 42))))
      (nl-check-type x integer))
    (should (eql x 42))))

(ert-deftest nl-condition-wp1-correctable-check-type-rechecks ()
  "A bad replacement is checked again; the second one is accepted."
  (let ((x "oops")
        (offers (list 'still-bad 9)))
    (nl-handler-bind ((wrong-type-argument
                       (lambda (_c) (nl-store-value (pop offers)))))
      (nl-check-type x integer))
    (should (eql x 9))
    (should (null offers))))

(ert-deftest nl-condition-wp1-correctable-check-type-uncorrected ()
  (let ((x 'sym))
    (should (equal (condition-case e
                       (nl-check-type x string)
                     (wrong-type-argument (list (nth 1 e) (nth 2 e))))
                   '(string sym)))
    (should (eq x 'sym))))

(ert-deftest nl-condition-wp1-correctable-assert-continue-with-value ()
  (let ((n 0))
    (should (null (nl-handler-bind ((nl-assertion-failed
                                     (lambda (_c)
                                       (nl-invoke-restart 'continue 5))))
                    (nl-assert (> n 3) (n)))))
    (should (= n 5))))

(ert-deftest nl-condition-wp1-correctable-assert-passes-and-fails ()
  (should (null (nl-assert t)))
  (should (eq (condition-case nil
                  (nl-assert (= 1 2))
                (nl-assertion-failed 'failed))
              'failed)))

;;; WP1: nl-signal-condition ---------------------------------------------

(ert-deftest nl-condition-wp1-signal-condition-unhandled-returns-nil ()
  (should (null (nl-signal-condition 'nlc-test-error '(1))))
  (should (null (nl-signal-condition 'nlc-test-undeclared '(1)))))

(ert-deftest nl-condition-wp1-signal-condition-undeclared-symbol-runs-handler ()
  (let ((seen nil))
    (should (null (nl-handler-bind ((nlc-test-undeclared
                                     (lambda (c) (setq seen c))))
                    (nl-signal-condition 'nlc-test-undeclared '(1 2)))))
    (should (equal seen '(nlc-test-undeclared 1 2)))))

(ert-deftest nl-condition-wp1-signal-condition-handler-can-escape ()
  (should (eq (nl-restart-case
                  (nl-handler-bind ((nlc-test-undeclared
                                     (lambda (_c)
                                       (nl-invoke-restart 'out 'escaped))))
                    (nl-signal-condition 'nlc-test-undeclared nil)
                    'not-escaped)
                (out (v) v))
              'escaped)))

;;; WP1: no stale duplicate suppression ---------------------------------

(ert-deftest nl-condition-wp1-no-stale-suppression-later-equal-signal ()
  "A later signal with equal data is not mistaken for an earlier nl-signal."
  (let ((n 0))
    (condition-case nil
        (nl-handler-bind ((error (lambda (_c) (setq n (1+ n)))))
          (condition-case nil (nl-signal 'error '("x")) (error nil))
          (signal 'error '("x")))
      (error nil))
    (should (= n 2))))

(ert-deftest nl-condition-wp1-no-stale-suppression-fires-exactly-once ()
  "The signal nl-signal itself falls back to still fires each handler once."
  (let ((n 0))
    (condition-case nil
        (nl-handler-bind ((error (lambda (_c) (setq n (1+ n)))))
          (nl-signal 'error '("x")))
      (error nil))
    (should (= n 1))))

(ert-deftest nl-condition-wp1-no-stale-suppression-nil-data-once ()
  "Nil data cannot be told apart by identity: still fires exactly once.
The documented price is that a stale nil-data entry may suppress one
later equal signal; consuming the entry bounds that to one signal."
  (let ((n 0))
    (condition-case nil
        (nl-handler-bind ((nlc-test-error (lambda (_c) (setq n (1+ n)))))
          (nl-signal 'nlc-test-error nil))
      (nlc-test-error nil))
    (should (= n 1))))

(provide 'nl-condition-test)

;;; nl-condition-test.el ends here