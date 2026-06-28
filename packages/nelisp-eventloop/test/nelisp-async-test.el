;;; nelisp-async-test.el --- tests for the async timer runtime -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-actor)
(require 'nelisp-eventloop)
(require 'nelisp-async)

(defmacro nelisp-async-test--fresh (&rest body)
  "Run BODY with a freshly reset actor registry + timer queue."
  (declare (indent 0))
  `(progn
     (nelisp-actor--reset)
     (nelisp-async-reset-timers)
     ,@body))

(ert-deftest nelisp-async-one-shot-fires ()
  (nelisp-async-test--fresh
    (let ((fired nil))
      (run-at-time 0.02 nil (lambda () (setq fired t)))
      (should (eq (nelisp-async-run) :idle))
      (should fired))))

(ert-deftest nelisp-async-one-shot-timing ()
  (nelisp-async-test--fresh
    (let ((at nil) (t0 (float-time)))
      (run-at-time 0.1 nil (lambda () (setq at (float-time))))
      (nelisp-async-run)
      ;; Fired no earlier than the requested delay (allow scheduler slack up).
      (should at)
      (should (>= (- at t0) 0.09)))))

(ert-deftest nelisp-async-cancel-prevents-fire ()
  (nelisp-async-test--fresh
    (let ((fired nil))
      (let ((tm (run-at-time 0.02 nil (lambda () (setq fired t)))))
        (cancel-timer tm))
      (should (eq (nelisp-async-run) :idle))
      (should-not fired))))

(ert-deftest nelisp-async-repeat-rearms ()
  (nelisp-async-test--fresh
    (let ((n 0) (tm nil))
      (setq tm (run-at-time 0.01 0.01
                            (lambda () (setq n (1+ n)) (when (>= n 3) (cancel-timer tm)))))
      (nelisp-async-run)
      (should (= n 3)))))

(ert-deftest nelisp-async-deadline-order ()
  ;; Three timers armed out of order fire in deadline order.
  (nelisp-async-test--fresh
    (let ((log nil))
      (run-at-time 0.06 nil (lambda () (push 'c log)))
      (run-at-time 0.02 nil (lambda () (push 'a log)))
      (run-at-time 0.04 nil (lambda () (push 'b log)))
      (nelisp-async-run)
      (should (equal '(a b c) (nreverse log))))))

(ert-deftest nelisp-async-sit-for-services-timers ()
  ;; `sit-for' must fire a due timer while it waits (Emacs semantics).
  (nelisp-async-test--fresh
    (let ((fired nil))
      (run-at-time 0.02 nil (lambda () (setq fired t)))
      (should (eq (sit-for 0.1) t))
      (should fired))))

(ert-deftest nelisp-async-run-stops-on-main-quit ()
  ;; A timer sends a quit; the driver returns the actor's terminal status.
  (nelisp-async-test--fresh
    (setq nelisp-eventloop--bindings (make-hash-table :test 'equal))
    (let ((main (nelisp-eventloop-spawn-main-actor)))
      (run-at-time 0.02 nil
                   (lambda () (nelisp-send main (nelisp-make-event 'quit nil))))
      (should (eq (nelisp-async-run main) :dead)))))

(ert-deftest nelisp-async-timer-routes-event-to-actor ()
  ;; End-to-end: scheduled timer -> event -> main actor dispatch -> command.
  (nelisp-async-test--fresh
    (setq nelisp-eventloop--bindings (make-hash-table :test 'equal))
    (let ((seen nil))
      (let* ((cmd (lambda (_ev) (setq seen (cons 'tick seen))))
             (main (nelisp-eventloop-spawn-main-actor)))
        (nelisp-eventloop-schedule-timer 0.02 cmd main)
        (run-at-time 0.08 nil
                     (lambda () (nelisp-send main (nelisp-make-event 'quit nil))))
        (nelisp-async-run main)
        (should (equal '(tick) seen))))))

(provide 'nelisp-async-test)
;;; nelisp-async-test.el ends here
