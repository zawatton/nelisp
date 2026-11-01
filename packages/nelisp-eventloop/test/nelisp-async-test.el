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
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-async-test--fresh
    (let ((fired nil))
      (run-at-time 0.02 nil (lambda () (setq fired t)))
      (should (eq (nelisp-async-run) :idle))
      (should fired))))

(ert-deftest nelisp-async-one-shot-timing ()
  (skip-unless (fboundp 'alloc-bytes))
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
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-async-test--fresh
    (let ((n 0) (tm nil))
      (setq tm (run-at-time 0.01 0.01
                            (lambda () (setq n (1+ n)) (when (>= n 3) (cancel-timer tm)))))
      (nelisp-async-run)
      (should (= n 3)))))

(ert-deftest nelisp-async-run-with-timer-deferred ()
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-async-test--fresh
    (let ((got nil))
      (let ((tm (nelisp-async-core-run-with-timer
                 0.01 nil (lambda (a b) (setq got (list a b))) 1 2)))
        (should (timerp tm))
        (should-not got))
      (nelisp-async-run)
      (should (equal got '(1 2))))))

(ert-deftest nelisp-async-run-with-timer-repeat-cancel ()
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-async-test--fresh
    (let ((n 0) (tm nil))
      (setq tm (nelisp-async-core-run-with-timer
                0.01 0.01
                (lambda () (setq n (1+ n)) (when (>= n 3) (cancel-timer tm)))))
      (nelisp-async-run)
      (should (= n 3)))))

(ert-deftest nelisp-async-run-with-idle-timer-fires-and-cancels ()
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-async-test--fresh
    (let ((fired nil) (cancelled nil))
      (should (timerp (nelisp-async-core-run-with-idle-timer
                       0.01 nil (lambda () (setq fired t)))))
      (cancel-timer (nelisp-async-core-run-with-idle-timer
                     0.01 nil (lambda () (setq cancelled t))))
      (nelisp-async-run)
      (should fired)
      (should-not cancelled))))

(ert-deftest nelisp-async-run-with-idle-timer-repeat ()
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-async-test--fresh
    (let ((n 0) (tm nil))
      (setq tm (nelisp-async-core-run-with-idle-timer
                0.01 t (lambda () (setq n (1+ n)) (when (>= n 2) (cancel-timer tm)))))
      (nelisp-async-run)
      (should (= n 2)))))

(ert-deftest nelisp-async-run-with-timer-invalid-secs ()
  (should-error (nelisp-async-core-run-with-timer "ABC" nil #'ignore))
  (should-error (nelisp-async-core-run-with-idle-timer '(1) nil #'ignore)))

(ert-deftest nelisp-async-deadline-order ()
  (skip-unless (fboundp 'alloc-bytes))
  ;; Three timers armed out of order fire in deadline order.
  (nelisp-async-test--fresh
    (let ((log nil))
      (run-at-time 0.06 nil (lambda () (push 'c log)))
      (run-at-time 0.02 nil (lambda () (push 'a log)))
      (run-at-time 0.04 nil (lambda () (push 'b log)))
      (nelisp-async-run)
      (should (equal '(a b c) (nreverse log))))))

(ert-deftest nelisp-async-sit-for-services-timers ()
  (skip-unless (fboundp 'alloc-bytes))
  ;; `sit-for' must fire a due timer while it waits (Emacs semantics).
  (nelisp-async-test--fresh
    (let ((fired nil))
      (run-at-time 0.02 nil (lambda () (setq fired t)))
      (should (eq (sit-for 0.1) t))
      (should fired))))

(ert-deftest nelisp-async-run-stops-on-main-quit ()
  (skip-unless (fboundp 'alloc-bytes))
  ;; A timer sends a quit; the driver returns the actor's terminal status.
  (nelisp-async-test--fresh
    (setq nelisp-eventloop--bindings (make-hash-table :test 'equal))
    (let ((main (nelisp-eventloop-spawn-main-actor)))
      (run-at-time 0.02 nil
                   (lambda () (nelisp-send main (nelisp-make-event 'quit nil))))
      (should (eq (nelisp-async-run main) :dead)))))

(ert-deftest nelisp-async-timer-routes-event-to-actor ()
  (skip-unless (fboundp 'alloc-bytes))
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
