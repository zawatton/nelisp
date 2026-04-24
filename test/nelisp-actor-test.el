;;; nelisp-actor-test.el --- Phase 4 actor runtime ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4.1 shipped the primitive skeleton with a run-to-completion
;; mock scheduler; the original 8 ERTs are preserved here in their
;; generator-adapted form (actor thunks now use `nelisp-actor-lambda'
;; and `receive-empty-returns-timeout' is replaced by
;; `receive-blocks-until-sent' since empty-mailbox receive now
;; suspends).  Phase 4.2 adds 10 further ERTs exercising the
;; cooperative scheduler: round-robin fairness, voluntary yield,
;; nil-message preservation, crash transition, timer start/stop,
;; scheduler idle semantics.
;;
;; See docs/design/10-phase4-actor.org §4.1 / §4.2.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp)
(require 'nelisp-actor)

(defun nelisp-actor-test--fixture ()
  "Reset actor runtime state before each test."
  (nelisp-actor--reset))

;;; Spawn + initial state (carried from Phase 4.1) -------------------

(ert-deftest nelisp-actor-spawn-initial-state ()
  "Fresh spawn yields :runnable actor with empty mailbox and owned-set hash."
  (nelisp-actor-test--fixture)
  (let ((actor (nelisp-spawn (nelisp-actor-lambda))))
    (should (nelisp-actor-p actor))
    (should (symbolp (nelisp-actor-id actor)))
    (should (eq (nelisp-actor-status actor) :runnable))
    (should (null (nelisp-actor-mailbox actor)))
    (should (hash-table-p (nelisp-actor-owned-set actor)))
    (should (nelisp-actor-iterator actor))))

(ert-deftest nelisp-actor-spawn-rejects-non-function ()
  "Non-function THUNK signals wrong-type-argument at spawn time."
  (nelisp-actor-test--fixture)
  (should-error (nelisp-spawn 42) :type 'wrong-type-argument))

;;; Self binding -----------------------------------------------------

(ert-deftest nelisp-actor-self-inside-thunk ()
  "`nelisp-self' returns the current actor during thunk execution."
  (nelisp-actor-test--fixture)
  (let (captured actor)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq captured (nelisp-self)))))
    (nelisp-actor-run-until-idle)
    (should (eq captured actor))))

(ert-deftest nelisp-actor-self-outside-thunk ()
  "`nelisp-self' is nil when called outside any actor."
  (nelisp-actor-test--fixture)
  (should (null (nelisp-self))))

;;; Send + receive FIFO ----------------------------------------------

(ert-deftest nelisp-actor-receive-fifo ()
  "Receive returns messages in send order."
  (nelisp-actor-test--fixture)
  (let (got actor)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (push (nelisp-receive) got)
                   (push (nelisp-receive) got)
                   (push (nelisp-receive) got))))
    (nelisp-send actor 'one)
    (nelisp-send actor 'two)
    (nelisp-send actor 'three)
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse got) '(one two three)))))

(ert-deftest nelisp-actor-receive-blocks-until-sent ()
  "Phase 4.2: receive on empty mailbox blocks, not returns `timeout'."
  (nelisp-actor-test--fixture)
  (let (result actor)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq result (nelisp-receive)))))
    (nelisp-actor-run-until-idle)
    (should (null result))
    (should (eq (nelisp-actor-status actor) :blocked-receive))
    (nelisp-send actor 'delivered)
    (should (eq (nelisp-actor-status actor) :runnable))
    (nelisp-actor-run-until-idle)
    (should (eq result 'delivered))
    (should (eq (nelisp-actor-status actor) :dead))))

;;; Send to dead actor -----------------------------------------------

(ert-deftest nelisp-actor-send-to-dead-errors ()
  "Sending to a :dead actor signals `nelisp-actor-error'."
  (nelisp-actor-test--fixture)
  (let ((actor (nelisp-spawn (nelisp-actor-lambda))))
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status actor) :dead))
    (should-error (nelisp-send actor 'msg)
                  :type 'nelisp-actor-error)))

;;; Spawn-heavy -------------------------------------------------------

(ert-deftest nelisp-actor-spawn-heavy-100 ()
  "100 spawns all run to :dead under run-until-idle."
  (nelisp-actor-test--fixture)
  (let (actors)
    (dotimes (_ 100)
      (push (nelisp-spawn (nelisp-actor-lambda)) actors))
    (should (= 100 (length actors)))
    (should (= 100 (length (nelisp-actor-list))))
    (should (= 100 (nelisp-actor-run-until-idle)))
    (dolist (a actors)
      (should (eq (nelisp-actor-status a) :dead)))))

;;; Phase 4.2 — cooperative scheduler specifics ----------------------

(ert-deftest nelisp-actor-yield-returns-control ()
  "Explicit `nelisp-yield' re-enqueues the actor and lets another run."
  (nelisp-actor-test--fixture)
  (let ((log nil))
    (nelisp-spawn
     (nelisp-actor-lambda
       (push 'a1 log) (nelisp-yield)
       (push 'a2 log) (nelisp-yield)
       (push 'a3 log)))
    (nelisp-spawn
     (nelisp-actor-lambda
       (push 'b1 log) (nelisp-yield)
       (push 'b2 log) (nelisp-yield)
       (push 'b3 log)))
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse log) '(a1 b1 a2 b2 a3 b3)))))

(ert-deftest nelisp-actor-scheduler-round-robin-after-yield ()
  "Three-actor round robin preserves FIFO order across yields."
  (nelisp-actor-test--fixture)
  (let ((log nil))
    (dolist (tag '(x y z))
      (nelisp-spawn
       (let ((tag tag))
         (nelisp-actor-lambda
           (push (list tag 1) log) (nelisp-yield)
           (push (list tag 2) log)))))
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse log)
                   '((x 1) (y 1) (z 1) (x 2) (y 2) (z 2))))))

(ert-deftest nelisp-actor-ping-pong-via-receive-blocking ()
  "Two actors exchange messages: empty receive blocks, send unblocks."
  (nelisp-actor-test--fixture)
  (let ((a-log nil) (b-log nil) actor-a actor-b)
    (setq actor-b
          (nelisp-spawn
           (nelisp-actor-lambda
             (let ((m (nelisp-receive)))
               (push m b-log)
               (nelisp-send (car m) (cons 'pong (cdr m)))))))
    (setq actor-a
          (nelisp-spawn
           (let ((b actor-b))
             (nelisp-actor-lambda
               (nelisp-send b (cons (nelisp-self) 'ping))
               (push (nelisp-receive) a-log)))))
    (nelisp-actor-run-until-idle)
    (should (equal a-log '((pong . ping))))
    (should (equal (length b-log) 1))
    (let ((got (car b-log)))
      (should (eq (car got) actor-a))
      (should (eq (cdr got) 'ping)))
    (should (eq (nelisp-actor-status actor-a) :dead))
    (should (eq (nelisp-actor-status actor-b) :dead))))

(ert-deftest nelisp-actor-receive-preserves-nil-message ()
  "Sending nil must not be confused with an empty mailbox."
  (nelisp-actor-test--fixture)
  (let ((received 'untouched) actor)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq received (list :got (nelisp-receive))))))
    (nelisp-send actor nil)
    (nelisp-actor-run-until-idle)
    (should (equal received '(:got nil)))
    (should (eq (nelisp-actor-status actor) :dead))))

(ert-deftest nelisp-actor-crashed-on-uncaught-error ()
  "Uncaught signal inside an actor transitions it to :crashed."
  (nelisp-actor-test--fixture)
  (let ((actor (nelisp-spawn
                (nelisp-actor-lambda
                  (error "boom")))))
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status actor) :crashed))
    (should (consp (nelisp-actor-last-error actor)))
    (should (eq (car (nelisp-actor-last-error actor)) 'error))
    (should-error (nelisp-send actor 'msg) :type 'nelisp-actor-error)))

(ert-deftest nelisp-actor-run-until-idle-empty-queue ()
  "Run-until-idle on an empty queue returns 0 without error."
  (nelisp-actor-test--fixture)
  (should (= 0 (nelisp-actor-run-until-idle))))

(ert-deftest nelisp-actor-blocked-receive-resumes-fifo ()
  "Multiple sends to a blocked actor queue FIFO and drain on resume."
  (nelisp-actor-test--fixture)
  (let ((log nil) actor)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (push (nelisp-receive) log)
                   (push (nelisp-receive) log)
                   (push (nelisp-receive) log))))
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status actor) :blocked-receive))
    (nelisp-send actor 'a)
    (nelisp-send actor 'b)
    (nelisp-send actor 'c)
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse log) '(a b c)))
    (should (eq (nelisp-actor-status actor) :dead))))

(ert-deftest nelisp-actor-cpu-bound-yields-keep-fairness ()
  "CPU-bound actor with explicit yields doesn't starve peers."
  (nelisp-actor-test--fixture)
  (let ((heavy-count 0) (light-count 0))
    (nelisp-spawn
     (nelisp-actor-lambda
       (dotimes (_ 5)
         (cl-incf heavy-count)
         (nelisp-yield))))
    (nelisp-spawn
     (nelisp-actor-lambda
       (dotimes (_ 5)
         (cl-incf light-count)
         (nelisp-yield))))
    (nelisp-actor-run-until-idle)
    (should (= heavy-count 5))
    (should (= light-count 5))))

(ert-deftest nelisp-actor-start-stop-timer-installation ()
  "`nelisp-actor-start' installs the timer and `stop' removes it."
  (nelisp-actor-test--fixture)
  (should (null nelisp-actor--timer))
  (nelisp-actor-start)
  (should (timerp nelisp-actor--timer))
  (nelisp-actor-start)  ; idempotent
  (should (timerp nelisp-actor--timer))
  (nelisp-actor-stop)
  (should (null nelisp-actor--timer))
  (nelisp-actor-stop))  ; idempotent on already-stopped

(ert-deftest nelisp-actor-stats-shape ()
  "`nelisp-actor-stats' returns the documented 7-key plist."
  (nelisp-actor-test--fixture)
  (let* ((actor (nelisp-spawn (nelisp-actor-lambda)))
         (stats (nelisp-actor-stats actor)))
    (dolist (k '(:id :status :mailbox-length :mailbox-cap
                 :supervisor :restart-policy :last-error))
      (should (plist-member stats k)))
    (should (symbolp (plist-get stats :id)))
    (should (eq (plist-get stats :status) :runnable))
    (should (= 0 (plist-get stats :mailbox-length)))))

(provide 'nelisp-actor-test)
;;; nelisp-actor-test.el ends here
