;;; nelisp-actor-test.el --- Phase 4.1 actor primitive ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4.1 scope: spawn / receive / send / self + mock-stub
;; scheduler.  Yield semantics, cooperative fairness, shared-immutable
;; message policy, and supervision each get their own test files as
;; subsequent sub-phases land (see docs/design/10-phase4-actor.org §4.2
;; onward).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp)
(require 'nelisp-actor)

(defun nelisp-actor-test--fixture ()
  "Reset actor runtime state before each test.  Side-effect-only."
  (nelisp-actor--reset))

;;; Spawn + initial state --------------------------------------------

(ert-deftest nelisp-actor-spawn-initial-state ()
  "Fresh spawn yields :runnable actor with empty mailbox and owned-set hash."
  (nelisp-actor-test--fixture)
  (let ((actor (nelisp-spawn (lambda () nil))))
    (should (nelisp-actor-p actor))
    (should (symbolp (nelisp-actor-id actor)))
    (should (eq (nelisp-actor-status actor) :runnable))
    (should (null (nelisp-actor-mailbox actor)))
    (should (hash-table-p (nelisp-actor-owned-set actor)))))

(ert-deftest nelisp-actor-spawn-rejects-non-function ()
  "Non-function THUNK signals wrong-type-argument at spawn time."
  (nelisp-actor-test--fixture)
  (should-error (nelisp-spawn 42) :type 'wrong-type-argument))

;;; Self binding ------------------------------------------------------

(ert-deftest nelisp-actor-self-inside-thunk ()
  "`nelisp-self' returns the current actor during thunk execution."
  (nelisp-actor-test--fixture)
  (let (captured actor)
    (setq actor (nelisp-spawn (lambda () (setq captured (nelisp-self)))))
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
                 (lambda ()
                   (push (nelisp-receive) got)
                   (push (nelisp-receive) got)
                   (push (nelisp-receive) got))))
    (nelisp-send actor 'one)
    (nelisp-send actor 'two)
    (nelisp-send actor 'three)
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse got) '(one two three)))))

(ert-deftest nelisp-actor-receive-empty-returns-timeout ()
  "Receive on an empty mailbox returns the symbol `timeout'."
  (nelisp-actor-test--fixture)
  (let (result)
    (nelisp-spawn (lambda () (setq result (nelisp-receive))))
    (nelisp-actor-run-until-idle)
    (should (eq result 'timeout))))

;;; Send to dead actor -----------------------------------------------

(ert-deftest nelisp-actor-send-to-dead-errors ()
  "Sending to a :dead actor signals `nelisp-actor-error'."
  (nelisp-actor-test--fixture)
  (let ((actor (nelisp-spawn (lambda () nil))))
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
      (push (nelisp-spawn (lambda () nil)) actors))
    (should (= 100 (length actors)))
    (should (= 100 (length (nelisp-actor-list))))
    (should (= 100 (nelisp-actor-run-until-idle)))
    (dolist (a actors)
      (should (eq (nelisp-actor-status a) :dead)))))

(provide 'nelisp-actor-test)
;;; nelisp-actor-test.el ends here
