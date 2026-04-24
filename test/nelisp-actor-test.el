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

;;; Phase 4.3 — shared-immutable message policy --------------------

(defun nelisp-actor-test--deliver-and-collect (msg)
  "Helper: spawn an actor, send MSG to it, drive, return the received value."
  (let (received actor)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq received (list :got (nelisp-receive))))))
    (nelisp-send actor msg)
    (nelisp-actor-run-until-idle)
    (cadr received)))

(ert-deftest nelisp-actor-copy-shares-symbols-and-numbers ()
  "Symbols and numbers pass the send boundary by reference."
  (nelisp-actor-test--fixture)
  (should (eq 'foo (nelisp-actor-test--deliver-and-collect 'foo)))
  (should (eq :key (nelisp-actor-test--deliver-and-collect :key)))
  ;; Fixnums are eq-unique in Emacs; floats need eql.
  (should (eq 42 (nelisp-actor-test--deliver-and-collect 42)))
  (should (eql 3.14 (nelisp-actor-test--deliver-and-collect 3.14)))
  (should (eq nil (nelisp-actor-test--deliver-and-collect nil)))
  (should (eq t (nelisp-actor-test--deliver-and-collect t))))

(ert-deftest nelisp-actor-copy-shares-actor-handles ()
  "Cl-struct actor handles pass by reference (records are shareable)."
  (nelisp-actor-test--fixture)
  (let* ((handle (nelisp-spawn (nelisp-actor-lambda)))
         (echoed (nelisp-actor-test--deliver-and-collect handle)))
    (should (eq echoed handle))))

(ert-deftest nelisp-actor-copy-deep-copies-cons ()
  "Mutating the sender's cons after send must not affect the recipient."
  (nelisp-actor-test--fixture)
  (let* ((orig (cons 'a 'b))
         (received nil)
         (actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq received (nelisp-receive))))))
    (nelisp-send actor orig)
    (setcar orig 'MUTATED)
    (setcdr orig 'ALSO-MUTATED)
    (nelisp-actor-run-until-idle)
    (should (equal received '(a . b)))
    (should-not (eq received orig))))

(ert-deftest nelisp-actor-copy-deep-copies-vector ()
  "Mutating the sender's vector via `aset' after send leaves the copy intact."
  (nelisp-actor-test--fixture)
  (let* ((orig (vector 1 2 3))
         (received nil)
         (actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq received (nelisp-receive))))))
    (nelisp-send actor orig)
    (aset orig 0 99)
    (nelisp-actor-run-until-idle)
    (should (equal received (vector 1 2 3)))
    (should-not (eq received orig))))

(ert-deftest nelisp-actor-copy-deep-copies-string ()
  "Mutating the sender's string via `aset' after send leaves the copy intact."
  (nelisp-actor-test--fixture)
  (let* ((orig (copy-sequence "hello"))
         (received nil)
         (actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq received (nelisp-receive))))))
    (nelisp-send actor orig)
    (aset orig 0 ?H)
    (nelisp-actor-run-until-idle)
    (should (equal received "hello"))
    (should-not (eq received orig))))

(ert-deftest nelisp-actor-copy-nested-structure ()
  "Nested cons/vector/string are all independently copied."
  (nelisp-actor-test--fixture)
  (let* ((inner-vec (vector 10 20))
         (inner-str (copy-sequence "abc"))
         (orig (list inner-vec inner-str 'tag))
         (received (nelisp-actor-test--deliver-and-collect orig)))
    (should (equal received (list (vector 10 20) "abc" 'tag)))
    ;; Outer list copied.
    (should-not (eq received orig))
    ;; Inner vector copied.
    (should-not (eq (nth 0 received) inner-vec))
    ;; Inner string copied.
    (should-not (eq (nth 1 received) inner-str))
    ;; Shareable tag passes by reference.
    (should (eq (nth 2 received) 'tag))))

(ert-deftest nelisp-actor-copy-preserves-shared-substructure ()
  "A tail referenced twice in the original stays shared in the copy."
  (nelisp-actor-test--fixture)
  (let* ((tail (cons 1 2))
         (orig (list tail tail))
         (received (nelisp-actor-test--deliver-and-collect orig)))
    (should (equal received (list (cons 1 2) (cons 1 2))))
    ;; Both positions point to the *same* copied cell, not two copies.
    (should (eq (nth 0 received) (nth 1 received)))
    ;; The copied tail is not the original tail.
    (should-not (eq (nth 0 received) tail))))

(ert-deftest nelisp-actor-copy-handles-circular-cons ()
  "A self-referential cons is copied without infinite recursion."
  (nelisp-actor-test--fixture)
  (let* ((orig (cons 'head nil))
         received actor)
    (setcdr orig orig)
    (setq actor (nelisp-spawn
                 (nelisp-actor-lambda
                   (setq received (nelisp-receive)))))
    (nelisp-send actor orig)
    (nelisp-actor-run-until-idle)
    ;; Copy retains the cycle.
    (should (eq (cdr received) received))
    (should (eq (car received) 'head))
    ;; And it is not the original.
    (should-not (eq received orig))))

;;; Phase 4.4 — channel primitive ----------------------------------

(ert-deftest nelisp-chan-unbuffered-rendezvous ()
  "Unbuffered channel: send and recv hand off directly."
  (nelisp-actor-test--fixture)
  (let* ((ch (nelisp-make-chan))
         (received nil))
    (nelisp-spawn
     (nelisp-actor-lambda
       (nelisp-chan-send ch 'hello)))
    (nelisp-spawn
     (let ((ch ch))
       (nelisp-actor-lambda
         (setq received (nelisp-chan-recv ch)))))
    (nelisp-actor-run-until-idle)
    (should (equal received '(:value hello)))))

(ert-deftest nelisp-chan-buffered-send-without-block ()
  "Buffered channel: sender completes without a receiver, up to capacity."
  (nelisp-actor-test--fixture)
  (let* ((ch (nelisp-make-chan 3))
         (sender (nelisp-spawn
                  (let ((ch ch))
                    (nelisp-actor-lambda
                      (nelisp-chan-send ch 'a)
                      (nelisp-chan-send ch 'b)
                      (nelisp-chan-send ch 'c))))))
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status sender) :dead))))

(ert-deftest nelisp-chan-buffered-send-blocks-when-full ()
  "Buffered channel: send blocks when the buffer is full until a recv drains it."
  (nelisp-actor-test--fixture)
  (let* ((ch (nelisp-make-chan 1))
         (received nil)
         (sender
          (nelisp-spawn
           (let ((ch ch))
             (nelisp-actor-lambda
               (nelisp-chan-send ch 'first)
               (nelisp-chan-send ch 'second))))))
    (nelisp-actor-run-until-idle)
    ;; Sender is blocked on the second send.
    (should (memq (nelisp-actor-status sender)
                  '(:blocked-receive :runnable)))
    (nelisp-spawn
     (let ((ch ch))
       (nelisp-actor-lambda
         (push (nelisp-chan-recv ch) received)
         (push (nelisp-chan-recv ch) received))))
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse received)
                   '((:value first) (:value second))))
    (should (eq (nelisp-actor-status sender) :dead))))

(ert-deftest nelisp-chan-recv-blocks-until-send ()
  "Unbuffered channel: recv blocks until a send arrives."
  (nelisp-actor-test--fixture)
  (let* ((ch (nelisp-make-chan))
         (received nil)
         (receiver
          (nelisp-spawn
           (let ((ch ch))
             (nelisp-actor-lambda
               (setq received (nelisp-chan-recv ch)))))))
    (nelisp-actor-run-until-idle)
    (should (null received))
    (should (memq (nelisp-actor-status receiver)
                  '(:blocked-receive :runnable)))
    (nelisp-spawn
     (let ((ch ch))
       (nelisp-actor-lambda
         (nelisp-chan-send ch 'delayed))))
    (nelisp-actor-run-until-idle)
    (should (equal received '(:value delayed)))
    (should (eq (nelisp-actor-status receiver) :dead))))

(ert-deftest nelisp-chan-close-wakes-pending-recv ()
  "Closing an empty channel wakes pending receivers with `(:closed)'."
  (nelisp-actor-test--fixture)
  (let* ((ch (nelisp-make-chan))
         (received nil)
         (receiver
          (nelisp-spawn
           (let ((ch ch))
             (nelisp-actor-lambda
               (setq received (nelisp-chan-recv ch)))))))
    (nelisp-actor-run-until-idle)
    (should (null received))
    (nelisp-chan-close ch)
    (nelisp-actor-run-until-idle)
    (should (equal received '(:closed)))
    (should (eq (nelisp-actor-status receiver) :dead))))

(ert-deftest nelisp-chan-close-errors-pending-sender ()
  "Closing a full buffered channel errors pending senders with `chan-send-on-closed'."
  (nelisp-actor-test--fixture)
  (let* ((ch (nelisp-make-chan 1))
         (sender
          (nelisp-spawn
           (let ((ch ch))
             (nelisp-actor-lambda
               (nelisp-chan-send ch 'fits)
               (nelisp-chan-send ch 'blocks))))))
    (nelisp-actor-run-until-idle)
    ;; Sender stuck on the second send.
    (should (memq (nelisp-actor-status sender)
                  '(:blocked-receive :runnable)))
    (nelisp-chan-close ch)
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status sender) :crashed))
    (let ((err (nelisp-actor-last-error sender)))
      (should (consp err))
      (should (eq (cadr err) 'chan-send-on-closed)))))

;;; Phase 4.5 — flat one-for-one supervisor ------------------------

(defun nelisp-actor-test--spec (id start-thunk policy)
  "Build a supervisor child-spec plist for the given ID, START-THUNK, POLICY."
  (list :id id :start start-thunk :restart policy))

(defun nelisp-actor-test--child-actor (sup)
  "Return the single live child actor of SUP, or nil when none."
  (let ((kids (cl-remove-if
               (lambda (a)
                 (memq (nelisp-actor-status a) '(:dead :crashed)))
               (nelisp-supervise-children sup))))
    (when kids (car kids))))

(ert-deftest nelisp-supervise-starts-all-children ()
  "Initial spawn: every child in the spec becomes a live actor."
  (nelisp-actor-test--fixture)
  (let ((sup (nelisp-supervise
              (list (nelisp-actor-test--spec
                     'a (nelisp-actor-lambda (nelisp-receive)) :permanent)
                    (nelisp-actor-test--spec
                     'b (nelisp-actor-lambda (nelisp-receive)) :permanent)))))
    (nelisp-actor-run-until-idle)
    ;; Two children + the supervisor itself.
    (should (eq (nelisp-actor-status sup) :blocked-receive))
    (let ((live (nelisp-supervise-children sup)))
      (should (= 2 (length live)))
      (dolist (a live)
        (should (eq (nelisp-actor-status a) :blocked-receive))))))

(ert-deftest nelisp-supervise-permanent-restarts-on-crash ()
  "`:permanent' policy respawns a crashed child (same thunk re-invoked)."
  (nelisp-actor-test--fixture)
  (let* ((lives 0)
         ;; Thunk builder: every call bumps `lives'; first life crashes,
         ;; second life blocks on receive so we can observe it.
         (thunk (nelisp-actor-lambda
                  (cl-incf lives)
                  (when (= lives 1) (error "planned-crash"))
                  (nelisp-receive)))
         (sup (nelisp-supervise
               (list (list :id 'worker
                           :start thunk
                           :restart :permanent)))))
    (nelisp-actor-run-until-idle)
    ;; Restart happened → lives incremented twice; surviving child is
    ;; blocked-receive.
    (should (= lives 2))
    (let ((live (nelisp-actor-test--child-actor sup)))
      (should live)
      (should (eq (nelisp-actor-status live) :blocked-receive)))))

(ert-deftest nelisp-supervise-permanent-restarts-on-normal-exit ()
  "`:permanent' policy respawns on normal completion too."
  (nelisp-actor-test--fixture)
  (let* ((lives 0)
         (thunk (nelisp-actor-lambda
                  (cl-incf lives)
                  (when (> lives 2) (nelisp-receive))))
         (sup (nelisp-supervise
               (list (list :id 'worker
                           :start thunk
                           :restart :permanent)))))
    (ignore sup)
    (nelisp-actor-run-until-idle)
    (should (>= lives 3))))

(ert-deftest nelisp-supervise-transient-restarts-on-crash-only ()
  "`:transient' policy respawns on crash but not on normal exit."
  (nelisp-actor-test--fixture)
  (let* ((lives 0)
         ;; First life errors (triggers restart), second life completes
         ;; normally (should NOT trigger restart).
         (thunk (nelisp-actor-lambda
                  (cl-incf lives)
                  (when (= lives 1) (error "planned-crash"))))
         (sup (nelisp-supervise
               (list (list :id 'worker
                           :start thunk
                           :restart :transient)))))
    (ignore sup)
    (nelisp-actor-run-until-idle)
    (should (= lives 2))))

(ert-deftest nelisp-supervise-temporary-never-restarts ()
  "`:temporary' policy never respawns the child."
  (nelisp-actor-test--fixture)
  (let* ((lives 0)
         (thunk (nelisp-actor-lambda
                  (cl-incf lives)
                  (error "planned-crash")))
         (sup (nelisp-supervise
               (list (list :id 'worker
                           :start thunk
                           :restart :temporary)))))
    (ignore sup)
    (nelisp-actor-run-until-idle)
    (should (= lives 1))))

(ert-deftest nelisp-supervise-rejects-unknown-strategy ()
  "`nelisp-supervise' rejects strategies outside Phase 4.5 scope."
  (nelisp-actor-test--fixture)
  (should-error
   (nelisp-supervise '() :strategy :one-for-all)
   :type 'nelisp-actor-error))

;;; Phase 4.6 — anvil PoC (merge gate, §2.8 A) ---------------------

;; Scenario #1 from docs/04-concurrency.org §4.5: four MCP-tool-
;; handler-shaped actors process concurrent calls in one Emacs
;; session without starving each other and without blocking the
;; dispatcher.  Demonstrates that the actor runtime resolves
;; anvil.el's single-threaded MCP-worker bottleneck (the motivation
;; Architecture B was built to work around — see anvil memory
;; `project_mcp_layer2_design.md').
;;
;; The MVP merge gate is this one scenario green (§2.8 A lock).
;; Scenarios #2 (shared-nothing mutation isolation) and #3
;; (supervised long-running handler pool survives a crash) remain
;; v0.4.x extensions.

(defun nelisp-poc--make-tool-handler (name)
  "Build a one-shot actor thunk that mimics an MCP tool handler.
Receives `(:call ARG REPLY-TO)', does three yield cycles of
simulated work, then replies `(list :result NAME ARG)'.  NAME
survives closure capture so test assertions can tag each handler's
response."
  (let ((name name))
    (nelisp-actor-lambda
      (let ((req (nelisp-receive)))
        (pcase req
          (`(:call ,arg ,reply-to)
           ;; Simulate work that yields between steps — ensures the
           ;; scheduler can round-robin other handlers in the pool.
           (nelisp-yield)
           (nelisp-yield)
           (nelisp-yield)
           (nelisp-send reply-to (list :result name arg))))))))

(ert-deftest nelisp-poc-parallel-tool-handlers ()
  "Four concurrent tool-handler actors all respond to one call each."
  (nelisp-actor-test--fixture)
  (let* ((handlers
          (mapcar (lambda (n) (nelisp-spawn
                                (nelisp-poc--make-tool-handler n)))
                  '(h1 h2 h3 h4)))
         (results nil))
    (nelisp-spawn
     (let ((handlers handlers))
       (nelisp-actor-lambda
         (let ((self (nelisp-self)))
           (dolist (h handlers)
             (nelisp-send h (list :call 'ping self))))
         (dotimes (_ 4)
           (push (nelisp-receive) results)))))
    (nelisp-actor-run-until-idle)
    ;; Every handler answered exactly once; all payloads tagged ping.
    (should (= 4 (length results)))
    (let ((tags (mapcar (lambda (r) (nth 1 r)) results)))
      (dolist (expected '(h1 h2 h3 h4))
        (should (memq expected tags))))
    (dolist (r results)
      (should (eq (car r) :result))
      (should (eq (nth 2 r) 'ping)))))

(ert-deftest nelisp-poc-handler-yields-do-not-starve-peers ()
  "Yielding handlers interleave under the cooperative scheduler."
  (nelisp-actor-test--fixture)
  (let ((trace nil))
    (dolist (tag '(x y z))
      (nelisp-spawn
       (let ((tag tag))
         (nelisp-actor-lambda
           (let ((req (nelisp-receive)))
             (pcase req
               (`(:tick ,_)
                (push (list tag 'a) trace)
                (nelisp-yield)
                (push (list tag 'b) trace)
                (nelisp-yield)
                (push (list tag 'c) trace))))))))
    ;; Deliver a tick to each handler.  They're spawned in x / y / z
    ;; order; the scheduler round-robins their yields so the trace
    ;; interleaves the three tags rather than emitting all of x then
    ;; y then z.
    (let ((actors (cl-remove-if
                   (lambda (a)
                     (memq (nelisp-actor-status a) '(:dead :crashed)))
                   (nelisp-actor-list))))
      (dolist (a actors)
        (nelisp-send a '(:tick nil))))
    (nelisp-actor-run-until-idle)
    (let ((reversed (nreverse trace)))
      ;; All 9 events landed.
      (should (= 9 (length reversed)))
      ;; Interleaving: the first three events include each of x/y/z
      ;; (not all from one actor), proving fairness.
      (let ((first-three (cl-subseq reversed 0 3)))
        (should (member '(x a) first-three))
        (should (member '(y a) first-three))
        (should (member '(z a) first-three))))))

(ert-deftest nelisp-poc-handler-pool-burst ()
  "A handler pool of 3 workers absorbs a burst of 6 calls correctly."
  (nelisp-actor-test--fixture)
  (let* ((pool
          (mapcar (lambda (n) (nelisp-spawn
                                (nelisp-poc--make-tool-handler n)))
                  '(w1 w2 w3)))
         (results nil))
    ;; Pool size = 3, calls = 6 → two "rounds" per worker.  Each
    ;; handler is a one-shot actor per Phase 4.6 PoC MVP, so we
    ;; size the pool to twice the concurrency by spawning extras.
    (let* ((extras
            (mapcar (lambda (n) (nelisp-spawn
                                  (nelisp-poc--make-tool-handler n)))
                    '(w4 w5 w6)))
           (all-workers (append pool extras)))
      (nelisp-spawn
       (let ((workers all-workers))
         (nelisp-actor-lambda
           (let ((self (nelisp-self)))
             (dolist (w workers)
               (nelisp-send w (list :call 'burst self))))
           (dotimes (_ 6)
             (push (nelisp-receive) results))))))
    (nelisp-actor-run-until-idle)
    (should (= 6 (length results)))
    ;; All responses are well-formed.
    (dolist (r results)
      (should (eq (car r) :result))
      (should (eq (nth 2 r) 'burst))
      (should (memq (nth 1 r) '(w1 w2 w3 w4 w5 w6))))
    ;; Every worker responded exactly once.
    (let ((tags (mapcar (lambda (r) (nth 1 r)) results)))
      (should (equal (sort (copy-sequence tags) #'string<)
                     '(w1 w2 w3 w4 w5 w6))))))

(provide 'nelisp-actor-test)
;;; nelisp-actor-test.el ends here
