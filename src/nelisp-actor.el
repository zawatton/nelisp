;;; nelisp-actor.el --- Phase 4.1 actor primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4.1 skeleton of NeLisp's actor runtime.  Ships the data
;; structures and public API (`nelisp-spawn' / `nelisp-receive' /
;; `nelisp-send' / `nelisp-self') plus a mock-stub scheduler driven
;; by `nelisp-actor-run-until-idle'.  Real cooperative scheduling
;; with suspended-continuation support lands in Phase 4.2 via
;; `generator.el' (docs/design/10-phase4-actor.org §5.3).
;;
;; Phase 4.1 semantics (mock stub):
;;   - `nelisp-spawn' creates an actor, snapshots its ownership
;;     boundary via `nelisp-gc-actor-boundary', and enqueues it as
;;     `:runnable'.  The thunk does NOT run until the caller invokes
;;     `nelisp-actor-run-until-idle'.
;;   - `nelisp-send' pushes MSG onto the target's FIFO mailbox and
;;     (re)enqueues the target when it was blocked on receive.
;;     Sending to a `:dead' / `:crashed' actor signals
;;     `nelisp-actor-error'.
;;   - `nelisp-receive' returns the head message in FIFO order, or
;;     the symbol `timeout' when the mailbox is empty.  Phase 4.2
;;     extends this with true yield semantics so a receive on an
;;     empty mailbox suspends.
;;   - `nelisp-self' returns the currently executing actor, or nil
;;     when called outside any actor thunk.
;;
;; See docs/design/10-phase4-actor.org §4.1 for scope, §5 for runtime
;; structure, §2 for locked decisions (A/D/B/A/B/B/A/A signoff
;; 2026-04-24).

;;; Code:

(require 'cl-lib)
(require 'nelisp-gc)

(define-error 'nelisp-actor-error "NeLisp actor error")

;;; Actor struct ------------------------------------------------------

(cl-defstruct (nelisp-actor (:constructor nelisp-actor--make)
                            (:copier nil))
  "A NeLisp actor (Phase 4.1 skeleton).
Field rationale in `docs/design/10-phase4-actor.org' §5.1.  Fields
`k' and `restart-policy' are recorded verbatim here; Phase 4.2
(scheduler) and Phase 4.5 (supervisor) give them runtime meaning."
  id                            ; unique generated symbol
  thunk                         ; zero-arg closure run by scheduler
  (mailbox nil)                 ; FIFO list, head = next to receive
  (mailbox-tail nil)            ; last cons for O(1) append
  (mailbox-cap nil)             ; nil = unbounded (Phase 4.2+ backpressure)
  (status :runnable)            ; :runnable :blocked-receive :dead :crashed
  (owned-set nil)               ; hash-table from `nelisp-gc-actor-boundary'
  (supervisor nil)              ; parent actor or nil
  (restart-policy :temporary)   ; :permanent :transient :temporary
  (k nil)                       ; suspended continuation (Phase 4.2+)
  (last-error nil))             ; condition-case err from last crash

;;; Runtime state -----------------------------------------------------

(defvar nelisp-actor--registry (make-hash-table :test 'eq)
  "All spawned actors keyed by `nelisp-actor-id'.
Entries persist past `:dead' so diagnostics (`nelisp-actor-stats')
remain inspectable; callers wanting live-only views use
`nelisp-actor-list' plus a status filter.")

(defvar nelisp-actor--run-queue nil
  "FIFO of runnable actors.  Phase 4.1 mock-stub scheduler drives
this via `nelisp-actor-run-until-idle'.")

(defvar nelisp-actor--run-queue-tail nil
  "Tail cons of `nelisp-actor--run-queue' for O(1) append.")

(defvar nelisp-actor--current nil
  "Dynamically bound to the actor whose thunk is currently executing.")

(defvar nelisp-actor--id-counter 0
  "Monotonic counter for actor-id generation.")

;;; Internal helpers --------------------------------------------------

(defun nelisp-actor--next-id ()
  "Return a fresh unique actor-id symbol of the form `nelisp-actor-N'."
  (cl-incf nelisp-actor--id-counter)
  (intern (format "nelisp-actor-%d" nelisp-actor--id-counter)))

(defun nelisp-actor--enqueue (actor)
  "Append ACTOR to `nelisp-actor--run-queue' unless already queued.
Uses the tail pointer for O(1) append.  The duplicate check is
O(n); acceptable at Phase 4.1 test volumes and revisited in 4.2
when the scheduler maintains explicit queued-status flags."
  (unless (memq actor nelisp-actor--run-queue)
    (let ((cell (list actor)))
      (if nelisp-actor--run-queue-tail
          (setcdr nelisp-actor--run-queue-tail cell)
        (setq nelisp-actor--run-queue cell))
      (setq nelisp-actor--run-queue-tail cell))))

(defun nelisp-actor--dequeue ()
  "Pop and return the head of the run queue, or nil when empty.
Clears the tail pointer on drain so the next enqueue starts fresh."
  (let ((actor (pop nelisp-actor--run-queue)))
    (unless nelisp-actor--run-queue
      (setq nelisp-actor--run-queue-tail nil))
    actor))

(defun nelisp-actor--mailbox-push (actor msg)
  "Append MSG to ACTOR's mailbox in O(1) via the tail pointer."
  (let ((cell (list msg)))
    (if (nelisp-actor-mailbox-tail actor)
        (setcdr (nelisp-actor-mailbox-tail actor) cell)
      (setf (nelisp-actor-mailbox actor) cell))
    (setf (nelisp-actor-mailbox-tail actor) cell)))

(defun nelisp-actor--mailbox-pop (actor)
  "Remove and return the head of ACTOR's mailbox.  Undefined on empty —
callers must guard with a non-empty check first."
  (let ((box (nelisp-actor-mailbox actor)))
    (setf (nelisp-actor-mailbox actor) (cdr box))
    (unless (cdr box)
      (setf (nelisp-actor-mailbox-tail actor) nil))
    (car box)))

;;; Public primitives -------------------------------------------------

(cl-defun nelisp-spawn (thunk &key mailbox-capacity supervisor
                              (restart-policy :temporary))
  "Spawn a new actor executing THUNK and return the actor object.

THUNK is a zero-argument closure the cooperative scheduler will run.
Phase 4.1 enqueues the actor as `:runnable' but does not drive it;
call `nelisp-actor-run-until-idle' to advance the queue.

Keyword arguments:
  MAILBOX-CAPACITY  Integer bound for the mailbox (Phase 4.2+
                    enforces backpressure); nil = unbounded.
  SUPERVISOR        Parent actor for crash restart (Phase 4.5); nil
                    means the actor is unsupervised.
  RESTART-POLICY    `:permanent' | `:transient' | `:temporary'.
                    Phase 4.5 enforces; Phase 4.1 records only.

Initial `owned-set' is snapshotted from `nelisp-gc-actor-boundary'
of THUNK so the Phase 4.3 shared-immutable message policy has a
frozen reference for send-time copy decisions."
  (unless (functionp thunk)
    (signal 'wrong-type-argument (list 'functionp thunk)))
  (let* ((id (nelisp-actor--next-id))
         (actor (nelisp-actor--make
                 :id id
                 :thunk thunk
                 :mailbox-cap mailbox-capacity
                 :supervisor supervisor
                 :restart-policy restart-policy
                 :owned-set (nelisp-gc-actor-boundary thunk))))
    (puthash id actor nelisp-actor--registry)
    (nelisp-actor--enqueue actor)
    actor))

(defun nelisp-self ()
  "Return the currently executing actor, or nil outside any thunk."
  nelisp-actor--current)

(defun nelisp-send (target msg)
  "Deliver MSG to TARGET actor's mailbox and return t.

Signals `nelisp-actor-error' with reason `send-to-dead-actor' when
TARGET is `:dead' or `:crashed'.  Re-enqueues TARGET when it was
blocked on receive so the scheduler drives it on the next
`nelisp-actor-run-until-idle' pass.

Phase 4.1 does not copy MSG; the shared-immutable policy (§4.3)
classifies cells and deep-copies mutable structure at send time."
  (unless (nelisp-actor-p target)
    (signal 'wrong-type-argument (list 'nelisp-actor-p target)))
  (when (memq (nelisp-actor-status target) '(:dead :crashed))
    (signal 'nelisp-actor-error
            (list 'send-to-dead-actor
                  (nelisp-actor-id target)
                  (nelisp-actor-status target))))
  (nelisp-actor--mailbox-push target msg)
  (when (eq (nelisp-actor-status target) :blocked-receive)
    (setf (nelisp-actor-status target) :runnable)
    (nelisp-actor--enqueue target))
  t)

(defun nelisp-receive (&optional timeout)
  "Return the next message in the current actor's mailbox.

With an empty mailbox:
  TIMEOUT = nil / 0 (Phase 4.1 default) → return the symbol `timeout'.
  TIMEOUT > 0 (Phase 4.2+)              → suspend until a message
                                          arrives or the timeout elapses.

Phase 4.1's mock scheduler cannot suspend, so positive timeouts
degrade to the non-blocking path (returning `timeout').  Callers
must be within an actor thunk; signals `nelisp-actor-error' with
reason `receive-outside-actor' otherwise."
  (ignore timeout)  ; Phase 4.2 honours it.
  (unless nelisp-actor--current
    (signal 'nelisp-actor-error (list 'receive-outside-actor)))
  (if (nelisp-actor-mailbox nelisp-actor--current)
      (nelisp-actor--mailbox-pop nelisp-actor--current)
    'timeout))

;;; Scheduler (Phase 4.1 mock stub) -----------------------------------

(defun nelisp-actor--run-one (actor)
  "Drive ACTOR's thunk once with `:current' dynamically bound to ACTOR.
On normal completion the actor transitions to `:dead'.  Uncaught
errors are captured in `last-error' and the actor becomes
`:crashed'; Phase 4.5 wires the supervisor restart policy here."
  (let ((nelisp-actor--current actor))
    (condition-case err
        (progn
          (funcall (nelisp-actor-thunk actor))
          (setf (nelisp-actor-status actor) :dead))
      (error
       (setf (nelisp-actor-last-error actor) err)
       (setf (nelisp-actor-status actor) :crashed)))))

(defun nelisp-actor-run-until-idle ()
  "Drive the run queue until empty and return the number of actors run.
Phase 4.1 runs each actor's thunk to completion on its single pass
through the queue; Phase 4.2 replaces this with a real cooperative
scheduler that supports yield and resume."
  (let ((count 0))
    (while nelisp-actor--run-queue
      (let ((actor (nelisp-actor--dequeue)))
        (when (eq (nelisp-actor-status actor) :runnable)
          (nelisp-actor--run-one actor)
          (cl-incf count))))
    count))

;;; Diagnostics -------------------------------------------------------

(defun nelisp-actor-list ()
  "Return every actor currently in `nelisp-actor--registry'.
Order is unspecified (hash-table iteration); callers needing spawn
order filter by `nelisp-actor-id'."
  (let (out)
    (maphash (lambda (_id actor) (push actor out)) nelisp-actor--registry)
    out))

(defun nelisp-actor-stats (actor)
  "Return a diagnostics plist for ACTOR.
Keys: `:id' `:status' `:mailbox-length' `:mailbox-cap'
      `:supervisor' `:restart-policy' `:last-error'."
  (unless (nelisp-actor-p actor)
    (signal 'wrong-type-argument (list 'nelisp-actor-p actor)))
  (list :id              (nelisp-actor-id actor)
        :status          (nelisp-actor-status actor)
        :mailbox-length  (length (nelisp-actor-mailbox actor))
        :mailbox-cap     (nelisp-actor-mailbox-cap actor)
        :supervisor      (nelisp-actor-supervisor actor)
        :restart-policy  (nelisp-actor-restart-policy actor)
        :last-error      (nelisp-actor-last-error actor)))

;;; Test helpers ------------------------------------------------------

(defun nelisp-actor--reset ()
  "Clear all actor state.  For ERT setup/teardown only."
  (clrhash nelisp-actor--registry)
  (setq nelisp-actor--run-queue nil
        nelisp-actor--run-queue-tail nil
        nelisp-actor--current nil
        nelisp-actor--id-counter 0))

(provide 'nelisp-actor)
;;; nelisp-actor.el ends here
