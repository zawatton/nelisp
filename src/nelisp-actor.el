;;; nelisp-actor.el --- Phase 4 actor runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4 actor runtime.  Ships the actor data structure and the
;; public API:
;;
;;   `nelisp-spawn'    spawn an actor from a thunk built with
;;                     `nelisp-actor-lambda'
;;   `nelisp-self'     currently executing actor
;;   `nelisp-send'     FIFO deliver a message to an actor
;;   `nelisp-receive'  macro — block until a message arrives
;;   `nelisp-yield'    macro — voluntarily yield to the scheduler
;;   `nelisp-actor-run-until-idle'   drain the run queue
;;   `nelisp-actor-start' / `stop'   drive the scheduler from an
;;                                   idle timer
;;   `nelisp-actor-list' / `nelisp-actor-stats'  diagnostics
;;
;; Phase 4.1 (fa304ed) shipped the primitive skeleton with a
;; run-to-completion mock-stub scheduler where empty receive returned
;; the symbol `timeout'.  Phase 4.2 replaces the scheduler with a
;; cooperative generator-driven loop: an actor thunk built via
;; `nelisp-actor-lambda' is CPS-transformed by `generator.el' so that
;; `nelisp-receive' and `nelisp-yield' truly suspend and later resume
;; from the same source position.
;;
;; See docs/design/10-phase4-actor.org §5 for runtime structure
;; (§5.3 locks the generator.el approach) and §2 for the locked
;; high-level decisions (A/D/B/A/B/B/A/A signoff 2026-04-24).

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'nelisp-gc)

(define-error 'nelisp-actor-error "NeLisp actor error")

;;; Actor struct ------------------------------------------------------

(cl-defstruct (nelisp-actor (:constructor nelisp-actor--make)
                            (:copier nil))
  "A NeLisp actor.
Field rationale in `docs/design/10-phase4-actor.org' §5.1.
`restart-policy' / `supervisor' / `last-error' are recorded here
for Phase 4.5 (supervisor) consumption; Phase 4.2 only sets
`last-error' on a crash."
  id                            ; unique generated symbol
  thunk                         ; iter-lambda returned at spawn
  (mailbox nil)                 ; FIFO list, head = next to receive
  (mailbox-tail nil)            ; last cons for O(1) append
  (mailbox-cap nil)             ; nil = unbounded (Phase 4.2+ backpressure)
  (status :runnable)            ; :runnable :blocked-receive :dead :crashed
  (owned-set nil)               ; hash-table from `nelisp-gc-actor-boundary'
  (supervisor nil)              ; parent actor or nil
  (restart-policy :temporary)   ; :permanent :transient :temporary
  (iterator nil)                ; live generator from (funcall thunk)
  (last-error nil))             ; condition-case err from last crash

;;; Runtime state -----------------------------------------------------

(defvar nelisp-actor--registry (make-hash-table :test 'eq)
  "All spawned actors keyed by `nelisp-actor-id'.
Entries persist past `:dead' so diagnostics (`nelisp-actor-stats')
remain inspectable; callers wanting live-only views filter on status.")

(defvar nelisp-actor--run-queue nil
  "FIFO of runnable actors scheduled by `nelisp-actor-run-until-idle'.
Blocked-on-receive actors sit outside the queue until `nelisp-send'
re-enqueues them.")

(defvar nelisp-actor--run-queue-tail nil
  "Tail cons of `nelisp-actor--run-queue' for O(1) append.")

(defvar nelisp-actor--current nil
  "Dynamically bound to the actor whose iterator is being advanced.
`nelisp-receive' / `nelisp-yield' / `nelisp-self' all read this.")

(defvar nelisp-actor--id-counter 0
  "Monotonic counter for actor-id generation.")

(defcustom nelisp-actor-tick-interval 0.05
  "Idle seconds between scheduler ticks when the timer driver is active.
Only consulted by `nelisp-actor-start'; `nelisp-actor-run-until-idle'
does not idle.  Phase 4.2 default is conservative; tuning lands with
the fairness bench in 4.7."
  :group 'nelisp
  :type 'number)

(defvar nelisp-actor--timer nil
  "Idle timer installed by `nelisp-actor-start', or nil when stopped.")

;;; Internal helpers --------------------------------------------------

(defun nelisp-actor--next-id ()
  "Return a fresh unique actor-id symbol of the form `nelisp-actor-N'."
  (cl-incf nelisp-actor--id-counter)
  (intern (format "nelisp-actor-%d" nelisp-actor--id-counter)))

(defun nelisp-actor--enqueue (actor)
  "Append ACTOR to `nelisp-actor--run-queue' unless already queued.
Uses the tail pointer for O(1) append.  Duplicate check is O(n);
acceptable at Phase 4.2 volumes and revisited when the scheduler
maintains an explicit queued-status flag."
  (unless (memq actor nelisp-actor--run-queue)
    (let ((cell (list actor)))
      (if nelisp-actor--run-queue-tail
          (setcdr nelisp-actor--run-queue-tail cell)
        (setq nelisp-actor--run-queue cell))
      (setq nelisp-actor--run-queue-tail cell))))

(defun nelisp-actor--dequeue ()
  "Pop and return the head of the run queue, or nil when empty."
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
  "Remove and return the head of ACTOR's mailbox.
Callers must confirm the mailbox is non-empty first; the return
shape does not distinguish `nil'-message from empty."
  (let ((box (nelisp-actor-mailbox actor)))
    (setf (nelisp-actor-mailbox actor) (cdr box))
    (unless (cdr box)
      (setf (nelisp-actor-mailbox-tail actor) nil))
    (car box)))

;;; Shared-immutable message copy (Phase 4.3) ------------------------

(defun nelisp-actor--shareable-p (obj)
  "Return non-nil when OBJ may be shared by reference across the send
boundary (Phase 4.3 conservative classifier).

Shared (immutable or opaque host-owned):
  - fixnums, floats
  - symbols (nil / t / keywords / ordinary / uninterned)
  - records (cl-defstruct instances, including actor handles)
  - compiled-function / buffer / marker / window / process / etc.

Deep-copied (structural mutable):
  - conses
  - vectors
  - strings
  - hash-tables

Stricter classification (e.g., detecting immutable-throughout cons
trees) is deferred to v0.4.x per §2.3's `conservative classifier
で start' note."
  (not (or (consp obj)
           (vectorp obj)
           (stringp obj)
           (hash-table-p obj))))

(defun nelisp-actor--copy-message-1 (obj visited)
  "Recursive deep-copy helper used by `nelisp-actor--copy-message'.
VISITED maps original → copy so cycles and shared substructure
survive the copy (a tail shared in N places in the original remains
shared in exactly the same way in the copy).

Shareable leaves (per `nelisp-actor--shareable-p') pass through
unchanged; everything else is duplicated with its contents walked."
  (cond
   ((nelisp-actor--shareable-p obj) obj)
   ((gethash obj visited) (gethash obj visited))
   ((consp obj)
    (let ((new (cons nil nil)))
      (puthash obj new visited)
      (setcar new (nelisp-actor--copy-message-1 (car obj) visited))
      (setcdr new (nelisp-actor--copy-message-1 (cdr obj) visited))
      new))
   ((stringp obj)
    (let ((new (copy-sequence obj)))
      (puthash obj new visited)
      new))
   ((vectorp obj)
    (let* ((len (length obj))
           (new (make-vector len nil)))
      (puthash obj new visited)
      (dotimes (i len)
        (aset new i (nelisp-actor--copy-message-1 (aref obj i) visited)))
      new))
   ((hash-table-p obj)
    (let ((new (make-hash-table
                :test (hash-table-test obj)
                :size (max 1 (hash-table-count obj))
                :weakness (hash-table-weakness obj))))
      (puthash obj new visited)
      (maphash (lambda (k v)
                 (puthash (nelisp-actor--copy-message-1 k visited)
                          (nelisp-actor--copy-message-1 v visited)
                          new))
               obj)
      new))
   (t obj)))

(defun nelisp-actor--copy-message (msg)
  "Return a deep copy of MSG per the Phase 4.3 shared-immutable policy.
Uses an `eq'-keyed visited table so cycles and shared substructure
are preserved in the copy without infinite recursion."
  (nelisp-actor--copy-message-1 msg (make-hash-table :test 'eq)))

;;; Actor body macros -------------------------------------------------

(defmacro nelisp-actor-lambda (&rest body)
  "Return a closure that, when invoked, yields an iterator over BODY.

BODY is CPS-transformed by `iter-lambda' so `nelisp-receive' and
`nelisp-yield' can suspend the actor and later resume at the same
textual position.  The resulting thunk is what `nelisp-spawn'
expects; passing a plain `(lambda () ...)' without this wrapper is
not supported in Phase 4.2 because `nelisp-receive'/`nelisp-yield'
must appear lexically inside an iter context.

Example:
  (nelisp-spawn
   (nelisp-actor-lambda
     (let ((msg (nelisp-receive)))
       (do-work msg)
       (nelisp-yield)
       (finalise))))"
  (declare (indent 0) (debug (&rest form)))
  `(iter-lambda () ,@body))

(defmacro nelisp-receive (&optional _timeout)
  "Receive the next message from the current actor's mailbox.

Expands to a loop around `iter-yield' that spins while the mailbox
is empty, so the scheduler regains control and the actor stays in
`:blocked-receive' until `nelisp-send' delivers a message and
re-enqueues it.

The _TIMEOUT argument is accepted for API stability with the design
doc §4 surface but is not honoured in Phase 4.2 — the scheduler
does not yet track deadlines.  Phase 4.x adds deadline enforcement
so that on expiry the macro yields the symbol `timeout'.

Only valid inside `nelisp-actor-lambda' body.  Outside it, the
embedded `iter-yield' signals at macroexpansion time."
  (declare (debug (&optional form)))
  (ignore _timeout)
  `(progn
     (while (null (nelisp-actor-mailbox nelisp-actor--current))
       (setf (nelisp-actor-status nelisp-actor--current) :blocked-receive)
       (iter-yield 'nelisp-actor--receive))
     (nelisp-actor--mailbox-pop nelisp-actor--current)))

(defmacro nelisp-yield ()
  "Voluntarily yield the current actor to the scheduler.
The actor stays `:runnable' and is re-enqueued at the tail of the
run queue, giving other runnable actors a turn before this one
resumes.  Only valid inside `nelisp-actor-lambda' body."
  (declare (debug nil))
  '(iter-yield 'nelisp-actor--yield))

;;; Public primitives -------------------------------------------------

(cl-defun nelisp-spawn (thunk &key mailbox-capacity supervisor
                              (restart-policy :temporary))
  "Spawn a new actor executing THUNK and return the actor object.

THUNK must be a closure produced by `nelisp-actor-lambda'.  Phase
4.2 invokes THUNK immediately to obtain the generator/iterator
driving the actor body; the actor is then enqueued as `:runnable'
but not advanced until the caller drives the scheduler via
`nelisp-actor-run-until-idle' or `nelisp-actor-start'.

Keyword arguments:
  MAILBOX-CAPACITY  Integer bound for the mailbox (Phase 4.2+
                    enforces backpressure); nil = unbounded.
  SUPERVISOR        Parent actor for crash restart (Phase 4.5).
  RESTART-POLICY    `:permanent' | `:transient' | `:temporary'.

Initial `owned-set' is snapshotted from `nelisp-gc-actor-boundary'
of THUNK so the Phase 4.3 shared-immutable message policy has a
frozen reference for send-time copy decisions."
  (unless (functionp thunk)
    (signal 'wrong-type-argument (list 'functionp thunk)))
  (let* ((id (nelisp-actor--next-id))
         (iter (funcall thunk))
         (actor (nelisp-actor--make
                 :id id
                 :thunk thunk
                 :iterator iter
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
TARGET is `:dead' or `:crashed'.  When TARGET is
`:blocked-receive', transitions it back to `:runnable' and
re-enqueues it so the scheduler drives the resume on the next pass.

Phase 4.3 applies the shared-immutable message policy before
delivery: `nelisp-actor--copy-message' deep-copies cons / vector /
string / hash-table structure, while shareable leaves
(symbols / numbers / records / opaque host objects) pass through
by reference.  Cycles and shared substructure are preserved in the
copy via an `eq'-keyed visited table."
  (unless (nelisp-actor-p target)
    (signal 'wrong-type-argument (list 'nelisp-actor-p target)))
  (when (memq (nelisp-actor-status target) '(:dead :crashed))
    (signal 'nelisp-actor-error
            (list 'send-to-dead-actor
                  (nelisp-actor-id target)
                  (nelisp-actor-status target))))
  (nelisp-actor--mailbox-push target (nelisp-actor--copy-message msg))
  (when (eq (nelisp-actor-status target) :blocked-receive)
    (setf (nelisp-actor-status target) :runnable)
    (nelisp-actor--enqueue target))
  t)

;;; Scheduler (Phase 4.2 cooperative) ---------------------------------

(defun nelisp-actor--step (actor)
  "Advance ACTOR's iterator one step and classify the outcome.

Binds `nelisp-actor--current' around the `iter-next' call so that
`nelisp-self' / `nelisp-receive' / `nelisp-yield' operate on the
correct actor.  Returns one of:
  `:yielded' — actor voluntarily yielded, still `:runnable'
  `:blocked' — actor yielded from a blocked receive; status set
              to `:blocked-receive' inside `nelisp-receive'
  `:done'    — body exhausted, status `:dead'
  `:crashed' — uncaught signal, `last-error' populated, status
              `:crashed'"
  (let ((nelisp-actor--current actor))
    (condition-case err
        (let ((val (iter-next (nelisp-actor-iterator actor))))
          (cond
           ((eq val 'nelisp-actor--receive) :blocked)
           (t :yielded)))
      (iter-end-of-sequence
       (setf (nelisp-actor-status actor) :dead)
       :done)
      (error
       (setf (nelisp-actor-last-error actor) err)
       (setf (nelisp-actor-status actor) :crashed)
       :crashed))))

(defun nelisp-actor-run-until-idle ()
  "Drive the run queue until no actor is runnable.
Returns the number of steps taken (one per `iter-next' call).
Blocked-on-receive actors remain parked outside the queue; they are
re-enqueued by `nelisp-send' when a message arrives."
  (let ((steps 0))
    (while nelisp-actor--run-queue
      (let ((actor (nelisp-actor--dequeue)))
        (when (eq (nelisp-actor-status actor) :runnable)
          (cl-incf steps)
          (pcase (nelisp-actor--step actor)
            (:yielded (nelisp-actor--enqueue actor))
            (:blocked nil)
            ((or :done :crashed) nil)))))
    steps))

;;; Event-loop integration --------------------------------------------

(defun nelisp-actor--tick ()
  "Scheduler tick invoked from the idle timer installed by
`nelisp-actor-start'.  Errors are caught and logged so a broken
actor cannot tear down the timer."
  (condition-case err
      (nelisp-actor-run-until-idle)
    (error
     (message "nelisp-actor: tick error %S" err))))

(defun nelisp-actor-start ()
  "Install an idle timer that drives the scheduler continuously.
Calling while the timer is already installed is a no-op.  The timer
fires every `nelisp-actor-tick-interval' seconds of host idle time."
  (interactive)
  (unless nelisp-actor--timer
    (setq nelisp-actor--timer
          (run-with-idle-timer nelisp-actor-tick-interval t
                               #'nelisp-actor--tick))))

(defun nelisp-actor-stop ()
  "Cancel the scheduler idle timer if one is active."
  (interactive)
  (when nelisp-actor--timer
    (cancel-timer nelisp-actor--timer)
    (setq nelisp-actor--timer nil)))

;;; Diagnostics -------------------------------------------------------

(defun nelisp-actor-list ()
  "Return every actor currently in `nelisp-actor--registry'.
Order is unspecified (hash-table iteration)."
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
  "Clear all actor state including the idle timer.  For ERT only."
  (nelisp-actor-stop)
  (clrhash nelisp-actor--registry)
  (setq nelisp-actor--run-queue nil
        nelisp-actor--run-queue-tail nil
        nelisp-actor--current nil
        nelisp-actor--id-counter 0))

(provide 'nelisp-actor)
;;; nelisp-actor.el ends here
