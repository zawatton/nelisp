;;; nelisp-eventloop.el --- NeLisp event loop -*- lexical-binding: t; -*-
;;
;; Phase 5-B.4 per Doc 13.  Reusable dispatcher + Phase 4 actor
;; integration for the "Emacs core 置換" event-loop layer:
;;
;;   - `nelisp-event' struct (kind / data / timestamp)
;;   - key-sequence -> command binding table with string / int
;;     normalisation
;;   - synchronous `nelisp-eventloop-step' / `-run-scripted' for
;;     test harnesses and batch dispatch
;;   - `nelisp-eventloop-spawn-main-actor' — Phase 4 actor-lambda
;;     that receives events, dispatches via the binding table, and
;;     terminates on a `quit' event
;;   - `nelisp-eventloop-schedule-timer' — host-assisted timer
;;     wrapper that routes a `timer' event back to a target actor
;;
;; Real tty raw-mode stdin input is out of scope for this MVP; the
;; pluggable `nelisp-eventloop-input-source' defvar lets E2E demos
;; (§3.5) script keystrokes without dragging ttybinding into the
;; core file.
;;
;;; Code:

(require 'cl-lib)
(require 'nelisp-actor)

(cl-defstruct (nelisp-event
               (:constructor nelisp-event--make)
               (:copier nil))
  (kind nil)
  (data nil)
  (timestamp 0))

(defun nelisp-make-event (kind data &optional timestamp)
  "Construct a `nelisp-event' of KIND carrying DATA.
TIMESTAMP defaults to `float-time' of the constructor call."
  (nelisp-event--make
   :kind kind
   :data data
   :timestamp (or timestamp (float-time))))

;;; Key-sequence bindings --------------------------------------------

(defvar nelisp-eventloop--bindings (make-hash-table :test 'equal)
  "Key-sequence string -> command function (takes the event).")

(defun nelisp-eventloop-bind (seq cmd)
  "Bind sequence SEQ to command function CMD.
CMD is a 1-argument function receiving the dispatched event."
  (puthash seq cmd nelisp-eventloop--bindings))

(defun nelisp-eventloop-unbind (seq)
  "Remove the binding for SEQ."
  (remhash seq nelisp-eventloop--bindings))

(defun nelisp-eventloop-lookup (seq)
  "Return the command bound to SEQ, or nil."
  (gethash seq nelisp-eventloop--bindings))

(defun nelisp-eventloop-reset-bindings ()
  "Clear every binding.  Useful in test setup."
  (clrhash nelisp-eventloop--bindings))

(defun nelisp-eventloop--key-seq (data)
  "Normalise key-event DATA to a sequence string.
An integer is converted via `char-to-string'; a string is taken
verbatim; any other value is `format'ed with %s."
  (cond
   ((stringp data) data)
   ((integerp data) (char-to-string data))
   (t (format "%s" data))))

;;; Dispatch core -----------------------------------------------------

(defun nelisp-eventloop--dispatch (ev)
  "Dispatch EV once via the binding table.
Returns the command's return value, or nil when no command fires.
`quit' events flip `nelisp-eventloop--running' to nil so the
synchronous loop (`nelisp-eventloop-run-scripted') exits cleanly."
  (pcase (nelisp-event-kind ev)
    ('key
     (let* ((seq (nelisp-eventloop--key-seq (nelisp-event-data ev)))
            (cmd (gethash seq nelisp-eventloop--bindings)))
       (when (functionp cmd) (funcall cmd ev))))
    ('timer
     (let ((cmd (nelisp-event-data ev)))
       (when (functionp cmd) (funcall cmd ev))))
    ('quit
     (setq nelisp-eventloop--running nil))
    (_ nil)))

;;; Synchronous queue loop -------------------------------------------

(defvar nelisp-eventloop--queue nil
  "Pending events, FIFO.")

(defvar nelisp-eventloop--running nil
  "Non-nil while the synchronous loop is draining the queue.")

(defun nelisp-eventloop-enqueue (ev)
  "Append EV to the synchronous queue."
  (setq nelisp-eventloop--queue (nconc nelisp-eventloop--queue (list ev))))

(defun nelisp-eventloop-drain ()
  "Discard every pending event without dispatching."
  (setq nelisp-eventloop--queue nil))

(defun nelisp-eventloop-step ()
  "Pop and dispatch one event from the queue, if any."
  (when nelisp-eventloop--queue
    (let ((ev (car nelisp-eventloop--queue)))
      (setq nelisp-eventloop--queue (cdr nelisp-eventloop--queue))
      (nelisp-eventloop--dispatch ev))))

(defun nelisp-eventloop-run-scripted (events)
  "Enqueue EVENTS (a list) and drain until empty or a `quit' event.
Used by ERT harnesses and the §3.5 editor demo to exercise
dispatch without touching a real terminal."
  (setq nelisp-eventloop--running t)
  (dolist (ev events)
    (nelisp-eventloop-enqueue ev))
  (while (and nelisp-eventloop--running nelisp-eventloop--queue)
    (nelisp-eventloop-step))
  nil)

;;; Actor-integrated main loop ---------------------------------------

(defun nelisp-eventloop-spawn-main-actor ()
  "Spawn the main event-loop actor.

Returned actor receives `nelisp-event' messages via `nelisp-send'
and runs them through `nelisp-eventloop--dispatch', yielding
between events so peers (timer senders, input feeders) interleave
fairly.  A `quit' event terminates the actor body."
  (nelisp-spawn
   (nelisp-actor-lambda
     (let ((running t))
       (while running
         (let ((ev (nelisp-receive)))
           (cond
            ((and (nelisp-event-p ev)
                  (eq (nelisp-event-kind ev) 'quit))
             (setq running nil))
            ((nelisp-event-p ev)
             (nelisp-eventloop--dispatch ev)
             (nelisp-yield))
            (t (nelisp-yield)))))))))

;;; Timer wrapper -----------------------------------------------------

(defun nelisp-eventloop-schedule-timer (seconds cmd target)
  "Schedule a `timer' event for TARGET actor after SECONDS.

Uses the host `run-at-time' primitive (borrowed in Phase 5-B.0)
to fire once.  When the timer elapses, a `nelisp-event' of kind
`timer' carrying CMD as its data is delivered to TARGET via
`nelisp-send' — the main actor then funcalls CMD from its own
dispatch.  Returns the underlying host timer object so callers
can `cancel-timer' if needed."
  (run-at-time
   seconds nil
   (lambda ()
     (when (and target (nelisp-actor-p target)
                (not (memq (nelisp-actor-status target)
                           '(:dead :crashed))))
       (nelisp-send target (nelisp-make-event 'timer cmd))))))

;;; Pluggable input source -------------------------------------------

(defvar nelisp-eventloop-input-source nil
  "Function returning the next `nelisp-event' or nil (no event).
Set by higher layers (real tty reader, scripted test feed).
Core dispatch does not consult this — spawners do.")

(provide 'nelisp-eventloop)
;;; nelisp-eventloop.el ends here
