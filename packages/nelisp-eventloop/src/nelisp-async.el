;;; nelisp-async.el --- Real async event loop (timers + TTY) for the standalone -*- lexical-binding: t; -*-
;;
;; The bare reader has no asynchronous runtime: the prelude ships a
;; *synchronous* `run-at-time' stub that fires immediately.  This module
;; provides the real thing on top of the standalone's OS primitives:
;;
;;   - a deadline-ordered timer queue (`run-at-time' / `cancel-timer'
;;     become deferred; due timers fire when the loop or `sit-for' runs);
;;   - efficient waiting via the `nl-nanosleep' builtin (no busy-loop);
;;   - a cooperative driver, `nelisp-async-run', that fires due timers,
;;     drives the `nelisp-actor' scheduler, and sleeps until the next
;;     deadline, multiplexing TTY input when a tty reader is installed
;;     (see `nelisp-async-tty-*' in the input layer).
;;
;; Loading this file UPGRADES `run-at-time' / `cancel-timer' / `sit-for'
;; from the prelude stubs to the real deferred implementations, so any
;; timer-driven code (e.g. `nelisp-eventloop-schedule-timer') runs on a
;; genuine clock.  Pure timekeeping uses `float-time' (wall clock); the
;; reader has no `clock_gettime' monotonic helper yet (Doc note), which is
;; adequate for cooperative scheduling.
;;
;;; Code:

(require 'cl-lib)
(require 'nelisp-actor)
(require 'nelisp-eventloop)

;;; Clock + sleep -----------------------------------------------------

(defun nelisp-async--now ()
  "Current time in seconds as a float (wall clock)."
  (float-time))

(defun nelisp-async--nanosleep (secs)
  "Sleep for SECS seconds (a float) using the `nl-nanosleep' builtin.
Sub-second precision is honoured; a non-positive SECS is a no-op."
  (when (and (numberp secs) (> secs 0))
    (let* ((whole (truncate secs))
           (nsec (truncate (* (- secs whole) 1000000000.0)))
           (ts (alloc-bytes 16 8)))
      (ptr-write-u64 ts 0 whole)
      (ptr-write-u64 ts 8 nsec)
      (nl-nanosleep ts))))

;;; Timer queue -------------------------------------------------------
;; A timer is a vector [DEADLINE REPEAT FN ARGS LIVE]:
;;   0 DEADLINE  float-time at/after which FN runs
;;   1 REPEAT    nil = one-shot; a number = re-arm REPEAT seconds later
;;   2 FN        function to call
;;   3 ARGS      argument list applied to FN
;;   4 LIVE      t until fired (one-shot) or cancelled

(defconst nelisp-async--t-deadline 0)
(defconst nelisp-async--t-repeat 1)
(defconst nelisp-async--t-fn 2)
(defconst nelisp-async--t-args 3)
(defconst nelisp-async--t-live 4)

(defvar nelisp-async--timers nil
  "List of live timer vectors, unordered.")

(defun nelisp-async--secs (time)
  "Coerce a `run-at-time' TIME argument to seconds-from-now (float).
Accepts a number (seconds) or nil (0); list time specs are not modelled."
  (cond ((null time) 0.0)
        ((numberp time) (float time))
        (t 0.0)))

(defun nelisp-async-run-at-time (time repeat fn &rest args)
  "Schedule FN to run after TIME seconds, then every REPEAT seconds.
TIME is a number of seconds (or nil = now); REPEAT is nil (one-shot)
or a number of seconds.  Returns the timer object for `cancel-timer'.
Deferred: FN fires when `nelisp-async-run' or `sit-for' next runs."
  (let ((tm (vector (+ (nelisp-async--now) (nelisp-async--secs time))
                    (and (numberp repeat) repeat)
                    fn args t)))
    (setq nelisp-async--timers (cons tm nelisp-async--timers))
    tm))

(defun nelisp-async-cancel-timer (tm)
  "Cancel timer TM so it never fires again."
  (when (vectorp tm)
    (aset tm nelisp-async--t-live nil))
  nil)

(defun nelisp-async--next-deadline ()
  "Return the earliest live-timer deadline, or nil when none are armed."
  (let ((best nil))
    (dolist (tm nelisp-async--timers)
      (when (aref tm nelisp-async--t-live)
        (let ((d (aref tm nelisp-async--t-deadline)))
          (when (or (null best) (< d best))
            (setq best d)))))
    best))

(defun nelisp-async--fire-due (now)
  "Fire every live timer whose deadline is <= NOW.
Re-arm repeating timers; drop one-shot timers; prune dead entries.
Returns the number of timers fired."
  (let ((fired 0))
    (dolist (tm nelisp-async--timers)
      (when (and (aref tm nelisp-async--t-live)
                 (<= (aref tm nelisp-async--t-deadline) now))
        (setq fired (1+ fired))
        (let ((repeat (aref tm nelisp-async--t-repeat)))
          ;; Re-arm BEFORE running so a callback that cancels wins.
          (if repeat
              (aset tm nelisp-async--t-deadline (+ now repeat))
            (aset tm nelisp-async--t-live nil))
          (apply (aref tm nelisp-async--t-fn) (aref tm nelisp-async--t-args)))))
    ;; Prune dead timers to bound the list.
    (when (> fired 0)
      (setq nelisp-async--timers
            (let (keep)
              (dolist (tm nelisp-async--timers)
                (when (aref tm nelisp-async--t-live) (setq keep (cons tm keep))))
              keep)))
    fired))

(defun nelisp-async-reset-timers ()
  "Drop every armed timer.  Test hygiene only."
  (setq nelisp-async--timers nil))

;;; sit-for: wait while servicing timers --------------------------------

(defun nelisp-async-sit-for (seconds &rest _)
  "Wait up to SECONDS, firing any due timers along the way.
Returns t (the headless reader has no input to interrupt on; the TTY
input layer overrides this to return nil when a key arrives)."
  (let ((end (+ (nelisp-async--now) (if (numberp seconds) seconds 0))))
    (nelisp-async--fire-due (nelisp-async--now))
    (let ((remain (- end (nelisp-async--now))))
      (while (> remain 0)
        (let* ((nd (nelisp-async--next-deadline))
               (gap (if nd (min remain (max 0.0 (- nd (nelisp-async--now))))
                      remain)))
          (nelisp-async--nanosleep (if (> gap 0) gap remain))
          (nelisp-async--fire-due (nelisp-async--now))
          (setq remain (- end (nelisp-async--now)))))))
  t)

;;; Cooperative driver ------------------------------------------------

(defun nelisp-async-run (&optional main)
  "Drive the async runtime until idle (or MAIN actor terminates).
Each turn: fire due timers, run the actor scheduler to quiescence,
then sleep until the next timer deadline.  Stops with:
  - the MAIN actor status (`:dead' / `:crashed') if MAIN is given and ends;
  - `:idle' when no timers remain and no actor is runnable.
This is the timer-only core; the TTY input layer wraps it to also block
on stdin via `poll(2)' between deadlines."
  (catch 'nelisp-async-done
    (while t
      (nelisp-async--fire-due (nelisp-async--now))
      (nelisp-actor-run-until-idle)
      (when (and main (memq (nelisp-actor-status main) '(:dead :crashed)))
        (throw 'nelisp-async-done (nelisp-actor-status main)))
      (let ((nd (nelisp-async--next-deadline)))
        (if (null nd)
            (throw 'nelisp-async-done :idle)
          (let ((gap (- nd (nelisp-async--now))))
            (when (> gap 0) (nelisp-async--nanosleep gap))))))))

;;; Upgrade the prelude stubs to the real deferred implementations -----

(defalias 'run-at-time #'nelisp-async-run-at-time)
(defalias 'cancel-timer #'nelisp-async-cancel-timer)
(defalias 'sit-for #'nelisp-async-sit-for)

(provide 'nelisp-async)
;;; nelisp-async.el ends here
