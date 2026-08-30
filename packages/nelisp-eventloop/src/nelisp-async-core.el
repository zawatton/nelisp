;;; nelisp-async-core.el --- generator-free deadline timer queue -*- lexical-binding: t; -*-
;;
;; Doc 184 P0.  This is the actor-free half of `nelisp-async.el' (Doc
;; unknown / packages/nelisp-eventloop), cut out verbatim so it can load
;; in the standalone binary on its own.  `nelisp-async.el' itself
;; `(require 'nelisp-actor)', and `nelisp-actor.el' unconditionally
;; `(require 'generator)' -- a file that is either absent from this tree
;; or, when supplied from a real Emacs checkout, fails at macro-expansion
;; time inside this substrate (`void-variable: (declarations)', Doc 184
;; S1.5).  None of that is reachable from the functions below: the
;; deadline-ordered timer queue (`-run-at-time' / `-cancel-timer' /
;; `-fire-due' / `-next-deadline' / `-sit-for') never touches an actor.
;; Only the higher-level drivers in `nelisp-async.el' (`nelisp-async-run'
;; / `-run-tty', which call `nelisp-actor-run-until-idle') do -- and
;; those stay in that file, gated on generator exactly as before.
;;
;; Loading THIS file alone upgrades `run-at-time' / `cancel-timer' /
;; `sit-for' from the prelude's synchronous immediate-fire stubs
;; (`scripts/nelisp-stdlib-prelude.el') to real deferred/repeating ones,
;; with no actor or generator dependency at all -- this is what
;; `packages/nelisp-process-adapter/src/nelisp-process-adapter.el' (Doc
;; 184 P1-P3) requires to get REPEAT-honouring timers sharing the same
;; poll loop as process I/O.
;;
;; This is a cut, not a rewrite: the vector shape and every function body
;; below are byte-for-byte what `nelisp-async.el' already had, modulo the
;; name prefix (`nelisp-async-' -> `nelisp-async-core-').
;;
;;; Code:

(declare-function alloc-bytes "ext:nelisp-runtime" (nbytes align))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))
(declare-function nl-nanosleep "ext:nelisp-runtime" (timespec-ptr))

;;; Clock + sleep -----------------------------------------------------

(defun nelisp-async-core--now ()
  "Current time in seconds as a float (wall clock)."
  (float-time))

(defun nelisp-async-core--nanosleep (secs)
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

(defconst nelisp-async-core--t-deadline 0)
(defconst nelisp-async-core--t-repeat 1)
(defconst nelisp-async-core--t-fn 2)
(defconst nelisp-async-core--t-args 3)
(defconst nelisp-async-core--t-live 4)

(defvar nelisp-async-core--timers nil
  "List of live timer vectors, unordered.")

(defun nelisp-async-core--secs (time)
  "Coerce a `run-at-time' TIME argument to seconds-from-now (float).
Accepts a number (seconds) or nil (0); list time specs are not modelled."
  (cond ((null time) 0.0)
        ((numberp time) (float time))
        (t 0.0)))

(defun nelisp-async-core-run-at-time (time repeat fn &rest args)
  "Schedule FN to run after TIME seconds, then every REPEAT seconds.
TIME is a number of seconds (or nil = now); REPEAT is nil (one-shot)
or a number of seconds.  Returns the timer object for `cancel-timer'.
Deferred: FN fires when a driver's fire-due/pump step next runs (this
module's own poll consumers: `nelisp-async-core-sit-for', or a caller
of `nelisp-async-core--fire-due' directly, such as
`nelisp-async-run'/`nelisp-process-adapter''s poll loop).

TIME is validated before FN is touched (fixed 2026-08-24, integration/
wave6 battery run -- restores the check the prelude's own pre-upgrade
`run-at-time' stub already did, which this defalias silently dropped,
a real emacs-parity regression: `nelisp-async-core--secs' quietly
mapped any non-number/non-nil TIME to 0.0 instead of signalling, so
e.g. `(run-at-time \"ABC\" nil (function ignore))' returned a timer
object where real Emacs signals `(error \"Invalid time specification\")'):
a bad TIME is a time error, not `invalid-function' about FN, since
real Emacs never reaches FN either."
  (unless (or (null time) (numberp time)
              (and (stringp time) (string-match-p "[0-9]" time))
              ;; A time VALUE is (HIGH LOW ...) -- at least two integers.
              ;; A one-element cons is not one, and Emacs says so before
              ;; it looks at FN.
              (and (consp time) (integerp (car time))
                   (consp (cdr time)) (integerp (car (cdr time)))))
    (signal 'error (list "Invalid time specification")))
  (let ((tm (vector (+ (nelisp-async-core--now) (nelisp-async-core--secs time))
                    (and (numberp repeat) repeat)
                    fn args t)))
    (setq nelisp-async-core--timers (cons tm nelisp-async-core--timers))
    tm))

;; Host fallback (fixed 2026-08-24, integration/wave6 full-battery
;; run): this file's `defalias' below is GLOBAL and applies whether it
;; is loaded standalone (the only case it was designed for -- there is
;; no competing "real" `timerp'/`cancel-timer' to preserve, only the
;; prelude's own synchronous stubs being upgraded) or under host Emacs
;; alongside code that creates REAL host timers via the ordinary
;; `run-at-time' (e.g. any other package's test file in the same ERT
;; batch process that happens to `require' this one first -- one
;; `nelisp-actor' test measured this exact failure:
;; `(wrong-type-argument timerp [t nil nil nil nil nil nil nil nil
;; nil])', a real host timer vector, `cancel-timer'd after some
;; earlier test in the same process loaded this file). Captured ONCE,
;; before the `defalias' below takes effect (`require' caching means
;; this file's top level, including this `defvar', only ever runs
;; once per Emacs session), so both functions can recognize a REAL
;; host timer and delegate instead of misreading it as a foreign
;; vector or, worse, `aset'ing into a slot that has nothing to do
;; with this module's own layout.
(defvar nelisp-async-core--host-timerp
  (and (fboundp 'timerp) (symbol-function 'timerp))
  "The `timerp' bound before this file's own `defalias', or nil.")
(defvar nelisp-async-core--host-cancel-timer
  (and (fboundp 'cancel-timer) (symbol-function 'cancel-timer))
  "The `cancel-timer' bound before this file's own `defalias', or nil.")

(defun nelisp-async-core-timerp (x)
  "Return non-nil when X is a timer vector this module created, OR a
real host timer object recognized by whatever `timerp' was bound
before this file's own `defalias' (see
`nelisp-async-core--host-timerp').
Shape check only for this module's own timers (length 5, numeric
deadline slot) -- a cancelled timer is still a timer (real Emacs's own
`timerp' agrees: cancelling changes a timer's enabled state, not its
type), so this deliberately does not look at the LIVE slot."
  (or (and (vectorp x) (= (length x) 5)
           (numberp (aref x nelisp-async-core--t-deadline)))
      (and nelisp-async-core--host-timerp
           (funcall nelisp-async-core--host-timerp x))))

(defun nelisp-async-core-cancel-timer (tm)
  "Cancel timer TM so it never fires again.
Delegates to whatever `cancel-timer' was bound before this file's own
`defalias' (see `nelisp-async-core--host-cancel-timer') for anything
that is not one of this module's own timer vectors AND is recognized
by the captured host `timerp' -- notably a real host-Emacs timer, so
this never `aset's into a slot from a layout that is not its own.
Signals `wrong-type-argument' for anything neither recognizes,
matching real Emacs's own `cancel-timer' (measured: `(cancel-timer
\"x\")' -> `(wrong-type-argument timerp \"x\")' on host Emacs 30.1) and
the prelude's own pre-upgrade stub this defalias replaces -- fixed
2026-08-24 (integration/wave6 battery run): the version this replaced
silently accepted anything `vectorp' rejected and returned nil
instead, a real emacs-parity regression Phase 2A's process-adapter
wiring exposed by making this the default `cancel-timer' on every
standalone build instead of an opt-in `--load'."
  (cond
   ((and (vectorp tm) (= (length tm) 5)
         (numberp (aref tm nelisp-async-core--t-deadline)))
    (aset tm nelisp-async-core--t-live nil)
    nil)
   ((and nelisp-async-core--host-timerp
         (funcall nelisp-async-core--host-timerp tm)
         nelisp-async-core--host-cancel-timer)
    (funcall nelisp-async-core--host-cancel-timer tm))
   (t (signal 'wrong-type-argument (list 'timerp tm)))))

(defun nelisp-async-core--next-deadline ()
  "Return the earliest live-timer deadline, or nil when none are armed."
  (let ((best nil))
    (dolist (tm nelisp-async-core--timers)
      (when (aref tm nelisp-async-core--t-live)
        (let ((d (aref tm nelisp-async-core--t-deadline)))
          (when (or (null best) (< d best))
            (setq best d)))))
    best))

(defun nelisp-async-core--fire-due (now)
  "Fire every live timer whose deadline is <= NOW.
Re-arm repeating timers; drop one-shot timers; prune dead entries.
Returns the number of timers fired."
  (let ((fired 0))
    (dolist (tm nelisp-async-core--timers)
      (when (and (aref tm nelisp-async-core--t-live)
                 (<= (aref tm nelisp-async-core--t-deadline) now))
        (setq fired (1+ fired))
        (let ((repeat (aref tm nelisp-async-core--t-repeat)))
          ;; Re-arm BEFORE running so a callback that cancels wins.
          (if repeat
              (aset tm nelisp-async-core--t-deadline (+ now repeat))
            (aset tm nelisp-async-core--t-live nil))
          (apply (aref tm nelisp-async-core--t-fn) (aref tm nelisp-async-core--t-args)))))
    ;; Prune dead timers to bound the list.
    (when (> fired 0)
      (setq nelisp-async-core--timers
            (let (keep)
              (dolist (tm nelisp-async-core--timers)
                (when (aref tm nelisp-async-core--t-live) (setq keep (cons tm keep))))
              keep)))
    fired))

(defun nelisp-async-core-reset-timers ()
  "Drop every armed timer.  Test hygiene only."
  (setq nelisp-async-core--timers nil))

;;; sit-for: wait while servicing timers --------------------------------

(defun nelisp-async-core-sit-for (seconds &rest _)
  "Wait up to SECONDS, firing any due timers along the way.
Returns t (the headless reader has no input to interrupt on; a TTY
input layer built on top of this core, e.g. `nelisp-async.el', can
override this to return nil when a key arrives)."
  (let ((end (+ (nelisp-async-core--now) (if (numberp seconds) seconds 0))))
    (nelisp-async-core--fire-due (nelisp-async-core--now))
    (let ((remain (- end (nelisp-async-core--now))))
      (while (> remain 0)
        (let* ((nd (nelisp-async-core--next-deadline))
               (gap (if nd (min remain (max 0.0 (- nd (nelisp-async-core--now))))
                      remain)))
          (nelisp-async-core--nanosleep (if (> gap 0) gap remain))
          (nelisp-async-core--fire-due (nelisp-async-core--now))
          (setq remain (- end (nelisp-async-core--now)))))))
  t)

;;; Upgrade the prelude stubs to the real deferred implementations -----
;; Same upgrade-on-load pattern `nelisp-async.el' already used: loading
;; this file redefines the four standard names unconditionally (no
;; `unless (fboundp ...)' guard -- that guard is what makes the prelude's
;; own synchronous stubs step aside for a real implementation once one is
;; loaded, matching how the prelude's own `run-at-time' stub documents
;; itself: "REPEAT ignored" only until something upgrades it).  `timerp'
;; joins the other three as of 2026-08-24 (integration/wave6 battery
;; run): the prelude's own `timerp' is an unconditional "there are no
;; timers in this runtime" `nil' stub (correct for ITS OWN synchronous
;; `run-at-time', which returns a `(nelisp--sync-timer FUNCTION)' list,
;; not a real timer object at all), but once `run-at-time' is upgraded
;; to return a real, cancellable timer vector, `timerp' must upgrade
;; alongside it or a genuine timer object stops being recognized as one.

(defalias 'run-at-time #'nelisp-async-core-run-at-time)
(defalias 'cancel-timer #'nelisp-async-core-cancel-timer)
(defalias 'sit-for #'nelisp-async-core-sit-for)
(defalias 'timerp #'nelisp-async-core-timerp)

(provide 'nelisp-async-core)
;;; nelisp-async-core.el ends here
