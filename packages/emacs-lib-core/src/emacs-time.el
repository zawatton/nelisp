;;; emacs-time.el --- Time + truncate polyfills for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 10 — extracted from `emacs-stub.el' (= the Phase 6
;; write-path polyfill).  Wraps the build-tool builtins
;; `nl-current-unix-time' when present, otherwise the NeLisp
;; `nelisp--syscall' bridge for Linux `time(2)'.
;;
;; Clock and timezone primitives are supplied by the NeLisp core.
;;
;; `truncate' is included here because its bulk-stub no-op (emitted
;; by `emacs-stub-bulk.el') was not real-integer-correct; this file's
;; version replaces it when the bulk stub fired first.
;;
;; Each definition is gated on the appropriate `unless (fboundp ...)'
;; or live-replace check.  Loading under host Emacs is a cheap no-op.

;;; Code:

(defconst emacs-time--unix-epoch-absolute-day 719163
  "Emacs calendar absolute day for 1970-01-01.")

(defun emacs-time--standalone-runtime-p ()
  "Return non-nil when NeLisp standalone runtime markers are available."
  (or (fboundp 'nl-current-unix-time)
      (fboundp 'nelisp--syscall)
      (fboundp 'nelisp--load-resolved-file)))

(defun emacs-time--host-runtime-p ()
  "Return non-nil when running under regular host Emacs.
Standalone NeLisp also binds `emacs-version' to a real string (for vendor
compatibility), so `(and (boundp ...) (stringp ...))' alone misfires
there; exclude standalone explicitly via
`emacs-time--standalone-runtime-p'."
  (and (boundp 'emacs-version)
       (stringp emacs-version)
       (not (emacs-time--standalone-runtime-p))))

(defun emacs-time--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (let ((function (condition-case nil
                           (symbol-function symbol)
                         (error nil))))
         (and function
              (not (eq function 'nelisp--unbound-marker))))))

(defun emacs-time--standalone-unix-time ()
  "Return Unix time from the standalone runtime, or nil if unavailable."
  (cond
   ((fboundp 'nl-current-unix-time)
    (nl-current-unix-time))
   ;; Linux x86_64/aarch64: __NR_time = 201.  Passing NULL avoids any
   ;; userspace pointer writes and returns the epoch seconds directly.
   ((fboundp 'nelisp--syscall)
    (condition-case nil
        (let ((value (nelisp--syscall 201 0)))
          (and (integerp value)
               (>= value 0)
               value))
      (error nil)))
   (t nil)))

;; Live-replace gate — same pattern as `truncate' below.  NeLisp v1.2.0's
;; native `float-time' ignored its optional argument; current NeLisp (062fa96c7+)
;; is GNU-correct (returns floats, honours TIME-VALUE), so the wrapper is now
;; installed only when `float-time' is unbound (see `emacs-time--install-float-time').

(defun emacs-time--install-float-time-p ()
  "Return non-nil when the compatibility `float-time' should be installed."
  (or (emacs-time--standalone-runtime-p)
      (not (emacs-time--host-runtime-p))
      (not (and (fboundp 'float-time)
                (let ((ft (ignore-errors (float-time))))
                  (and (numberp ft) (> ft 0)))))))

(defun emacs-time--float-time (&optional time-value)
  "Return current time, or convert TIME-VALUE to seconds since the epoch."
  (if time-value
      (emacs-time--to-number time-value)
    (if (fboundp 'emacs-time--standalone-float-time)
        (emacs-time--standalone-float-time)
      (or (emacs-time--standalone-unix-time) 0))))

(defun emacs-time--install-float-time ()
  "Install the compatibility `float-time' implementation."
  (when (and (emacs-time--standalone-runtime-p)
             (emacs-time--function-cell-live-p 'float-time)
             (not (fboundp 'emacs-time--standalone-float-time)))
    (defalias 'emacs-time--standalone-float-time
      (symbol-function 'float-time)))
  (unless (fboundp 'float-time)
    (defalias 'float-time #'emacs-time--float-time)))

(when (emacs-time--install-float-time-p)
  (emacs-time--install-float-time))

(defun emacs-time--to-number (tv)
  "Convert an Emacs time value TV to a comparable number of seconds.
Handles an integer / float (seconds), a (TICKS . HZ) pair, and the
(HIGH LOW [USEC [PSEC]]) list form.  nil reads the current time.  This does
not route through the standalone `float-time', which ignores its argument."
  (cond
   ((null tv) (float-time))
   ((numberp tv) tv)
   ((and (consp tv) (numberp (cdr tv)))
    (/ (float (car tv)) (cdr tv)))
   ((consp tv)
    (let ((high (or (nth 0 tv) 0))
          (low (or (nth 1 tv) 0))
          (usec (or (nth 2 tv) 0))
          (psec (or (nth 3 tv) 0)))
      (if (and (>= high 65536)
               (= low 0)
               (= usec 0)
               (= psec 0))
          high
        (+ (* high 65536.0) low
           (/ usec 1000000.0)
           (/ psec 1000000000000.0)))))
   (t 0)))


(unless (boundp 'display-time-mode)
  (defvar display-time-mode nil
    "Headless no-op compatibility toggle for `display-time-mode'."))

(unless (emacs-time--function-cell-live-p 'display-time-mode)
  (defalias 'display-time-mode #'ignore))

(defun emacs-time--civil-from-days (days)
  "Return (YEAR MONTH DAY) for DAYS since 1970-01-01."
  (let* ((z (+ days 719468))
         (era (/ (if (>= z 0) z (- z 146096)) 146097))
         (doe (- z (* era 146097)))
         (yoe (/ (- doe (/ doe 1460) (/ doe 36524) (/ doe 146096)) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (/ yoe 4) (- (/ yoe 100)))))
         (mp (/ (+ (* 5 doy) 2) 153))
         (day (+ (- doy (/ (+ (* 153 mp) 2) 5)) 1))
         (month (+ mp (if (< mp 10) 3 -9)))
         (year (+ y (if (<= month 2) 1 0))))
    (list year month day)))

(unless (fboundp 'decoded-time-second)
  (defun decoded-time-second (time) (nth 0 time)))

(unless (fboundp 'decoded-time-minute)
  (defun decoded-time-minute (time) (nth 1 time)))

(unless (fboundp 'decoded-time-hour)
  (defun decoded-time-hour (time) (nth 2 time)))

(unless (fboundp 'decoded-time-day)
  (defun decoded-time-day (time) (nth 3 time)))

(unless (fboundp 'decoded-time-month)
  (defun decoded-time-month (time) (nth 4 time)))

(unless (fboundp 'decoded-time-year)
  (defun decoded-time-year (time) (nth 5 time)))

(unless (fboundp 'calendar-extract-month)
  (defun calendar-extract-month (date) (nth 0 date)))

(unless (fboundp 'calendar-extract-day)
  (defun calendar-extract-day (date) (nth 1 date)))

(unless (fboundp 'calendar-extract-year)
  (defun calendar-extract-year (date) (nth 2 date)))

(unless (and (emacs-time--host-runtime-p)
             (fboundp 'calendar-absolute-from-gregorian))
  (defun calendar-absolute-from-gregorian (date)
    "Return an absolute day number for Gregorian DATE = (MONTH DAY YEAR)."
    (pcase-let ((`(,month ,day ,year) date))
      (let* ((y (if (<= month 2) (1- year) year))
             (m (if (<= month 2) (+ month 12) month)))
        (+ day
           (/ (+ (* 153 (- m 3)) 2) 5)
           (* 365 y)
           (/ y 4)
           (- (/ y 100))
           (/ y 400)
           -306)))))

(unless (and (emacs-time--host-runtime-p)
             (fboundp 'calendar-gregorian-from-absolute))
  (defun calendar-gregorian-from-absolute (absolute)
    "Return Gregorian (MONTH DAY YEAR) for ABSOLUTE day number."
    (pcase-let ((`(,year ,month ,day)
                 (emacs-time--civil-from-days
                  (- absolute emacs-time--unix-epoch-absolute-day))))
      (list month day year))))

(unless (fboundp 'calendar-sum)
  (defmacro calendar-sum (index initial condition expression)
    "Accumulate EXPRESSION while CONDITION holds, iterating INDEX."
    `(let ((,index ,initial)
           (calendar-sum--acc 0))
       (while ,condition
         (setq calendar-sum--acc (+ calendar-sum--acc ,expression))
         (setq ,index (1+ ,index)))
       calendar-sum--acc)))

(unless (fboundp 'time-since)
  (defun time-since (time)
    "Return elapsed seconds since TIME."
    (time-subtract (current-time) time)))

;;; Doc 06 B2: timers (run-with-timer / run-with-idle-timer / cancel-timer).
;; Minimal pure-Elisp implementation (no cl-defstruct).  A timer is a vector
;;   [emacs-timer TRIGGER REPEAT FN ARGS IDLE-DELAY FIRED-P].
;; Firing is driven by `emacs-timer-run-pending' / `emacs-timer-run-idle',
;; which the runtime event loop calls each tick.

(unless (boundp 'timer-list) (defvar timer-list nil
  "List of active (non-idle) timers."))
(unless (boundp 'timer-idle-list) (defvar timer-idle-list nil
  "List of active idle timers."))

(defvar emacs-timer--idle-since nil
  "Beginning of the current shared idle period; reset only by input.")

(defun emacs-timer-idle-seconds ()
  "Return elapsed idle time, starting a period on the first wait."
  (let ((now (emacs-timer--now)))
    (unless emacs-timer--idle-since (setq emacs-timer--idle-since now))
    (max 0 (- now emacs-timer--idle-since))))

(defun emacs-timer--now ()
  (if (fboundp 'float-time) (float-time) 0))

(defun emacs-timer--make (trigger repeat fn args idle-delay)
  (vector 'emacs-timer trigger repeat fn args idle-delay nil))

(defun emacs-timer-p (obj)
  "Return non-nil when OBJ is one of our timer vectors."
  (and (vectorp obj) (> (length obj) 0) (eq (aref obj 0) 'emacs-timer)))

(defun emacs-timer-run-with-timer (secs repeat fn &rest args)
  "Schedule FN after SECS seconds, repeating every REPEAT seconds when set."
  (let ((tm (emacs-timer--make (+ (emacs-timer--now) (or secs 0)) repeat fn args nil)))
    (setq timer-list (cons tm timer-list))
    tm))

(defun emacs-timer-run-with-idle-timer (secs repeat fn &rest args)
  "Schedule FN to run after SECS seconds of idle time."
  (let ((tm (emacs-timer--make nil repeat fn args (or secs 0))))
    (setq timer-idle-list (cons tm timer-idle-list))
    tm))

(defun emacs-timer-cancel (timer)
  "Remove TIMER from the active timer lists."
  (setq timer-list (delq timer timer-list)
        timer-idle-list (delq timer timer-idle-list))
  nil)

;; Timers made by GNU timer.el (`run-at-time', `run-with-idle-timer' resolve
;; to it) are records of their own shape.  GNU's C `timer_check' services them
;; by calling `timer-event-handler'; the shared wait loop is that caller here.
(defvar emacs-timer--gnu-idle-fired nil
  "GNU idle timers already run in the current idle period.")

(defun emacs-timer--gnu-p (timer)
  "Return non-nil when TIMER is a GNU timer.el timer record."
  (and (not (emacs-timer-p timer))
       (fboundp 'timer-event-handler) (fboundp 'timer--time)
       (fboundp 'timerp) (timerp timer)))

(defun emacs-timer--gnu-run-pending ()
  "Run due GNU regular timers through `timer-event-handler'; return count."
  (let ((fired 0) (now (current-time)))
    (dolist (tm (copy-sequence timer-list))
      (when (and (emacs-timer--gnu-p tm) (memq tm timer-list)
                 (not (timer--triggered tm))
                 (not (time-less-p now (timer--time tm))))
        (setq fired (1+ fired))
        (timer-event-handler tm)))
    fired))

(defun emacs-timer--gnu-run-idle (idle-seconds)
  "Run GNU idle timers whose delay has elapsed once per idle period."
  (let ((fired 0))
    (dolist (tm (copy-sequence timer-idle-list))
      (when (and (emacs-timer--gnu-p tm) (memq tm timer-idle-list)
                 (not (memq tm emacs-timer--gnu-idle-fired))
                 (<= (float-time (timer--time tm)) idle-seconds))
        (setq fired (1+ fired))
        (push tm emacs-timer--gnu-idle-fired)
        (timer-event-handler tm)))
    fired))

(defun emacs-timer-run-pending (&optional now)
  "Fire due regular timers (TRIGGER <= NOW); reschedule repeating ones.
Return the number fired.
`timer-list' is shared with the host when running under real Emacs
\(the `unless (boundp ...)' guard above only installs our own private
variable when the host has not already bound one), so a host timer of
the host's own vector shape can be present alongside ours; skip
anything that is not one of our `emacs-timer-p' vectors instead of
misreading its slots."
  (let ((now (or now (emacs-timer--now))) (fired 0))
    (dolist (tm (copy-sequence timer-list))
      (when (and (emacs-timer-p tm) (aref tm 1) (<= (aref tm 1) now))
        (setq fired (1+ fired))
        ;; Retire/reschedule before calling Lisp: callbacks can wait again or
        ;; cancel themselves without recursively firing this due timer.
        (if (aref tm 2)
            (aset tm 1 (+ now (aref tm 2)))
          (setq timer-list (delq tm timer-list)))
        (condition-case _ (apply (aref tm 3) (aref tm 4)) (error nil))))
    (+ fired (emacs-timer--gnu-run-pending))))

(defun emacs-timer-run-idle (idle-seconds)
  "Fire idle timers whose delay <= IDLE-SECONDS and not already fired this
idle period.  Return the number fired.
See `emacs-timer-run-pending' for why non-`emacs-timer-p' entries (e.g.
a host Emacs's own pre-registered idle timers such as
`show-paren-function', which share `timer-idle-list' with us) must be
skipped rather than indexed with our private vector layout."
  (let ((fired 0))
    (dolist (tm (copy-sequence timer-idle-list))
      (when (and (emacs-timer-p tm) (not (aref tm 6)) (<= (aref tm 5) idle-seconds))
        (aset tm 6 t)
        (setq fired (1+ fired))
        (condition-case _ (apply (aref tm 3) (aref tm 4)) (error nil))
        (unless (aref tm 2)
          (setq timer-idle-list (delq tm timer-idle-list)))))
    (+ fired (emacs-timer--gnu-run-idle idle-seconds))))

(defun emacs-timer-reset-idle ()
  "Clear the per-idle-period fired flag (call when input resets idle time)."
  (setq emacs-timer--idle-since nil
        emacs-timer--gnu-idle-fired nil)
  (dolist (tm timer-idle-list) (when (emacs-timer-p tm) (aset tm 6 nil))))

(defun emacs-timer-next-delay (maximum)
  "Return seconds until the next regular or unfired idle timer, capped at MAXIMUM.
Frontends use this deadline to block on their input fd without delaying
callbacks or waking continuously when no timer is scheduled."
  (let ((delay maximum) (now (emacs-timer--now))
        (idle (emacs-timer-idle-seconds)))
    (dolist (tm timer-list)
      (when (and (emacs-timer-p tm) (aref tm 1))
        (setq delay (min delay (max 0 (- (aref tm 1) now)))))
      (when (and (emacs-timer--gnu-p tm) (not (timer--triggered tm)))
        (setq delay (min delay (max 0 (- (float-time (timer--time tm)) now))))))
    (dolist (tm timer-idle-list)
      (when (and (emacs-timer-p tm) (not (aref tm 6)))
        (setq delay (min delay (max 0 (- (aref tm 5) idle)))))
      (when (and (emacs-timer--gnu-p tm)
                 (not (memq tm emacs-timer--gnu-idle-fired)))
        (setq delay (min delay (max 0 (- (float-time (timer--time tm)) idle))))))
    delay))

(unless (and (not (fboundp 'nelisp--write-stdout-bytes))
             (fboundp 'timerp)
             (not (get 'timerp 'emacs-stub-bulk)))
  (defun timerp (obj) (emacs-timer-p obj))
  (put 'timerp 'emacs-stub-bulk nil))
(unless (and (not (fboundp 'nelisp--write-stdout-bytes))
             (fboundp 'run-with-timer)
             (not (get 'run-with-timer 'emacs-stub-bulk)))
  (defun run-with-timer (secs repeat fn &rest args)
    (apply #'emacs-timer-run-with-timer secs repeat fn args))
  (put 'run-with-timer 'emacs-stub-bulk nil))
(unless (and (not (fboundp 'nelisp--write-stdout-bytes))
             (fboundp 'run-at-time)
             (not (get 'run-at-time 'emacs-stub-bulk)))
  (defun run-at-time (time repeat fn &rest args)
    "MVP: TIME is treated as a number of seconds (or nil = now); string time
specifications are not parsed."
    (apply #'emacs-timer-run-with-timer (if (numberp time) time 0) repeat fn args))
  (put 'run-at-time 'emacs-stub-bulk nil))
(unless (and (not (fboundp 'nelisp--write-stdout-bytes))
             (fboundp 'run-with-idle-timer)
             (not (get 'run-with-idle-timer 'emacs-stub-bulk)))
  (defun run-with-idle-timer (secs repeat fn &rest args)
    (apply #'emacs-timer-run-with-idle-timer secs repeat fn args))
  (put 'run-with-idle-timer 'emacs-stub-bulk nil))
(unless (and (not (fboundp 'nelisp--write-stdout-bytes))
             (fboundp 'cancel-timer)
             (not (get 'cancel-timer 'emacs-stub-bulk)))
  (defun cancel-timer (timer) (emacs-timer-cancel timer))
  (put 'cancel-timer 'emacs-stub-bulk nil))

(provide 'emacs-time)

;;; emacs-time.el ends here
