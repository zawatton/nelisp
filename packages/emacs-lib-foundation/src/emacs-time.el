;;; emacs-time.el --- Time + truncate polyfills for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 10 — extracted from `emacs-stub.el' (= the Phase 6
;; write-path polyfill).  Wraps the build-tool builtins
;; `nl-current-unix-time' when present, otherwise the NeLisp
;; `nelisp--syscall' bridge for Linux `time(2)'.
;;
;; Real Emacs `current-time' returns a HIGH/LOW/MICRO list — anvil
;; callsites only pull `(truncate (float-time))' so we expose that
;; path directly without bothering with the legacy list shape.
;;
;; `truncate' is included here because its bulk-stub no-op (emitted
;; by `emacs-stub-bulk.el') was not real-integer-correct; this file's
;; version replaces it when the bulk stub fired first.
;;
;; Each definition is gated on the appropriate `unless (fboundp ...)'
;; or live-replace check.  Loading under host Emacs is a cheap no-op.

;;; Code:

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

;; Live-replace gate — same pattern as `truncate' below.  We only
;; override `float-time' / `current-time' when the host's binding is
;; missing or is the no-op bulk stub (`emacs-stub-bulk.el' returns nil).
;; Under regular Emacs the host's correct implementations are kept
;; intact so `accept-process-output' and other timing-sensitive code
;; paths continue to work during ERT runs.

(unless (and (fboundp 'float-time)
             (let ((ft (ignore-errors (float-time))))
               (and (numberp ft) (> ft 0))))
  (defun float-time (&optional time-value)
    "Return seconds since the Unix epoch.
TIME-VALUE is accepted for API compatibility but only a nil value
is supported (= read current time)."
    ;; Keep the argument for API compatibility.  Do not call `ignore'
    ;; here: raw NeLisp does not define it before `emacs-stub' loads.
    (if time-value nil nil)
    (or (emacs-time--standalone-unix-time) 0)))

(unless (and (fboundp 'current-time)
             (let ((ct (ignore-errors (current-time))))
               (and (consp ct)
                    (or (numberp (car ct))
                        (and (consp (cdr ct)) (numberp (car ct)))))))
  (defun current-time ()
    "Return current time as (HIGH LOW USEC PSEC) — Phase 6 simplified
shape that returns (T 0 0 0) where T is the Unix epoch as a single
integer.  anvil-memory only ever feeds this back into `truncate' /
`float-time' so the legacy 3-cell shape is unnecessary here."
    (list (float-time) 0 0 0)))

(unless (and (fboundp 'truncate)
             ;; If truncate is the no-op bulk stub, override with real impl.
             (not (get 'truncate 'emacs-stub-bulk)))
  (defun truncate (number &optional divisor)
    "Phase 10 (= ex-Phase 6) polyfill: integer truncation toward zero.
NUMBER may be int or float; DIVISOR optional (= NUMBER / DIVISOR)."
    (cond
     ((null number) 0)
     (divisor
      (truncate (/ number divisor)))
     ((integerp number) number)
     ((< number 0)
      (- (truncate (- number))))
     ((>= number 1)
      ;; Avoid float literals and `while' in this early bootstrap body:
      ;; standalone-reader currently segfaults while installing that shape.
      (+ 1 (truncate (- number 1))))
     (t 0)))
  (put 'truncate 'emacs-stub-bulk nil))

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
      (+ (* high 65536.0) low
         (/ usec 1000000.0)
         (/ psec 1000000000000.0))))
   (t 0)))

(unless (and (fboundp 'time-less-p) (not (get 'time-less-p 'emacs-stub-bulk)))
  (defun time-less-p (t1 t2)
    "Return non-nil if time value T1 is less than time value T2.
Compares the seconds magnitudes via `emacs-time--to-number'; full
picosecond-exact comparison is not modeled."
    (< (emacs-time--to-number t1) (emacs-time--to-number t2)))
  (put 'time-less-p 'emacs-stub-bulk nil))

;;; Doc 06 B2: timers (run-with-timer / run-with-idle-timer / cancel-timer).
;; Minimal pure-Elisp implementation (no cl-defstruct).  A timer is a vector
;;   [emacs-timer TRIGGER REPEAT FN ARGS IDLE-DELAY FIRED-P].
;; Firing is driven by `emacs-timer-run-pending' / `emacs-timer-run-idle',
;; which the runtime event loop calls each tick.

(unless (boundp 'timer-list) (defvar timer-list nil
  "List of active (non-idle) timers."))
(unless (boundp 'timer-idle-list) (defvar timer-idle-list nil
  "List of active idle timers."))

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

(defun emacs-timer-run-pending (&optional now)
  "Fire due regular timers (TRIGGER <= NOW); reschedule repeating ones.
Return the number fired."
  (let ((now (or now (emacs-timer--now))) (fired 0))
    (dolist (tm (copy-sequence timer-list))
      (when (and (aref tm 1) (<= (aref tm 1) now))
        (setq fired (1+ fired))
        (condition-case _ (apply (aref tm 3) (aref tm 4)) (error nil))
        (if (aref tm 2)
            (aset tm 1 (+ now (aref tm 2)))
          (setq timer-list (delq tm timer-list)))))
    fired))

(defun emacs-timer-run-idle (idle-seconds)
  "Fire idle timers whose delay <= IDLE-SECONDS and not already fired this
idle period.  Return the number fired."
  (let ((fired 0))
    (dolist (tm (copy-sequence timer-idle-list))
      (when (and (not (aref tm 6)) (<= (aref tm 5) idle-seconds))
        (aset tm 6 t)
        (setq fired (1+ fired))
        (condition-case _ (apply (aref tm 3) (aref tm 4)) (error nil))
        (unless (aref tm 2)
          (setq timer-idle-list (delq tm timer-idle-list)))))
    fired))

(defun emacs-timer-reset-idle ()
  "Clear the per-idle-period fired flag (call when input resets idle time)."
  (dolist (tm timer-idle-list) (aset tm 6 nil)))

(unless (and (fboundp 'timerp)
             (not (get 'timerp 'emacs-stub-bulk)))
  (defun timerp (obj) (emacs-timer-p obj))
  (put 'timerp 'emacs-stub-bulk nil))
(unless (and (fboundp 'run-with-timer)
             (not (get 'run-with-timer 'emacs-stub-bulk)))
  (defun run-with-timer (secs repeat fn &rest args)
    (apply #'emacs-timer-run-with-timer secs repeat fn args))
  (put 'run-with-timer 'emacs-stub-bulk nil))
(unless (and (fboundp 'run-at-time)
             (not (get 'run-at-time 'emacs-stub-bulk)))
  (defun run-at-time (time repeat fn &rest args)
    "MVP: TIME is treated as a number of seconds (or nil = now); string time
specifications are not parsed."
    (apply #'emacs-timer-run-with-timer (if (numberp time) time 0) repeat fn args))
  (put 'run-at-time 'emacs-stub-bulk nil))
(unless (and (fboundp 'run-with-idle-timer)
             (not (get 'run-with-idle-timer 'emacs-stub-bulk)))
  (defun run-with-idle-timer (secs repeat fn &rest args)
    (apply #'emacs-timer-run-with-idle-timer secs repeat fn args))
  (put 'run-with-idle-timer 'emacs-stub-bulk nil))
(unless (and (fboundp 'cancel-timer)
             (not (get 'cancel-timer 'emacs-stub-bulk)))
  (defun cancel-timer (timer) (emacs-timer-cancel timer))
  (put 'cancel-timer 'emacs-stub-bulk nil))

(provide 'emacs-time)

;;; emacs-time.el ends here
