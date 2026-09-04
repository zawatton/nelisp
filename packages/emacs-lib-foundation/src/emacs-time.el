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

(defconst emacs-time--unix-epoch-absolute-day 719163
  "Emacs calendar absolute day for 1970-01-01.")

(defvar emacs-time--current-time-zone-cache nil
  "Cached `(SECONDS NAME)' result for `current-time-zone'.")

(defun emacs-time--host-runtime-p ()
  "Return non-nil when running under regular host Emacs."
  (and (boundp 'emacs-version)
       (stringp emacs-version)))

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

(defun emacs-time--chomp-trailing-newline (text)
  "Return TEXT without one trailing newline."
  (if (and (stringp text)
           (> (length text) 0)
           (= (aref text (1- (length text))) ?\n))
      (substring text 0 (1- (length text)))
    text))

(defun emacs-time--date-output (format)
  "Return `date' output for FORMAT, or nil on failure."
  (when (fboundp 'call-process)
    (with-temp-buffer
      (when (eq 0 (call-process "date" nil t nil format))
        (emacs-time--chomp-trailing-newline (buffer-string))))))

(defun emacs-time--parse-zone-offset (offset)
  "Convert OFFSET in ±HHMM format to seconds, or nil on invalid input."
  (when (and (stringp offset)
             (= (length offset) 5))
    (let* ((sign-char (aref offset 0))
           (sign (cond
                  ((= sign-char ?+) 1)
                  ((= sign-char ?-) -1)
                  (t nil)))
           (h1 (and sign (aref offset 1)))
           (h2 (and sign (aref offset 2)))
           (m1 (and sign (aref offset 3)))
           (m2 (and sign (aref offset 4))))
      (when (and sign
                 (>= h1 ?0) (<= h1 ?9)
                 (>= h2 ?0) (<= h2 ?9)
                 (>= m1 ?0) (<= m1 ?9)
                 (>= m2 ?0) (<= m2 ?9))
        (* sign
           (+ (* (+ (* (- h1 ?0) 10) (- h2 ?0)) 3600)
              (* (+ (* (- m1 ?0) 10) (- m2 ?0)) 60)))))))

(defun emacs-time--fallback-current-time-zone ()
  "Return a standalone `(SECONDS NAME)' timezone tuple."
  (or emacs-time--current-time-zone-cache
      (setq emacs-time--current-time-zone-cache
            (let ((offset (emacs-time--date-output "+%z"))
                  (name (emacs-time--date-output "+%Z")))
              (if offset
                  (let ((seconds (emacs-time--parse-zone-offset offset)))
                    (if seconds
                        (list seconds (or name "UTC"))
                      '(0 "UTC")))
                '(0 "UTC"))))))

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
    (if time-value
        (emacs-time--to-number time-value)
      (or (emacs-time--standalone-unix-time) 0))))

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
    (list (or (emacs-time--standalone-unix-time) (float-time)) 0 0 0)))

(unless (fboundp 'current-time-zone)
  (defun current-time-zone (&optional _time)
    "Return the local timezone as `(SECONDS NAME)'."
    (emacs-time--fallback-current-time-zone)))

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

(defun emacs-time--truncate-seconds (seconds)
  "Truncate SECONDS without recursing through huge standalone floats."
  (cond
   ((integerp seconds) seconds)
   ((and (numberp seconds) (> seconds 1000000))
    (or (emacs-time--standalone-unix-time)
        (truncate seconds)))
   (t (truncate seconds))))

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

(unless (and (fboundp 'time-convert)
             (not (get 'time-convert 'emacs-stub-bulk)))
  (defun time-convert (time form)
    "Convert TIME to FORM.
This standalone polyfill covers the forms used by Org and core code:
`integer', `float', `list', nil/t, and integer tick rates."
    (let* ((seconds (emacs-time--to-number time))
           (whole (emacs-time--truncate-seconds seconds)))
      (cond
       ((eq form 'integer) whole)
       ((eq form 'float) seconds)
       ((or (null form) (eq form t) (eq form 'list))
        (list whole 0 0 0))
       ((integerp form)
        (cons (if (integerp seconds)
                  (* seconds form)
                (emacs-time--truncate-seconds (* seconds form)))
              form))
       (t (list whole 0 0 0)))))
  (put 'time-convert 'emacs-stub-bulk nil))

(defun emacs-time--seconds-and-days (tv)
  "Return (SECS DAYS REM) for time value TV.
SECS is the truncated Unix epoch seconds, DAYS is the whole-day offset from
1970-01-01, and REM is the second offset within that day."
  (let* ((secs (emacs-time--truncate-seconds (emacs-time--to-number tv)))
         (days (if (>= secs 0)
                   (/ secs 86400)
                 (/ (- secs 86399) 86400)))
         (rem (- secs (* days 86400))))
    (list secs days rem)))

(unless (and (emacs-time--host-runtime-p)
             (fboundp 'time-to-days))
  (defun time-to-days (&optional time)
    "Return the absolute day number for TIME.
This follows Emacs calendar absolute-day convention, where 1970-01-01 is
day 719163."
    (+ emacs-time--unix-epoch-absolute-day
       (nth 1 (emacs-time--seconds-and-days time)))))

(unless (and (emacs-time--host-runtime-p)
             (fboundp 'days-to-time))
  (defun days-to-time (days)
    "Return a time value for Emacs absolute day DAYS."
    (list (* (- days emacs-time--unix-epoch-absolute-day) 86400) 0 0 0)))

(unless (fboundp 'seconds-to-time)
  (defun seconds-to-time (seconds)
    "Return a time value for SECONDS."
    (list seconds 0 0 0)))

(unless (boundp 'display-time-mode)
  (defvar display-time-mode nil
    "Headless no-op compatibility toggle for `display-time-mode'."))

(unless (boundp 'emacs-time--time-zone-rule)
  (defvar emacs-time--time-zone-rule nil
    "Stored timezone rule for the standalone time substrate."))

(unless (fboundp 'set-time-zone-rule)
  (defun set-time-zone-rule (rule)
    "Record RULE for the standalone time substrate."
    (setq emacs-time--time-zone-rule rule
          emacs-time--current-time-zone-cache nil)
    rule))

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

(unless (fboundp 'decode-time)
  (defun decode-time (&optional time zone form)
    "Decode TIME into an Emacs-style calendar list.
ZONE and FORM are accepted for API compatibility and ignored.  The return
shape is (SEC MIN HOUR DAY MONTH YEAR DOW DST ZONE) using UTC-like fields."
    (ignore zone form)
    (pcase-let* ((`(,_secs ,days ,rem) (emacs-time--seconds-and-days time))
                 (`(,year ,month ,day) (emacs-time--civil-from-days days))
                 (hour (/ rem 3600))
                 (minute (/ (% rem 3600) 60))
                 (second (% rem 60))
                 (dow (% (+ days 4) 7)))
      (when (< dow 0)
        (setq dow (+ dow 7)))
      (list second minute hour day month year dow nil 0))))

(unless (and (emacs-time--host-runtime-p)
             (fboundp 'encode-time))
  (defun encode-time (&rest args)
    "Encode decoded time ARGS into the standalone time representation."
    (let* ((values (if (and (= (length args) 1) (consp (car args)))
                       (car args)
                     args))
           (sec (or (nth 0 values) 0))
           (min (or (nth 1 values) 0))
           (hour (or (nth 2 values) 0))
           (day (or (nth 3 values) 1))
           (month (or (nth 4 values) 1))
           (year (or (nth 5 values) 1970))
           (absolute (calendar-absolute-from-gregorian (list month day year)))
           (unix-days (- absolute emacs-time--unix-epoch-absolute-day)))
      (list (+ (* unix-days 86400) (* hour 3600) (* min 60) sec) 0 0 0))))

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

(unless (and (fboundp 'time-less-p) (not (get 'time-less-p 'emacs-stub-bulk)))
  (defun time-less-p (t1 t2)
    "Return non-nil if time value T1 is less than time value T2.
Compares the seconds magnitudes via `emacs-time--to-number'; full
picosecond-exact comparison is not modeled."
    (< (emacs-time--to-number t1) (emacs-time--to-number t2)))
  (put 'time-less-p 'emacs-stub-bulk nil))

(unless (fboundp 'time-subtract)
  (defun time-subtract (t1 t2)
    "Return the elapsed seconds of T1 minus T2.
This compatibility runtime uses a numeric seconds representation."
    (- (emacs-time--to-number t1)
       (emacs-time--to-number t2))))

(unless (fboundp 'time-add)
  (defun time-add (t1 t2)
    "Return the sum of time values T1 and T2 as seconds."
    (+ (emacs-time--to-number t1)
       (emacs-time--to-number t2))))

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
