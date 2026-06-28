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

;;; TTY input layer ---------------------------------------------------
;; Multiplexes keyboard input with the timer queue using poll(2): the driver
;; blocks in `poll' until either a keystroke arrives or the next timer is due,
;; so an interactive app spends no CPU while idle.  All OS contact is through
;; `syscall-direct' (poll=7, read=0, ioctl=16).

(defvar nelisp-async-stdin-fd 0
  "File descriptor polled and read for interactive input.")

(defun nelisp-async--poll-fd (fd timeout-ms)
  "Poll FD for readability up to TIMEOUT-MS (-1 blocks indefinitely).
Return 1 (readable or EOF), 0 (timed out), or -1 (poll error)."
  (let ((pfd (alloc-bytes 8 8)))
    (ptr-write-u64 pfd 0 (+ fd 4294967296))   ; fd | (POLLIN << 32)
    (let ((rc (syscall-direct 7 pfd 1 timeout-ms 0 0 0)))
      (cond ((< rc 0) -1)
            ((= rc 0) 0)
            ;; revents (byte 6): POLLIN(1) | POLLHUP(16).
            ((= (logand (ptr-read-u8 pfd 6) 17) 0) 0)
            (t 1)))))

(defun nelisp-async--read-byte (fd)
  "Read one byte from FD.  Return 0-255, -1 at EOF, or -2 on error/EAGAIN."
  (let* ((buf (alloc-bytes 8 8))
         (n (syscall-direct 0 fd buf 1 0 0 0)))
    (cond ((= n 1) (ptr-read-u8 buf 0))
          ((= n 0) -1)
          (t -2))))

;;; Raw mode (termios via ioctl TCGETS/TCSETS) ------------------------

(defconst nelisp-async--tcgets 21505)   ; 0x5401
(defconst nelisp-async--tcsets 21506)   ; 0x5402

(defvar nelisp-async--saved-termios nil
  "Cons (FD . BUFFER) of the termios saved by `nelisp-async-tty-raw-on'.")

(defun nelisp-async--u32-clear (buf off mask)
  (ptr-write-u32 buf off (logand (ptr-read-u32 buf off) (lognot mask))))

(defun nelisp-async--u32-set (buf off mask)
  (ptr-write-u32 buf off (logior (ptr-read-u32 buf off) mask)))

(defun nelisp-async-tty-raw-on (&optional fd)
  "Put FD (default stdin) into raw mode: no echo, no canonical line buffering.
Saves the previous termios (restore with `nelisp-async-tty-raw-off').
Return t on success, nil if the TCGETS/TCSETS ioctl failed (e.g. not a tty)."
  (let* ((fd (or fd nelisp-async-stdin-fd))
         (buf (alloc-bytes 64 8)))
    (if (< (syscall-direct 16 fd nelisp-async--tcgets buf 0 0 0) 0)
        nil
      (let ((saved (alloc-bytes 64 8)) (i 0))
        (while (< i 64)
          (ptr-write-u8 saved i (ptr-read-u8 buf i))
          (setq i (1+ i)))
        (setq nelisp-async--saved-termios (cons fd saved)))
      ;; cfmakeraw on the kernel `struct termios':
      (nelisp-async--u32-clear buf 0 1515)    ; c_iflag: BRKINT ICRNL INPCK ISTRIP IXON ...
      (nelisp-async--u32-clear buf 4 1)       ; c_oflag: OPOST
      (nelisp-async--u32-clear buf 12 32843)  ; c_lflag: ECHO ECHONL ICANON ISIG IEXTEN
      (nelisp-async--u32-clear buf 8 304)     ; c_cflag: CSIZE PARENB
      (nelisp-async--u32-set buf 8 48)        ; c_cflag |= CS8
      (ptr-write-u8 buf 23 1)                 ; c_cc[VMIN]  = 1
      (ptr-write-u8 buf 22 0)                 ; c_cc[VTIME] = 0
      (>= (syscall-direct 16 fd nelisp-async--tcsets buf 0 0 0) 0))))

(defun nelisp-async-tty-raw-off ()
  "Restore the termios saved by the last `nelisp-async-tty-raw-on'."
  (when nelisp-async--saved-termios
    (let ((fd (car nelisp-async--saved-termios))
          (buf (cdr nelisp-async--saved-termios)))
      (syscall-direct 16 fd nelisp-async--tcsets buf 0 0 0)
      (setq nelisp-async--saved-termios nil)
      t)))

;;; Interactive driver: timers + TTY input ---------------------------

(defun nelisp-async-run-tty (main &optional fd)
  "Async loop multiplexing timers and TTY input on FD (default stdin).
Each input byte is delivered to MAIN as a `key' event; due timers fire;
the loop blocks in `poll(2)' until the next deadline or a keystroke, so it
is idle-CPU-free.  Stops with MAIN's terminal status (`:dead'/`:crashed'),
`:eof' (input closed with no timers left), or `:idle'."
  (let ((src (or fd nelisp-async-stdin-fd)))
    (catch 'nelisp-async-done
      (while t
        (nelisp-async--fire-due (nelisp-async--now))
        (nelisp-actor-run-until-idle)
        (when (memq (nelisp-actor-status main) '(:dead :crashed))
          (throw 'nelisp-async-done (nelisp-actor-status main)))
        (let ((nd (nelisp-async--next-deadline)))
          (cond
           ;; Live input source: block in poll up to the next deadline.
           (src
            (let* ((timeout-ms (if nd
                                   (let ((ms (truncate
                                              (* 1000.0 (- nd (nelisp-async--now))))))
                                     (if (< ms 0) 0 ms))
                                 -1))
                   (ready (nelisp-async--poll-fd src timeout-ms)))
              (when (= ready 1)
                (let ((b (nelisp-async--read-byte src)))
                  (cond
                   ((>= b 0)
                    (nelisp-send main (nelisp-make-event 'key b)))
                   ((= b -1)
                    ;; EOF: stop polling this source; fall through to timers.
                    (setq src nil)))))))
           ;; No input, but timers armed: sleep to the next deadline.
           (nd
            (let ((gap (- nd (nelisp-async--now))))
              (when (> gap 0) (nelisp-async--nanosleep gap))))
           ;; Nothing to wait for.
           (t (throw 'nelisp-async-done :eof))))))))

(defun nelisp-async-run-tty-raw (main &optional fd)
  "Like `nelisp-async-run-tty' but enter/leave raw mode around the loop."
  (let ((fd (or fd nelisp-async-stdin-fd)))
    (nelisp-async-tty-raw-on fd)
    (unwind-protect
        (nelisp-async-run-tty main fd)
      (nelisp-async-tty-raw-off))))

;;; Upgrade the prelude stubs to the real deferred implementations -----

(defalias 'run-at-time #'nelisp-async-run-at-time)
(defalias 'cancel-timer #'nelisp-async-cancel-timer)
(defalias 'sit-for #'nelisp-async-sit-for)

(provide 'nelisp-async)
;;; nelisp-async.el ends here
