;;; nelisp-async.el --- Real async event loop (timers + TTY) for the standalone -*- lexical-binding: t; -*-
;;
;; The bare reader has no asynchronous runtime: the prelude ships a
;; *synchronous* `run-at-time' stub that fires immediately.  This module
;; provides the real thing on top of the standalone's OS primitives:
;;
;;   - a deadline-ordered timer queue (`run-at-time' / `cancel-timer'
;;     become deferred; due timers fire when the loop or `sit-for' runs) --
;;     as of Doc 184 P0, this half lives in `nelisp-async-core.el' (no
;;     actor/generator dependency) and is REQUIRED, not duplicated, here;
;;   - efficient waiting via the `nl-nanosleep' builtin (no busy-loop);
;;   - a cooperative driver, `nelisp-async-run', that fires due timers,
;;     drives the `nelisp-actor' scheduler, and sleeps until the next
;;     deadline, multiplexing TTY input when a tty reader is installed
;;     (see `nelisp-async-tty-*' in the input layer).
;;
;; Loading this file UPGRADES `run-at-time' / `cancel-timer' / `sit-for'
;; from the prelude stubs to the real deferred implementations (via
;; `nelisp-async-core', which this file requires), so any timer-driven
;; code (e.g. `nelisp-eventloop-schedule-timer') runs on a genuine clock.
;; Pure timekeeping uses `float-time' (wall clock); the reader has no
;; `clock_gettime' monotonic helper yet (Doc note), which is adequate for
;; cooperative scheduling.
;;
;; This file itself still `(require 'nelisp-actor)', which unconditionally
;; `(require 'generator)' -- a dependency this substrate cannot satisfy
;; today (Doc 184 S1.4/S1.5: absent outright, or a macro-expansion-time
;; failure even when a real generator.el is supplied).  So `nelisp-async-run'
;; / `-run-tty' (the actor-driven functions below) remain unreachable
;; standalone; only `nelisp-async-core.el''s timer queue is.  A caller that
;; wants deferred timers WITHOUT the actor/TTY layer should
;; `(require 'nelisp-async-core)' directly instead of this file -- that is
;; exactly what `packages/nelisp-process-adapter/src/nelisp-process-adapter.el'
;; (Doc 184 P1-P3) does.
;;
;;; Code:

(require 'cl-lib)
(require 'nelisp-actor)
(require 'nelisp-eventloop)
(require 'nelisp-async-core)
(declare-function alloc-bytes "ext:nelisp-runtime" (nbytes align))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))
(declare-function syscall-direct "ext:nelisp-runtime"
                  (nr a0 a1 a2 a3 a4 a5))

;;; Compatibility aliases for the pre-split names --------------------
;; `nelisp-async-reset-timers' is used by external callers (this package's
;; own test suite resets the queue between cases); the others below are
;; used only within this file, but keeping the alias avoids an
;; incompatible rename for anything else that already spells them this
;; way.  The real implementations now live in `nelisp-async-core.el'
;; (Doc 184 P0); this is a cut, not a fork.

(defalias 'nelisp-async-reset-timers #'nelisp-async-core-reset-timers)

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
      (nelisp-async-core--fire-due (nelisp-async-core--now))
      (nelisp-actor-run-until-idle)
      (when (and main (memq (nelisp-actor-status main) '(:dead :crashed)))
        (throw 'nelisp-async-done (nelisp-actor-status main)))
      (let ((nd (nelisp-async-core--next-deadline)))
        (if (null nd)
            (throw 'nelisp-async-done :idle)
          (let ((gap (- nd (nelisp-async-core--now))))
            (when (> gap 0) (nelisp-async-core--nanosleep gap))))))))

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
        (nelisp-async-core--fire-due (nelisp-async-core--now))
        (nelisp-actor-run-until-idle)
        (when (memq (nelisp-actor-status main) '(:dead :crashed))
          (throw 'nelisp-async-done (nelisp-actor-status main)))
        (let ((nd (nelisp-async-core--next-deadline)))
          (cond
           ;; Live input source: block in poll up to the next deadline.
           (src
            (let* ((timeout-ms (if nd
                                   (let ((ms (truncate
                                              (* 1000.0 (- nd (nelisp-async-core--now))))))
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
            (let ((gap (- nd (nelisp-async-core--now))))
              (when (> gap 0) (nelisp-async-core--nanosleep gap))))
           ;; Nothing to wait for.
           (t (throw 'nelisp-async-done :eof))))))))

(defun nelisp-async-run-tty-raw (main &optional fd)
  "Like `nelisp-async-run-tty' but enter/leave raw mode around the loop."
  (let ((fd (or fd nelisp-async-stdin-fd)))
    (nelisp-async-tty-raw-on fd)
    (unwind-protect
        (nelisp-async-run-tty main fd)
      (nelisp-async-tty-raw-off))))

;;; `run-at-time' / `cancel-timer' / `sit-for' are already upgraded by
;;; `(require 'nelisp-async-core)' above (Doc 184 P0) -- no separate
;;; defalias block needed here.

(provide 'nelisp-async)
;;; nelisp-async.el ends here
