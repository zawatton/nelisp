;;; phase-events.el --- startup phase event recorder (Doc 235)  -*- lexical-binding: t; -*-

;; Records named timestamps into memory and writes them as ONE s-expression
;; at `nelisp-phase-flush'.  Everything is a no-op unless the environment
;; variable NELISP_PHASE_EVENTS names an output file; the variable is read
;; once at load time so that the disabled path allocates nothing.
;;
;; Event layout: (NAME NS DETAIL).  NS is an integer.  When
;; `nelisp--monotonic-ns' is available it is CLOCK_MONOTONIC nanoseconds
;; (the same clock domain as the runtime boot record and Python's
;; time.monotonic_ns on Linux).  Otherwise it is wall-clock nanoseconds and
;; the flushed record carries `:clock wall'; the validator rejects those.

;;; Code:

(declare-function nelisp--monotonic-ns "ext:nelisp-runtime" ())
(declare-function nelisp--boot-phase-ns "ext:nelisp-runtime" ())

(defvar nelisp-phase--file (getenv "NELISP_PHASE_EVENTS")
  "Output file for phase events, or nil when recording is disabled.")

(defvar nelisp-phase--events nil
  "Reverse-chronological list of recorded (NAME NS DETAIL) events.")

(defvar nelisp-phase--clock
  (if (fboundp 'nelisp--monotonic-ns) 'monotonic 'wall)
  "Clock kind used for every event of this process.")

(defun nelisp-phase-now ()
  "Return the current clock reading in integer nanoseconds, or nil if disabled.
Performs no consing beyond the integer itself; safe inside a timed window."
  (when nelisp-phase--file
    (if (eq nelisp-phase--clock 'monotonic)
        (nelisp--monotonic-ns)
      (truncate (* 1e9 (float-time))))))

(defun nelisp-phase-event-at (name ns &optional detail)
  "Record event NAME with the already-read timestamp NS (and DETAIL)."
  (when (and nelisp-phase--file ns)
    (setq nelisp-phase--events
          (cons (list name ns detail) nelisp-phase--events))))

(defun nelisp-phase-event (name &optional detail)
  "Record event NAME now.  DETAIL is an optional small datum."
  (when nelisp-phase--file
    (nelisp-phase-event-at name (nelisp-phase-now) detail)))

(defun nelisp-phase-flush ()
  "Append all recorded events as one readable s-expression to the output file.
Includes the runtime boot record, pid, run id, image identity and driver
path.  Writes without a message and clears the in-memory list."
  (when nelisp-phase--file
    (let ((record
           (list :run-id (getenv "NELISP_PHASE_RUN_ID")
                 :clock nelisp-phase--clock
                 :boot (and (fboundp 'nelisp--boot-phase-ns)
                            (nelisp--boot-phase-ns))
                 :pid (and (fboundp 'emacs-pid) (emacs-pid))
                 :image-sha256 (getenv "NELISP_PRODUCER_IMAGE_SHA256")
                 :driver (or (getenv "NELISP_PHASE_DRIVER")
                             (and (boundp 'load-file-name) load-file-name))
                 :warmups (getenv "BENCH_WARMUPS")
                 :events (reverse nelisp-phase--events))))
      (write-region (concat (prin1-to-string record) "\n") nil
                    nelisp-phase--file t 'silent)
      (setq nelisp-phase--events nil))))

(provide 'phase-events)
;;; phase-events.el ends here
