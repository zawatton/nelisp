;;; emacs-cc-profiler-1.el --- profiler primitives -*- lexical-binding: t; -*-

(defvar emacs-cc-profiler-1--cpu-running nil)
(defvar emacs-cc-profiler-1--cpu-log nil)
(defvar emacs-cc-profiler-1--memory-running nil)
(defvar emacs-cc-profiler-1--memory-log nil)

(unless (fboundp 'function-equal)
  (defun function-equal (f1 f2)
    "Return non-nil if F1 and F2 come from the same source.
Used to determine if different closures are just different instances of
the same lambda expression, or are really unrelated function."
    (eq f1 f2)))

(unless (fboundp 'profiler-cpu-log)
  (defun profiler-cpu-log ()
    "Return the current cpu profiler log.
The log is a hash-table mapping backtraces to counters which represent
the amount of time spent at those points.  Every backtrace is a vector
of functions, where the last few elements may be nil.

If the profiler has not run since the last invocation of
`profiler-cpu-log' (or was never run at all), return nil.  If the
profiler is currently running, allocate a new log for future samples
before returning.

(fn)"
    (let ((log emacs-cc-profiler-1--cpu-log))
      (when emacs-cc-profiler-1--cpu-running
        (setq emacs-cc-profiler-1--cpu-log (make-hash-table :test 'equal)))
      log)))

(unless (fboundp 'profiler-cpu-running-p)
  (defun profiler-cpu-running-p ()
    "Return non-nil if cpu profiler is running.

(fn)"
    emacs-cc-profiler-1--cpu-running))

(unless (fboundp 'profiler-cpu-start)
  (defun profiler-cpu-start (sampling-interval)
    "Start or restart the cpu profiler.
It takes call-stack samples each SAMPLING-INTERVAL nanoseconds, approximately.
See also `profiler-log-size' and `profiler-max-stack-depth'.

(fn SAMPLING-INTERVAL)"
    (unless (and (integerp sampling-interval) (> sampling-interval 0))
      (error "Invalid sampling interval"))
    (when emacs-cc-profiler-1--cpu-running
      (error "CPU profiler is already running"))
    (setq emacs-cc-profiler-1--cpu-running t
          emacs-cc-profiler-1--cpu-log (make-hash-table :test 'equal))
    t))

(unless (fboundp 'profiler-cpu-stop)
  (defun profiler-cpu-stop ()
    "Stop the cpu profiler.  The profiler log is not affected.
Return non-nil if the profiler was running.

(fn)"
    (prog1 emacs-cc-profiler-1--cpu-running
      (setq emacs-cc-profiler-1--cpu-running nil))))

(unless (fboundp 'profiler-memory-log)
  (defun profiler-memory-log ()
    "Return the current memory profiler log.
The log is a hash-table mapping backtraces to counters which represent
the amount of memory allocated at those points.  Every backtrace is a vector
of functions, where the last few elements may be nil.

If the profiler has not run since the last invocation of
`profiler-memory-log' (or was never run at all), return nil.  If the
profiler is currently running, allocate a new log for future samples
before returning.

(fn)"
    (let ((log emacs-cc-profiler-1--memory-log))
      (when emacs-cc-profiler-1--memory-running
        (setq emacs-cc-profiler-1--memory-log (make-hash-table :test 'equal)))
      log)))

(unless (fboundp 'profiler-memory-running-p)
  (defun profiler-memory-running-p ()
    "Return non-nil if memory profiler is running.

(fn)"
    emacs-cc-profiler-1--memory-running))

(unless (fboundp 'profiler-memory-start)
  (defun profiler-memory-start ()
    "Start/restart the memory profiler.
The memory profiler will take samples of the call-stack whenever a new
allocation takes place.  Note that most small allocations only trigger
the profiler occasionally.
See also `profiler-log-size' and `profiler-max-stack-depth'.

(fn)"
    (when emacs-cc-profiler-1--memory-running
      (error "Memory profiler is already running"))
    (setq emacs-cc-profiler-1--memory-running t
          emacs-cc-profiler-1--memory-log (make-hash-table :test 'equal))
    t))

(unless (fboundp 'profiler-memory-stop)
  (defun profiler-memory-stop ()
    "Stop the memory profiler.  The profiler log is not affected.
Return non-nil if the profiler was running.

(fn)"
    (prog1 emacs-cc-profiler-1--memory-running
      (setq emacs-cc-profiler-1--memory-running nil))))

(provide 'emacs-cc-profiler-1)
