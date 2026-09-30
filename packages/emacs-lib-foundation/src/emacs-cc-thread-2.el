;;; emacs-cc-thread-2.el --- thread C primitive replacements -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-thread-2--check-thread (thread)
  "Signal GNU-compatible type error unless THREAD is a thread."
  (unless (and (fboundp 'threadp) (threadp thread))
    (signal 'wrong-type-argument (list 'threadp thread)))
  thread)

(unless (fboundp 'thread-last-error)
  (defun thread-last-error (&optional cleanup)
    "Return the last error form recorded by a dying thread.
If CLEANUP is non-nil, remove this error form from history."
    ;; GNU's batch build has no recorded error form until another thread dies.
    (when cleanup nil)))

(unless (fboundp 'thread-live-p)
  (defun thread-live-p (thread)
    "Return t if THREAD is alive, or nil if it has exited."
    (emacs-cc-thread-2--check-thread thread)
    ;; Runtimes without native thread handles have no live thread objects.
    nil))

(unless (fboundp 'thread-name)
  (defun thread-name (thread)
    "Return the name of the THREAD.
The name is the same object that was passed to `make-thread'."
    (emacs-cc-thread-2--check-thread thread)
    nil))

(unless (fboundp 'thread-set-buffer-disposition)
  (defun thread-set-buffer-disposition (thread value)
    "Set THREAD's buffer disposition.
See `make-thread' for the description of possible values.

Buffer disposition of the main thread cannot be modified."
    (emacs-cc-thread-2--check-thread thread)
    (unless (null value)
      (signal 'wrong-type-argument (list 'null value)))
    nil))

(unless (fboundp 'thread-signal)
  (defun thread-signal (thread error-symbol data)
    "Signal an error in a thread.
This acts like `signal', but arranges for the signal to be raised
in THREAD.  If THREAD is the current thread, acts just like `signal'.
This will interrupt a blocked call to `mutex-lock', `condition-wait',
or `thread-join' in the target thread.
If THREAD is the main thread, just the error message is shown."
    (emacs-cc-thread-2--check-thread thread)
    (unless (symbolp error-symbol)
      (signal 'wrong-type-argument (list 'symbolp error-symbol)))
    ;; In a threadless batch runtime, the only representable destination is
    ;; the current execution context, where GNU delegates to `signal'.
    (signal error-symbol data)))

(unless (fboundp 'thread-yield)
  (defun thread-yield ()
    "Yield the CPU to another thread."
    (let ((yielded nil)) yielded)))

(provide 'emacs-cc-thread-2)

;;; emacs-cc-thread-2.el ends here
