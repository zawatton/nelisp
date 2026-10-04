;;; emacs-cc-thread-1.el --- thread.c primitives in Lisp -*- lexical-binding: t; -*-

;; Records expose GNU's type names while keeping the existing slot layout.
;; Accept old vectors too, since a running bundle may already contain them.
(defun emacs-cc-thread-1--object-p (object type tag)
  (or (and (recordp object) (> (length object) 0)
           (eq (aref object 0) type))
      (and (vectorp object) (> (length object) 0)
           (eq (aref object 0) tag))))
(defun emacs-cc-thread-1--type-error (pred object)
  (signal 'wrong-type-argument (list pred object)))
(defun emacs-cc-thread-1--mutex-p (object)
  (or (and (fboundp 'mutexp) (mutexp object))
      (emacs-cc-thread-1--object-p object 'mutex 'emacs-cc-thread-1-mutex)))
(defun emacs-cc-thread-1--condition-p (object)
  (or (and (fboundp 'condition-variable-p) (condition-variable-p object))
      (emacs-cc-thread-1--object-p object 'condition-variable 'emacs-cc-thread-1-condition)))
(defun emacs-cc-thread-1--thread-p (object)
  (or (and (fboundp 'threadp) (threadp object))
      (emacs-cc-thread-1--object-p object 'thread 'emacs-cc-thread-1-thread)))

(unless (fboundp 'all-threads)
  (defun all-threads () "Return a list of all the live threads."
    (cons (current-thread) emacs-cc-thread-1--threads)))
(unless (fboundp 'current-thread)
  (defun current-thread () "Return the current thread."
    (or emacs-cc-thread-1--current-thread
        (setq emacs-cc-thread-1--current-thread
              (record 'thread nil nil)))))
(unless (fboundp 'condition-mutex)
  (defun condition-mutex (cond) "Return the mutex associated with condition variable COND."
    (unless (emacs-cc-thread-1--condition-p cond)
      (emacs-cc-thread-1--type-error 'condition-variable-p cond))
    (aref cond 1)))
(unless (fboundp 'condition-name)
  (defun condition-name (cond) "Return the name of condition variable COND. If no name was given when COND was created, return nil."
    (unless (emacs-cc-thread-1--condition-p cond) (emacs-cc-thread-1--type-error 'condition-variable-p cond))
    (aref cond 2)))
(unless (fboundp 'condition-notify)
  (defun condition-notify (cond &optional all) "Notify COND, a condition variable."
    (ignore all) (unless (emacs-cc-thread-1--condition-p cond) (emacs-cc-thread-1--type-error 'condition-variable-p cond))
    (signal 'error (list "Condition variable’s mutex is not held by current thread"))))
(unless (fboundp 'condition-wait)
  (defun condition-wait (cond) "Wait for the condition variable COND to be notified."
    (unless (emacs-cc-thread-1--condition-p cond) (emacs-cc-thread-1--type-error 'condition-variable-p cond))
    (signal 'error (list "Condition variable’s mutex is not held by current thread"))))
(unless (fboundp 'make-condition-variable)
  (defun make-condition-variable (mutex &optional name) "Make a condition variable associated with MUTEX."
    (unless (emacs-cc-thread-1--mutex-p mutex) (emacs-cc-thread-1--type-error 'mutexp mutex))
    (record 'condition-variable mutex name)))
(unless (fboundp 'make-mutex)
  (defun make-mutex (&optional name) "Create a mutex."
    (record 'mutex name nil)))
(unless (fboundp 'make-thread)
  (defun make-thread (function &optional name buffer-disposition) "Start a new thread and run FUNCTION in it."
    (unless (functionp function) (emacs-cc-thread-1--type-error 'functionp function))
    (when (and name (not (stringp name))) (emacs-cc-thread-1--type-error 'stringp name))
    (let ((thread (record 'thread name buffer-disposition)))
      (funcall function) (push thread emacs-cc-thread-1--threads) thread)))
(unless (fboundp 'mutex-name)
  (defun mutex-name (mutex) "Return the name of MUTEX. If no name was given when MUTEX was created, return nil."
    (unless (emacs-cc-thread-1--mutex-p mutex) (emacs-cc-thread-1--type-error 'mutexp mutex))
    (aref mutex 1)))
(unless (fboundp 'thread--blocker)
  (defun thread--blocker (thread) "Return the object that THREAD is blocking on."
    (unless (emacs-cc-thread-1--thread-p thread) (emacs-cc-thread-1--type-error 'threadp thread)) nil))
(unless (fboundp 'thread-buffer-disposition)
  (defun thread-buffer-disposition (thread) "Return the value of THREAD's buffer disposition."
    (unless (emacs-cc-thread-1--thread-p thread) (emacs-cc-thread-1--type-error 'threadp thread))
    (and (emacs-cc-thread-1--object-p thread 'thread 'emacs-cc-thread-1-thread)
         (aref thread 2))))

(defvar emacs-cc-thread-1--current-thread nil)
(defvar emacs-cc-thread-1--threads nil)
(provide 'emacs-cc-thread-1)
