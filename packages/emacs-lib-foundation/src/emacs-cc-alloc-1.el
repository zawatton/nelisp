;;; emacs-cc-alloc-1.el --- Allocation primitives -*- lexical-binding: t; -*-

(defun emacs-cc-alloc-1--memory-info-local ()
  "Return Linux memory information in kilobytes, or nil if unavailable."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents "/proc/meminfo")
        (let ((total (and (re-search-forward "^MemTotal:[ \t]+\\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1))))
              free swap-total swap-free)
          (goto-char (point-min))
          (when (re-search-forward "^MemAvailable:[ \t]+\\([0-9]+\\)" nil t)
            (setq free (string-to-number (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^SwapTotal:[ \t]+\\([0-9]+\\)" nil t)
            (setq swap-total (string-to-number (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^SwapFree:[ \t]+\\([0-9]+\\)" nil t)
            (setq swap-free (string-to-number (match-string 1))))
          (when (and total free swap-total swap-free)
            (list total free swap-total swap-free))))
    (error nil)))

(unless (fboundp 'garbage-collect-heapsize)
  (defun garbage-collect-heapsize ()
    "Return a list with info on amount of space in use."
    (garbage-collect)
    (list '(conses 16 0 0) '(symbols 48 0 0) '(strings 32 0 0)
          '(string-bytes 1 0) '(vectors 16 0) '(vector-slots 8 0 0)
          '(floats 8 0 0) '(intervals 56 0 0) '(buffers 1064 0))))

(unless (fboundp 'garbage-collect-maybe)
  (defun garbage-collect-maybe (factor)
    "Call `garbage-collect' if enough allocation happened."
    (unless (and (integerp factor) (>= factor 0))
      (signal 'wrong-type-argument (list 'wholenump factor)))
    nil))

(unless (fboundp 'make-finalizer)
  (defun make-finalizer (function)
    "Make a finalizer that will run FUNCTION."
    (unless (functionp function)
      (signal 'wrong-type-argument (list 'functionp function)))
    (record 'finalizer function)))

(unless (fboundp 'malloc-info)
  (defun malloc-info ()
    "Report malloc information to stderr."
    (when (fboundp 'external-debugging-output)
      (princ "" external-debugging-output))
    nil))

(unless (fboundp 'malloc-trim)
  (defun malloc-trim (&optional leave-padding)
    "Release free heap memory to the OS."
    (unless (and (integerp (or leave-padding 0)) (>= (or leave-padding 0) 0))
      (signal 'wrong-type-argument (list 'wholenump leave-padding)))
    t))

(unless (fboundp 'memory-info)
  (defun memory-info ()
    "Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP)."
    (emacs-cc-alloc-1--memory-info-local)))

(provide 'emacs-cc-alloc-1)
