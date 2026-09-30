;;; emacs-cc-timefns-1.el --- timefns.c compatibility -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'current-cpu-time)
  (defun current-cpu-time ()
    "Return the current CPU time and its resolution.
This runtime does not expose a process CPU clock; return its zero origin
with microsecond resolution."
    (cons 0 1000000)))

(provide 'emacs-cc-timefns-1)
