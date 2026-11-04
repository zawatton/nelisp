;;; emacs-cc-sysdep-1.el --- sysdep.c primitives -*- lexical-binding: t; -*-

(unless (fboundp 'get-internal-run-time)
  (defun get-internal-run-time ()
    "Return the current run time used by Emacs.
The time is returned as in the style of `current-time'.

On systems that can't determine the run time, `get-internal-run-time'
does the same thing as `current-time'."
    (current-time)))

(provide 'emacs-cc-sysdep-1)
