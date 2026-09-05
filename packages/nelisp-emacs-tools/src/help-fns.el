;;; help-fns.el --- Lightweight help-fns shim for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Re-export the daily-driver describe-* implementation as the standard
;; `help-fns' feature without loading GNU Emacs's much larger help-fns.el.

;;; Code:

(defvar help-fns--standalone-p
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version)))
  "Non-nil under standalone NeLisp.
The NeLisp reader binds `emacs-version' just like host Emacs, so a bare
`(not (boundp 'emacs-version))' test misfires there.  Detect the
standalone path by a NeLisp-only primitive, matching
`help-mode--standalone-p' in `help-mode.el'.")

(defun help-fns--host-load-standard ()
  "Load host Emacs's standard help-fns library."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory (or (and (boundp 'load-file-name) load-file-name)
                         (and (boundp 'buffer-file-name) buffer-file-name)
                         default-directory)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "help-fns" nil t))))

(if help-fns--standalone-p
    (require 'emacs-help)
  (help-fns--host-load-standard))

(provide 'help-fns)

;;; help-fns.el ends here
