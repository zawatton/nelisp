;;; files-runtime.el --- runtime predicates for lightweight files shims  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Shared runtime predicates for `files.el' and
;; `files-standalone-buffer.el'.  Keeping this tiny layer below both
;; modules avoids a BUF -> FEAT dependency from the standalone buffer
;; substrate back into the user-facing files facade.

;;; Code:

;;;###autoload
(defun files-standalone-runtime-p ()
  "Return non-nil when the lightweight file facade should install wrappers."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(provide 'files-runtime)

;;; files-runtime.el ends here
