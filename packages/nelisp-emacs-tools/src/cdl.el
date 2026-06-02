;;; cdl.el --- lightweight Common Data Language helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Standard-name facade for Emacs' small cdl.el helper.  The actual OS
;; bridge is the existing pure-Elisp `emacs-process-builtins' layer, so
;; this file only preserves the public command surface and argument shape.

;;; Code:

(require 'emacs-process-builtins)

(defun cdl-get-file (filename)
  "Run FILENAME through ncdump and insert the result at point."
  (interactive "fCDF file: ")
  (message "ncdump in progress...")
  (let ((start (point)))
    (call-process "ncdump" nil t nil (expand-file-name filename))
    (goto-char start))
  (message "ncdump in progress...done"))

(defun cdl-put-region (filename start end)
  "Run region START..END through ncgen and write FILENAME."
  (interactive "FNew CDF file: \nr")
  (message "ncgen in progress...")
  (call-process-region start end "ncgen"
                       nil nil nil "-o" (expand-file-name filename))
  (message "ncgen in progress...done"))

(provide 'cdl)

;;; cdl.el ends here
