;;; isearch.el --- Lightweight isearch shim for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defvar isearch--standalone-p (not (boundp 'emacs-version)))

(defun isearch--host-load-standard ()
  "Load host Emacs's standard isearch library."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "isearch" nil t))))

(if isearch--standalone-p
    (progn
      (require 'emacs-isearch)
      (unless (fboundp 'isearch-forward-regexp)
        (defun isearch-forward-regexp (&optional no-recursive-edit)
          "Run the lightweight forward search in regexp-compatible entry form."
          (interactive)
          (isearch-forward t no-recursive-edit))))
  (isearch--host-load-standard))

(provide 'isearch)

;;; isearch.el ends here
