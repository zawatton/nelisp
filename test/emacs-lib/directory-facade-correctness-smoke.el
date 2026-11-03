;;; -*- lexical-binding: t; -*-
(when (fboundp 'emacs-callproc-populate-process-environment)
  (setq process-environment nil)
  (emacs-callproc-populate-process-environment))
(let* ((root (getenv "K1_DIRECTORY_CASE_ROOT"))
       (path (concat root "/leaf"))
       (old-facade (symbol-function 'make-directory))
       (facade-calls 0))
  (unwind-protect
      (progn
        ;; Model GNU files.el's public facade.  Its internal call must keep
        ;; reaching the original creation leaf, even after this retargeting.
        (fset 'make-directory
              (lambda (directory &optional parents)
                (ignore parents)
                (setq facade-calls (1+ facade-calls))
                (when (> facade-calls 3) (error "recursive public facade"))
                (make-directory-internal directory)))
        (let ((result (condition-case err (make-directory-internal path)
                        (error (car err)))))
          (princ (format "K1-DIRECTORY|%S|exists=%S|facade-calls=%d|\n"
                         result (file-directory-p path) facade-calls))))
    (fset 'make-directory old-facade)))
(princ "K1-DIRECTORY-DONE\n")
