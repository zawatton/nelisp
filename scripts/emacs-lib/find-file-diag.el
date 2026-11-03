;;; find-file-diag.el --- standalone find-file diagnostic -*- lexical-binding: t; -*-

;;; Code:

(defvar find-file-diag-file nil)

(defun find-file-diag-run ()
  "Print staged diagnostics for `find-file-noselect'."
  (condition-case err
      (let* ((path find-file-diag-file)
             (abs nil)
             (buf nil))
        (princ (format "find-file-diag path=%S\n" path))
        (setq abs (nelisp-ec-expand-file-name path))
        (princ (format "find-file-diag abs=%S exists=%S\n"
                       abs (nelisp-ec-file-exists-p abs)))
        (princ "find-file-diag clean-killed\n")
        (emacs-fileio--clean-killed)
        (let ((bname (nelisp-ec-file-name-nondirectory abs)))
          (princ (format "find-file-diag bname=%S\n" bname))
          (princ "find-file-diag generate-buffer\n")
          (setq buf (nelisp-ec-generate-new-buffer
                     (if (and bname (> (length bname) 0))
                         bname
                       " *find-file*"))))
        (princ "find-file-diag with-current-buffer\n")
        (nelisp-ec-with-current-buffer buf
          (princ "find-file-diag insert-file\n")
          (when (nelisp-ec-file-exists-p abs)
            (nelisp-ec-insert-file-contents abs))
          (princ "find-file-diag record-visit\n")
          (setq emacs-fileio--buffer-files
                (cons (cons buf abs)
                      (assq-delete-all buf emacs-fileio--buffer-files))))
        (princ "find-file-diag manual-find-done\n")
        (princ (format "find-file-diag bufferp=%S\n" (bufferp buf)))
        (princ (format "find-file-diag size=%S\n" (buffer-size buf)))
        (princ (format "find-file-diag name=%S\n" (buffer-file-name buf))))
    (error
     (princ (format "find-file-diag error=%S\n" err))
     (signal (car err) (cdr err)))))

(provide 'find-file-diag)

;;; find-file-diag.el ends here
