;;; pp.el --- Lightweight pp shim for standalone NeLisp  -*- lexical-binding: t; -*-

;;; Code:

(defun pp--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defvar pp--standalone-p (pp--standalone-runtime-p))

(defun pp--host-load-standard ()
  "Load host Emacs's standard pp library."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory
                     (or (and (boundp 'load-file-name) load-file-name)
                         (and (boundp 'buffer-file-name) buffer-file-name))))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "pp" nil t))))

(if pp--standalone-p
    (progn
      (unless (fboundp 'pp)
        (defun pp (object &optional _stream)
          (prin1-to-string object)))
      (unless (fboundp 'pp-to-string)
        (defun pp-to-string (object)
          (prin1-to-string object))))
  (pp--host-load-standard))

(provide 'pp)

;;; pp.el ends here
