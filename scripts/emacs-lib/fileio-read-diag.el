;;; fileio-read-diag.el --- standalone fileio read diagnostic -*- lexical-binding: t; -*-

;;; Code:

(defvar fileio-read-diag-file nil)

(defun fileio-read-diag-run ()
  "Print staged diagnostics for `nelisp-ec-insert-file-contents'."
  (let ((path fileio-read-diag-file))
    (condition-case err
        (progn
          (princ (format "diag path=%S\n" path))
          (princ (format "diag exists=%S\n" (nelisp-ec-file-exists-p path)))
          (let ((raw (nelisp-ec--read-raw-bytes path)))
            (princ (format "diag raw-string=%S raw-len=%S\n"
                           (stringp raw)
                           (and (stringp raw) (length raw))))
            (let* ((decoded (if (fboundp 'nelisp--syscall-read-file)
                                raw
                              (plist-get (nelisp-coding-utf8-decode raw) :string))))
              (princ (format "diag decoded-string=%S decoded-len=%S\n"
                             (stringp decoded)
                             (and (stringp decoded) (length decoded))))
              (let ((buf (nelisp-ec-generate-new-buffer "fileio-read-diag")))
                (nelisp-ec-with-current-buffer buf
                  (nelisp-ec-insert decoded)
                  (princ (format "diag inserted-len=%S\n"
                                 (length (nelisp-ec-buffer-string)))))))))
      (error
       (princ (format "diag error=%S\n" err))
       (signal (car err) (cdr err))))))

(provide 'fileio-read-diag)

;;; fileio-read-diag.el ends here
