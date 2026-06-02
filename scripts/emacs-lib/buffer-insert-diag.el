;;; buffer-insert-diag.el --- standalone buffer insert diagnostic -*- lexical-binding: t; -*-

;;; Code:

(defvar buffer-insert-diag-file nil)

(defun buffer-insert-diag--sample (text length)
  "Return at most LENGTH chars from TEXT."
  (substring text 0 (min length (length text))))

(defun buffer-insert-diag--case (label text)
  "Run one insertion diagnostic for LABEL and TEXT."
  (princ (format "insert-diag case=%S len=%S status=start\n"
                 label (length text)))
  (princ (format "insert-diag case=%S step=string-empty-p value=%S\n"
                 label (string-empty-p text)))
  (princ (format "insert-diag case=%S step=make-text-buffer\n" label))
  (let ((tb (make-text-buffer)))
    (princ (format "insert-diag case=%S step=text-buffer-set-cursor-call\n" label))
    (text-buffer-set-cursor tb 0)
    (princ (format "insert-diag case=%S step=text-buffer-set-cursor-done\n" label))
    (princ (format "insert-diag case=%S step=text-buffer-insert-call\n" label))
    (text-buffer-insert tb text)
    (princ (format "insert-diag case=%S step=text-buffer-insert-done length=%S\n"
                   label (text-buffer-length tb))))
  (let ((buf (nelisp-ec-generate-new-buffer (format "insert-diag-%S" label))))
    (nelisp-ec-with-current-buffer buf
      (princ (format "insert-diag case=%S step=insert-call\n" label))
      (nelisp-ec-insert text)
      (princ (format "insert-diag case=%S status=done size=%S text-len=%S\n"
                     label
                     (nelisp-ec-buffer-size)
                     (length (nelisp-ec-buffer-string)))))))

(defun buffer-insert-diag-run ()
  "Print staged diagnostics for `nelisp-ec-insert'."
  (condition-case err
      (let* ((path buffer-insert-diag-file)
             (file-text (and path (nelisp--syscall-read-file path))))
        (princ (format "insert-diag path=%S file-len=%S\n"
                       path
                       (and (stringp file-text) (length file-text))))
        (buffer-insert-diag--case 'tiny "abc")
        (when (stringp file-text)
          (buffer-insert-diag--case 'file-10 (buffer-insert-diag--sample file-text 10))
          (buffer-insert-diag--case 'file-100 (buffer-insert-diag--sample file-text 100))
          (buffer-insert-diag--case 'file-1000 (buffer-insert-diag--sample file-text 1000))
          (buffer-insert-diag--case 'file-all file-text)))
    (error
     (princ (format "insert-diag error=%S\n" err))
     (signal (car err) (cdr err)))))

(provide 'buffer-insert-diag)

;;; buffer-insert-diag.el ends here
