;;; par-simple-vendor.el --- GNU simple.el command parity -*- lexical-binding: t; -*-

(defmacro par-simple-vendor--row (name &rest body)
  `(progn
     (prin1 (cons ',name (condition-case err (progn ,@body)
                           (error (list 'error (car err))))))
     (terpri)))

(par-simple-vendor--row owners
  (mapcar (lambda (name)
            (let ((file (symbol-file name 'defun)))
              (and file (equal (file-name-base file) "simple")
                   (or (not (fboundp 'nemacs-runtime-dump-reinit))
                       (string-match-p "/vendor/emacs-lisp/" file))
                   t)))
          '(beginning-of-buffer end-of-buffer goto-line goto-line-relative
            exchange-point-and-mark transpose-words transpose-lines)))

(par-simple-vendor--row motion
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char 7)
    (let ((start (progn (beginning-of-buffer) (list (point) (mark t))))
          (end (progn (end-of-buffer) (list (point) (mark t)))))
      (list start end))))

(par-simple-vendor--row goto-line-narrowed
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (set-mark 6)
    (narrow-to-region 5 14)
    (let* ((widen-automatically t)
           (relative (goto-line-relative 2))
           (absolute (goto-line 1)))
      (list relative absolute (point-min) (point-max) (mark t)))))

(par-simple-vendor--row exchange-mark
  (with-temp-buffer
    (insert "abc")
    (let ((transient-mark-mode t)
          (exchange-point-and-mark-highlight-region t))
      (set-mark 1)
      (activate-mark)
      (exchange-point-and-mark '(4))
      (list (point) (mark t) (and mark-active t)))))

(par-simple-vendor--row transpose
  (list (with-temp-buffer
          (insert "one two three")
          (goto-char 5)
          (transpose-words 1)
          (list (buffer-string) (point)))
        (with-temp-buffer
          (insert "one\ntwo\nthree\n")
          (goto-char 5)
          (transpose-lines 1)
          (list (split-string (buffer-string) "\n") (point)))))
