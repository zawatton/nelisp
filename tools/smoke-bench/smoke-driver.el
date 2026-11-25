;;; smoke-driver.el --- Checked timing rows for the smoke substrate -*- lexical-binding: t; -*-

(require 'replace)
(load (expand-file-name "textmodes/text-mode.elc"
                        (or (getenv "NELISP_GNU_LISP")
                            "/usr/local/share/emacs/31.1/lisp"))
      nil t t)

(defvar sf-bench-bytes (unibyte-string 31 139 8 0 10 255 128 0))

(defun sf-bench-text-mode (n)
  (with-temp-buffer
    (dotimes (_ n) (text-mode))
    major-mode))

(defun sf-bench-skip (n set)
  (with-temp-buffer
    (insert "one two three")
    (let ((result 0))
      (dotimes (_ n)
        (goto-char (point-min))
        (setq result (+ result (skip-chars-forward set))))
      result)))

(defun sf-bench-query (n)
  (with-temp-buffer
    (let ((text "x12 x34")
          (inhibit-message t)
          (message-log-max nil))
      (dotimes (_ n)
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (let ((unread-command-events '(?!)))
          (query-replace-regexp "x\\([0-9]+\\)" "id-\\1")))
      (buffer-string))))

(defun sf-bench-print (n)
  (let ((length 0))
    (dotimes (_ n)
      (setq length (+ length (length (prin1-to-string sf-bench-bytes)))))
    length))

(defun sf-bench-message-log (n)
  (with-temp-buffer
    (let ((inhibit-message t)
          (message-log-max 1000)
          (messages-buffer-name (buffer-name)))
      (insert (mapconcat #'identity (make-list message-log-max "Old entry\n") ""))
      (dotimes (index n)
        (message "Message %06d" index))
      (goto-char (point-max))
      (forward-line -1)
      (list (count-lines (point-min) (point-max))
            (buffer-substring-no-properties (point) (line-end-position))))))

(defun es-run (name n)
  (let ((start (float-time)))
    (condition-case fault
        (let ((result (pcase name
                        ('text-mode (sf-bench-text-mode n))
                        ('skip-literal (sf-bench-skip n "a-z"))
                        ('skip-word-class (sf-bench-skip n "[:word:]"))
                        ('query-replace-regexp (sf-bench-query n))
                        ('binary-print (sf-bench-print n))
                        ('message-log-trim (sf-bench-message-log n))
                        (_ (error "Unknown benchmark row %S" name)))))
          (princ (format "ES %s %d %.9f %S\n" name n (- (float-time) start) result)))
      (error
       (princ (format "ES %s %d %.9f ERROR:%S\n" name n (- (float-time) start) fault))))))

;;; smoke-driver.el ends here
