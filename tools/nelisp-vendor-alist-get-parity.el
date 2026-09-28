;;; nelisp-vendor-alist-get-parity.el --- alist-get vendor parity -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-corpus-parity)

(defconst nelisp-vendor-alist-get-parity-cases
  '((default-eq
     (let ((key (copy-sequence "k"))
           (alist (list (cons "k" 7))))
       (list (alist-get key alist 'missing)
             (alist-get key alist 'missing nil #'equal))))
    (setf-mutation-alias
     (let* ((cell (cons 'key 1))
            (alist (list cell))
            (alias alist))
       (setf (alist-get 'key alist) 2)
       (list (alist-get 'key alias) cell (eq alist alias))))
    (setf-new-entry
     (let ((alist '((a . 1))))
       (setf (alist-get 'b alist) 2)
       (list (alist-get 'b alist) alist))))
  "Host differential cases for GNU `alist-get' and its supported setf place.")

(defun nelisp-vendor-alist-get-parity-run ()
  "Compare the focused alist-get cases between Host Emacs and standalone."
  (interactive)
  (let* ((root (nelisp-bytecode-corpus--root))
         (default-directory root)
         (output (expand-file-name "target/vendor-alist-get-parity/" root))
         (host (or (getenv "NELISP_EMACS")
                   (expand-file-name invocation-name invocation-directory)))
         (standalone (expand-file-name (or (getenv "NELISP_BIN")
                                           "target/nelisp") root))
         (passed 0)
         failures)
    (make-directory output t)
    (dolist (case nelisp-vendor-alist-get-parity-cases)
      (let* ((name (car case))
             (form (cadr case))
             (printed (nelisp-bytecode-corpus--print form))
             (stem (expand-file-name (symbol-name name) output))
             (host-expression (concat stem ".host.el"))
             (host-out (concat stem ".host.out"))
             (host-err (concat stem ".host.err"))
             (standalone-out (concat stem ".standalone.out"))
             (standalone-err (concat stem ".standalone.err")))
        (nelisp-bytecode-corpus--write
         host-expression
         (nelisp-bytecode-corpus--host-expression form))
        (let* ((host-status
                (nelisp-bytecode-corpus--run
                 host (list "--batch" "-Q" "-l" host-expression)
                 host-out host-err))
               (standalone-status
                (nelisp-bytecode-corpus--run
                 standalone (list "--eval" printed)
                 standalone-out standalone-err))
               (host-value (nelisp-bytecode-corpus--result-text host-out))
               (standalone-value
                (nelisp-bytecode-corpus--result-text standalone-out)))
          (if (and (equal host-status 0) (equal standalone-status 0)
                   (equal host-value standalone-value))
              (cl-incf passed)
            (push (list name host-status standalone-status
                        host-value standalone-value)
                  failures)))))
    (setq failures (nreverse failures))
    (with-temp-file (expand-file-name "report.txt" output)
      (insert (format "cases=%d\npassed=%d\nfailed=%d\n"
                      (length nelisp-vendor-alist-get-parity-cases)
                      passed (length failures)))
      (dolist (failure failures)
        (insert (format "%S host-status=%S standalone-status=%S host=%S standalone=%S\n"
                        (nth 0 failure) (nth 1 failure) (nth 2 failure)
                        (nth 3 failure) (nth 4 failure)))))
    (princ (with-temp-buffer
             (insert-file-contents (expand-file-name "report.txt" output))
             (buffer-string)))
    (when failures (kill-emacs 1))))

(provide 'nelisp-vendor-alist-get-parity)

;;; nelisp-vendor-alist-get-parity.el ends here
