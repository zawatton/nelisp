;;; script-env-check.el --- script arguments and copy-file -*- lexical-binding: t -*-

;; Usage: CHECK=args|copy-file|all nelisp --load test/perf/script-env-check.el -- a b
;;
;; nelisp-autocad's scripts take arguments after `--' and copy files.  GNU
;; Emacs 31.1 leaves ("--" "a" "b") in `command-line-args-left' while the
;; file loads; NeLisp left it nil, and `copy-file' was void.  Prints one
;; SCRIPT-ENV line and exits non-zero on failure.

(let* ((check (or (getenv "CHECK") "all"))
       (failures nil))
  (when (member check '("args" "all"))
    (unless (equal command-line-args-left '("--" "a" "b"))
      (push (list 'command-line-args-left command-line-args-left) failures)))
  (when (member check '("copy-file" "all"))
    (condition-case err
        (let* ((dir (make-temp-file "script-env" t))
               (from (expand-file-name "from.txt" dir))
               (to (expand-file-name "to.txt" dir)))
          (unwind-protect
              (progn
                (with-temp-file from (insert "日本 copy\n"))
                (copy-file from to)
                (unless (equal (with-temp-buffer (insert-file-contents to) (buffer-string))
                               "日本 copy\n")
                  (push 'copy-file-content failures))
                (unless (eq 'file-already-exists
                            (car (condition-case e (progn (copy-file from to) nil)
                                   (error e))))
                  (push 'copy-file-exists-error failures))
                (copy-file from to t)
                (unless (file-exists-p to) (push 'copy-file-ok-if-exists failures)))
            (dolist (f (list from to)) (when (file-exists-p f) (delete-file f)))
            (delete-directory dir)))
      (error (push (list 'copy-file err) failures))))
  (dolist (f (reverse failures)) (princ (format "  MISMATCH %S\n" f)))
  (princ (format "SCRIPT-ENV %s failures=%d %s\n" check (length failures)
                 (if failures "FAIL" "PASS")))
  (kill-emacs (if failures 1 0)))

;;; script-env-check.el ends here
