;;; standalone-bytecode-word-check.el --- GNU bytecode equality corpus -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-bytecode-word-check.el
;;
;; Replays test/bytecode-word-corpus.eld: functions byte-compiled by host
;; GNU Emacs (see test/bytecode-word-corpus-gen.el), each case called in
;; order, and the `%S' text of every answer compared with GNU's.  A signal
;; compares as (error ERROR-SYMBOL).  Prints one BYTECODE-WORD line per
;; mismatch and a summary line; exits non-zero on any mismatch.

(let* ((file (expand-file-name "bytecode-word-corpus.eld"
                               (file-name-directory load-file-name)))
       (data (with-temp-buffer
               (insert-file-contents file)
               (car (read-from-string (buffer-string)))))
       (fails 0) (n 0))
  (dolist (p (nth 1 data))
    (eval (list 'defvar (car p)) t)
    (set (car p) (cdr p)))
  (dolist (p (nth 2 data))
    (fset (car p) (cdr p)))
  (dolist (c (nth 3 data))
    (setq n (1+ n))
    (let ((got (format "%S" (condition-case e (apply (car c) (copy-tree (cadr c) t))
                              (error (list 'error (car e)))))))
      (unless (equal got (nth 2 c))
        (setq fails (1+ fails))
        (princ (format "BYTECODE-WORD FAIL %S %S got %s want %s\n"
                       (car c) (cadr c) got (nth 2 c))))))
  (princ (format "BYTECODE-WORD %s %d/%d cases match GNU %s\n"
                 (if (= fails 0) "PASS" "FAIL") (- n fails) n (car data)))
  (kill-emacs (if (= fails 0) 0 1)))

;;; standalone-bytecode-word-check.el ends here
