;;; standalone-prn-string-escape-check.el --- printer string escapes as GNU prints them -*- lexical-binding: t -*-

;; Usage: CASES=test/fixtures/prn-string-escape-cases.eld nelisp --load test/standalone-prn-string-escape-check.el
;;
;; Each case is ([NEWLINES CONTROL MULTIBYTE] STRING EXPECTED): EXPECTED is
;; GNU Emacs 31.1's (substring (prin1-to-string STRING) 1 -1) with
;; `print-escape-newlines', `print-escape-control-characters' and
;; `print-escape-multibyte' bound as given.  It covers every ASCII character,
;; newlines with and without escaping, non-ASCII, hex-digit separators,
;; unibyte bytes and raw bytes.  `nelisp--prn-string-escaped' must match
;; each one whichever path (native `nelisp--repr' or the escape loop) takes
;; it.  Prints one PRN-ESC line and exits non-zero on failure.

(let* ((file (or (getenv "CASES") "test/fixtures/prn-string-escape-cases.eld"))
       (cases (with-temp-buffer (insert-file-contents file)
                                (car (read-from-string (buffer-string)))))
       (bad 0) (shown 0))
  (dolist (c cases)
    (let* ((f (nth 0 c)) (s (nth 1 c)) (want (nth 2 c))
           (got (let ((print-escape-newlines (aref f 0))
                      (print-escape-control-characters (aref f 1))
                      (print-escape-multibyte (aref f 2)))
                  (nelisp--prn-string-escaped s))))
      (unless (equal got want)
        (setq bad (1+ bad))
        (when (< shown 12)
          (setq shown (1+ shown))
          (princ (format "  BAD %S %S got=%S want=%S\n" f (append s nil)
                         (append got nil) (append want nil)))))))
  (princ (format "PRN-ESC cases=%d bad=%d %s\n" (length cases) bad
                 (if (= bad 0) "PASS" "FAIL")))
  (kill-emacs (if (= bad 0) 0 1)))

;;; standalone-prn-string-escape-check.el ends here
