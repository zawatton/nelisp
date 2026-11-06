;;; probe-autoload.el --- GNU 31.1 startup autoloads of vendored libraries  -*- lexical-binding: t; -*-

;; Each name below is an autoload in a fresh `emacs -Q' 31.1 (measured
;; 2026-11-05 with `autoloadp') and its file is vendored here.  Prints
;; PROBE-AUTOLOAD-PASS when every one is fboundp at NeLisp startup, before
;; any `require'.

;;; Code:

(let ((missing nil))
  (dolist (s '(rx rx-define rx-let rx-to-string pp pp-to-string ert-deftest
               ert-run-tests-batch string-pad url-recreate-url))
    (unless (fboundp s) (push s missing)))
  (princ (if missing
             (format "PROBE-AUTOLOAD-FAIL undefined: %S\n" (nreverse missing))
           "PROBE-AUTOLOAD-PASS\n")))

;;; probe-autoload.el ends here
