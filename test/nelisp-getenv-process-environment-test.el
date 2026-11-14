;;; nelisp-getenv-process-environment-test.el --- getenv/setenv follow `process-environment'  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; In GNU Emacs `getenv' and `setenv' read and write `process-environment'
;; (a list of "VAR=VALUE" strings, first match wins, a bare "VAR" entry means
;; unset), and every child process inherits the current value of that
;; variable, a `let'-bound one included.  Each case here runs a form file on
;; the built `target/nelisp' binary and compares its output with what GNU
;; Emacs prints for the same form.

;;; Code:

(require 'ert)

(defun nelisp-getenv-pe--run (forms)
  "Run FORMS (a string of Lisp) on the standalone reader; return its first output line."
  (let ((binary (expand-file-name "target/nelisp" default-directory))
        (file (make-temp-file "nelisp-getenv-pe-" nil ".el")))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader gate owns it"))
    (unwind-protect
        (progn
          (with-temp-file file (insert forms "\n(terpri)\n"))
          (with-temp-buffer
            (let ((rc (call-process binary nil (list (current-buffer) nil) nil
                                    "--load" file)))
              (unless (= rc 0)
                (ert-fail (format "standalone run failed: rc=%S output=%S"
                                  rc (buffer-string))))
              ;; The runner echoes the last form's value after the output.
              (goto-char (point-min))
              (buffer-substring (point) (line-end-position)))))
      (delete-file file))))

(ert-deftest nelisp-getenv-pe-let-bound-entry ()
  (should (equal (nelisp-getenv-pe--run
                  "(setenv \"SUMIGAP_X\" \"base\")
(prin1 (list (getenv \"SUMIGAP_X\")
  (let ((process-environment (cons \"SUMIGAP_X=bound\" process-environment)))
    (getenv \"SUMIGAP_X\"))
  (getenv \"SUMIGAP_X\")))")
                 "(\"base\" \"bound\" \"base\")")))

(ert-deftest nelisp-getenv-pe-let-bound-home ()
  (should (equal (nelisp-getenv-pe--run
                  "(prin1 (let ((process-environment (list \"HOME=/zzz\"))) (getenv \"HOME\")))")
                 "\"/zzz\"")))

(ert-deftest nelisp-getenv-pe-setenv-then-getenv ()
  (should (equal (nelisp-getenv-pe--run
                  "(prin1 (list (getenv \"SUMIGAP_N\")
  (setenv \"SUMIGAP_N\" \"1\") (getenv \"SUMIGAP_N\")
  (setenv \"SUMIGAP_N\" \"2\") (getenv \"SUMIGAP_N\")
  (car (member \"SUMIGAP_N=2\" process-environment))))")
                 "(nil \"1\" \"1\" \"2\" \"2\" \"SUMIGAP_N=2\")")))

(ert-deftest nelisp-getenv-pe-unset-entries ()
  (should (equal (nelisp-getenv-pe--run
                  "(setenv \"SUMIGAP_U\" \"v\")
(prin1 (list (getenv \"SUMIGAP_U\")
  (let ((process-environment (cons \"SUMIGAP_U\" process-environment)))
    (getenv \"SUMIGAP_U\"))
  (progn (setenv \"SUMIGAP_U\" nil) (getenv \"SUMIGAP_U\"))
  (let ((process-environment (list \"A=1\" \"A=2\" \"B=\")))
    (list (getenv \"A\") (getenv \"B\") (getenv \"C\")))))")
                 "(\"v\" nil nil (\"1\" \"\" nil))")))

(ert-deftest nelisp-getenv-pe-child-sees-let-bound ()
  (should (equal (nelisp-getenv-pe--run
                  "(setenv \"SUMIGAP_X\" \"base\")
(prin1 (list
  (with-temp-buffer
    (call-process \"sh\" nil t nil \"-c\" \"printf %s \\\"$SUMIGAP_X\\\"\")
    (buffer-string))
  (let ((process-environment (cons \"SUMIGAP_X=bound\" process-environment)))
    (with-temp-buffer
      (call-process \"sh\" nil t nil \"-c\" \"printf %s \\\"$SUMIGAP_X\\\"\")
      (buffer-string)))
  (let ((process-environment (cons \"SUMIGAP_X\" process-environment)))
    (with-temp-buffer
      (call-process \"sh\" nil t nil \"-c\" \"printf %s \\\"${SUMIGAP_X-unset}\\\"\")
      (buffer-string)))))")
                 "(\"base\" \"bound\" \"unset\")")))

(provide 'nelisp-getenv-process-environment-test)
;;; nelisp-getenv-process-environment-test.el ends here
