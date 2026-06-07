;;; nelisp-cli-test.el --- CLI metadata tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cli)
(require 'nelisp-sys)

(ert-deftest nelisp-cli-records-startup-metadata-for-sys-layer ()
  "Doc 44 startup metadata is recorded from the CLI argv entry point."
  (let ((process-environment '("DOC44_ENV=ok"))
        (nelisp--startup-argc nil)
        (nelisp--startup-argv nil)
        (nelisp--startup-envp nil)
        stdout)
    (cl-letf (((symbol-function 'nelisp--write-stdout-bytes)
               (lambda (s) (setq stdout s))))
      (should (= (nelisp-cli-main '("nelisp" "--version")) 0)))
    (should (string-match-p "nelisp" stdout))
    (should (= (nelisp-sys-startup-argc) 2))
    (should (equal (nelisp-sys-startup-argv)
                   '("nelisp" "--version")))
    (should (equal (nelisp-sys-startup-envp)
                   '("DOC44_ENV=ok")))))

(provide 'nelisp-cli-test)
;;; nelisp-cli-test.el ends here
