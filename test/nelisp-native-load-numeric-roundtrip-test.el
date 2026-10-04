;;; nelisp-native-load-numeric-roundtrip-test.el --- Numeric boxing regression -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'nelisp-native-load)
(defconst numeric-roundtrip-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(ert-deftest numeric-roundtrip/pinned-numbers-retain-evaluator-values ()
  (dolist (value (list 9223372036854775808 -9223372036854775809 2.5 -0.0
                      1.0e+INF -1.0e+INF 0.0e+NaN -0.0e+NaN))
    (let ((tag (if (floatp value) 3 13)) call)
      (cl-letf (((symbol-function 'ptr-read-u64) (lambda (_addr _offset) tag))
                ((symbol-function 'nelisp--native-unbox-reference)
                 (lambda (addr env frame) (setq call (list addr env frame)) value)))
        (should (eq (nelisp-native-load-unbox 123 456 789) value))
        (should (equal call '(123 456 789)))))))
(ert-deftest numeric-roundtrip/unrooted-numbers-remain-refused ()
  (dolist (tag '(3 13))
    (let (called)
      (cl-letf (((symbol-function 'ptr-read-u64) (lambda (_addr _offset) tag))
                ((symbol-function 'nelisp--native-unbox-reference)
                 (lambda (&rest _) (setq called t))))
        (dolist (context '((nil nil) (0 789) (456 0) (-1 789)))
          (should-error (apply #'nelisp-native-load-unbox 123 context)))
        (should-not called)))))
(defun numeric-roundtrip-test-reader (backend)
  (let* ((default-directory numeric-roundtrip-test-root)
         (binary (expand-file-name (concat "target/nelisp-" backend)))
         (cold (concat binary ".cold")))
    (unless (file-executable-p binary) (ert-skip "reader not built"))
    (with-temp-buffer
      (let ((rc (apply #'call-process "timeout" nil (current-buffer) nil
                       "-k" "5" "290" binary
                       (append (when (file-exists-p cold) (list "--cold-load-from" cold))
                               '("-L" "lisp" "-L" "src" "-L" "scripts"
                                 "--load" "test/standalone-native-load-numeric-roundtrip-driver.el")))))
        (should (equal (list rc (buffer-string))
                       '(0 "NUMERIC-ROUNDTRIP-PASS bignums=3 ieee=9 refusals=5\n")))))))
(ert-deftest numeric-roundtrip/standalone-static () (numeric-roundtrip-test-reader "static"))
(ert-deftest numeric-roundtrip/standalone-dynamic () (numeric-roundtrip-test-reader "dyn"))
(provide 'nelisp-native-load-numeric-roundtrip-test)
