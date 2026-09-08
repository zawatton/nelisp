;;; nelisp-lambda-body-loop-test.el --- lambda body loop regression -*- lexical-binding: t; -*-
(require 'ert)
(defconst nelisp-lambda-body-loop-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(ert-deftest nelisp-lambda-body-loop/no-recursive-body-driver ()
  (let* ((file (expand-file-name "lisp/nelisp-cc-apply-lambda-inner.el"
                                nelisp-lambda-body-loop-test--root))
         (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (start (string-match "(defun nl_ali_body (body env out cap-flag" text))
         (end (and start (string-match "\n    ;; After nl_push_and_bind" text start))))
    (should start) (should end)
    (let ((body (substring text start end)))
      (should (string-match-p "(while (> state 0)" body))
      (should-not (string-match-p "(nl_ali_body " body)))))
(ert-deftest nelisp-lambda-body-loop/2500-forms ()
  (let ((binary (expand-file-name "target/nelisp" nelisp-lambda-body-loop-test--root)))
    (unless (file-executable-p binary) (ert-skip "target/nelisp is not built"))
    (let ((file (make-temp-file "nelisp-lambda-body-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "((lambda ()\n")
              (dotimes (i 2500) (insert (format "(setq lambda-test %d)\n" i)))
              (insert "lambda-test))\n"))
            (with-temp-buffer
              (should (= 0 (call-process binary nil t nil "--load" file)))
              (should (string-match-p "2499" (buffer-string)))))
        (delete-file file)))))

(ert-deftest nelisp-lambda-body-loop/error-pops-once ()
  "An error in a lambda body leaves the outer binding usable."
  (let ((binary (expand-file-name "target/nelisp" nelisp-lambda-body-loop-test--root)))
    (unless (file-executable-p binary) (ert-skip "target/nelisp is not built"))
    (with-temp-buffer
      (should (= 0 (call-process binary nil t nil "--eval"
                                 "(progn (setq outer 41 seen nil) (condition-case _ ((lambda () (setq inner 1) (error \"x\"))) (error (setq seen t))) (list outer seen))")))
      (should (string-match-p "(41 t)" (buffer-string))))))
(provide 'nelisp-lambda-body-loop-test)

