;;; nelisp-try-completion-test.el --- standalone completion parity -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-try-completion-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-try-completion-test--binary ()
  (let ((binary (or (getenv "NELISP_BIN")
                    (expand-file-name "target/nelisp"
                                      nelisp-try-completion-test--root))))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built"))
    binary))

(defun nelisp-try-completion-test--invoke (binary form)
  (let ((stderr-file
         (make-temp-file
          (expand-file-name "try-completion-stderr-"
                            (file-name-directory binary)))))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (call-process binary nil (list t stderr-file) nil
                                       "--eval" form))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string))))
            (list status (buffer-string) stderr)))
      (delete-file stderr-file))))

(defun nelisp-try-completion-test--standalone-batch (expressions)
  (let* ((binary (nelisp-try-completion-test--binary))
         (form (format
                "(progn (princ \"<<\") (prin1 (mapcar (lambda (source) (condition-case err (eval (car (read-from-string source)) t) (error (list 'uncaught-error err)))) '%S)) (princ \">>\") nil)"
                expressions))
         (result (nelisp-try-completion-test--invoke binary form))
         (status (nth 0 result))
         (output (nth 1 result))
         (stderr (nth 2 result)))
    (unless (string-empty-p stderr)
      (ert-fail (format "unexpected standalone stderr: %s" stderr)))
    (unless (and (integerp status) (= status 0))
      (ert-fail (format "standalone batch failed: rc=%S output=%s" status output)))
    (unless (string-match "<<\\(.*?\\)>>" output)
      (ert-fail (format "standalone produced no batch value: %s" output)))
    (match-string 1 output)))

(defun nelisp-try-completion-test--host-batch (expressions)
  (prin1-to-string
   (mapcar (lambda (source)
             (condition-case err
                 (eval (car (read-from-string source)) t)
               (error (list 'uncaught-error err))))
           expressions)))

(ert-deftest nelisp-try-completion-list-host-parity ()
  (let ((expressions
         '("(try-completion \"x\" nil)"
             "(try-completion \"1\" '(1))"
             "(try-completion \"x\" '(\"foo\"))"
             "(try-completion \"foo\" '(\"foo\"))"
             "(try-completion \"fo\" '(\"foo\"))"
             "(try-completion \"fo\" '(\"foo\" \"food\"))"
             "(try-completion \"fo\" '(foo food))"
             "(let ((completion-ignore-case t)) (try-completion \"FOO\" '(\"foo\")))"
             "(let ((case-fold-search nil) (completion-ignore-case t) (completion-regexp-list '(\"FOO\"))) (try-completion \"f\" '(\"foo\")))"
             "(let ((completion-ignore-case t)) (try-completion \"FOO\" '(\"Foo\" \"foo\")))"
             "(try-completion \"foo\" '(\"foo\" \"food\"))"
             "(try-completion \"foo\" '(\"food\" \"foo\"))"
             "(let ((completion-ignore-case t)) (try-completion \"foo\" '(\"foo\" \"Foo\")))"
             "(try-completion \"foo\" '(\"foo\" \"foo\"))"
             "(try-completion \"fo\" '((\"foo\" . 1) (\"food\" . 2)) (lambda (entry) (= (cdr entry) 2)))"
             "(let ((completion-ignore-case t)) (try-completion \"fo\" '(\"Foo\" \"food\")))"
             "(let ((completion-regexp-list '(\"bar$\"))) (try-completion \"fo\" '(\"foo\" \"foobar\")))"
             "(let ((ob (obarray-make))) (intern \"foo\" ob) (intern \"food\" ob) (try-completion \"fo\" ob))")))
    (should (equal (nelisp-try-completion-test--standalone-batch expressions)
                   (nelisp-try-completion-test--host-batch expressions)))))

(ert-deftest nelisp-try-completion-hash-predicate-and-function-table ()
  (let ((hash-expression
         "(let ((table (make-hash-table :test 'equal))) (puthash \"foo\" 1 table) (puthash \"food\" 2 table) (puthash 42 3 table) (try-completion \"fo\" table (lambda (key value) (= value 2))))")
        (function-expression
         "(try-completion \"fo\" (lambda (string predicate _ignore) (if (string= string \"fo\") 'handled nil)))"))
    (let ((expressions (list hash-expression function-expression)))
      (should (equal (nelisp-try-completion-test--standalone-batch expressions)
                     (nelisp-try-completion-test--host-batch expressions))))))

(ert-deftest nelisp-try-completion-errors-and-obarray-boundary ()
  (let ((expressions
         '("(condition-case err (try-completion 1 nil) (wrong-type-argument err))"
           "(condition-case err (try-completion \"x\" [nil]) (wrong-type-argument err))"
           "(fboundp 'try-completion)")))
    (should (equal "((wrong-type-argument stringp 1) (wrong-type-argument obarrayp [nil]) t)"
                   (nelisp-try-completion-test--standalone-batch expressions)))))

(provide 'nelisp-try-completion-test)

;;; nelisp-try-completion-test.el ends here
