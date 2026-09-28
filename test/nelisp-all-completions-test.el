;;; nelisp-all-completions-test.el --- standalone completion parity -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-all-completions-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-all-completions-test--binary ()
  (let ((binary (or (getenv "NELISP_BIN")
                    (expand-file-name "target/nelisp"
                                      nelisp-all-completions-test--root))))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built"))
    binary))

(defun nelisp-all-completions-test--invoke (binary form)
  (let ((stderr-file
         (make-temp-file
          (expand-file-name "all-completions-stderr-"
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

(defun nelisp-all-completions-test--expressions (expressions)
  (let* ((binary (nelisp-all-completions-test--binary))
         (form (format
                "(progn (princ \"<<\") (prin1 (mapcar (lambda (source) (condition-case err (eval (car (read-from-string source)) t) (error (list 'uncaught-error err)))) '%S)) (princ \">>\") nil)"
                expressions))
         (result (nelisp-all-completions-test--invoke binary form))
         (status (nth 0 result))
         (output (nth 1 result))
         (stderr (nth 2 result)))
    (unless (string-empty-p stderr)
      (ert-fail (format "unexpected standalone stderr: %s" stderr)))
    (unless (and (integerp status) (= status 0))
      (ert-fail (format "standalone failed: rc=%S output=%s" status output)))
    (unless (string-match "<<\\(.*?\\)>>" output)
      (ert-fail (format "standalone produced no batch value: %s" output)))
    (match-string 1 output)))

(defun nelisp-all-completions-test--host-expressions (expressions)
  (prin1-to-string
   (mapcar (lambda (source)
             (condition-case err
                 (eval (car (read-from-string source)) t)
               (error (list 'uncaught-error err))))
           expressions)))

(ert-deftest nelisp-all-completions-list-alist-and-filter-parity ()
  (let ((expressions
         '("(all-completions \"fo\" '(\"food\" \"foo\" \"foo\" foo 7))"
           "(all-completions \"fo\" '(foo food foo))"
           "(all-completions \"fo\" '((\"foo\" . 1) (\"food\" . 2) bar))"
           "(all-completions \"fo\" '((\"foo\" . 1) (\"food\" . 2)) (lambda (entry) (= (cdr entry) 2)))"
           "(let ((completion-ignore-case t)) (all-completions \"FO\" '(\"foo\" \"Foo\" \"FOOD\")))"
           "(let ((case-fold-search nil) (completion-ignore-case t) (completion-regexp-list '(\"FOO\"))) (all-completions \"f\" '(\"foo\")))"
           "(let ((case-fold-search t) (completion-ignore-case nil) (completion-regexp-list '(\"FOO\"))) (all-completions \"f\" '(\"foo\")))"
           "(let ((completion-regexp-list '(\"bar$\" \"foo\"))) (all-completions \"fo\" '(\"foo\" \"foobar\" \"barfoo\")))"
           "(all-completions \"z\" '(\"foo\"))"
           "(let ((ob (obarray-make))) (intern \"foo\" ob) (intern \"food\" ob) (sort (all-completions \"fo\" ob) #'string<))")))
    (should (equal (nelisp-all-completions-test--expressions expressions)
                   (nelisp-all-completions-test--host-expressions expressions)))))

(ert-deftest nelisp-all-completions-hash-and-function-table-parity ()
  (let ((expressions
         '("(let ((table (make-hash-table :test 'equal))) (puthash \"foo\" 1 table) (puthash \"food\" 2 table) (puthash 42 3 table) (sort (all-completions \"fo\" table (lambda (key value) (= value 2))) #'string<))"
           "(let ((table (make-hash-table :test 'eq)) (seen nil)) (puthash 'foo 1 table) (list (all-completions \"fo\" table (lambda (key value) (setq seen (list key value)) t)) seen))"
           "(let ((calls nil)) (list (all-completions \"fo\" (lambda (string predicate action) (setq calls (list string predicate action)) 'handled) 'pred) calls))")))
    (should (equal (nelisp-all-completions-test--expressions expressions)
                   (nelisp-all-completions-test--host-expressions expressions)))))

(ert-deftest nelisp-all-completions-errors-and-global-obarray-boundary ()
  (let ((expressions
         '("(condition-case err (all-completions 1 nil) (wrong-type-argument err))"
           "(condition-case err (all-completions \"fo\" [nil]) (wrong-type-argument err))"
           "(fboundp 'all-completions)")))
    (should (equal "((wrong-type-argument stringp 1) (wrong-type-argument obarrayp [nil]) t)"
                   (nelisp-all-completions-test--expressions expressions))))
  (should (equal "((unsupported-feature global-mapatoms))"
                 (nelisp-all-completions-test--expressions
                  '("(condition-case err (mapatoms (lambda (_symbol) nil)) (unsupported-feature err))")))))

(ert-deftest nelisp-all-completions-vendor-require-passes-completion-stage ()
  (let* ((binary (nelisp-all-completions-test--binary))
         (result (nelisp-all-completions-test--invoke binary "(require 'bytecomp)"))
         (status (nth 0 result))
         (output (concat (nth 1 result) (nth 2 result)))
         (stderr (nth 2 result)))
    (if (and (integerp status) (= status 0))
        (should (string-empty-p stderr))
      (should (integerp status))
      (should (string-match-p "nelisp: uncaught error:" stderr))
      (should-not (string-match-p "void-function: (try-completion)"
                                  output))
      (should-not (string-match-p "void-function: (all-completions)"
                                  output)))))

(provide 'nelisp-all-completions-test)

;;; nelisp-all-completions-test.el ends here
