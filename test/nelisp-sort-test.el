;;; nelisp-sort-test.el --- GNU Emacs 31 sort parity -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-sort-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-sort-test--binary ()
  (let ((binary (or (getenv "NELISP_BIN")
                    (expand-file-name "target/nelisp" nelisp-sort-test--root))))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built"))
    binary))

(defun nelisp-sort-test--invoke (binary form)
  (let ((stderr-file
         (make-temp-file
          (expand-file-name "sort-stderr-" (file-name-directory binary)))))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (call-process binary nil (list t stderr-file) nil
                                       "--eval" form))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string))))
            (list status (buffer-string) stderr)))
      (delete-file stderr-file))))

(defun nelisp-sort-test--host-values (expressions)
  (prin1-to-string
   (mapcar (lambda (source)
             (condition-case err
                 (eval (car (read-from-string source)) t)
               (error err)))
           expressions)))

(defun nelisp-sort-test--standalone-values (expressions)
  (let* ((binary (nelisp-sort-test--binary))
         (form (format
                "(progn (princ \"<<\") (prin1 (mapcar (lambda (source) (condition-case err (eval (car (read-from-string source)) t) (error err))) '%S)) (princ \">>\") nil)"
                expressions))
         (result (nelisp-sort-test--invoke binary form))
         (status (nth 0 result))
         (output (nth 1 result))
         (stderr (nth 2 result)))
    (unless (string-empty-p stderr)
      (ert-fail (format "unexpected standalone stderr: %s" stderr)))
    (unless (and (integerp status) (= status 0))
      (ert-fail (format "standalone sort batch failed: rc=%S output=%s"
                        status output)))
    (unless (string-match "<<\\(.*?\\)>>" output)
      (ert-fail (format "standalone produced no batch result: %s" output)))
    (match-string 1 output)))

(ert-deftest nelisp-sort-keyword-and-legacy-host-parity ()
  (let ((expressions
         '("(let ((xs '(3 1 2))) (let ((out (sort xs #'<))) (list out xs (eq out xs))))"
           "(let ((xs '(3 1 2))) (let ((out (sort xs :lessp #'<))) (list out xs (eq out xs))))"
           "(sort '((1 . a) (1 . b) (0 . c)) :key #'car :lessp #'<)"
           "(sort '(3 1 2) :lessp #'< :reverse t)"
           "(let ((xs [3 1 2])) (let ((out (sort xs :lessp #'<))) (list out xs (eq out xs))))"
           "(let ((xs [3 1 2])) (let ((out (sort xs :lessp #'< :in-place t))) (list out xs (eq out xs))))"
           "(let ((xs [3 1 2])) (let ((out (sort xs #'<))) (list out xs (eq out xs))))"
           "(sort '(3 1 2))"
           "(let ((calls 0)) (list (sort '(3 1 2) :key (lambda (x) (setq calls (1+ calls)) x) :lessp #'<) calls))"
           "(let ((xs '(3 1 2))) (let ((out (sort xs))) (list out xs (eq out xs))))"
           "(sort '(3 1 2) :lessp #'< :lessp #'>)")))
    (should (equal (nelisp-sort-test--standalone-values expressions)
                   (nelisp-sort-test--host-values expressions)))))

(ert-deftest nelisp-sort-errors-match-host ()
  (let ((expressions
         '("(condition-case err (sort \"abc\" :in-place t) (wrong-type-argument err))"
           "(condition-case err (sort '(3 . 1) #'<) (wrong-type-argument err))"
           "(let ((xs (list 1))) (setcdr xs xs) (condition-case err (sort xs) (error (list (car err) (length (cdr err)) (eq (cadr err) xs)))))"
           "(condition-case err (sort '(3 1 2) :unknown t) (error err))"
           "(condition-case err (sort '(3 1 2) :lessp) (error err))"
           "(condition-case err (sort '(3 1 2) :lessp 42) (error err))")))
    (should (equal (nelisp-sort-test--standalone-values expressions)
                   (nelisp-sort-test--host-values expressions)))))

(ert-deftest nelisp-sort-unblocks-bytecomp-sort-keywords ()
  (let* ((binary (nelisp-sort-test--binary))
         (result (nelisp-sort-test--invoke binary "(require 'bytecomp)"))
         (status (nth 0 result))
         (output (concat (nth 1 result) (nth 2 result)))
         (stderr (nth 2 result)))
    (if (and (integerp status) (= status 0))
        (should (string-empty-p stderr))
      (should (integerp status))
      (should (string-match-p "nelisp: uncaught error:" stderr))
      (should-not (string-match-p "wrong-number-of-arguments: (lambda 3)"
                                  output))
      (should-not (string-match-p "[0-9]+: sort$" stderr)))))

(provide 'nelisp-sort-test)

;;; nelisp-sort-test.el ends here
