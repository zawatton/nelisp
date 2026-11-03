;;; nelisp-locate-library-test.el --- standalone lookup regression -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-locate-library-test--prelude
  (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-locate-file-fallback-direct-ert-regression ()
  "Evaluate the exact prelude fallback and reject a directory shadow."
  (let* ((original-locate-file (symbol-function 'locate-file))
         (original-locate-library (symbol-function 'locate-library))
         (had-check (fboundp 'nelisp--check-string))
         (original-check (and had-check
                              (symbol-function 'nelisp--check-string)))
         (root (make-temp-file "nelisp-locate-fallback" t))
         (shadow-dir (expand-file-name "shadow.elc" root))
         (shadow-source (expand-file-name "shadow.el" root))
         (ordered-bytecode (expand-file-name "ordered.elc" root))
         (ordered-source (expand-file-name "ordered.el" root))
         (directory-only (expand-file-name "directory-only.elc" root)))
    (unwind-protect
        (progn
          (make-directory shadow-dir)
          (make-directory directory-only)
          (with-temp-file shadow-source (insert "source"))
          (with-temp-file ordered-bytecode (insert "bytecode"))
          (with-temp-file ordered-source (insert "source"))
          (fmakunbound 'locate-file)
          (fmakunbound 'locate-library)
          (fset 'nelisp--check-string
                (lambda (value)
                  (unless (stringp value)
                    (signal 'wrong-type-argument (list 'stringp value)))))
          (with-temp-buffer
            (insert-file-contents nelisp-locate-library-test--prelude)
            (goto-char (point-min))
            (search-forward "(unless (fboundp 'locate-file)")
            (goto-char (match-beginning 0))
            (let ((start (point)))
              (forward-sexp)
              (eval (read (buffer-substring-no-properties start (point)))))
            (goto-char (point-min))
            (search-forward "(unless (fboundp 'locate-library)")
            (goto-char (match-beginning 0))
            (let ((start (point)))
              (forward-sexp)
              (eval (read (buffer-substring-no-properties start (point))))))
          (should (equal (locate-file "shadow" (list root) '(".elc" ".el"))
                         shadow-source))
          (should-not (locate-file "directory-only" (list root) '(".elc")))
          (let ((load-path (list root)))
            (should (equal (locate-library "ordered") ordered-bytecode))))
      (fset 'locate-file original-locate-file)
      (fset 'locate-library original-locate-library)
      (if had-check
          (fset 'nelisp--check-string original-check)
        (fmakunbound 'nelisp--check-string))
      (delete-directory root t))))

(ert-deftest nelisp-locate-library-standalone-regression ()
  "Exercise directory rejection, .elc lookup order, and source require."
  (should (= 0 (call-process "bash" nil nil nil
                             "test/standalone-locate-library-smoke.sh"))))

(provide 'nelisp-locate-library-test)
;;; nelisp-locate-library-test.el ends here
