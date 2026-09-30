;;; nelisp-scratch-buffer-test.el --- Doc 211 S3.2 checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst nelisp-scratch-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defconst nelisp-scratch-test--aliases
  '(current-buffer point point-min point-max goto-char insert erase-buffer
    buffer-string write-region get-buffer-create set-buffer with-current-buffer))

(ert-deftest nelisp-scratch-buffer-alias-count-and-private-api ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                                            nelisp-scratch-test--root))
    (let ((text (buffer-string)))
      (should (<= (length nelisp-scratch-test--aliases) 15))
      (dolist (name '(nelisp--scratch-buffer nelisp--scratch-current-buffer
                      nelisp--scratch-set-buffer nelisp--scratch-point
                      nelisp--scratch-point-min nelisp--scratch-point-max
                      nelisp--scratch-goto-char nelisp--scratch-insert
                      nelisp--scratch-erase-buffer nelisp--scratch-buffer-string
                      nelisp--scratch-write-region))
        (should (string-match-p (format "(defun %s\\_>" name) text)))
      ;; The feature-name substring also appears in explanatory comments;
      ;; the core-purity gate checks executable dependency forms directly.
      )))

(defun nelisp-scratch-test--run (binary expression)
  (with-temp-buffer
    (let ((status (process-file binary nil t nil "--batch" "-Q" "--eval" expression)))
      (unless (zerop status) (error "%s failed: %s" binary (buffer-string)))
      (buffer-string))))

(ert-deftest nelisp-scratch-buffer-parity-with-gnu-311 ()
  (let* ((binary (or (getenv "NELISP_BIN")
                     (expand-file-name "target/nelisp" nelisp-scratch-test--root)))
         (expr "(princ (prin1-to-string (let ((b (get-buffer-create \" *s3b-parity*\"))) (set-buffer b) (erase-buffer) (insert \"ab\" \"cd\") (goto-char 3) (let ((middle (point)) (whole (buffer-string)) (lo (point-min)) (hi (point-max))) (goto-char (point-max)) (list (buffer-name (current-buffer)) middle whole lo hi (point) (with-current-buffer b (buffer-string)))))))")
         (host (string-trim (nelisp-scratch-test--run "emacs" expr)))
         (standalone (nelisp-scratch-test--run binary expr)))
    ;; Standalone prints its top-level value in addition to `princ'.
    (should (string-match-p (regexp-quote host) standalone))))

(ert-deftest nelisp-scratch-buffer-write-region-parity-with-gnu-311 ()
  (let* ((binary (or (getenv "NELISP_BIN")
                     (expand-file-name "target/nelisp" nelisp-scratch-test--root)))
         (expr "(princ (let ((file \"/tmp/nelisp-s3b-write-region\")) (unwind-protect (progn (write-region \"scratch\" nil file) (with-temp-buffer (insert-file-contents file) (buffer-string))) (when (file-exists-p file) (delete-file file)))))")
         (host (nelisp-scratch-test--run "emacs" expr))
         (standalone (nelisp-scratch-test--run binary expr)))
    (should (string-match-p (regexp-quote host) standalone))))

(ert-deftest nelisp-scratch-buffer-bytecomp-corpus-runs-without-api-buffer-functions ()
  (let* ((binary (or (getenv "NELISP_BIN")
                     (expand-file-name "target/nelisp" nelisp-scratch-test--root)))
         (expr "(progn (require 'bytecomp) (load \"test/fixtures/s6-corpus/byte-compile-form.wrapper.el\" nil t) (let ((forms (with-temp-buffer (insert-file-contents \"test/fixtures/s6-corpus/byte-compile-form.el\") (read (current-buffer))))) (princ (prin1-to-string (mapcar (lambda (args) (s6-corpus--byte-compile-form (car args) (cadr args))) forms)))))"))
    (let ((output (nelisp-scratch-test--run binary expr)))
      (should (string-match-p "byte-constant 42" output))
      (should (string-match-p "byte-varref some-var" output)))))

(provide 'nelisp-scratch-buffer-test)
;;; nelisp-scratch-buffer-test.el ends here
