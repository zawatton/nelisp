;;; nelisp-progn-loop-test.el --- long progn stack regression -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-progn-loop-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest nelisp-progn-loop/no-public-recursion ()
  "The progn WORD driver (nl_sf_progn_w, 4bc6a481d) must use state iteration, not self recursion."
  (let* ((file (expand-file-name "lisp/nelisp-cc-sf-progn.el"
                                nelisp-progn-loop-test--root))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (start (string-match "(defun nl_sf_progn_w (args env _pad _pad1)" text))
         (end (and start (string-match "\n    ;; (rc, OUT) adapter" text start))))
    (should start)
    (should end)
    (let ((body (substring text start end)))
      (should (string-match-p "(while (> state 0)" body))
      (should-not (string-match-p "(nl_sf_progn_w " body))
      (should-not (string-match-p "(nl_sf_progn " body)))))

(ert-deftest nelisp-progn-loop/2500-forms-returns-last ()
  "A 2500-form progn completes and returns its last value."
  (let ((binary (expand-file-name "target/nelisp" nelisp-progn-loop-test--root)))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built"))
    (let ((file (make-temp-file "nelisp-progn-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "(progn\n")
              (dotimes (i 2500) (insert (format "(setq progn-test %d)\n" i)))
              (insert "progn-test)\n"))
            (with-temp-buffer
              (should (= 0 (call-process binary nil t nil "--load" file)))
              (should (string-match-p "2499" (buffer-string)))))
        (delete-file file)))))

(provide 'nelisp-progn-loop-test)
