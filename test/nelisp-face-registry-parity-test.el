;;; nelisp-face-registry-parity-test.el --- headless face parity -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-face-registry-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-face-registry-test--binary ()
  (let ((binary (or (getenv "NELISP_BIN")
                    (expand-file-name
                     (if (eq system-type 'windows-nt)
                         "target/nelisp.exe" "target/nelisp")
                     nelisp-face-registry-test--root))))
    (unless (file-executable-p binary)
      (if (getenv "NELISP_BIN")
          (error "NELISP_BIN is not executable: %s" binary)
        (ert-skip "standalone binary is not built")))
    binary))

(defun nelisp-face-registry-test--output (program args)
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (unless (and (integerp status) (= status 0))
        (error "%s failed (%S): %s" program status (buffer-string)))
      (buffer-string))))

(ert-deftest nelisp-headless-face-registry-matches-host-core-contract ()
  ;; GNU batch leaves default attributes unspecified; the native headless
  ;; bootstrap initializes them. FACEP promises presence, not identical
  ;; environment-specific attribute values. Keep the custom-face record,
  ;; identity, size, attribute preservation and type-error assertions below.
  (let* ((expression
          "(princ (format \"%S\" (let* ((face 'nelisp-face-registry-parity-probe) (record nil)) (let ((before (facep face))) (make-face face) (setq record (facep face)) (aset record 1 'sentinel) (make-face face) (list before (and (facep 'default) t) (eq (make-empty-face face) face) (vectorp (facep face)) (= (length (facep face)) 20) (eq (aref (facep face) 0) 'face) (eq (aref (facep face) 1) 'sentinel) (eq (facep (symbol-name face)) (facep face)) (numberp (get face 'face)) (condition-case err (progn (make-face 17) 'no-error) (error (car err))))))))")
         (host (nelisp-face-registry-test--output
                "emacs" (list "--batch" "-Q" "--eval" expression
                              "--eval" "nil")))
         (nelisp (nelisp-face-registry-test--output
                  (nelisp-face-registry-test--binary)
                  (list "--eval" expression "--eval" "nil"))))
    (should (equal nelisp host))))

(ert-deftest nelisp-set-face-documentation-matches-host-property-contract ()
  (let* ((expression
          "(princ (format \"%S\" (let ((face 'nelisp-face-doc-probe)) (make-face face) (list (set-face-documentation face \"face docs\") (get face 'face-documentation) (condition-case err (progn (set-face-documentation 17 \"bad\") 'no-error) (error (car err)))))))")
         (host (nelisp-face-registry-test--output
                "emacs" (list "--batch" "-Q" "--eval" expression
                              "--eval" "nil")))
         (nelisp (nelisp-face-registry-test--output
                  (nelisp-face-registry-test--binary)
                  (list "--eval" expression "--eval" "nil"))))
    (should (equal nelisp host))))

(ert-deftest nelisp-custom-handle-all-keywords-matches-host-tag-contract ()
  (let* ((expression
          "(princ (format \"%S\" (let ((symbol 'nelisp-custom-keyword-parity-probe)) (list (custom-handle-all-keywords symbol '(:tag \"Tag\" :version \"31.1\" :package-version (\"pkg\" . \"1\")) nil) (get symbol 'custom-tag) (get symbol 'custom-version) (get symbol 'custom-package-version) (condition-case err (custom-handle-all-keywords symbol '(:tag) nil) (error (car err))) (condition-case err (custom-handle-all-keywords symbol '(junk) nil) (error (car err)))))))")
         (host (nelisp-face-registry-test--output
                "emacs" (list "--batch" "-Q" "--eval" expression
                              "--eval" "nil")))
         (nelisp (nelisp-face-registry-test--output
                  (nelisp-face-registry-test--binary)
                  (list "--eval" expression "--eval" "nil"))))
    (should (equal nelisp host))))

(ert-deftest nelisp-button-type-substrate-matches-host-contract ()
  (let* ((expression
          "(progn (require 'ansi-osc) (princ (format \"%S\" (let ((type 'nelisp-button-type-parity-probe)) (define-button-type type 'help-echo \"Probe\") (list (featurep 'ansi-osc) (symbol-name (get type 'button-category-symbol)) (get (get type 'button-category-symbol) 'help-echo) (symbol-name (get 'ansi-osc-hyperlink 'button-category-symbol)))))))")
         (host (nelisp-face-registry-test--output
                "emacs" (list "--batch" "-Q" "--eval" expression
                              "--eval" "nil")))
         (nelisp (nelisp-face-registry-test--output
                  (nelisp-face-registry-test--binary)
                  (list "--eval" expression "--eval" "nil"))))
    (should (equal nelisp host))))

(ert-deftest nelisp-regexp-opt-provider-loads-on-host-and-standalone ()
  (let* ((expression
          "(progn (require 'regexp-opt) (princ (format \"%S\" (featurep 'regexp-opt))))")
         (host (nelisp-face-registry-test--output
                "emacs" (list "--batch" "-Q" "--eval" expression
                              "--eval" "nil")))
         (nelisp (nelisp-face-registry-test--output
                  (nelisp-face-registry-test--binary)
                  (list "--eval" expression "--eval" "nil"))))
    (should (equal nelisp host))))

(provide 'nelisp-face-registry-parity-test)
;;; nelisp-face-registry-parity-test.el ends here
