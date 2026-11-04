;;; nelisp-web-runtime-w0-test.el --- W0 diagnostic checks -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name
       "../tools/ai/web-runtime-w0-probe.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest nelisp-web-runtime-w0/required-application-observable ()
  ;; A host reference check. This is not Wasm application execution.
  (should (equal (mapcar #'nelisp-eval web-runtime-w0-probe--application)
                 '(7 "NeLisp" 9))))

(ert-deftest nelisp-web-runtime-w0/image-roundtrip-preserves-closure-form ()
  (let* ((image (nelisp-heap-image-encode-roots
                 (list (cons "application" web-runtime-w0-probe--application))))
         (decoded (cdr (assoc "application" (nelisp-heap-image-decode-form image)))))
    (should (equal decoded web-runtime-w0-probe--application))
    (should (web-runtime-w0-probe--contains-p decoded 'lambda))
    (should (web-runtime-w0-probe--contains-p decoded 'cons))
    (should (web-runtime-w0-probe--contains-p decoded 'concat))))

(ert-deftest nelisp-web-runtime-w0/first-source-operation-is-owned ()
  (require 'nelisp-cc-eval-inner)
  (let ((source (symbol-value 'nelisp-cc-eval-inner--source)))
    (should (equal (web-runtime-w0-probe--operation-owners source 'atomic-fetch-add)
                   '(nl_stash_void_variable)))
    (should-not (web-runtime-w0-probe--operation-owners source 'absent-operation))))

(ert-deftest nelisp-web-runtime-w0/compilation-preserves-provider-source ()
  (require 'nelisp-cc-eval-inner)
  (let* ((output (make-temp-file "web-runtime-w0-" nil ".wasm"))
         (source (symbol-value 'nelisp-cc-eval-inner--source))
         (before (copy-tree source)))
    (unwind-protect
        (progn
          ;; Accept either compilation or an explicit unsupported operation.
          ;; This check must survive a later implementation of the blocker.
          (condition-case nil
              (nelisp-aot-compile-to-object source output
                                            :arch 'wasm32 :format 'wasm)
            (nelisp-aot-compiler-error nil))
          (should (equal before source)))
      (delete-file output))))

(provide 'nelisp-web-runtime-w0-test)
;;; nelisp-web-runtime-w0-test.el ends here
