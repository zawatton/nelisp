;;; nelisp-aot-executable-relocation-test.el --- Final executable closure -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-standalone-build)
(load (expand-file-name "perf/codegen-runtime/compile.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest executable-reloc-rejects-unknown-call-before-write ()
  (let* ((dir (make-temp-file "executable-reloc-" t)) (path (expand-file-name "bad" dir)))
    (unwind-protect
        (progn
          (should-error (nelisp-aot-compile-sexp '(exit (extern-call missing_runtime_helper)) path)
                        :type 'nelisp-aot-compiler-error)
          (should-not (file-exists-p path)))
      (delete-directory dir t))))

(ert-deftest executable-reloc-rejects-unknown-data-before-write ()
  (let* ((dir (make-temp-file "executable-reloc-" t)) (path (expand-file-name "bad" dir)))
    (unwind-protect
        (progn
          (should-error (nelisp-aot-compile-sexp '(exit (data-addr missing_runtime_data)) path)
                        :type 'nelisp-aot-compiler-error)
          (should-not (file-exists-p path)))
      (delete-directory dir t))))

(ert-deftest executable-reloc-resolves-genuine-local-extern ()
  (let* ((dir (make-temp-file "executable-reloc-" t)) (path (expand-file-name "good" dir)))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp '(seq (defun genuine (x) (+ x 1))
                                       (exit (extern-call genuine 6))) path)
          (should (= (call-process path nil nil nil) 7)))
      (delete-directory dir t))))

(ert-deftest executable-reloc-relocatable-unit-keeps-unknown-extern ()
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (unit (nelisp-standalone--compile-to-unit "object-contract"
                 '(seq (defun owner () (extern-call legitimate_external))))))
    (should (cl-find "legitimate_external" (plist-get unit :relocs)
                     :key (lambda (r) (plist-get r :symbol)) :test #'equal))))

(ert-deftest executable-reloc-lowered-provider-missing-rejected ()
  (let ((nelisp-standalone--target 'linux-x86_64))
    ;; No explicit source call names nl_val_tag; target sexp-tag lowering does.
    (should (string-match-p "Missing genuine fixture provider: nl_val_tag"
              (error-message-string (should-error
               (codegen-runtime--closed-unit '(seq (defun owner (x) (sexp-tag x))) nil nil)))))))

(ert-deftest executable-reloc-closure-shared-recursive-terminates ()
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (catalog '((defun a (x) (extern-call b x))
                    (defun b (x) (extern-call a x))))
         (unit (codegen-runtime--closed-unit
                '(seq (defun owner (x) (+ (extern-call a x) (extern-call b x)))) catalog nil))
         (names (mapcar (lambda (s) (plist-get s :name))
                        (cl-remove-if (lambda (s) (eq (plist-get s :section) 'undef))
                                      (plist-get unit :symbols)))))
    (should (= (cl-count "a" names :test #'equal) 1))
    (should (= (cl-count "b" names :test #'equal) 1))))

(ert-deftest executable-reloc-missing-transitive-and-conflict-rejected ()
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (string-match-p "Missing genuine fixture provider: missing_transitive"
             (error-message-string (should-error (codegen-runtime--closed-unit
                   '(seq (defun owner (x) (extern-call a x)))
                   '((defun a (x) (extern-call missing_transitive x))) nil)))))
    (should-error (codegen-runtime--provider 'a '((defun a (x) x) (defun a (x y) y))))
    (should (string-match-p "Fixture lowered extern arity mismatch: a"
             (error-message-string (should-error (codegen-runtime--closed-unit
                   '(seq (defun owner (x) (extern-call a x)))
                   '((defun a (x y) y)) nil)))))
    (should-error (codegen-runtime--closed-unit
                   '(seq (defun owner () 1)) nil
                   (list (list :name "other" :symbols
                               (list (list :name "owner" :section 'text :bind 'global))))))
    (should (string-match-p "Fixture call targets a data owner: owned_data"
             (error-message-string (should-error (codegen-runtime--closed-unit
                   '(seq (defun owner () (extern-call owned_data))) nil
                   (list (list :name "data-owner" :symbols
                               (list (list :name "owned_data" :section 'data :bind 'global :type 'object)))))))))))

(provide 'nelisp-aot-executable-relocation-test)
