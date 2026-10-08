;;; nelisp-bytecode-native-unary-test.el --- unary bytecode lowering -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-package)

(defun nelisp-bytecode-native-unary-test--function (operation &optional code constants depth)
  (make-byte-code 257 (or code (unibyte-string (if (eq operation 'car) 64 65) 135))
                  (or constants []) (or depth 2)))

(ert-deftest nelisp-bytecode-native-unary-admits-only-exact-car-cdr-templates ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (operation '(car cdr))
    (let* ((function (nelisp-bytecode-native-unary-test--function operation))
           (input (nelisp-bytecode-compiler-input-build function))
           (artifact (concat (make-temp-name
                              (expand-file-name "nelisp-unary-" temporary-file-directory))
                             ".nelr"))
           (entry "nl_native_wrong_operation_probe")
           (result (nelisp-bytecode-native-compiler-build function artifact entry)))
      (should (eq (plist-get input :status) 'complete))
      (should (eq (nelisp-bytecode-native-compiler-unary-template-operation input)
                  operation))
      (should (eq (plist-get result :status) 'unsupported))
      (should (eq (plist-get result :artifact-kind) 'raw-runtime-v2))
      (should (equal (plist-get result :gateway-import)
                     (format "nl_native_%s_v2" operation)))
      (should-not (file-exists-p artifact)))
    (let* ((other (if (eq operation 'car) 'cdr 'car))
           (opposite
            (nelisp-bytecode-native-unary-test--function
             operation (unibyte-string (if (eq operation 'car) 65 64) 135))))
      (should (eq (nelisp-bytecode-native-compiler-unary-template-operation
                   (nelisp-bytecode-compiler-input-build opposite))
                  other))
      (dolist (function
               (list
                (nelisp-bytecode-native-unary-test--function
                 operation (unibyte-string (if (eq operation 'car) 64 65) 137 135)
                 [] 3)
                (nelisp-bytecode-native-unary-test--function operation nil [nil])))
        (should-not
         (nelisp-bytecode-native-compiler-unary-template-operation
          (nelisp-bytecode-compiler-input-build function)))))))

(ert-deftest nelisp-bytecode-native-unary-package-preflight-is-before-effects ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((root (make-temp-file "nelisp-package-raw-unary-" t))
         (elc (expand-file-name "module.elc" root))
         (function (nelisp-bytecode-native-unary-test--function 'cdr))
         (output (expand-file-name "package" root))
         (make-directory-function (symbol-function 'make-directory))
         (directory-calls 0) (backend-calls 0) (failure nil))
    (unwind-protect
        (progn
          (with-temp-file elc
            (insert ";ELC\n")
            (prin1 (list 'defalias (list 'quote 'nelisp-package-cdr) function)
                   (current-buffer))
            (insert "\n")
            (prin1 '(provide 'nelisp-package-raw-cdr) (current-buffer)))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest arguments)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-function arguments)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _arguments)
                       (setq backend-calls (1+ backend-calls))
                       (error "unary refusal control reached backend"))))
            (setq failure
                  (condition-case error-data
                      (progn
                        (nelisp-bytecode-native-package-compile-elc
                         elc 'nelisp-package-raw-cdr '(nelisp-package-cdr) output)
                        nil)
                    (error (error-message-string error-data)))))
          (should (equal failure
                         "bytecode-native-package: nelisp-package-cdr lowers to raw-runtime-v2 and cannot enter a boxed .neln package"))
          (should (= directory-calls 0))
          (should (= backend-calls 0))
          (should-not (file-exists-p output))
          ;; Removing only the legacy unary guard must still be stopped by
          ;; rooted-stack preflight before directory or backend effects.
          (setq directory-calls 0 backend-calls 0 failure nil)
          (cl-letf (((symbol-function
                      'nelisp-bytecode-native-compiler-unary-template-operation)
                     (lambda (_input) nil))
                    ((symbol-function 'make-directory)
                     (lambda (&rest arguments)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-function arguments)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _arguments)
                       (setq backend-calls (1+ backend-calls))
                       (error "unary guard-removal negative control reached backend"))))
            (setq failure
                  (condition-case error-data
                      (progn
                       (nelisp-bytecode-native-package-compile-elc
                         elc 'nelisp-package-raw-cdr '(nelisp-package-cdr) output)
                        nil)
                    (error (error-message-string error-data)))))
          (should (equal failure
                         "bytecode-native-package: nelisp-package-cdr lowers to rooted raw-runtime-v2 and cannot enter a boxed .neln package"))
          (should (= directory-calls 0))
          (should (= backend-calls 0))
          (should-not (file-exists-p output))
          ;; Remove both package admission guards and intercept the captured
          ;; backend API actually used by cacheable package functions.
          (setq directory-calls 0 backend-calls 0 failure nil)
          (let ((nelisp-bytecode-native-compiler-package-preflight-api
                 (plist-put
                  (copy-sequence nelisp-bytecode-native-compiler-package-preflight-api)
                  :build
                  (lambda (&rest _arguments)
                    (setq backend-calls (1+ backend-calls))
                    (error "unary guard-removal negative control reached backend")))))
          (cl-letf (((symbol-function
                      'nelisp-bytecode-native-compiler-unary-template-operation)
                     (lambda (_input) nil))
                    ((symbol-function
                      'nelisp-bytecode-native-compiler-rooted-stack-input-p)
                     (lambda (_input) nil))
                    ((symbol-function 'make-directory)
                     (lambda (&rest arguments)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-function arguments)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _arguments)
                       (setq backend-calls (1+ backend-calls))
                       (error "unary guard-removal negative control reached backend"))))
            (setq failure
                  (condition-case error-data
                      (progn
                        (nelisp-bytecode-native-package-compile-elc
                         elc 'nelisp-package-raw-cdr '(nelisp-package-cdr) output)
                        nil)
                    (error (error-message-string error-data)))))
          (should (equal failure "unary guard-removal negative control reached backend"))
          (should (> directory-calls 0))
          (should (= backend-calls 1)))
          (should-not (file-exists-p output)))
      (delete-directory root t))))

(provide 'nelisp-bytecode-native-unary-test)
;;; nelisp-bytecode-native-unary-test.el ends here
