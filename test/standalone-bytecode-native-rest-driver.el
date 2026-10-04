;;; standalone-bytecode-native-rest-driver.el --- REST call boundary probe -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-rest-build ()
  "Build an ELF body for the verified GNU required-plus-REST return template."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (api-root (getenv "NELISP_ARTIFACT_API_ROOT"))
         (artifact (getenv "NELISP_BC_NATIVE_REST_ARTIFACT"))
         (elc (getenv "NELISP_BC_NATIVE_REST_ELC"))
         (function nil)
         (input nil)
         (result nil))
    (unless (and root api-root artifact elc)
      (error "REST smoke paths are unset"))
    (load (expand-file-name "lisp/nelisp-bytecode-compiler-input.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-consumer.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-artifact.el" api-root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-ir.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-frame-ir.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-compiler-input.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-constant.el" root) nil nil t)
    (setq function
          (cdr (assq 'nelisp-test-bytecode-native-rest-function
                     (nelisp-bytecode-native-package--elc-definitions
                      (nelisp-bytecode-native-package--read-elc-forms elc)))))
    (unless (byte-code-function-p function)
      (error "source-free .elc did not provide the compiled REST function"))
    (setq input (nelisp-bytecode-compiler-input-build function))
    (unless (and (eq (plist-get input :status) 'complete)
                 (plist-get input :rest-argument-p)
                 (plist-get input :rest-slot-return-template-p)
                 (= (plist-get input :required-argument-count) 1)
                 (= (plist-get input :initial-stack-depth) 2)
                 (equal (plist-get input :rest-native-code) (unibyte-string 135)))
      (error "unexpected GNU REST descriptor/frame: %S" input))
    (setq result
          (nelisp-bytecode-native-constant-return-build
           (plist-get input :rest-native-code) []
           artifact "nl_bc_rest_return" 2 nil 2 1))
    (unless (and (eq (plist-get result :status) 'complete)
                 (= (plist-get result :return-argument-index) 1)
                 (= (plist-get result :rest-required-count) 1))
      (error "REST body compilation failed: %S" result))
    t))

(defun nelisp-test-bytecode-native-rest-run ()
  "Compare GNU bytecode REST behavior with the rooted native boundary."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BC_NATIVE_REST_ARTIFACT"))
         (elc (getenv "NELISP_BC_NATIVE_REST_ELC"))
         (required (cons 'required-marker nil))
         (extra-a (cons 'extra-a nil))
         (extra-b (cons 'extra-b nil))
         (arguments (list required extra-a extra-b))
         (unit nil)
         (native-result nil)
         (equivalent nil)
         (survives-gc nil)
         (too-few-refused nil)
         (tampered-refused nil))
    (load (expand-file-name "lisp/nelisp-bytecode-compiler-input.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-consumer.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root) nil nil t)
    (let* ((forms (nelisp-bytecode-native-package--read-elc-forms elc))
           (compiled (cdr (assq 'nelisp-test-bytecode-native-rest-function
                                (nelisp-bytecode-native-package--elc-definitions forms))))
           (input (and compiled (nelisp-bytecode-compiler-input-build compiled))))
      (unless (and input (eq (plist-get input :status) 'complete)
                   (plist-get input :rest-argument-p)
                   (plist-get input :rest-slot-return-template-p)
                   (= (plist-get input :required-argument-count) 1)
                   (= (plist-get input :initial-stack-depth) 2)
                   (equal (plist-get input :rest-native-code)
                          (unibyte-string 135)))
        (error "standalone source-free REST template was not admitted: %S" input)))
    (load (expand-file-name "lisp/nelisp-native-load.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root) nil nil t)
    (setq unit
          (nelisp-native-boxed-unit-open-rest
           artifact "nl_bc_rest_return" [] 1))
    (unwind-protect
        (progn
          (garbage-collect)
          (puthash 'required required nelisp--globals)
          (puthash 'extra-a extra-a nelisp--globals)
          (puthash 'extra-b extra-b nelisp--globals)
          (setq native-result
                (nelisp-native-boxed-unit-call-rest
                 unit (nelisp-eval '(list required extra-a extra-b))))
          (setq equivalent
                (and (equal native-result (cdr arguments))
                     (eq (car native-result) extra-a)
                     (eq (cadr native-result) extra-b)
                     (null (cddr native-result))))
          (garbage-collect)
          (setq survives-gc
                (and (eq (car native-result) extra-a)
                     (eq (cadr native-result) extra-b)))
          (condition-case nil
              (progn
                (nelisp-native-boxed-unit-call-rest unit nil)
                (setq too-few-refused nil))
            (error (setq too-few-refused t)))
          (let* ((manifest (nelisp-native-load-manifest artifact))
                 (native (plist-get manifest :native))
                 (entry (car (plist-get native :defuns)))
                 (bad-entry (plist-put (copy-sequence entry)
                                       :rest-required-count 0))
                 (bad-native (plist-put (copy-sequence native) :defuns
                                        (list bad-entry)))
                 (bad-manifest (copy-sequence manifest))
                 (bad-file (concat artifact ".tampered")))
            (setq bad-manifest (plist-put bad-manifest :native bad-native))
            (unwind-protect
                (progn
                  (with-temp-file bad-file (prin1 bad-manifest (current-buffer)))
                  (condition-case nil
                      (progn (nelisp-native-load-artifact bad-file
                                                          "nl_bc_rest_return")
                             (setq tampered-refused nil))
                    (error (setq tampered-refused t))))
              (when (file-exists-p bad-file) (delete-file bad-file))))
          (unless (and equivalent survives-gc too-few-refused tampered-refused)
            (error "native REST boundary mismatch: %S"
                   (list equivalent survives-gc too-few-refused
                         tampered-refused)))
          (list equivalent survives-gc too-few-refused tampered-refused))
      (when unit (nelisp-native-boxed-unit-close unit)))))

(provide 'standalone-bytecode-native-rest-driver)
