;;; standalone-bytecode-native-optional-default-driver.el --- S3.4b smoke -*- lexical-binding: t; -*-

(defun nelisp-test-optional-default-compile ()
  "Compile the pinned descriptor-513 default branch and reject near misses."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_OPTIONAL_DEFAULT_ARTIFACT"))
         (code (unibyte-string 137 134 5 0 1 135))
         (function (make-byte-code 513 code [] 3))
         (_ (load (expand-file-name "lisp/nelisp-bytecode-native-boxed-branch.el" root)
                  nil nil t))
         (_ (load (expand-file-name "lisp/nelisp-bytecode-native-compiler.el" root)
                  nil nil t))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_optional_default"))
         (wrong-descriptor
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 514 code [] 3)
           (concat artifact ".wrong-descriptor") "nl_wrong_descriptor"))
         (malformed
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 513 (unibyte-string 137 134 250 0 1 135) [] 3)
           (concat artifact ".malformed") "nl_malformed_branch"))
         (call
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 513 (unibyte-string 137 33 135) [] 3)
           (concat artifact ".call") "nl_unsupported_call"))
         (call-input (plist-get call :input)))
    (unless (and artifact
                 (eq (plist-get result :status) 'complete)
                 (file-readable-p artifact)
                 (eq (plist-get wrong-descriptor :status) 'unsupported)
                 (not (file-exists-p (concat artifact ".wrong-descriptor")))
                 (eq (plist-get malformed :status) 'malformed)
                 (not (file-exists-p (concat artifact ".malformed")))
                 (eq (plist-get call :status) 'unsupported)
                 (eq (plist-get (plist-get call-input :frame-result) :status) 'complete)
                 (eq (plist-get (plist-get call-input :ir-result) :status) 'unsupported)
                 (not (file-exists-p (concat artifact ".call"))))
      (error "optional-default compile/refusal failed: result=%S wrong=%S malformed=%S call=%S"
             (plist-get result :status) (plist-get wrong-descriptor :status)
             (plist-get malformed :status)
             (list (plist-get call :status)
                   (plist-get (plist-get call-input :frame-result) :status)
                   (plist-get (plist-get call-input :ir-result) :status))))
    t))

(defun nelisp-test-optional-default-native-call ()
  "Compare VM/native missing, nil, nonnil, and mutable object identities."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_OPTIONAL_DEFAULT_ARTIFACT"))
         (function (make-byte-code 513 (unibyte-string 137 134 5 0 1 135) [] 3))
         (_ (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root)
                  nil nil t))
         (unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_optional_default" [] 2 1))
         (base (cons 'base nil))
         (supplied (cons 'supplied nil))
         vm-missing native-missing vm-nil native-nil vm-value native-value)
    (unwind-protect
        (progn
          (garbage-collect)
          (setq vm-missing (funcall function base)
                native-missing (nelisp-native-boxed-unit-call unit (list base))
                vm-nil (funcall function base nil)
                native-nil (nelisp-native-boxed-unit-call unit (list base nil))
                vm-value (funcall function base supplied)
                native-value (nelisp-native-boxed-unit-call unit (list base supplied)))
          (unless (and (eq vm-missing base) (eq native-missing base)
                       (eq vm-nil base) (eq native-nil base)
                       (eq vm-value supplied) (eq native-value supplied))
            (error "optional-default VM/native identity mismatch before GC"))
          (garbage-collect)
          (setcar supplied 'mutated)
          (setcdr supplied (list 'tail))
          (garbage-collect)
          (setq vm-value (funcall function base supplied)
                native-value (nelisp-native-boxed-unit-call unit (list base supplied)))
          (unless (and (eq vm-value supplied) (eq native-value supplied)
                       (eq (car native-value) 'mutated)
                       (equal (cdr native-value) '(tail)))
            (error "optional-default identity/mutation mismatch after forced GC"))
          t)
      (nelisp-native-boxed-unit-close unit))))

(provide 'standalone-bytecode-native-optional-default-driver)
;;; standalone-bytecode-native-optional-default-driver.el ends here
