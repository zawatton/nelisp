;;; standalone-bytecode-native-constant-driver.el --- verified CFG native probe -*- lexical-binding: t; -*-

(defvar nelisp-test-bytecode-native-special-argument nil)

(defun nelisp-test-bytecode-native-constant-build ()
  "Build a source-free `.neln' from byte-code CONST; RETURN and negatives."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (api-root (getenv "NELISP_ARTIFACT_API_ROOT"))
         (artifact (getenv "NELISP_BC_NATIVE_ARTIFACT"))
         (wrong-abi (getenv "NELISP_BC_NATIVE_WRONG_ABI"))
         (malformed (getenv "NELISP_BC_NATIVE_MALFORMED"))
         (branch (getenv "NELISP_BC_NATIVE_BRANCH"))
         (call (getenv "NELISP_BC_NATIVE_CALL"))
         (maximum (getenv "NELISP_BC_NATIVE_MAXIMUM"))
         (too-many (getenv "NELISP_BC_NATIVE_TOO_MANY"))
         (zero-args (getenv "NELISP_BC_NATIVE_ZERO_ARGS"))
         (unknown-var (getenv "NELISP_BC_NATIVE_UNKNOWN_VAR"))
         (special-var (getenv "NELISP_BC_NATIVE_SPECIAL_VAR"))
         (duplicate-args (getenv "NELISP_BC_NATIVE_DUPLICATE_ARGS"))
         (optional-args (getenv "NELISP_BC_NATIVE_OPTIONAL_ARGS"))
         (api-file (expand-file-name "lisp/nelisp-artifact.el" api-root))
         (code (unibyte-string 192 193 1 137 136 136 10 135))
         (source-constants (vector (cons 'first-build-constant nil)
                                  (cons 'second-build-constant nil)
                                  'user-argument))
         (bytecode-function (make-byte-code '(user-argument)
                                            code source-constants 4))
         (constants (aref bytecode-function 2))
         (argument-symbols (aref bytecode-function 0))
         result maximum-result malformed-rejected branch-rejected call-rejected
         maximum-rejected too-many-rejected zero-args-rejected unknown-var-rejected)
    (unless (and root api-root artifact wrong-abi malformed branch call
                 maximum too-many zero-args unknown-var special-var
                 duplicate-args optional-args)
      (error "bytecode-native smoke paths are unset"))
    ;; Load the source-free writer from its clean current-HEAD patch tree;
    ;; its dependencies resolve through the isolated implementation tree.
    (load api-file nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-ir.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-frame-ir.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-constant.el" root)
          nil nil t)
    (setq result
          (nelisp-bytecode-native-constant-return-build
           code constants artifact "nl_bc_const_return" 1 argument-symbols))
    (unless (and (eq (plist-get result :status) 'complete)
                 (= (plist-get result :return-argument-index) 3)
                 (equal (string-to-list (plist-get result :machine-code))
                        '(72 137 200 72 137 236 93 195))
                 (equal (mapcar (lambda (insn) (plist-get insn :kind))
                                (append
                                 (plist-get
                                  (aref (plist-get (plist-get result :verified-cfg)
                                                   :blocks)
                                        0)
                                  :instructions)
                                 nil))
                        '(constant constant stack-ref dup discard discard
                                   variable-ref return)))
      (error "bytecode-native CFG lowering mismatch: %S" result))
    (condition-case error-data
        (nelisp-bytecode-native-constant-return-build
         (unibyte-string 0) constants malformed "nl_bc_malformed" 0)
      (error
       (setq malformed-rejected
             (and (string-match-p "byte-code rejected"
                                  (error-message-string error-data))
                  (not (file-exists-p malformed))))))
    (unless malformed-rejected
      (error "malformed byte-code negative control failed"))
    (condition-case error-data
        (nelisp-bytecode-native-constant-return-build
         (unibyte-string 130 3 0 192 135) (vector (cons 'branch nil))
         branch "nl_bc_branch" 0)
      (error
       (setq branch-rejected
             (and (string-match-p "unsupported control-flow"
                                  (error-message-string error-data))
                  (not (file-exists-p branch))))))
    (unless branch-rejected
      (error "branch negative control failed"))
    (condition-case error-data
        (nelisp-bytecode-native-constant-return-build
         (unibyte-string 192 32 135) [nil]
         call "nl_bc_call" 0)
      (error
       (setq call-rejected
             (and (string-match-p "unsupported call effect"
                                  (error-message-string error-data))
                  (not (file-exists-p call))))))
    (unless call-rejected
      (error "call negative control failed"))
    (let ((pool (vector (cons 'max-0 nil) (cons 'max-1 nil)
                        (cons 'max-2 nil) (cons 'max-3 nil)
                        (cons 'max-4 nil) (cons 'max-5 nil))))
      (setq maximum-result
            (nelisp-bytecode-native-constant-return-build
             (unibyte-string 197 135) pool maximum "nl_bc_const6_return" 0))
      (unless (and (eq (plist-get maximum-result :status) 'complete)
                   (= (plist-get maximum-result :return-argument-index) 5)
                   (= (+ (length (plist-get maximum-result :constants))
                         (plist-get maximum-result :user-arity))
                      6))
        (error "six-argument CFG build mismatch: %S" maximum-result))
      (condition-case error-data
          (nelisp-bytecode-native-constant-return-build
           (unibyte-string 197 135) pool too-many "nl_bc_too_many" 1)
        (error
         (setq too-many-rejected
               (and (string-match-p "exceeds six" (error-message-string error-data))
                    (not (file-exists-p too-many))))))
      (condition-case error-data
          (nelisp-bytecode-native-constant-return-build
           (unibyte-string 192 135) [] zero-args "nl_bc_zero_args" 0)
        (error
         (setq zero-args-rejected
               (and (string-match-p "zero-argument result"
                                    (error-message-string error-data))
                    (not (file-exists-p zero-args)))))))
    (unless (and too-many-rejected zero-args-rejected)
      (error "arity boundary negative controls failed"))
    (condition-case error-data
        (nelisp-bytecode-native-constant-return-build
         (unibyte-string 8 135) [user-argument]
         unknown-var "nl_bc_unknown_var" 1)
      (error
       (setq unknown-var-rejected
             (and (string-match-p "not a declared user argument"
                                  (error-message-string error-data))
                  (not (file-exists-p unknown-var))))))
    (unless unknown-var-rejected
      (error "unknown variable-ref negative control failed"))
    (dolist (case `((,special-var (nelisp-bytecode-native-constant-return-build
                                  (unibyte-string 8 135)
                                  [nelisp-test-bytecode-native-special-argument]
                                  ,special-var "nl_bc_special_arg" 1
                                  '(nelisp-test-bytecode-native-special-argument)))
                    (,duplicate-args (nelisp-bytecode-native-constant-return-build
                                      (unibyte-string 192 135) [(a)]
                                      ,duplicate-args "nl_bc_duplicate_arg" 2 '(x x)))
                    (,optional-args (nelisp-bytecode-native-constant-return-build
                                     (unibyte-string 192 135) [(a)]
                                     ,optional-args "nl_bc_optional_arg" 2
                                     '(x &optional y)))))
      (condition-case error-data
          (eval (cadr case))
        (error
         (unless (and (string-match-p "unsupported positional argument descriptor"
                                      (error-message-string error-data))
                      (not (file-exists-p (car case))))
           (error "argument descriptor negative control failed: %S" case)))))
    ;; Change the entry's declared argument representation to create an
    ;; invalid ABI control without changing the valid compiled artifact.
    (let* ((text (with-temp-buffer
                   (insert-file-contents artifact)
                   (buffer-string)))
           (needle ":param-repr sexp-ptr"))
      (unless (string-match-p needle text)
        (error "compiled artifact lacks parameter representation metadata"))
      (with-temp-file wrong-abi
        (insert (replace-regexp-in-string
                 needle ":param-repr raw-i64" text nil t))))
    (princ (format "artifact=%s\nwrong-abi=%s\n" artifact wrong-abi))))

(defun nelisp-test-bytecode-native-constant-run ()
  "Verify constant-root lifetime, direct native return, and wrong ABI refusal."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BC_NATIVE_ARTIFACT"))
         (wrong-abi (getenv "NELISP_BC_NATIVE_WRONG_ABI"))
         (maximum (getenv "NELISP_BC_NATIVE_MAXIMUM"))
         (unit nil)
         (closed-unit nil)
         (identity-before nil)
         (identity-after nil)
         (closed-rejected nil)
         (pool-cleared nil)
         (wrong-abi-rejected nil)
         (maximum-before nil)
         (maximum-after nil))
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root)
          nil nil t)
    (let* ((code (unibyte-string 192 193 1 137 136 136 10 135))
           (constants (vector (cons 'bytecode-constant-0 nil)
                              (cons 'bytecode-constant-1 nil)
                              'user-argument))
           (bytecode-function (make-byte-code '(user-argument)
                                              code constants 4))
           (function-constants (aref bytecode-function 2))
           (user-value (cons 'runtime-user-argument nil))
           (vm-answer (funcall bytecode-function user-value)))
      (unless (eq function-constants constants)
        (error "runtime byte-code constructor replaced its constant vector"))
      (setq unit
            (nelisp-native-boxed-unit-open-with-constants
             artifact "nl_bc_const_return" function-constants 1))
      (garbage-collect)
      (let ((answer (nelisp-native-boxed-unit-call unit (list user-value))))
        (setq identity-before
              (and (eq answer vm-answer)
                   (eq answer user-value)))
        (garbage-collect)
        (setq identity-after
              (and (eq answer (funcall bytecode-function user-value))
                   (eq answer user-value))))
      (nelisp-native-boxed-unit-close unit)
      (setq closed-unit unit
            unit nil
            pool-cleared (null (aref closed-unit 2)))
      (condition-case nil
          (progn (nelisp-native-boxed-unit-call closed-unit (list user-value)) nil)
        (error (setq closed-rejected t))))
    (condition-case error-data
        (nelisp-native-boxed-unit-open-with-constants
         wrong-abi "nl_bc_const_return" [nil] 1)
      (error
       (setq wrong-abi-rejected
             (and (string-match-p "explicit boxed Sexp-to-Sexp repr"
                                  (error-message-string error-data))
                  t))))
    (let* ((max-code (unibyte-string 197 135))
           (max-constants (vector (cons 'max-0 nil) (cons 'max-1 nil)
                                  (cons 'max-2 nil) (cons 'max-3 nil)
                                  (cons 'max-4 nil) (cons 'max-5 nil)))
           (max-function (make-byte-code 0 max-code max-constants 1))
           (function-constants (aref max-function 2))
           (max-unit
            (nelisp-native-boxed-unit-open-with-constants
             maximum "nl_bc_const6_return" function-constants 0)))
      (unless (eq function-constants max-constants)
        (error "maximum-arity byte-code changed its constant vector"))
      (garbage-collect)
      (let ((answer (nelisp-native-boxed-unit-call max-unit nil)))
        (setq maximum-before
              (and (eq answer (funcall max-function))
                   (eq answer (aref function-constants 5))))
        (garbage-collect)
        (setq maximum-after
              (and (eq answer (funcall max-function))
                   (eq answer (aref function-constants 5)))))
      (nelisp-native-boxed-unit-close max-unit))
    (unless (and identity-before identity-after pool-cleared closed-rejected
                 wrong-abi-rejected maximum-before maximum-after)
      (error "bytecode-native result failed: %S"
             (list identity-before identity-after pool-cleared
                   closed-rejected wrong-abi-rejected
                   maximum-before maximum-after)))
    (list identity-before identity-after pool-cleared closed-rejected
          wrong-abi-rejected maximum-before maximum-after)))

(provide 'standalone-bytecode-native-constant-driver)
;;; standalone-bytecode-native-constant-driver.el ends here
