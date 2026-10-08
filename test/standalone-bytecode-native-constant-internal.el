;;; standalone-bytecode-native-constant-internal.el --- in-runtime native constant test -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-constant-internal-run ()
  "Compile, load, call, and GC-root a boxed constant entirely in NeLisp."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BC_NATIVE_ARTIFACT"))
         (wrong-abi (getenv "NELISP_BC_NATIVE_WRONG_ABI"))
         (malformed (getenv "NELISP_BC_NATIVE_MALFORMED"))
         (object (cons 'bytecode-constant nil))
         (constants (vector object))
         (code (unibyte-string 192 135))
         (build nil)
         (unit nil)
         (closed nil)
         (identity-before nil)
         (identity-after nil)
         (pool-cleared nil)
         (closed-rejected nil)
         (malformed-rejected nil)
         (wrong-abi-rejected nil))
    (unless (and root artifact wrong-abi malformed)
      (error "bytecode-native internal smoke paths are unset"))
    (load (expand-file-name "lisp/nelisp-artifact.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-ir.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-frame-ir.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-constant.el" root)
          nil nil t)
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root)
          nil nil t)
    (setq build
          (nelisp-bytecode-native-constant-return-build
           code constants artifact "nl_bc_const_internal" 0))
    (unless (and (eq (plist-get build :status) 'complete)
                 (= (plist-get build :return-argument-index) 0)
                 (equal (string-to-list (plist-get build :machine-code))
                        '(72 137 248 72 137 236 93 195)))
      (error "bytecode-native in-runtime compile failed: %S" build))
    (condition-case error-data
        (nelisp-bytecode-native-constant-return-build
         (unibyte-string 0) constants malformed "nl_bc_bad" 0)
      (error
       (setq malformed-rejected
             (and (string-match-p "byte-code rejected"
                                  (error-message-string error-data))
                  (not (file-exists-p malformed))))))
    (unless malformed-rejected
      (error "bytecode-native malformed control failed"))
    (with-temp-file wrong-abi
      (insert-file-contents artifact)
      (goto-char (point-min))
      (unless (search-forward ":param-repr sexp-ptr" nil t)
        (error "bytecode-native artifact lacks boxed metadata"))
      (replace-match ":param-repr raw-i64" t t))
    (unwind-protect
        (progn
          (setq unit
                (nelisp-native-boxed-unit-open-with-constants
                 artifact "nl_bc_const_internal" constants 0))
          (garbage-collect)
          (let ((answer (nelisp-native-boxed-unit-call unit nil)))
            (setq identity-before
                  (and (eq answer object)
                       (eq answer (aref constants 0))
                       (eq answer (aref (aref unit 2) 0)))
                  identity-after
                  (progn
                    (garbage-collect)
                    (and (eq answer object)
                         (eq answer (aref constants 0))
                         (eq answer (aref (aref unit 2) 0))))))
          (nelisp-native-boxed-unit-close unit)
          (setq closed unit unit nil
                pool-cleared (null (aref closed 2)))
          (condition-case nil
              (progn (nelisp-native-boxed-unit-call closed nil) nil)
            (error (setq closed-rejected t))))
      (when unit
        (nelisp-native-boxed-unit-close unit)))
    (condition-case error-data
        (nelisp-native-boxed-unit-open-with-constants
         wrong-abi "nl_bc_const_internal" [nil] 0)
      (error
       (setq wrong-abi-rejected
             (and (string-match-p "explicit boxed Sexp-to-Sexp repr"
                                  (error-message-string error-data))
                  t))))
    (unless (and identity-before identity-after pool-cleared closed-rejected
                 wrong-abi-rejected)
      (error "bytecode-native in-runtime call failed: %S"
             (list identity-before identity-after pool-cleared closed-rejected
                   wrong-abi-rejected)))
    (list identity-before identity-after pool-cleared malformed-rejected
          closed-rejected wrong-abi-rejected)))

(provide 'standalone-bytecode-native-constant-internal)
;;; standalone-bytecode-native-constant-internal.el ends here
