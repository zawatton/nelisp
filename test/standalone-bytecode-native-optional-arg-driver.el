;;; standalone-bytecode-native-optional-arg-driver.el --- packed optional argument smoke -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-optional-arg-run ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_OPTIONAL_ARG_ARTIFACT"))
         (wrong-artifact (concat artifact ".wrong-arity"))
         (effect-artifact (concat artifact ".effect"))
         (function (make-byte-code 513 (unibyte-string 135) [] 3))
         (wrong-function (make-byte-code 769 (unibyte-string 135) [] 4))
         (effect-function (make-byte-code 513 (unibyte-string 33 135) [] 3))
         (required (cons 'required-value nil))
         (supplied (cons 'mutable-optional nil))
         result wrong effect unit vm-missing native-missing vm-supplied native-supplied)
    (unless (and root artifact (not (file-exists-p artifact)))
      (error "optional argument smoke paths are invalid"))
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (load (expand-file-name "lisp/nelisp-bytecode-native-compiler.el" root)
          nil nil t)
    (setq result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_optional_arg"))
    (unless (and (eq (plist-get result :status) 'complete)
                 (file-readable-p artifact))
      (error "optional return was not compiled: %S" (plist-get result :reason)))
    (setq wrong (nelisp-bytecode-native-compiler-build
                 wrong-function wrong-artifact "nl_optional_wrong_arity")
          effect (nelisp-bytecode-native-compiler-build
                  effect-function effect-artifact "nl_optional_effect"))
    (unless (and (eq (plist-get wrong :status) 'unsupported)
                 (eq (plist-get effect :status) 'unsupported)
                 (stringp (plist-get effect :reason))
                 (plist-get (plist-get (plist-get effect :input) :ir-result)
                            :unsupported)
                 (eq (plist-get
                      (plist-get (plist-get effect :input) :frame-result)
                      :status)
                     'complete)
                 (not (file-exists-p wrong-artifact))
                 (not (file-exists-p effect-artifact)))
      (error "arity/effect negative controls failed"))
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root)
          nil nil t)
    (setq unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_optional_arg" [] 2 1))
    (unwind-protect
        (progn
          (setq vm-missing (funcall function required)
                native-missing (nelisp-native-boxed-unit-call unit (list required))
                vm-supplied (funcall function required supplied)
                native-supplied (nelisp-native-boxed-unit-call
                                 unit (list required supplied)))
          (unless (and (null vm-missing) (null native-missing)
                       (eq vm-supplied supplied) (eq native-supplied supplied))
            (error "optional VM/native results differ before GC"))
          (garbage-collect)
          (setcar supplied 'mutated-optional)
          (setcdr supplied '(retained-tail))
          (garbage-collect)
          (setq vm-supplied (funcall function required supplied)
                native-supplied (nelisp-native-boxed-unit-call
                                 unit (list required supplied)))
          (unless (and (eq vm-supplied supplied) (eq native-supplied supplied)
                       (eq (car native-supplied) 'mutated-optional)
                       (equal (cdr native-supplied) '(retained-tail)))
            (error "optional object identity/mutation changed across GC"))
          (unless (and (condition-case nil
                           (progn (funcall function) nil)
                         (wrong-number-of-arguments t))
                       (condition-case nil
                           (progn (funcall function required supplied 'extra) nil)
                         (wrong-number-of-arguments t))
                       (condition-case nil
                           (progn (nelisp-native-boxed-unit-call unit nil) nil)
                         (error t))
                       (condition-case nil
                           (progn (nelisp-native-boxed-unit-call
                                   unit (list required supplied 'extra)) nil)
                         (error t)))
            (error "VM/native wrong arity was not refused"))
          t)
      (nelisp-native-boxed-unit-close unit))))

(provide 'standalone-bytecode-native-optional-arg-driver)
;;; standalone-bytecode-native-optional-arg-driver.el ends here
