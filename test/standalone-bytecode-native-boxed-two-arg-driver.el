;;; standalone-bytecode-native-boxed-two-arg-driver.el --- two-arg boxed branch probe -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-boxed-two-arg-run ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_TWO_ARG_ARTIFACT"))
         (hidden (cons 'hidden-result nil))
         (constants (vector hidden))
         (function (make-byte-code 514
                                   (unibyte-string 137 134 6 0 192 135 135)
                                   [nil] 3))
         (arg1 (cons 'first-argument nil))
         (arg2 (cons 'mutable-second nil))
         (wrong-artifact (concat artifact ".wrong-arity"))
         (call-artifact (concat artifact ".call"))
         unit result wrong call vm-false native-false vm-true native-true)
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (load (expand-file-name "lisp/nelisp-bytecode-native-compiler.el" root)
          nil nil t)
    (aset (aref function 2) 0 hidden)
    (setq result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_boxed_two_arg"))
    (unless (eq (plist-get result :status) 'complete)
      (error "standalone packed514 compiler refused input: %S"
             (plist-get result :reason)))
    (setq wrong
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 771 (unibyte-string 137 134 6 0 192 135 135)
                           [hidden] 4)
           wrong-artifact "nl_boxed_wrong_arity")
          call
          (nelisp-bytecode-native-compiler-build
           (make-byte-code 514 (unibyte-string 137 131 7 0 32 135 192 135)
                           [hidden] 4)
           call-artifact "nl_boxed_call"))
    (unless (and (eq (plist-get wrong :status) 'unsupported)
                 (eq (plist-get call :status) 'unsupported)
                 (not (file-exists-p wrong-artifact))
                 (not (file-exists-p call-artifact)))
      (error "standalone public compiler negative controls failed"))
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root) nil nil t)
    (setq unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_boxed_two_arg" constants 2))
    (unwind-protect
        (progn
          (setq vm-false (funcall function arg1 nil)
                native-false (nelisp-native-boxed-unit-call unit (list arg1 nil))
                vm-true (funcall function arg1 arg2)
                native-true (nelisp-native-boxed-unit-call unit (list arg1 arg2)))
          (unless (and (eq vm-false hidden) (eq native-false hidden)
                       (eq vm-true arg2) (eq native-true arg2))
            (error "packed514 VM/native identity mismatch before mutation"))
          (garbage-collect)
          (setcar arg2 'mutated-second)
          (setcdr arg2 (list 'tail))
          (garbage-collect)
          (setq vm-true (funcall function arg1 arg2)
                native-true (nelisp-native-boxed-unit-call unit (list arg1 arg2)))
          (unless (and (eq vm-true arg2) (eq native-true arg2)
                       (eq (car native-true) 'mutated-second)
                       (equal (cdr native-true) '(tail)))
            (error "packed514 identity/mutation mismatch after GC"))
          (list (eq vm-false native-false) (eq vm-true native-true)
                (eq (car native-true) 'mutated-second)))
      (nelisp-native-boxed-unit-close unit))))

(provide 'standalone-bytecode-native-boxed-two-arg-driver)
;;; standalone-bytecode-native-boxed-two-arg-driver.el ends here
