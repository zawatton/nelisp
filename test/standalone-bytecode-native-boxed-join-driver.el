;;; standalone-bytecode-native-boxed-join-driver.el --- boxed join probe -*- lexical-binding: t; -*-

(defconst nelisp-test-bytecode-native-boxed-join-code
  (unibyte-string 137 131 8 0 192 130 12 0 193 130 12 0 135))

(defun nelisp-test-bytecode-native-boxed-join-build ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_JOIN_ARTIFACT")))
    (dolist (file '("lisp/nelisp-artifact.el"
                    "lisp/nelisp-bytecode-ir.el"
                    "lisp/nelisp-bytecode-frame-ir.el"
                    "lisp/nelisp-bytecode-native-boxed-branch.el"))
      (load (expand-file-name file root) nil nil t))
    (nelisp-bytecode-native-boxed-branch-build
     nelisp-test-bytecode-native-boxed-join-code [nil nil]
     artifact "nl_bc_boxed_join" 257)
    (princ "standalone-bytecode-native-boxed-join: BUILT\n")))

(defun nelisp-test-bytecode-native-boxed-join-run ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_JOIN_ARTIFACT"))
         (left (cons 'left-value nil))
         (right (cons 'right-value nil))
         (pool (vector left right))
         (vm (make-byte-code 257 nelisp-test-bytecode-native-boxed-join-code
                             (vector left right) 2))
         unit false-vm true-vm false-native true-native)
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root) nil nil t)
    (setq unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_bc_boxed_join" pool 1))
    (unwind-protect
        (progn
          (setq false-vm (funcall vm nil))
          (garbage-collect)
          (setq false-native (nelisp-native-boxed-unit-call unit '(nil)))
          (setq true-vm (funcall vm (cons 'truthy-condition nil)))
          (garbage-collect)
          (setq true-native
                (nelisp-native-boxed-unit-call unit (list (cons 'truthy-condition nil))))
          (unless (and (eq false-vm right) (eq false-native right)
                       (eq true-vm left) (eq true-native left)
                       (eq false-vm false-native) (eq true-vm true-native))
            (error "boxed join VM/native rooted identity mismatch"))
          (list t t))
      (nelisp-native-boxed-unit-close unit))))

(provide 'standalone-bytecode-native-boxed-join-driver)
;;; standalone-bytecode-native-boxed-join-driver.el ends here
