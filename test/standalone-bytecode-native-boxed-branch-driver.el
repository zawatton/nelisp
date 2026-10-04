;;; standalone-bytecode-native-boxed-branch-driver.el --- boxed branch probe -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-boxed-branch-build ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_BRANCH_ARTIFACT"))
         (code (unibyte-string 137 134 6 0 192 135 135))
         (constants [nil])
         (rooted (cons 'branch-return nil))
         result refused)
    (aset constants 0 rooted)
    (dolist (file '("lisp/nelisp-artifact.el"
                    "lisp/nelisp-bytecode-ir.el"
                    "lisp/nelisp-bytecode-frame-ir.el"
                    "lisp/nelisp-bytecode-native-boxed-branch.el"))
      (load (expand-file-name file root) nil nil t))
    (setq result
          (nelisp-bytecode-native-boxed-branch-build
           code constants artifact "nl_bc_boxed_branch" 257))
    (condition-case nil
        (nelisp-bytecode-native-boxed-branch-build
         (unibyte-string 137 131 7 0 32 135 192 135) constants
         (concat artifact ".unsupported") "nl_bc_boxed_call" 257)
      (error (setq refused t)))
    (unless (and (eq (plist-get result :status) 'complete) refused)
      (error "boxed branch build or call refusal failed"))
    (princ "standalone-bytecode-native-boxed-branch: BUILT\n")))

(defun nelisp-test-bytecode-native-boxed-branch-run ()
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BOXED_BRANCH_ARTIFACT"))
         (pool [(cons 'branch-return nil)])
         unit false-result t-result cons-arg cons-result)
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root) nil nil t)
    (setq unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_bc_boxed_branch" pool 1))
    (unwind-protect
        (progn
          (garbage-collect)
          (setq false-result (nelisp-native-boxed-unit-call unit '(nil))
                t-result (nelisp-native-boxed-unit-call unit '(t))
                cons-arg (cons 'truthy-condition nil)
                cons-result (nelisp-native-boxed-unit-call unit (list cons-arg)))
          (unless (and (eq false-result (aref pool 0))
                       (eq t-result t) (eq cons-result cons-arg))
            (error "boxed branch truthiness/identity mismatch: %S"
                   (list (eq false-result (aref pool 0)) (eq t-result t)
                         (eq cons-result cons-arg) false-result t-result
                         cons-result cons-arg (aref pool 0))))
          (list (eq false-result (aref pool 0)) (eq t-result t)
                (eq cons-result cons-arg)))
      (nelisp-native-boxed-unit-close unit))))

(defun nelisp-test-bytecode-native-optional-elc-build ()
  "Build native entries from the materialized GNU optional .elc functions."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_OPTIONAL_ELC"))
         (or-artifact (getenv "NELISP_OPTIONAL_OR_ARTIFACT"))
         (and-artifact (getenv "NELISP_OPTIONAL_AND_ARTIFACT"))
         or-result and-result bad-result)
    (load (expand-file-name "lisp/nelisp-bytecode-compiler-input.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-constant.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-boxed-branch.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-bytecode-native-compiler.el" root) nil nil t)
    (load elc nil nil t)
    (let* ((or-function (symbol-function 'nelisp-native-optional-or))
           (and-function (symbol-function 'nelisp-native-optional-and))
           (code (copy-sequence (aref or-function 1)))
           (bad-path (concat and-artifact ".mutated")))
      (setq or-result
            (nelisp-bytecode-native-compiler-build
             or-function or-artifact "nl_bc_optional_or")
            and-result
            (nelisp-bytecode-native-compiler-build
             and-function and-artifact "nl_bc_optional_and"))
      ;; Redirect the taken edge onto the fallthrough block. This preserves
      ;; valid instruction decoding but breaks the verified branch dataflow.
      (aset code 2 4)
      (setq bad-result
            (nelisp-bytecode-native-compiler-build
             (make-byte-code (aref or-function 0) code
                             (aref or-function 2) (aref or-function 3))
             bad-path "nl_bc_optional_mutated"))
      (unless (and (equal (string-to-list (aref or-function 1))
                          '(8 134 5 0 9 135))
                   (equal (string-to-list (aref and-function 1))
                          '(8 133 5 0 9 135))
                   (eq (plist-get or-result :status) 'complete)
                   (eq (plist-get and-result :status) 'complete)
                   (memq (plist-get bad-result :status) '(unsupported malformed))
                   (file-readable-p or-artifact) (file-readable-p and-artifact)
                   (not (file-exists-p bad-path)))
        (error "optional .elc compile/refusal failed: or=%S and=%S mutated=%S"
               (plist-get or-result :status) (plist-get and-result :status)
               (plist-get bad-result :status))))
    (princ "optional-elc: BUILT\n")))

(defun nelisp-test-bytecode-native-optional-elc-run ()
  "Check source-free VM/native parity and rooted identity for optional .elc."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_OPTIONAL_ELC"))
         (or-artifact (getenv "NELISP_OPTIONAL_OR_ARTIFACT"))
         (and-artifact (getenv "NELISP_OPTIONAL_AND_ARTIFACT"))
         (or-unit nil) (and-unit nil)
         (or-value (cons 'or-rooted-value nil))
         (or-supplied (cons 'or-rooted-supplied nil))
         (and-value (cons 'and-rooted-value nil))
         (and-supplied (cons 'and-rooted-supplied nil))
         or-vm-default or-vm-supplied and-vm-false and-vm-true
         or-native-default or-native-supplied and-native-false and-native-true)
    (load elc nil nil t)
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" root) nil nil t)
    (setq or-unit (nelisp-native-boxed-unit-open-with-constants
                   or-artifact "nl_bc_optional_or" [] 2 1)
          and-unit (nelisp-native-boxed-unit-open-with-constants
                    and-artifact "nl_bc_optional_and" [] 2 1))
    (unwind-protect
        (progn
          (garbage-collect)
          (setq or-vm-default (funcall 'nelisp-native-optional-or or-value)
                or-vm-supplied (funcall 'nelisp-native-optional-or
                                        or-value or-supplied)
                and-vm-false (funcall 'nelisp-native-optional-and
                                      and-value nil)
                and-vm-true (funcall 'nelisp-native-optional-and
                                     and-value and-supplied)
                or-native-default (nelisp-native-boxed-unit-call or-unit
                                                               (list or-value))
                or-native-supplied (nelisp-native-boxed-unit-call or-unit
                                                                (list or-value or-supplied))
                and-native-false (nelisp-native-boxed-unit-call and-unit
                                                                (list and-value nil))
                and-native-true (nelisp-native-boxed-unit-call and-unit
                                                               (list and-value and-supplied)))
          (garbage-collect)
          (unless (and (eq or-vm-default or-value)
                       (eq or-vm-supplied or-supplied)
                       (null and-vm-false) (eq and-vm-true and-value)
                       (eq or-native-default or-value)
                       (eq or-native-supplied or-supplied)
                       (null and-native-false) (eq and-native-true and-value))
            (error "optional .elc VM/native or GC identity mismatch"))
          (list t t t t t t t t))
      (when or-unit (nelisp-native-boxed-unit-close or-unit))
      (when and-unit (nelisp-native-boxed-unit-close and-unit)))))

(provide 'standalone-bytecode-native-boxed-branch-driver)
;;; standalone-bytecode-native-boxed-branch-driver.el ends here
