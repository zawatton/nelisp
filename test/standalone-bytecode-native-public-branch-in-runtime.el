;;; standalone-bytecode-native-public-branch-in-runtime.el --- public packed branch E2E -*- lexical-binding: t; -*-

(defun nelisp-test-public-packed-branch-entry ()
  "Compile, load, and call public packed branch and join artifacts in NeLisp."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (branch-artifact (getenv "NELISP_PUBLIC_BRANCH_ARTIFACT"))
         (join-artifact (getenv "NELISP_PUBLIC_JOIN_ARTIFACT"))
         (call-artifact (getenv "NELISP_PUBLIC_CALL_ARTIFACT"))
         (bad-artifact (getenv "NELISP_PUBLIC_BAD_ARTIFACT"))
         (branch-code (unibyte-string 137 134 6 0 192 135 135))
         (join-code (unibyte-string 137 131 8 0 192 130 12 0
                                    193 130 12 0 135))
         (branch-value (cons 'branch-result nil))
         (left (cons 'left-result nil))
         (right (cons 'right-result nil))
         (truthy (cons 'condition nil))
         (branch-function (make-byte-code 257 branch-code
                                          (vector branch-value) 2))
         (join-function (make-byte-code 257 join-code
                                        (vector left right) 2))
         (call-function (make-byte-code
                         257 (unibyte-string 137 131 7 0 32 135 192 135)
                         [nil] 2))
         (bad-function (make-byte-code 257 (unibyte-string 131) [] 1))
         (branch-build nil) (join-build nil) (call-build nil) (bad-build nil)
         (branch-unit nil) (join-unit nil)
         (branch-false-vm nil) (branch-true-vm nil)
         (branch-false-native nil) (branch-true-native nil)
         (join-false-vm nil) (join-true-vm nil)
         (join-false-native nil) (join-true-native nil))
    (unless (and root branch-artifact join-artifact call-artifact bad-artifact
                 (boundp 'nelisp-bytecode-runtime-dialect-id))
      (error "public packed branch smoke inputs are unset"))
    (load (expand-file-name "lisp/nelisp-artifact.el" root) nil nil t)
    (require 'nelisp-bytecode-native-compiler)
    (require 'nelisp-native-boxed-unit)
    (unless (equal nelisp-bytecode-runtime-dialect-id
                   (concat "GNU Emacs 31.1; inventory-sha256="
                           nelisp-bytecode-compiler-input--inventory-sha256))
      (error "standalone runtime bytecode identity differs from pinned inventory"))
    (setq branch-build
          (nelisp-bytecode-native-compiler-build
           branch-function branch-artifact "nl_public_packed_branch")
          join-build
          (nelisp-bytecode-native-compiler-build
           join-function join-artifact "nl_public_packed_join")
          call-build
          (nelisp-bytecode-native-compiler-build
           call-function call-artifact "nl_public_packed_call")
          bad-build
          (nelisp-bytecode-native-compiler-build
           bad-function bad-artifact "nl_public_packed_bad"))
    (unless (and (eq (plist-get branch-build :status) 'complete)
                 (eq (plist-get join-build :status) 'complete)
                 (eq (plist-get call-build :status) 'unsupported)
                 (eq (plist-get bad-build :status) 'malformed)
                 (file-readable-p branch-artifact)
                 (file-readable-p join-artifact)
                 (not (file-exists-p call-artifact))
                 (not (file-exists-p bad-artifact)))
      (error "public packed compile or refusal failed: %S %S %S %S"
             branch-build join-build call-build bad-build))
    (setq branch-unit
          (nelisp-native-boxed-unit-open-with-constants
           branch-artifact "nl_public_packed_branch" (aref branch-function 2) 1)
          join-unit
          (nelisp-native-boxed-unit-open-with-constants
           join-artifact "nl_public_packed_join" (aref join-function 2) 1))
    (unwind-protect
        (progn
          (setq branch-false-vm (funcall branch-function nil))
          (garbage-collect)
          (setq branch-false-native
                (nelisp-native-boxed-unit-call branch-unit '(nil)))
          (setq branch-true-vm (funcall branch-function truthy))
          (garbage-collect)
          (setq branch-true-native
                (nelisp-native-boxed-unit-call branch-unit (list truthy)))
          (setq join-false-vm (funcall join-function nil))
          (garbage-collect)
          (setq join-false-native
                (nelisp-native-boxed-unit-call join-unit '(nil)))
          (setq join-true-vm (funcall join-function truthy))
          (garbage-collect)
          (setq join-true-native
                (nelisp-native-boxed-unit-call join-unit (list truthy)))
          (unless (and (eq branch-false-vm branch-value)
                       (eq branch-false-native branch-value)
                       (eq branch-false-vm branch-false-native)
                       (eq branch-true-vm truthy)
                       (eq branch-true-native truthy)
                       (eq branch-true-vm branch-true-native)
                       (eq join-false-vm right)
                       (eq join-false-native right)
                       (eq join-false-vm join-false-native)
                       (eq join-true-vm left)
                       (eq join-true-native left)
                       (eq join-true-vm join-true-native))
            (error "public packed branch/join VM/native parity failed"))
          t)
      (when branch-unit (nelisp-native-boxed-unit-close branch-unit))
      (when join-unit (nelisp-native-boxed-unit-close join-unit)))))

(provide 'standalone-bytecode-native-public-branch-in-runtime)
;;; standalone-bytecode-native-public-branch-in-runtime.el ends here
