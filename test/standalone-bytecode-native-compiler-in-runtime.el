;;; standalone-bytecode-native-compiler-in-runtime.el --- NeLisp compiler entry E2E -*- lexical-binding: t; -*-

(defun nelisp-test-public-bytecode-native-entry ()
  "Compile and call a materialized byte-code function entirely in NeLisp."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (artifact (getenv "NELISP_BC_ENTRY_ARTIFACT"))
         (bad-artifact (getenv "NELISP_BC_ENTRY_BAD_ARTIFACT"))
         (mismatch-artifact (getenv "NELISP_BC_ENTRY_MISMATCH_ARTIFACT"))
         (api-file (expand-file-name "lisp/nelisp-artifact.el" root))
         (function (make-byte-code '(x) (unibyte-string 8 135) [x] 1))
         (malformed (make-byte-code 0 (unibyte-string 192 135) [] 1))
         (value (cons 'standalone-bytecode-result nil))
         (vm-answer (funcall function value))
         (build nil) (bad-build nil) (mismatch-build nil)
         (unit nil) (native-before nil) (native-after nil))
    (unless (and root artifact bad-artifact mismatch-artifact
                 (boundp 'nelisp-bytecode-runtime-dialect-id))
      (error "standalone bytecode compiler smoke inputs are unset"))
    (load api-file nil nil t)
    (require 'nelisp-bytecode-native-compiler)
    (require 'nelisp-native-boxed-unit)
    (unless (equal nelisp-bytecode-runtime-dialect-id
                   (concat "GNU Emacs 31.1; inventory-sha256="
                           nelisp-bytecode-compiler-input--inventory-sha256))
      (error "standalone runtime bytecode identity differs from pinned inventory"))
    (setq build
          (nelisp-bytecode-native-compiler-build
           function artifact "nl_bc_public_materialized_arg"))
    (unless (and (eq (plist-get build :status) 'complete)
                 (eq vm-answer value)
                 (file-readable-p artifact))
      (error "standalone public bytecode compilation failed: %S" build))
    (unwind-protect
        (progn
          (setq unit
                (nelisp-native-boxed-unit-open-with-constants
                 artifact "nl_bc_public_materialized_arg" (aref function 2) 1))
          (garbage-collect)
          (setq native-before
                (nelisp-native-boxed-unit-call unit (list value)))
          (setcdr value 'mutated)
          (garbage-collect)
          (setq native-after
                (nelisp-native-boxed-unit-call unit (list value)))
          (unless (and (eq native-before value)
                       (eq native-after value)
                       (eq (cdr native-after) 'mutated))
            (error "standalone VM/native object identity changed across GC")))
      (when unit (nelisp-native-boxed-unit-close unit)))
    (setq bad-build
          (nelisp-bytecode-native-compiler-build
           malformed bad-artifact "nl_bc_public_malformed"))
    (unless (and (eq (plist-get bad-build :status) 'malformed)
                 (not (file-exists-p bad-artifact)))
      (error "standalone malformed bytecode control failed: %S" bad-build))
    (let ((nelisp-bytecode-runtime-dialect-id "GNU Emacs 31.2; wrong inventory"))
      (setq mismatch-build
            (nelisp-bytecode-native-compiler-build
             function mismatch-artifact "nl_bc_public_mismatch")))
    (unless (and (eq (plist-get mismatch-build :status) 'unsupported)
                 (not (file-exists-p mismatch-artifact)))
      (error "standalone dialect mismatch control failed: %S" mismatch-build))
    t))

(provide 'standalone-bytecode-native-compiler-in-runtime)
;;; standalone-bytecode-native-compiler-in-runtime.el ends here
