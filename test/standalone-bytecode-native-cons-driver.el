;;; standalone-bytecode-native-cons-driver.el --- Raw-v2 CONS E2E -*- lexical-binding: t; -*-

(let* ((root (getenv "NELISP_REPO_ROOT"))
       (load-prefer-newer t))
  (unless (and root (file-directory-p root))
    (error "bytecode CONS smoke repository root is unset"))
  (dolist (relative '("lisp/nelisp-bytecode-ir.el"
                      "lisp/nelisp-bytecode-frame-ir.el"
                      "lisp/nelisp-bytecode-compiler-input.el"
                      "lisp/nelisp-bytecode-native-compiler.el"
                      "lisp/nelisp-bytecode-native-package.el"
                      "src/nelisp-gnu-bytecode-vm.el"))
    (load (expand-file-name relative root) nil t)))

(require 'nelisp-bytecode-native-package)
(require 'nelisp-native-load)
(require 'nelisp-gnu-bytecode-vm)

(defun nelisp-test-public-bytecode-native-cons ()
  "Compile source-free GNU ELC and compare VM and authenticated native CONS."
  (let* ((elc (getenv "NELISP_BC_CONS_ELC"))
         (artifact (getenv "NELISP_BC_CONS_ARTIFACT"))
         (bad-artifact (getenv "NELISP_BC_CONS_BAD_ARTIFACT"))
         (forms (nelisp-bytecode-native-package--read-elc-forms elc))
         (function (cdr (assq 'nelisp_bytecode_cons_probe
                              (nelisp-bytecode-native-package--elc-definitions
                               forms))))
         (input (nelisp-bytecode-compiler-input-build function))
         (build (nelisp-bytecode-native-compiler-build
                 function artifact "nl_native_cons_probe"))
         (bad-function (make-byte-code 514 (unibyte-string 1 1 66 135)
                                       [nil] 4))
         (bad-build (nelisp-bytecode-native-compiler-build
                     bad-function bad-artifact "nl_native_cons_probe"))
         (vm-function
          (nelisp-gnu-bytecode-vm--lower-function
           function (make-hash-table :test 'eq) (make-hash-table :test 'eq)))
         (handle nil)
         (left (cons 'left (list 'before-gc)))
         (right (cons 'right (list 'before-gc)))
         (vm-result (nelisp-bc-run vm-function (list left right)))
         (native-result nil)
         (gc nil)
         (mutation nil)
         (wrong-arity nil)
         (import-mutation nil))
    (unless (and (eq (plist-get input :status) 'complete)
                 (equal (plist-get input :code) (unibyte-string 1 1 66 135))
                 (eq (plist-get build :status) 'complete)
                 (eq (plist-get build :artifact-kind) 'raw-runtime-v2)
                 (eq (plist-get build :return-repr) 'u64)
                 (eq (plist-get build :evaluator-return-repr) 'sexp)
                 (eq (plist-get bad-build :status) 'unsupported)
                 (not (file-exists-p bad-artifact)))
      (error "bytecode CONS compile admission mismatch: %S" build))
    (setq handle
          (nelisp-native-load-raw-v2-artifact
           artifact "nl_native_cons_probe"
           (nelisp-native-load-running-binary-sha256)))
    (setq native-result
          (nelisp-native-load-raw-v2-cons-call handle left right))
    (unless (and (consp vm-result) (consp native-result)
                 (eq (car vm-result) left) (eq (cdr vm-result) right)
                 (eq (car native-result) left) (eq (cdr native-result) right))
      (error "GNU VM/native CONS identity mismatch"))
    (setq gc (garbage-collect))
    (setcdr left '(after-gc))
    (setq mutation
          (and gc (equal (cdr (car vm-result)) '(after-gc))
               (equal (cdr (car native-result)) '(after-gc))))
    (setq wrong-arity
          (condition-case nil
              (progn (nelisp-native-load-raw-v2-cons-call handle left) nil)
            (wrong-number-of-arguments t)))
    (let ((mutated (copy-sequence handle)))
      (setq mutated
            (plist-put mutated :imports
                       '("nl_native_cons_v2" "nl_native_cdr_v2")))
      (setq import-mutation
            (condition-case nil
                (progn
                  (nelisp-native-load-raw-v2-cons-call mutated left right)
                  nil)
              (error t))))
    (unless (and mutation wrong-arity import-mutation)
      (error "bytecode CONS parity/refusal mismatch: %S"
             (list mutation wrong-arity import-mutation)))
    t))

(nelisp-test-public-bytecode-native-cons)

;;; standalone-bytecode-native-cons-driver.el ends here
