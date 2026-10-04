;;; standalone-bytecode-native-boxed-numeric-gc-driver.el --- boxed numeric GC acceptance -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun nelisp-test-bytecode-native-boxed-numeric-gc-build ()
  "Compile a materialized identity byte-code function to a native artifact."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (api-root (or (getenv "NELISP_ARTIFACT_API_ROOT") root))
         (artifact (getenv "NELISP_BC_ENTRY_ARTIFACT"))
         (function (make-byte-code '(value) (unibyte-string 8 135)
                                   (vector 'value) 1))
         result)
    (unless (and root artifact (not (file-exists-p artifact)))
      (error "boxed numeric GC smoke paths are invalid"))
    (load (expand-file-name "lisp/nelisp-artifact.el" api-root) nil nil t)
    (require 'nelisp-bytecode-native-compiler)
    (load (expand-file-name "lisp/nelisp-native-load.el" root) nil nil t)
    (setq result
          (nelisp-bytecode-native-compiler-build
           function artifact "nl_bc_boxed_numeric_identity"))
    (unless (and (eq (plist-get result :status) 'complete)
                 (file-readable-p artifact))
      (error "materialized boxed identity compilation failed: %S" result))
    result))

(defun nelisp-test-bytecode-native-boxed-numeric-gc-run ()
  "Check numeric identity and mutable cons semantics across forced GC."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (api-root (or (getenv "NELISP_ARTIFACT_API_ROOT") root))
         (artifact (getenv "NELISP_BC_ENTRY_ARTIFACT"))
         (function (make-byte-code '(value) (unibyte-string 8 135)
                                   (vector 'value) 1))
         (constants (aref function 2))
         (bignum (1+ most-positive-fixnum))
         (negative-zero -0.0)
         (smallest-subnormal 4.9406564584124654e-324)
         (largest-finite 1.7976931348623157e308)
         (cons-value (cons 'before nil))
         (values (list most-positive-fixnum most-negative-fixnum bignum
                       1.25 negative-zero smallest-subnormal largest-finite
                       cons-value))
         (vm-values (mapcar function values))
         unit native-results failures (all-vm-identical t))
    (load (expand-file-name "lisp/nelisp-native-load.el" root) nil nil t)
    (load (expand-file-name "lisp/nelisp-native-boxed-unit.el" api-root)
          nil nil t)
    (setq unit (nelisp-native-boxed-unit-open-with-constants
                artifact "nl_bc_boxed_numeric_identity" constants 1))
    (unwind-protect
        (progn
          (garbage-collect)
          (setq native-results
                (mapcar (lambda (value)
                          (condition-case error-data
                              (nelisp-native-boxed-unit-call unit (list value))
                            (error
                             (list :native-error error-data))))
                        values))
          (let ((remaining-values values)
                (remaining-results native-results))
            (while remaining-values
              (unless (if (and (numberp (car remaining-values))
                               (numberp (car remaining-results)))
                          (= (car remaining-values) (car remaining-results))
                        (eq (car remaining-values) (car remaining-results)))
                (push (cons (car remaining-values) (car remaining-results))
                      failures))
              (setq remaining-values (cdr remaining-values)
                    remaining-results (cdr remaining-results)))
            (setq failures (nreverse failures)))
          (dolist (failure failures)
            (princ (format "NATIVE_CATEGORY_FAILURE value=%S error=%S\n"
                           (car failure) (cdr failure))))
          (while values
            (unless (eq (car values) (car vm-values))
              (setq all-vm-identical nil))
            (setq values (cdr values) vm-values (cdr vm-values)))
          (unless all-vm-identical
            (error "VM identity mismatch"))
          (unless (equal (prin1-to-string (nth 4 native-results)) "-0.0")
            (push (cons negative-zero
                        (list :wrong-float-sign (nth 4 native-results)))
                  failures))
          (setcdr cons-value 'after-gc-mutation)
          (garbage-collect)
          (let ((after (nelisp-native-boxed-unit-call unit (list cons-value))))
            (unless (and (eq after cons-value)
                         (eq (cdr after) 'after-gc-mutation))
              (error "mutable cons identity/state was lost across GC: %S" after)))
          (when failures
            (error "native numeric/object categories failed: %S" failures))
          (princ "boxed numeric/object identity, negative zero, GC and cons mutation PASS\n")
          t)
      (nelisp-native-boxed-unit-close unit))))

(defun nelisp-test-bytecode-native-boxed-numeric-gc-suite ()
  "Build materialized byte-code and run its native acceptance in this process."
  (nelisp-test-bytecode-native-boxed-numeric-gc-build)
  (nelisp-test-bytecode-native-boxed-numeric-gc-run)
  (princ "NELISP_BC_BOXED_NUMERIC_GC_PASS"))

(defun nelisp-test-bytecode-native-boxed-numeric-gc-negative-controls ()
  "Reject malformed bignum bounds before reading limbs and unknown tags."
  (load (expand-file-name "lisp/nelisp-native-load.el"
                          (getenv "NELISP_REPO_ROOT")) nil nil t)
  (unless (and (= (nelisp-native-load--decode-float64 #x3ff4000000000000)
                  1.25)
               (equal (prin1-to-string
                       (nelisp-native-load--decode-float64 #x8000000000000000))
                      "-0.0")
               (= (nelisp-native-load--decode-float64 1)
                  4.9406564584124654e-324)
               (= (nelisp-native-load--decode-float64 #x7fefffffffffffff)
                  1.7976931348623157e308))
    (error "finite binary64 decoder controls failed"))
  (unless (condition-case nil
              (progn (nelisp-native-load--decode-float64 #x7ff0000000000000) nil)
            (error t))
    (error "infinite binary64 was accepted"))
  (fset 'ptr-read-u64
        (lambda (_address offset)
          (cond ((= offset 8) 0) ((= offset 16) 128)
                ((= offset 24) (1+ nelisp-native-load-max-bignum-limbs))
                (t 99))))
  (fset 'ptr-read-u32
        (lambda (&rest _args) (error "invalid bignum reached limb read")))
  (unless (condition-case nil
              (progn (nelisp-native-load--decode-bignum 64) nil)
            (error t))
    (error "oversized bignum limb count was accepted"))
  (fset 'ptr-read-u64 (lambda (_address _offset) 99))
  (unless (condition-case nil
              (progn (nelisp-native-load-unbox 64) nil)
            (error t))
    (error "unknown result tag was accepted"))
  (princ "decoder controls PASS (binary64 boundaries; invalid bignum count and unknown tag rejected)\n")
  t)

(provide 'standalone-bytecode-native-boxed-numeric-gc-driver)
;;; standalone-bytecode-native-boxed-numeric-gc-driver.el ends here
