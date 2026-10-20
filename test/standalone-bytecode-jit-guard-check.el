;;; standalone-bytecode-jit-guard-check.el --- Cold JIT guard transition -*- lexical-binding: t; -*-

;; The baked fallback exists before a real JIT hook is installed. Replacing
;; its function cell must enable the ordinary live lookup immediately.
(defvar bc-guard-code (make-byte-code 257 (unibyte-string 84 135) [] 2))
(defvar bc-guard-name (concat "nelisp-bytecode-jit--" "runtime-dispatch"))
(unless (and (fboundp (intern bc-guard-name))
             (= (funcall bc-guard-code 5) 6)
             (= (apply bc-guard-code '(6)) 7)
             (= (funcall (make-byte-code 0 (unibyte-string 192 193 33 135)
                                         (vector bc-guard-code 7) 2)) 8))
  (error "Unconfigured bytecode JIT path failed"))
(let ((hook (intern bc-guard-name)))
  (let ((original (symbol-function hook)))
  (unwind-protect
      (progn
        (fset hook (lambda (_fn &rest _args) [t 41]))
        (unless (and (= (funcall bc-guard-code 5) 41)
                     (= (apply bc-guard-code '(6)) 41)
                     (= (funcall (make-byte-code 0 (unibyte-string 192 193 33 135)
                                                 (vector bc-guard-code 7) 2)) 41))
          (error "Late JIT hook installation was skipped"))
        (fmakunbound hook)
        ;; Preserve the existing live-lookup error for a removed hook.
        (unless (equal (condition-case e (funcall bc-guard-code 5) (error e))
                       '(void-function nil))
          (error "Removed JIT hook changed its lookup error")))
    (fset hook original))))
(princ "BYTECODE-JIT-GUARD checks=8 failures=0 PASS\n")
nil
