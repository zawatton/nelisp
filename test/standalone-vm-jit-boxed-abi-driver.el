;;; standalone-vm-jit-boxed-abi-driver.el --- AOT fixture for the boxed ABI gate -*- lexical-binding: t; -*-

;;; Code:

(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defun nelisp-test-vm-jit-boxed-abi-fingerprint (fn)
  "Return the stable bytecode fingerprint for FN."
  (secure-hash
   'sha256
   (prin1-to-string
    (list (aref fn 0)
          (mapconcat (lambda (byte) (number-to-string byte))
                     (append (aref fn 1) nil) ",")
          (aref fn 2) (aref fn 3)))))

(defun nelisp-test-vm-jit-boxed-abi-host-probe ()
  "Print the exact GNU Emacs bytecode reference result."
  (let* ((eq-fn (make-byte-code 514 (unibyte-string 1 1 61 135) [] 4))
         (cadr-fn (make-byte-code 257 (unibyte-string 137 65 64 135) [] 2))
         (same-cons (cons 'abi nil))
         (distinct-cons (cons 'abi nil))
         (invalid (condition-case data
                      (progn (funcall cadr-fn 1) 'missed)
                    (wrong-type-argument data))))
    (prin1 (list (if (string-match-p "\\`31\\.1\\(?:\\'\\|\\.\\)"
                                    emacs-version) t nil)
                 (nelisp-test-vm-jit-boxed-abi-fingerprint eq-fn)
                 (nelisp-test-vm-jit-boxed-abi-fingerprint cadr-fn)
                 (list (funcall eq-fn same-cons same-cons)
                       (funcall eq-fn same-cons distinct-cons)
                       invalid)))))

(defun nelisp-test-build-vm-jit-boxed-abi-fixture ()
  "Build the two-argument boxed ABI fixture into the requested artifact directory."
  (let* ((directory (getenv "NELISP_ABI_ARTIFACT_DIR"))
         (source (expand-file-name "gc-eq-two.el" directory))
         (artifact (expand-file-name "gc-eq-two.neln" directory))
         (nelisp-aot-compiler--dynamic-user-calls t))
    (unless directory
      (error "NELISP_ABI_ARTIFACT_DIR is unset"))
    (make-directory directory t)
    (with-temp-file source
      (insert "(defun gc-eq-two (left right)\n"
              "  (seq (garbage-collect) (eq left right)))\n"
              "(provide (quote gc-eq-two))\n"))
    (nelisp-artifact-compile-file source artifact nil nil nil nil nil 'neln)
    (unless (file-exists-p artifact)
      (error "AOT fixture was not written: %s" artifact))
    (princ (format "aot-fixture=%s\n" artifact))))

(provide 'standalone-vm-jit-boxed-abi-driver)

;;; standalone-vm-jit-boxed-abi-driver.el ends here
