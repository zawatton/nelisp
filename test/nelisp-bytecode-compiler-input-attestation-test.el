;;; nelisp-bytecode-compiler-input-attestation-test.el --- runtime metadata attestation controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defmacro nelisp-bytecode-compiler-input-attestation-test--deftest (name args &rest body)
  "Define NAME with trampoline changes confined to its test body."
  `(ert-deftest ,name ,args
     (let ((native-comp-enable-subr-trampolines nil)) ,@body)))
(require 'nelisp-bytecode-compiler-input)
(defvar nelisp-bytecode-runtime-dialect-id)

(nelisp-bytecode-compiler-input-attestation-test--deftest dialect-metadata-genuine-gnu-and-forged-marker ()
  (let ((nelisp-bytecode-runtime-dialect-id
         (concat "GNU Emacs 31.1; inventory-sha256="
                 nelisp-bytecode-compiler-input--inventory-sha256)))
    (should (eq (plist-get (nelisp-bytecode-compiler-input-dialect) :status) 'pinned))
    (should-not (plist-get (nelisp-bytecode-compiler-input-dialect) :runtime-evidence))
    (let ((emacs-version "31.2"))
      (should (eq (plist-get (nelisp-bytecode-compiler-input-dialect) :status) 'unsupported)))
    (let ((byte-code-vector (copy-sequence byte-code-vector)))
      (aset byte-code-vector 0 'forged-opcode)
      (should (eq (plist-get (nelisp-bytecode-compiler-input-dialect) :status) 'unsupported)))
    (let ((original (symbol-function 'nelisp-bytecode-compiler-input--sha256-file)))
      (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input--sha256-file)
                 (lambda (path)
                   (if (string-suffix-p "bytecomp.el.gz" path)
                       "mutated-source" (funcall original path)))))
        (should (eq (plist-get (nelisp-bytecode-compiler-input-dialect) :status) 'unsupported))))))

(nelisp-bytecode-compiler-input-attestation-test--deftest dialect-metadata-native-helper-mocks-and-aliases-refused ()
  (let ((nelisp-bytecode-runtime-dialect-id
         (concat "GNU Emacs 31.1; inventory-sha256="
                 nelisp-bytecode-compiler-input--inventory-sha256)))
    (dolist (helper (list (lambda (_) :status) (symbol-function 'eval) (symbol-function 'read)))
      (let ((nelisp-bytecode-compiler-input--runtime-source-evaluator helper))
        (cl-letf (((symbol-function 'nelisp--eval-source-string) helper))
          (should-not (nelisp-bytecode-compiler-input--standalone-runtime-p))
          (should (eq (plist-get (nelisp-bytecode-compiler-input-dialect) :status) 'unsupported)))))
    (let ((nelisp-bytecode-compiler-input--runtime-source-evaluator (symbol-function 'eval)))
      (cl-letf (((symbol-function 'nelisp--eval-source-string) (symbol-function 'read)))
        (should-not (nelisp-bytecode-compiler-input--standalone-runtime-p))))))

(nelisp-bytecode-compiler-input-attestation-test--deftest dialect-metadata-probe-primitive-replacements-refused ()
  (dolist (name '(subrp funcall eq equal))
    (let* ((called nil)
           (helper (lambda (_) (setq called t) :status))
           (nelisp-bytecode-compiler-input--runtime-source-evaluator helper)
           (original (symbol-function name))
           (replacement (lambda (&rest args) (apply original args)))
           result)
      (cl-letf (((symbol-function 'nelisp--eval-source-string) helper)
                ((symbol-function name) replacement))
        (setq result (nelisp-bytecode-compiler-input--standalone-runtime-p)))
      (should-not result)
      (should-not called))))

(nelisp-bytecode-compiler-input-attestation-test--deftest dialect-metadata-true-returning-primitive-forgeries-refused ()
  (dolist (name '(subrp funcall eq equal))
    (let ((nelisp-bytecode-runtime-dialect-id
           (concat "GNU Emacs 31.1; inventory-sha256="
                   nelisp-bytecode-compiler-input--inventory-sha256))
          (nelisp-bytecode-compiler-input--runtime-source-evaluator (symbol-function 'read))
          result)
      (cl-letf (((symbol-function 'nelisp--eval-source-string) (symbol-function 'read))
                ((symbol-function name) (lambda (&rest _) t)))
        (setq result (nelisp-bytecode-compiler-input-dialect)))
      (should (eq (plist-get result :status) 'unsupported)))))

(provide 'nelisp-bytecode-compiler-input-attestation-test)
