;;; v1-single-validation-test.el --- Call-local compile validation -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-aot-compiler)

(defun v1-test--build (after-compile check-result &optional shared-v2)
  "Build real byte-code through the real compiler and inspect CHECK-RESULT.
AFTER-COMPILE runs after compile-file has returned its manifest."
  (let* ((directory (make-temp-file "v1-validation-" t))
         (artifact (expand-file-name "fixture.nelr" directory))
         (function (byte-compile '(lambda (a b) (cons a b))))
         (input (nelisp-bytecode-compiler-input-build function))
         (compiler (symbol-function 'nelisp-native-load-raw-v2-compile-file))
         (validator (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p))
         (nelisp-bytecode-native-rooted-cfg-contract--validation-count 0)
         (nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
         (nelisp-bytecode-native-rooted-cfg-native--registry nil))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
                   (lambda () (make-string 64 ?a)))
                  ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
                   (lambda () t))
                  ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
                   (lambda (&rest args)
                     (let ((manifest (apply compiler args)))
                       (when after-compile (funcall after-compile manifest validator))
                       manifest))))
          (funcall check-result
                   (lambda ()
                     (if shared-v2
                         (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2
                          input artifact 'off)
                       (nelisp-bytecode-native-rooted-cfg-native-build input artifact)))))
      (fset 'nelisp-bytecode-native-rooted-cfg-contract-valid-p validator)
      (delete-directory directory t))))

(ert-deftest v1/one-semantic-reconstruction-per-compile ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (shared-v2 '(nil t))
    (v1-test--build
     nil
     (lambda (build)
       (let ((result (funcall build)))
         (should (eq (plist-get result :status) 'complete))
         (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 1))
         (should (eq (plist-get result :contract)
                     (plist-get (plist-get result :manifest) :native-rooted-cfg-contract)))
         (should (nelisp-bytecode-native-rooted-cfg-native-authenticated-result-p result))))
     shared-v2)))

(ert-deftest v1/contract-mutation-revalidated-and-refused ()
  (skip-unless (equal emacs-version "31.1"))
  (v1-test--build
   (lambda (manifest _)
     (plist-put (plist-get manifest :native-rooted-cfg-contract) :status-base 513))
   (lambda (build)
     (should-error (funcall build))
     (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 2)))
   t))

(ert-deftest v1/rebound-validator-forces-reconstruction ()
  (skip-unless (equal emacs-version "31.1"))
  (v1-test--build
   (lambda (_manifest validator)
     (fset 'nelisp-bytecode-native-rooted-cfg-contract-valid-p
           (lambda (contract &optional mode) (funcall validator contract mode))))
   (lambda (build)
     (should (eq (plist-get (funcall build) :status) 'complete))
     (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 2)))
   t))

(ert-deftest v1/public-check-still-reconstructs ()
  (skip-unless (equal emacs-version "31.1"))
  (v1-test--build
   nil
   (lambda (build)
     (let ((manifest (plist-get (funcall build) :manifest)))
       (setq nelisp-bytecode-native-rooted-cfg-contract--validation-count 0
             nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
       (should-not (nelisp-native-load-raw-v2-check
                    manifest nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
       (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 1))))
   t))

(ert-deftest v1/post-compile-structural-mutation-refused ()
  (skip-unless (equal emacs-version "31.1"))
  (v1-test--build
   (lambda (manifest _)
     (plist-put (nelisp-native-load--raw-export
                 (plist-get manifest :native)
                 nelisp-bytecode-native-rooted-cfg-contract-shared-entry)
                :arity 3))
   (lambda (build)
     (should-error (funcall build))
     (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 1)))
   t))

(ert-deftest v1/equal-but-distinct-contract-revalidated ()
  (skip-unless (equal emacs-version "31.1"))
  (v1-test--build
   (lambda (manifest _)
     (plist-put manifest :native-rooted-cfg-contract
                (copy-tree (plist-get manifest :native-rooted-cfg-contract) t)))
   (lambda (build)
     (should (eq (plist-get (funcall build) :status) 'complete))
     (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 2)))
   t))

(provide 'v1-single-validation-test)
