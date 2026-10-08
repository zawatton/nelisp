;;; nelisp-bytecode-native-rooted-conditional-public-test.el --- explicit public route -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-native-rooted-conditional)

(defun nelisp-bytecode-native-rooted-conditional-public-test--function (&optional constants code descriptor)
  (make-byte-code (or descriptor 771)
                  (or code (unibyte-string 2 131 6 0 1 135 135))
                  (or constants []) 4))

(ert-deftest nelisp-bytecode-native-rooted-conditional-public-predicate-is-pinned-and-capture-free ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (nelisp-bytecode-native-rooted-conditional-public-test--function))
         (input (nelisp-bytecode-compiler-input-build function)))
    (should (nelisp-bytecode-native-rooted-conditional-input-p input))
    (dolist (mutation
             (list (lambda (x) (plist-put x :constants [not-empty]))
                   (lambda (x) (plist-put x :argument-descriptor 770))
                   (lambda (x) (plist-put x :capture-values-available t))
                   (lambda (x) (plist-put x :potential-capture-placeholder-p t))
                   (lambda (x) (plist-put x :code (unibyte-string 2 131 6 0 2 135 135)))))
      (let ((bad (copy-tree input)))
        (setq bad (funcall mutation bad))
        (should-not (nelisp-bytecode-native-rooted-conditional-input-p bad))))))

(ert-deftest nelisp-bytecode-native-rooted-conditional-public-dispatch-is-explicit ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (nelisp-bytecode-native-rooted-conditional-public-test--function))
         (valid (nelisp-bytecode-compiler-input-build function))
         (calls 0) result)
      (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
               (lambda (_) valid))
              ((symbol-function 'nelisp-bytecode-native-rooted-conditional-build)
               (lambda (input path)
                 (setq calls (1+ calls))
                 (list :status 'complete :input input :artifact-path path))))
      (setq result
            (nelisp-bytecode-native-compiler-build
             function "ok.nelr" "nl_native_rooted_conditional_probe_v1"))
      (should (eq (plist-get result :status) 'complete))
      (should (= calls 1))
      (dolist (bad-route '(("wrong.nelr" "nl_native_rooted_conditional_probe_v2")
                           ("wrong.elc" "nl_native_rooted_conditional_probe_v1")))
        (should (eq (plist-get (nelisp-bytecode-native-compiler-build
                               function (car bad-route) (cadr bad-route)) :status)
                    'unsupported))
        (should (= calls 1)))
      (let ((bad (copy-tree valid)))
        (plist-put bad :constants [unexpected])
        (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
                   (lambda (_) bad)))
          (should (eq (plist-get (nelisp-bytecode-native-compiler-build
                                  function "bad.nelr"
                                  "nl_native_rooted_conditional_probe_v1") :status)
                      'unsupported)))
        (should (= calls 1))))))

(ert-deftest nelisp-bytecode-native-rooted-conditional-package-preflight-precedes-effects ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((root (make-temp-file "rooted-conditional-package-" t))
         (elc (expand-file-name "module.elc" root))
         (function (nelisp-bytecode-native-rooted-conditional-public-test--function))
         (output (expand-file-name "package" root))
         (make-directory-real (symbol-function 'make-directory))
         (directory-calls 0) failure)
    (unwind-protect
        (progn
          (with-temp-file elc
            (insert ";ELC\n")
            (prin1 (list 'defalias (list 'quote 'conditional-function) function)
                   (current-buffer))
            (insert "\n")
            (prin1 '(provide 'conditional-feature) (current-buffer)))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest args)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-real args)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _) (error "conditional package reached backend"))))
            (setq failure
                  (condition-case err
                      (progn (nelisp-bytecode-native-package-compile-elc
                              elc 'conditional-feature '(conditional-function) output)
                             nil)
                    (error (error-message-string err)))))
          (should (equal failure
                         "bytecode-native-package: conditional-function lowers to the fixed rooted conditional raw-v2 probe and cannot enter a boxed .neln package"))
          (should (= directory-calls 0))
          (should-not (file-exists-p output)))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-rooted-conditional-malformed-input-never-dispatches ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
               (lambda (_) '(:status malformed :reason "invalid frame")))
              ((symbol-function 'nelisp-bytecode-native-rooted-conditional-build)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (error "malformed input reached conditional backend"))))
      (should (eq (plist-get
                   (nelisp-bytecode-native-compiler-build
                    'not-bytecode "bad.nelr"
                    "nl_native_rooted_conditional_probe_v1")
                   :status)
                  'malformed))
      (should (= calls 0)))))

(provide 'nelisp-bytecode-native-rooted-conditional-public-test)
