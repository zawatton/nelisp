;;; nelisp-bytecode-native-rooted-branch-public-test.el --- explicit route tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-native-rooted-branch)

(defconst nelisp-bytecode-native-rooted-branch-public-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-bytecode-native-rooted-branch-public-test--function ()
  (unless (equal emacs-version "31.1")
    (ert-skip "Requires pinned GNU Emacs 31.1"))
  (let* ((directory (make-temp-file "rooted-branch-public-" t))
         (source (expand-file-name "fixture.el" directory))
         (elc (concat source "c")) function)
    (unwind-protect
        (progn
          (copy-file
           (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-rooted-branch.el"
                             nelisp-bytecode-native-rooted-branch-public-test--root)
           source t)
          (should (byte-compile-file source))
          (delete-file source)
          (load elc nil t t)
          (setq function (symbol-function 'gnu-rooted-branch))
          (fmakunbound 'gnu-rooted-branch)
          function)
      (when (fboundp 'gnu-rooted-branch) (fmakunbound 'gnu-rooted-branch))
      (delete-directory directory t))))

(ert-deftest nelisp-rooted-branch-public-route-is-explicit-and-pinned ()
  (let* ((function (nelisp-bytecode-native-rooted-branch-public-test--function))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-branch-plan input))
         (calls 0) result)
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get plan :status) 'complete))
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
               (lambda (_) input))
              ((symbol-function 'nelisp-bytecode-native-rooted-branch-build)
               (lambda (seen path)
                 (setq calls (1+ calls))
                 (should (eq seen input))
                 (should (equal path "accepted.nelr"))
                 (list :status 'complete))))
      (setq result
            (nelisp-bytecode-native-compiler-build
             function "accepted.nelr" nelisp-bytecode-native-rooted-branch-entry))
      (should (eq (plist-get result :status) 'complete))
      (should (= calls 1))
      (dolist (bad-route
               '(("wrong.elc" "nl_native_rooted_branch_probe_v1")
                 ("wrong.nelr" "nl_native_rooted_branch_probe_v2")
                 ("wrong.nelr" "nl_native_bytecode_call1_exit")))
        (should (eq (plist-get
                     (nelisp-bytecode-native-compiler-build
                      function (car bad-route) (cadr bad-route)) :status)
                    'unsupported))
        (should (= calls 1))))
    (dolist (mutation
             (list (lambda (x) (plist-put x :argument-descriptor 772))
                   (lambda (x) (plist-put x :argument-count 4))
                   (lambda (x) (plist-put x :argument-min 2))
                   (lambda (x) (plist-put x :constants [unexpected]))
                   (lambda (x) (plist-put x :capture-values-available t))
                   (lambda (x) (plist-put x :potential-capture-placeholder-p t))
                   (lambda (x) (plist-put x :code
                                          (unibyte-string 2 131 7 0 2 64 135 65 135)))))
      (let ((bad (copy-tree input)))
        (funcall mutation bad)
        (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
                   (lambda (_) bad))
                  ((symbol-function 'nelisp-bytecode-native-rooted-branch-build)
                   (lambda (&rest _)
                     (setq calls (1+ calls))
                     (error "invalid branch reached backend"))))
          (should (eq (plist-get
                       (nelisp-bytecode-native-compiler-build
                        function "invalid.nelr"
                        nelisp-bytecode-native-rooted-branch-entry)
                       :status)
                      'unsupported))
          (should (= calls 1)))))
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
               (lambda (_) '(:status malformed :reason "test malformed")))
              ((symbol-function 'nelisp-bytecode-native-rooted-branch-build)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (error "malformed branch reached backend"))))
      (should (eq (plist-get
                   (nelisp-bytecode-native-compiler-build
                    function "invalid.nelr"
                    nelisp-bytecode-native-rooted-branch-entry)
                   :status)
                  'malformed))
      (should (= calls 1)))))

(ert-deftest nelisp-rooted-branch-package-refusal-precedes-effects ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((directory (make-temp-file "rooted-branch-package-" t))
         (source (expand-file-name "fixture.el" directory))
         (elc (concat source "c"))
         (output (expand-file-name "package" directory))
         (make-directory-real (symbol-function 'make-directory))
         (directory-calls 0) (backend-calls 0) failure)
    (unwind-protect
        (progn
          (copy-file
           (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-rooted-branch.el"
                             nelisp-bytecode-native-rooted-branch-public-test--root)
           source t)
          (with-temp-buffer
            (insert-file-contents source)
            (goto-char (point-max))
            (insert "\n(provide 'gnu-rooted-branch-feature)\n")
            (write-region (point-min) (point-max) source nil 'silent))
          (should (byte-compile-file source))
          (delete-file source)
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest args)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-real args)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _)
                       (setq backend-calls (1+ backend-calls))
                       (error "rooted branch package reached backend"))))
            (setq failure
                  (condition-case err
                      (progn
                        (nelisp-bytecode-native-package-compile-elc
                         elc 'gnu-rooted-branch-feature '(gnu-rooted-branch) output)
                        nil)
                    (error (error-message-string err)))))
          (should (equal failure
                         "bytecode-native-package: gnu-rooted-branch lowers to the fixed rooted branch raw-v2 probe and cannot enter a boxed .neln package"))
          (should (= directory-calls 0))
          (should (= backend-calls 0))
          (should-not (file-exists-p output)))
      (delete-directory directory t))))

(provide 'nelisp-bytecode-native-rooted-branch-public-test)
