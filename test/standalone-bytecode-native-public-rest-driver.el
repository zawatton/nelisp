;;; standalone-bytecode-native-public-rest-driver.el --- public REST admission -*- lexical-binding: t; -*-

(defun nelisp-test-public-rest-prepare-elc ()
  "Compile a real REST function and write a mutated-descriptor .elc control."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (source (getenv "NELISP_PUBLIC_REST_SOURCE"))
         (elc (getenv "NELISP_PUBLIC_REST_ELC"))
         (bad-elc (getenv "NELISP_PUBLIC_REST_BAD_ELC")))
    (with-temp-file source
      (insert "(defalias 'nelisp-public-rest-function\n"
              "  (lambda (required &rest values) values))\n"
              "(provide 'nelisp-public-rest-fixture)\n"))
    (unless (byte-compile-file source)
      (error "GNU byte compilation failed"))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (let* ((forms (nelisp-bytecode-native-package--read-elc-forms elc))
           (definitions (nelisp-bytecode-native-package--elc-definitions forms))
           (function (cdr (assq 'nelisp-public-rest-function definitions))))
      (unless (and (byte-code-function-p function)
                   (equal (aref function 1) (unibyte-string 8 135)))
        (error "unexpected GNU REST fixture bytecode"))
      (let ((bad-function
             (make-byte-code '(required &optional optional &rest values)
                             (aref function 1) (aref function 2)
                             (aref function 3))))
        (let ((definition
               (cl-find-if
                (lambda (form)
                  (and (eq (car-safe form) 'defalias)
                       (eq (if (and (consp (cadr form))
                                    (eq (caadr form) 'quote))
                               (cadadr form) (cadr form))
                           'nelisp-public-rest-function)))
                forms)))
          (unless definition (error "REST defalias form absent from .elc"))
          (setcar (cddr definition) bad-function)))
      (with-temp-file bad-elc
        (insert ";ELC\n")
        (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
    (delete-file source)
    t))

(defun nelisp-test-public-rest-compile-package ()
  "Compile good and corrupted source-free .elc inputs through the public API."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_PUBLIC_REST_ELC"))
         (bad-elc (getenv "NELISP_PUBLIC_REST_BAD_ELC"))
         (directory (getenv "NELISP_PUBLIC_REST_PACKAGE"))
         (bad-directory (concat directory ".bad")))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (unless (and (not (file-exists-p (concat (file-name-sans-extension elc) ".el")))
                 (condition-case nil
                     (progn
                       (nelisp-bytecode-native-package-compile-elc
                        bad-elc 'nelisp-public-rest-fixture
                        '(nelisp-public-rest-function) bad-directory)
                       nil)
                   (error t))
                 (not (file-exists-p bad-directory)))
      (error "corrupted REST descriptor was not refused before publication"))
    (let* ((result
            (nelisp-bytecode-native-package-compile-elc
             elc 'nelisp-public-rest-fixture
             '(nelisp-public-rest-function) directory))
           (entry (car (plist-get
                        (nelisp-bytecode-native-package--read-manifest
                         (plist-get result :manifest)) :entries))))
      (unless (and (eq (plist-get result :status) 'complete)
                   (file-readable-p (plist-get result :manifest))
                   (= (plist-get entry :minimum) 1)
                   (null (plist-get entry :maximum))
                   (= (plist-get entry :rest-required-count) 1))
        (error "public REST package publication mismatch: %S" entry)))
    t))

(defun nelisp-test-public-rest-cold-call ()
  "Cold-open the public package and verify REST values, arity, and GC identity."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest (getenv "NELISP_PUBLIC_REST_MANIFEST"))
         (package nil)
         (required (cons 'required-marker nil))
         (extra-a (cons 'extra-a nil))
         (extra-b (cons 'extra-b nil))
         (arguments nil)
         (result nil)
         (wrong-arity-refused nil)
         (count-before nil))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (setq package (nelisp-bytecode-native-package-open manifest))
    (unwind-protect
        (progn
          (puthash 'required required nelisp--globals)
          (puthash 'extra-a extra-a nelisp--globals)
          (puthash 'extra-b extra-b nelisp--globals)
          (setq arguments (nelisp-eval '(list required extra-a extra-b)))
          (garbage-collect)
          (setq result
                (nelisp-bytecode-native-package-call
                 package 'nelisp-public-rest-function arguments))
          (setq count-before
                (nelisp-bytecode-native-package-native-call-count
                 package 'nelisp-public-rest-function))
          (condition-case nil
              (progn
                (nelisp-bytecode-native-package-call
                 package 'nelisp-public-rest-function nil)
                (setq wrong-arity-refused nil))
            (error (setq wrong-arity-refused t)))
          (garbage-collect)
          (unless (and (= count-before 1) wrong-arity-refused
                       (= (nelisp-bytecode-native-package-native-call-count
                           package 'nelisp-public-rest-function) 1)
                       (equal result (cdr arguments))
                       (eq (car result) extra-a) (eq (cadr result) extra-b)
                       (null (cddr result)))
            (error "public REST package call mismatch: %S"
                   (list count-before wrong-arity-refused result arguments
                         (and result (eq (car result) extra-a))
                         (and result (cadr result)
                              (eq (cadr result) extra-b)))))
          (list :ok t :native-calls count-before :gc-identity t
                :wrong-arity-refused wrong-arity-refused))
      (when package (nelisp-bytecode-native-package-close package)))))

(provide 'standalone-bytecode-native-public-rest-driver)
;;; standalone-bytecode-native-public-rest-driver.el ends here
