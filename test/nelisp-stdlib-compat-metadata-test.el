;;; nelisp-stdlib-compat-metadata-test.el --- Compatibility metadata checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-stdlib-compat-metadata)

(defun nelisp-test-compat-metadata-with-standalone (thunk)
  "Run THUNK with isolated metadata cells and explicit test evidence."
  (let* ((symbols '(nelisp-version nelisp-emacs-lisp-compatibility-target))
         (saved (mapcar (lambda (symbol)
                          (list symbol (boundp symbol)
                                (and (boundp symbol) (symbol-value symbol))))
                        symbols))
         (boundp-function (symbol-function 'boundp)))
    (unwind-protect
        (progn
          (mapc #'makunbound symbols)
          (set 'nelisp-version "test-runtime")
          (cl-letf (((symbol-function 'boundp)
                     (lambda (symbol)
                       (and (not (eq symbol 'emacs-version))
                            (funcall boundp-function symbol)))))
            (funcall thunk)))
      (dolist (cell saved)
        (if (nth 1 cell) (set (car cell) (nth 2 cell))
          (makunbound (car cell)))))))

(ert-deftest nelisp-compat-metadata-preserves-actual-host ()
  (let ((version emacs-version)
        (major emacs-major-version)
        (minor emacs-minor-version)
        (function (symbol-function 'emacs-version))
        (dialect-function (and (fboundp 'nelisp-bytecode-compiler-input-dialect)
                               (symbol-function 'nelisp-bytecode-compiler-input-dialect)))
        (feature (featurep 'nelisp)))
    (should (eq (nelisp-stdlib-compat-metadata-install) 'preserved))
    (should (equal version emacs-version))
    (should (= major emacs-major-version))
    (should (= minor emacs-minor-version))
    (should (eq function (symbol-function 'emacs-version)))
    (should (eq dialect-function
                (and (fboundp 'nelisp-bytecode-compiler-input-dialect)
                     (symbol-function 'nelisp-bytecode-compiler-input-dialect))))
    (should (eq feature (featurep 'nelisp)))))

(ert-deftest nelisp-compat-metadata-refuses-mismatched-target ()
  (nelisp-test-compat-metadata-with-standalone
   (lambda ()
     (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                (lambda () '(:status pinned :dialect "GNU Emacs 31.1"
                             :runtime-evidence standalone-build-verified))))
       (should-error (nelisp-stdlib-compat-metadata-install "30.1"))
       (should-not (boundp 'emacs-version))
       (should-not (boundp 'nelisp-emacs-lisp-compatibility-target))))))

(ert-deftest nelisp-compat-metadata-refuses-unverified-target ()
  (nelisp-test-compat-metadata-with-standalone
   (lambda ()
     (dolist (evidence '((:status unsupported :dialect "GNU Emacs 31.1")
                         (:status pinned :dialect "GNU Emacs 31.1")
                         (:status pinned :dialect "GNU Emacs 30.1"
                          :runtime-evidence standalone-build-verified)))
       (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                  (lambda () evidence)))
         (should-error (nelisp-stdlib-compat-metadata-install))
         (should-not (boundp 'emacs-version))
         (should-not (boundp 'nelisp-emacs-lisp-compatibility-target)))))))

(ert-deftest nelisp-compat-metadata-refuses-missing-runtime-identity ()
  (nelisp-test-compat-metadata-with-standalone
   (lambda ()
     (makunbound 'nelisp-version)
     (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                (lambda () '(:status pinned :dialect "GNU Emacs 31.1"
                             :runtime-evidence standalone-build-verified))))
       (should-error (nelisp-stdlib-compat-metadata-install))
       (should-not (boundp 'emacs-version))))))

(provide 'nelisp-stdlib-compat-metadata-test)
