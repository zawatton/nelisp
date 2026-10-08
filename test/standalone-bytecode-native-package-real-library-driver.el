;;; standalone-bytecode-native-package-real-library-driver.el --- GNU library corpus probe -*- lexical-binding: t; -*-

(defun nelisp-test-real-library-package-compile ()
  "Compile one function from the complete GNU cal-french .elc as data."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_REAL_LIBRARY_ELC"))
         (directory (getenv "NELISP_REAL_LIBRARY_PACKAGE")))
    (require 'nelisp-substitute-command-keys)
    (require 'nelisp-display-batch)
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (unless (and (not (featurep 'cal-french))
                 (not (fboundp 'calendar-french-accents-p)))
      (error "real library compile evaluated its ELC"))
    (let ((result
           (nelisp-bytecode-native-package-compile-elc
            elc 'cal-french '(calendar-french-accents-p) directory)))
      (unless (and (eq (plist-get result :status) 'complete)
                   (file-readable-p (plist-get result :manifest))
                   (not (featurep 'cal-french))
                   (not (fboundp 'calendar-french-accents-p))
                   (not (file-exists-p
                         (expand-file-name "cal-french.el" directory))))
        (error "real GNU library compile did not publish a source-free package"))
      t)))

(defun nelisp-test-real-library-package-cold-run ()
  "Cold-open real cal-french ELC and demand one actual native call."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest (getenv "NELISP_REAL_LIBRARY_MANIFEST")))
    (require 'nelisp-substitute-command-keys)
    (require 'nelisp-display-batch)
    (add-to-list 'load-path
                 (expand-file-name "vendor/emacs-lisp/emacs-lisp" root))
    (require 'easymenu)
    (add-to-list 'load-path
                 (expand-file-name "vendor/emacs-lisp/calendar" root))
    (require 'calendar)
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (unless (and (not (featurep 'cal-french))
                 (not (fboundp 'calendar-french-accents-p)))
      (error "cold process started with cal-french already loaded"))
    (let ((eval-count 0)
          (eval-function
           (symbol-function 'nelisp-bytecode-native-package--eval-elc-forms))
          (package nil)
          (second-open nil))
      (fset 'nelisp-bytecode-native-package--eval-elc-forms
            (lambda (forms)
              (setq eval-count (1+ eval-count))
              (funcall eval-function forms)))
      (unwind-protect
          (progn
            (setq package (nelisp-bytecode-native-package-open manifest)
                  second-open (nelisp-bytecode-native-package-open manifest))
            (unless (and (featurep 'cal-french)
                         (fboundp 'calendar-french-accents-p)
                         (= eval-count 1))
              (error "real GNU library top-level forms did not run exactly once"))
            (let* ((native-value
                    (nelisp-bytecode-native-package-call
                     package 'calendar-french-accents-p nil))
                   (native-count
                    (nelisp-bytecode-native-package-native-call-count
                     package 'calendar-french-accents-p))
                   (nelisp-bytecode-native-package-native-enabled nil)
                   (vm-value
                    (nelisp-bytecode-native-package-call
                     package 'calendar-french-accents-p nil))
                   (after-vm-count
                    (nelisp-bytecode-native-package-native-call-count
                     package 'calendar-french-accents-p)))
              (unless (and (eq native-value t) (= native-count 1)
                           (eq vm-value t) (= after-vm-count 1))
                (error "real library call did not prove native and VM routes"))
              "native-library-result:(t 1 t 1; load-evals=1)"))
        (fset 'nelisp-bytecode-native-package--eval-elc-forms eval-function)
        (when package (nelisp-bytecode-native-package-close package))
        (when second-open (nelisp-bytecode-native-package-close second-open))))))

(provide 'standalone-bytecode-native-package-real-library-driver)
;;; standalone-bytecode-native-package-real-library-driver.el ends here
