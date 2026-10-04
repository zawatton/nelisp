;;; standalone-bytecode-native-package-many-driver.el --- >32 package smoke -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun nelisp-test-bytecode-native-package-many-names ()
  "Return the 33 independently materialized fixture function names."
  (mapcar (lambda (index)
            (intern (format "nelisp-native-package-many-identity-%02d" index)))
          (number-sequence 0 32)))

(defun nelisp-test-bytecode-native-package-many-load-library ()
  "Load the package API from the current test checkout."
  (let ((root (getenv "NELISP_REPO_ROOT")))
    (unless (and root (file-directory-p root))
      (error "many-package: missing NELISP_REPO_ROOT"))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)))

(defun nelisp-test-bytecode-native-package-many-refused-p (thunk)
  "Return non-nil when THUNK signals an error."
  (condition-case nil
      (progn (funcall thunk) nil)
    (error t)))

(defun nelisp-test-bytecode-native-package-many-compile ()
  "Compile all 33 fixture functions through the public package entry."
  (nelisp-test-bytecode-native-package-many-load-library)
  (let* ((elc (getenv "NELISP_PACKAGE_ELC"))
         (directory (getenv "NELISP_PACKAGE_DIRECTORY"))
         (duplicate-directory (getenv "NELISP_DUPLICATE_PACKAGE_DIRECTORY"))
         (missing-directory (getenv "NELISP_MISSING_PACKAGE_DIRECTORY"))
         (names (nelisp-test-bytecode-native-package-many-names))
         (result (nelisp-bytecode-native-package-compile-elc
                  elc 'nelisp-native-package-many-functions names directory)))
    (unless (and (= (length names) 33)
                 (eq (plist-get result :status) 'complete)
                 (= (length (plist-get result :entries)) 33)
                 (file-readable-p (plist-get result :manifest)))
      (error "many-package: public compile did not publish all 33 entries"))
    (unless (and
             (nelisp-test-bytecode-native-package-many-refused-p
              (lambda ()
                (nelisp-bytecode-native-package-compile-elc
                 elc 'nelisp-native-package-many-functions
                 (cons (car names) names) duplicate-directory)))
             (not (file-exists-p duplicate-directory))
             (nelisp-test-bytecode-native-package-many-refused-p
              (lambda ()
                (nelisp-bytecode-native-package-compile-elc
                 elc 'nelisp-native-package-many-functions
                 (append names '(nelisp-native-package-many-missing))
                 missing-directory)))
             (not (file-exists-p missing-directory)))
      (error "many-package: duplicate or missing function was not refused"))
    (princ "many-package-compile: entries=33 duplicate=refused missing=refused\n")
    t))

(defun nelisp-test-bytecode-native-package-many-hash-control ()
  "Refuse a changed native entry before evaluating the packaged .elc."
  (nelisp-test-bytecode-native-package-many-load-library)
  (let ((manifest (getenv "NELISP_TAMPERED_PACKAGE_MANIFEST")))
    (unless (and manifest
                 (nelisp-test-bytecode-native-package-many-refused-p
                  (lambda ()
                    (nelisp-bytecode-native-package-open manifest)))
                 (not (boundp 'nelisp-native-package-many-load-count))
                 (not (featurep 'nelisp-native-package-many-functions)))
      (error "many-package: stale native hash was admitted or ran .elc"))
    t))

(defun nelisp-test-bytecode-native-package-many-drop-last-entry ()
  "Mutate a package manifest to remove its final function entry."
  (nelisp-test-bytecode-native-package-many-load-library)
  (let* ((manifest-path (getenv "NELISP_PACKAGE_MANIFEST"))
         (manifest (nelisp-bytecode-native-package--read-manifest manifest-path))
         (entries (plist-get manifest :entries)))
    (unless (= (length entries) 33)
      (error "many-package: mutation precondition expected 33 entries"))
    (plist-put manifest :entries (butlast entries))
    (with-temp-file manifest-path
      (prin1 manifest (current-buffer)))
    t))

(defun nelisp-test-bytecode-native-package-many-cold-run ()
  "Cold-load the package and call the first, middle, and last native entries."
  (nelisp-test-bytecode-native-package-many-load-library)
  (let* ((manifest (getenv "NELISP_PACKAGE_MANIFEST"))
         (names (nelisp-test-bytecode-native-package-many-names))
         (package (nelisp-bytecode-native-package-open manifest))
         (selected (list (nth 0 names) (nth 16 names) (nth 32 names)))
         (value (cons 'many-package-root '(many-package-tail)))
         (expected-loaded-units 0))
    (unwind-protect
        (progn
          (unless (and (= (hash-table-count (aref package 3)) 33)
                       (= nelisp-native-package-many-load-count 1))
            (error "many-package: cold load did not retain all 33 entries"))
          (unless (= (hash-table-count (aref package 6)) 0)
            (error "many-package: package open eagerly loaded native units"))
          (garbage-collect)
          (dolist (name selected)
            (unless (eq (nelisp-bytecode-native-package-call
                         package name (list value))
                        value)
              (error "many-package: native identity failed for %S" name))
            (garbage-collect)
            (unless (= (nelisp-bytecode-native-package-native-call-count
                        package name)
                       1)
              (error "many-package: %S did not execute natively" name))
            (setq expected-loaded-units (1+ expected-loaded-units))
            (unless (= (hash-table-count (aref package 6))
                       expected-loaded-units)
              (error "many-package: native unit load was not lazy for %S" name)))
          (princ (format "many-package-cold: entries=33 native=%S lazy-load=0-to-3 gc-identity=PASS\n"
                         selected))
          t)
      (nelisp-bytecode-native-package-close package))))

(provide 'standalone-bytecode-native-package-many-driver)
;;; standalone-bytecode-native-package-many-driver.el ends here
