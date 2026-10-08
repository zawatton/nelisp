;;; standalone-bytecode-native-package-coldload-driver.el --- package smoke -*- lexical-binding: t; -*-

(defun nelisp-test-bytecode-native-package-compile ()
  "Compile the two-function fixture from unread .elc data."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_PACKAGE_ELC"))
         (directory (getenv "NELISP_PACKAGE_DIRECTORY")))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (unless (and (not (equal
                       (nelisp-bytecode-native-package--native-entry-name 'a-b)
                       (nelisp-bytecode-native-package--native-entry-name 'a_b)))
                 (string-match-p
                  "\\`[A-Za-z_][A-Za-z0-9_]*\\'"
                  (nelisp-bytecode-native-package--native-entry-name 'a-b)))
      (error "native entry encoding collided for hostile names"))
    (let ((result
           (nelisp-bytecode-native-package-compile-elc
            elc 'nelisp-native-package-coldload-fixture
            '(nelisp-native-package-default nelisp-native-package-optional-value)
            directory)))
      (unless (and (eq (plist-get result :status) 'complete)
                   (not (boundp 'nelisp-native-package-coldload-count))
                   (not (featurep 'nelisp-native-package-coldload-fixture))
                   (not (fboundp 'nelisp-native-package-default))
                   (file-readable-p (plist-get result :manifest))
                   (not (file-exists-p
                         (expand-file-name "package-coldload.el" directory))))
        (error "package compile evaluated .elc or omitted its manifest: %S"
               (plist-get result :status)))
      (let ((unsupported-directory (concat directory ".unsupported")))
        (unless (and (condition-case nil
                         (progn
                           (nelisp-bytecode-native-package-compile-elc
                            elc 'nelisp-native-package-coldload-fixture
                            '(nelisp-native-package-identity) unsupported-directory)
                           nil)
                       (error t))
                     (not (file-exists-p unsupported-directory)))
          (error "unsupported packed identity function left package output")))
      t)))

(defun nelisp-test-bytecode-native-package-cold-run ()
  "Cold-open the package and check side effects, native calls, and redefinition."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest (getenv "NELISP_PACKAGE_MANIFEST")))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (let ((package (nelisp-bytecode-native-package-open manifest))
          (second-open nil)
          (base (cons 'base nil))
          (supplied (cons 'supplied nil))
          default-missing default-nil default-supplied
          optional-missing optional-nil optional-supplied vm-disabled)
      (unwind-protect
          (progn
            ;; Opening the same .elc package twice follows require's once rule.
            (setq second-open (nelisp-bytecode-native-package-open manifest))
            (unless (and (= nelisp-native-package-coldload-count 1)
                         (not (file-exists-p
                               (expand-file-name "package-coldload.el"
                                                 (file-name-directory manifest)))))
              (error "cold package load side effect did not run exactly once"))
            (garbage-collect)
            (setq default-missing
                  (nelisp-bytecode-native-package-call
                   package 'nelisp-native-package-default (list base))
                  default-nil
                  (nelisp-bytecode-native-package-call
                   package 'nelisp-native-package-default (list base nil))
                  default-supplied
                  (nelisp-bytecode-native-package-call
                   package 'nelisp-native-package-default (list base supplied))
                  optional-missing
                  (nelisp-bytecode-native-package-call
                   package 'nelisp-native-package-optional-value (list base))
                  optional-nil
                  (nelisp-bytecode-native-package-call
                   package 'nelisp-native-package-optional-value (list base nil))
                  optional-supplied
                  (nelisp-bytecode-native-package-call
                   package 'nelisp-native-package-optional-value (list base supplied)))
            (unless (and (eq default-missing base) (eq default-nil base)
                         (eq default-supplied supplied)
                         (null optional-missing) (null optional-nil)
                         (eq optional-supplied supplied)
                         (= (nelisp-bytecode-native-package-native-call-count
                             package 'nelisp-native-package-default) 3)
                         (= (nelisp-bytecode-native-package-native-call-count
                             package 'nelisp-native-package-optional-value) 3))
              (error "cold package calls did not use both native entries"))
            (garbage-collect)
            (setcar supplied 'changed)
            (setcdr supplied '(tail))
            (garbage-collect)
            (unless (eq (nelisp-bytecode-native-package-call
                         package 'nelisp-native-package-default
                         (list base supplied)) supplied)
              (error "native package lost supplied object identity across GC"))
            (unless (eq (nelisp-bytecode-native-package-call
                         package 'nelisp-native-package-optional-value
                         (list base supplied)) supplied)
              (error "native optional-value lost object identity across GC"))
            (let ((before
                   (nelisp-bytecode-native-package-native-call-count
                    package 'nelisp-native-package-default))
                  (nelisp-bytecode-native-package-native-enabled nil))
              (setq vm-disabled
                    (nelisp-bytecode-native-package-call
                     package 'nelisp-native-package-default (list base)))
              (unless (and (eq vm-disabled base)
                           (= before
                              (nelisp-bytecode-native-package-native-call-count
                               package 'nelisp-native-package-default)))
                (error "disabled-native control did not use the VM")))
            (fset 'nelisp-native-package-default
                  (lambda (_value) 'redefined))
            (unless (and (eq (nelisp-bytecode-native-package-call
                              package 'nelisp-native-package-default (list base))
                             'redefined)
                         (= (nelisp-bytecode-native-package-native-call-count
                             package 'nelisp-native-package-default) 4)
                         (= (nelisp-bytecode-native-package-native-call-count
                             package 'nelisp-native-package-optional-value) 4))
              (error "package call ignored function-cell redefinition"))
            (princ "native-calls: default=4 optional-value=4; vm-disabled-control=PASS; redefinition=PASS\n")
            t)
        (nelisp-bytecode-native-package-close package)
        (nelisp-bytecode-native-package-close second-open)))))

(defun nelisp-test-bytecode-native-package-stale-control ()
  "Refuse a stale artifact before loading any .elc top-level form."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest (getenv "NELISP_STALE_PACKAGE_MANIFEST")))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (unless (and (condition-case nil
                     (progn (nelisp-bytecode-native-package-open manifest) nil)
                   (error t))
                 (not (boundp 'nelisp-native-package-coldload-count)))
      (error "stale package was admitted or ran .elc side effects"))
    t))

(defun nelisp-test-bytecode-native-package-truncated-control ()
  "Reject a truncated authenticated .elc before evaluating its first form."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest-path (getenv "NELISP_TRUNCATED_PACKAGE_MANIFEST")))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (let* ((manifest
            (nelisp-bytecode-native-package--read-manifest manifest-path))
           (elc-path
            (expand-file-name (plist-get manifest :elc)
                              (file-name-directory manifest-path))))
      (with-temp-buffer
        (insert ";ELC\n(defvar nelisp-native-package-truncated-count 0)\n"
                "(setq nelisp-native-package-truncated-count 1)\n(provide")
        (write-region (buffer-string) nil elc-path nil 'silent))
      (plist-put manifest :elc-sha256
                 (nelisp-bytecode-native-package--sha256-file elc-path))
      (with-temp-file manifest-path (prin1 manifest (current-buffer)))
      (unless (and (condition-case nil
                       (progn
                         (nelisp-bytecode-native-package-open manifest-path)
                         nil)
                     (error t))
                   (not (boundp 'nelisp-native-package-truncated-count)))
        (error "truncated .elc ran an earlier top-level side effect")))
    t))

(defun nelisp-test-bytecode-native-package-tampered-manifest-control ()
  "Reject manifest path escape and duplicate entries before .elc evaluation."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest-path (getenv "NELISP_TAMPERED_PACKAGE_MANIFEST")))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (let ((original
           (nelisp-bytecode-native-package--read-manifest manifest-path)))
      (plist-put original :elc "../outside.elc")
      (with-temp-file manifest-path (prin1 original (current-buffer)))
      (unless (and (condition-case nil
                       (progn
                         (nelisp-bytecode-native-package-open manifest-path)
                         nil)
                     (error t))
                   (not (boundp 'nelisp-native-package-coldload-count)))
        (error "manifest path traversal reached .elc evaluation"))
      (plist-put original :elc "module.elc")
      (with-temp-file manifest-path (prin1 original (current-buffer)))
      (setq original
            (nelisp-bytecode-native-package--read-manifest manifest-path))
      (let ((entries (plist-get original :entries)))
        (plist-put original :entries (append entries (list (car entries)))))
      (with-temp-file manifest-path (prin1 original (current-buffer)))
      (unless (and (condition-case nil
                       (progn
                         (nelisp-bytecode-native-package-open manifest-path)
                         nil)
                     (error t))
                   (not (boundp 'nelisp-native-package-coldload-count)))
        (error "duplicate manifest entries reached .elc evaluation")))
    t))

(defun nelisp-test-bytecode-native-package-race-publish ()
  "Publish the fixture while holding both processes at the absent-dir check."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_PACKAGE_ELC"))
         (directory (expand-file-name (getenv "NELISP_RACE_PACKAGE_DIRECTORY")))
         (barrier (getenv "NELISP_RACE_BARRIER"))
         (identifier (getenv "NELISP_RACE_ID"))
         (own-ready (expand-file-name (concat "ready-" identifier) barrier))
         (other-ready (expand-file-name
                       (concat "ready-" (if (equal identifier "a") "b" "a"))
                       barrier))
         (file-exists-p-function (symbol-function 'file-exists-p))
         (arrived nil)
         (result nil))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (unwind-protect
        (progn
          (fset 'file-exists-p
                (lambda (path)
                  (let ((exists (funcall file-exists-p-function path)))
                    (when (and (not arrived) (not exists)
                               (equal (expand-file-name path) directory))
                      (setq arrived t)
                      (with-temp-file own-ready (insert identifier))
                      (let ((deadline (+ (float-time) 30.0)))
                        (while (and (not (funcall file-exists-p-function own-ready))
                                    (< (float-time) deadline))
                          (sit-for 0.01))
                        (while (and (not (funcall file-exists-p-function other-ready))
                                    (< (float-time) deadline))
                          (sit-for 0.01)))
                      (unless (and (funcall file-exists-p-function own-ready)
                                   (funcall file-exists-p-function other-ready))
                        (error "package publication race barrier timed out")))
                    exists)))
          (setq result
                (nelisp-bytecode-native-package-compile-elc
                 elc 'nelisp-native-package-coldload-fixture
                 '(nelisp-native-package-default
                   nelisp-native-package-optional-value)
                 directory))
          (unless (eq (plist-get result :status) 'complete)
            (error "publication race winner returned %S" result))
          t)
      (fset 'file-exists-p file-exists-p-function))))

(defun nelisp-test-bytecode-native-package-fingerprint-control ()
  "Reject an ABI fingerprint mismatch before .elc effects or artifact reads."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (manifest-path (getenv "NELISP_FINGERPRINT_PACKAGE_MANIFEST"))
         (directory (file-name-directory manifest-path))
         (manifest nil)
         (hashes-before nil)
         (hashes-after nil)
         (failure nil))
    (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
          nil nil t)
    (setq manifest
          (nelisp-bytecode-native-package--read-manifest manifest-path))
    (setq manifest
          (plist-put manifest :abi-fingerprint "tampered-abi-fingerprint"))
    (with-temp-file manifest-path (prin1 manifest (current-buffer)))
    (setq hashes-before
          (mapcar (lambda (path)
                    (cons (file-name-nondirectory path)
                          (nelisp-bytecode-native-package--sha256-file path)))
                  (directory-files directory t "\\`[^.]")))
    (when (boundp 'nelisp-native-package-coldload-count)
      (makunbound 'nelisp-native-package-coldload-count))
    (when (featurep 'nelisp-native-package-coldload-fixture)
      (error "fingerprint control started with fixture already provided"))
    (setq failure
          (condition-case error-data
              (progn (nelisp-bytecode-native-package-open manifest-path) nil)
            (error (error-message-string error-data))))
    (setq hashes-after
          (mapcar (lambda (path)
                    (cons (file-name-nondirectory path)
                          (nelisp-bytecode-native-package--sha256-file path)))
                  (directory-files directory t "\\`[^.]")))
    (unless (and (equal failure
                        "bytecode-native-package: ABI fingerprint mismatch")
                 (not (boundp 'nelisp-native-package-coldload-count))
                 (not (featurep 'nelisp-native-package-coldload-fixture))
                 (not (fboundp 'nelisp-native-package-default))
                 (equal hashes-before hashes-after))
      (error "fingerprint mismatch caused package effects or artifact changes"))
    t))

(provide 'standalone-bytecode-native-package-coldload-driver)
;;; standalone-bytecode-native-package-coldload-driver.el ends here
