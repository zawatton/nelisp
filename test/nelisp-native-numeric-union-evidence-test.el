;;; nelisp-native-numeric-union-evidence-test.el --- Host-only union selection -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-compiler-startup-evidence)
(require 'nelisp-native-rooted-startup-evidence)
(defconst nelisp-numeric-union-test--root
  (expand-file-name (or (getenv "NELISP_UNION_CANONICAL_ROOT")
                       (expand-file-name ".." (file-name-directory load-file-name)))))
(defun nelisp-numeric-union-test--derive (path generator)
  (nelisp-native-compiler-startup-evidence--rewrite
   (nelisp-native-compiler-startup-evidence--rename
    (nelisp-native-compiler-startup-evidence--forms path)) generator))
(let ((source (or (getenv "NELISP_UNION_RENDER_SOURCE")
                  (expand-file-name "lisp/nelisp-native-rooted-startup-evidence.el"
                                    nelisp-numeric-union-test--root))))
  (dolist (form (nelisp-numeric-union-test--derive source t)) (eval form t)))
(ert-deftest nelisp-numeric-union-genuine-and-counterfeit-selection ()
  (let* ((certificate (getenv "NELISP_NUMERIC_CERTIFICATE"))
         (directory (getenv "NELISP_NUMERIC_CAPTURE"))
         (review (getenv "NELISP_NUMERIC_BINDING_SHA256"))
         (template (make-temp-file "numeric-union-template-"))
         (mutant (make-temp-file "numeric-union-mutant-"))
         (nelisp-native-compiler-startup-evidence--source-root nelisp-numeric-union-test--root)
         (capture (list :manifest (expand-file-name "active-build.json" directory)
                        :metadata (expand-file-name "active-unit-metadata.json" directory)
                        :generated-data (expand-file-name "generated-data-owner.json" directory)))
         (layout (car (nelisp-native-load-rooted-production-contract)))
         (closure (car (nelisp-native-rooted-startup-evidence--json certificate 4194304))))
    (unwind-protect
        (progn
          (with-temp-file template
            (dolist (form (nelisp-native-compiler-startup-evidence--proof-api
                          (nelisp-numeric-union-test--derive
                           (expand-file-name "templates/nelisp-native-rooted-abi-proof.el.in"
                                             nelisp-numeric-union-test--root) nil)))
              (prin1 form (current-buffer)) (insert "\n")))
          (let* ((default-path (expand-file-name "prelink-closure.json" directory))
                 (default-digest (nelisp-native-rooted-build-evidence-source-hash default-path 4194304))
                 (default (nelisp-native-compiler-derived-startup-evidence-render
                           capture default-path default-digest template layout)))
            (when (getenv "NELISP_UNION_DEFAULT_OUTPUT")
              (with-temp-file (getenv "NELISP_UNION_DEFAULT_OUTPUT")
                (insert (plist-get default :startup-source))))
            (unless (getenv "NELISP_UNION_RENDER_SOURCE")
              (should (equal default
                             (nelisp-native-compiler-derived-startup-evidence-render
                              capture default-path default-digest template layout nil nil)))))
          (let* ((result (nelisp-native-compiler-derived-startup-evidence-render
                          capture certificate (nelisp-native-rooted-build-evidence-source-hash certificate 4194304)
                          template layout 'numeric-union review))
                 (expected (with-temp-buffer
                             (insert (plist-get result :startup-source))
                             (goto-char (point-min))
                             (cadr (nth 3 (read (current-buffer)))))))
            (should (= (length (plist-get expected :functions)) 164))
            (should (= (length (plist-get expected :protocol-roots)) 9))
            (should (equal (plist-get expected :operation-eligibility) '(constructor)))
            (should (= (length (plist-get expected :compiler-exports)) 8))
            (should (equal (plist-get expected :numeric-source-binding-sha256) review)))
          (should-error (nelisp-native-compiler-derived-startup-evidence-render
                         capture certificate (nelisp-native-rooted-build-evidence-source-hash certificate 4194304)
                         template layout 'numeric-union nil))
          (dolist (field '(domain roots source_binding_sha256 helper_count_policy operation_eligibility))
            (let ((copy (copy-tree closure)))
              (setf (alist-get field copy) "counterfeit")
              (with-temp-file mutant (insert (json-encode copy)))
              (should-error (nelisp-native-compiler-derived-startup-evidence-render
                             capture mutant (nelisp-native-rooted-build-evidence-source-hash mutant 4194304)
                             template layout 'numeric-union review))))
          (let ((copy (copy-tree closure)))
            (setf (alist-get 'size (car (alist-get 'records copy))) 65537)
            (with-temp-file mutant (insert (json-encode copy)))
            (should-error (nelisp-native-compiler-derived-startup-evidence-render
                           capture mutant (nelisp-native-rooted-build-evidence-source-hash mutant 4194304)
                           template layout 'numeric-union review))))
      (delete-file template) (delete-file mutant))))
(ert-run-tests-batch-and-exit)
