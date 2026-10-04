;;; standalone-bytecode-native-artifact-lifecycle.el --- artifact lifecycle smoke -*- lexical-binding: t; -*-

(defun nelisp-test-artifact-lifecycle--tamper-manifest
    (source destination key value)
  "Copy SOURCE to DESTINATION, changing top-level manifest KEY to VALUE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally source)
    (let* ((newline (string-match "\n" (buffer-string)))
           (header (and newline (substring (buffer-string) 0 (1+ newline))))
           (manifest (and newline
                          (car (read-from-string
                                (substring (buffer-string) (1+ newline)))))))
      (unless (and header (listp manifest))
        (error "cannot parse raw artifact manifest for tamper control"))
      (plist-put manifest key value)
      (plist-put manifest :artifact-sha256
                 (nelisp-native-load--sha256
                  (prin1-to-string
                   (nelisp-native-load--raw-plist-without
                    manifest :artifact-sha256))))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert header (prin1-to-string manifest) "\n")
        (write-region (point-min) (point-max) destination nil 'silent)))))

(defun nelisp-test-bytecode-native-artifact-lifecycle ()
  "Check deterministic raw manifests, invalidation, and native lifecycle.

All functions below are materialized byte-code objects. No source form is
compiled or evaluated to produce the native artifacts."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (dir (getenv "NELISP_ARTIFACT_LIFECYCLE_DIR"))
         (code-a (unibyte-string 192 135))
         (code-b (unibyte-string 193 135))
         (function-a (make-byte-code nil code-a [7] 1))
         (function-a-again (make-byte-code nil code-a [7] 1))
         (function-bytecode-change (make-byte-code nil code-b [7 9] 1))
         (function-constant-change (make-byte-code nil code-a [8] 1))
         (path-a (expand-file-name "same-a.nelr" dir))
         (path-again (expand-file-name "same-b.nelr" dir))
         (path-code (expand-file-name "changed-code.nelr" dir))
         (path-constant (expand-file-name "changed-constant.nelr" dir))
         (path-bad-abi (expand-file-name "bad-abi.nelr" dir))
         (path-bad-sha (expand-file-name "bad-sha.nelr" dir))
         (boxed-path (expand-file-name "boxed.neln" dir))
         (artifact-a nil) (artifact-again nil) (artifact-code nil)
         (artifact-constant nil) (unit-id nil) (generation 0)
         (boxed-unit nil) (root-object (cons 'artifact-root nil)))
    (unless (and root dir (file-directory-p dir))
      (error "artifact lifecycle paths are unset"))
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (require 'nelisp-bytecode-native-compiler)
    (require 'nelisp-bytecode-native-compiler-raw)
    (require 'nelisp-native-boxed-unit)
    (require 'nelisp-native-unit)
    (setq artifact-a (nelisp-bytecode-native-compiler-raw-build
                      function-a path-a "nl_artifact_lifecycle_raw" nil)
          artifact-again (nelisp-bytecode-native-compiler-raw-build
                          function-a-again path-again "nl_artifact_lifecycle_raw" nil)
          artifact-code (nelisp-bytecode-native-compiler-raw-build
                         function-bytecode-change path-code "nl_artifact_lifecycle_raw" nil)
          artifact-constant (nelisp-bytecode-native-compiler-raw-build
                             function-constant-change path-constant "nl_artifact_lifecycle_raw" nil))
    (unless (and (cl-every (lambda (result) (eq (plist-get result :status) 'complete))
                           (list artifact-a artifact-again artifact-code artifact-constant))
                 (cl-every #'file-readable-p (list path-a path-again path-code path-constant)))
      (error "source-free raw artifact build failed: %S %S %S %S"
             artifact-a artifact-again artifact-code artifact-constant))
    (let* ((manifest-a (nelisp-native-load-manifest path-a))
           (manifest-again (nelisp-native-load-manifest path-again))
           (manifest-code (nelisp-native-load-manifest path-code))
           (manifest-constant (nelisp-native-load-manifest path-constant))
           (native-a (plist-get manifest-a :native))
           (native-again (plist-get manifest-again :native))
           (native-code (plist-get manifest-code :native))
           (native-constant (plist-get manifest-constant :native))
           (stable (list :kind :format :runtime-abi :layout-id :binary-sha256
                         :source-sha256 :artifact-sha256))
           (stable-native '(:object-sha256 :object-size :text-size :data-size
                            :bss-size :imports :defuns)))
      (unless (and (equal (mapcar (lambda (key) (plist-get manifest-a key)) stable)
                          (mapcar (lambda (key) (plist-get manifest-again key)) stable))
                   (equal (mapcar (lambda (key) (plist-get native-a key)) stable-native)
                          (mapcar (lambda (key) (plist-get native-again key)) stable-native)))
        (error "equivalent source-free builds differ in stable manifest fields"))
      (unless (and (not (equal (plist-get native-a :object-sha256)
                               (plist-get native-code :object-sha256)))
                   (not (equal (plist-get native-a :object-sha256)
                               (plist-get native-constant :object-sha256))))
        (error "byte-code or constant change did not invalidate native object identity"))
      (let ((bad-abi (copy-tree manifest-a))
            (bad-sha (copy-tree manifest-a)))
        (plist-put bad-abi :runtime-abi "nelisp-runtime-raw-invalid")
        (plist-put bad-sha :binary-sha256 (make-string 64 ?0))
        (dolist (bad (list bad-abi bad-sha))
          (plist-put bad :artifact-sha256
                     (nelisp-native-load--sha256
                      (prin1-to-string
                       (nelisp-native-load--raw-plist-without
                        bad :artifact-sha256)))))
        (unless (memq :raw-runtime-abi
                      (mapcar #'car
                              (nelisp-native-load-raw-check
                               bad-abi "nl_artifact_lifecycle_raw")))
          (error "negative control: changed runtime ABI was not identified"))
        (unless (null (nelisp-native-load-raw-check
                       bad-sha "nl_artifact_lifecycle_raw"))
          (error "negative control: SHA-only artifact was malformed")))
      (nelisp-test-artifact-lifecycle--tamper-manifest
       path-a path-bad-abi :runtime-abi "nelisp-runtime-raw-invalid")
      (nelisp-test-artifact-lifecycle--tamper-manifest
       path-a path-bad-sha :binary-sha256 (make-string 64 ?0)))
    ;; Exercise actual stage/publish/call replacement generations.
    (dotimes (_ 3)
      (let* ((staged (nelisp-native-unit-stage path-a unit-id
                                               '("nl_artifact_lifecycle_raw")))
             (candidate (plist-get staged :candidate-id))
             (published nil))
        (unless (eq (plist-get staged :status) 'staged)
          (error "native stage rejected valid artifact: %S" staged))
        (setq unit-id (plist-get staged :unit-id)
              published (nelisp-native-unit-publish candidate))
        (unless (eq (plist-get published :status) 'published)
          (error "native publish failed: %S" published))
        (setq generation (plist-get published :generation))
        (garbage-collect)
        (unless (= (nelisp-native-unit-call unit-id "nl_artifact_lifecycle_raw" nil) 7)
          (error "published native entry returned wrong value"))
        (when (= (1+ _) 1)
          (dolist (bad-path (list path-bad-abi path-bad-sha))
            (let ((refused (nelisp-native-unit-stage
                            bad-path unit-id '("nl_artifact_lifecycle_raw"))))
              (unless (eq (plist-get refused :status) 'rejected)
                (error "negative control: stage accepted tampered artifact %s: %S"
                       bad-path refused))))
          (unless (= (plist-get (nelisp-native-unit-status unit-id) :generation) 1)
            (error "rejected artifact advanced active generation")))))
    ;; A separately loaded boxed entry must retain its constant root and return
    ;; the identical live Lisp object across repeated collections.
    (let ((boxed-build
           (nelisp-bytecode-native-compiler-build
            (make-byte-code '(x) (unibyte-string 8 135) [x] 1)
            boxed-path "nl_artifact_lifecycle_boxed")))
      (unless (and (eq (plist-get boxed-build :status) 'complete)
                   (file-readable-p boxed-path))
        (error "boxed lifecycle artifact build failed: %S" boxed-build)))
    (dotimes (_ 3)
      (unwind-protect
          (progn
            (setq boxed-unit
                  (nelisp-native-boxed-unit-open-with-constants
                   boxed-path "nl_artifact_lifecycle_boxed" [root-object] 1))
            (dotimes (_ 2)
              (garbage-collect)
              (unless (eq (nelisp-native-boxed-unit-call boxed-unit (list root-object))
                          root-object)
                (error "boxed root/object identity changed across GC"))))
        (when boxed-unit
          (nelisp-native-boxed-unit-close boxed-unit)
          (setq boxed-unit nil))))
    (unless (= generation 3)
      (error "expected three published generations, got %S" generation))
    (princ "NATIVE-BYTECODE-ARTIFACT-LIFECYCLE: PASS\n")
    t))

(provide 'standalone-bytecode-native-artifact-lifecycle)
;;; standalone-bytecode-native-artifact-lifecycle.el ends here
