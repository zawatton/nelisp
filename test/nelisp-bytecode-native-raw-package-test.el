;;; nelisp-bytecode-native-raw-package-test.el --- raw package boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-native-raw-package)

(defconst nelisp-bytecode-native-raw-package-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(defvar nelisp-bytecode-native-raw-package-test--side-effect nil)

(defun nelisp-bytecode-native-raw-package-test--fixture (root function-body)
  "Write a genuine GNU 31.1 ELC fixture in ROOT using FUNCTION-BODY."
  (unless (equal emacs-version "31.1")
    (error "Raw package fixture requires GNU Emacs 31.1, got %s" emacs-version))
  (let ((source (expand-file-name "module.el" root)))
    (with-temp-file source
      (insert ";;; -*- lexical-binding: t; -*-\n"
              "(defvar nelisp-bytecode-native-raw-package-test--side-effect nil)\n"
              "(setq nelisp-bytecode-native-raw-package-test--side-effect t)\n"
              (format "(defun raw-package-fixture (value) %s)\n" function-body)
              "(provide 'raw-package-fixture)\n"))
    (unless (byte-compile-file source) (error "GNU 31.1 fixture compilation failed"))
    (delete-file source)
    (concat source "c")))

(defun nelisp-bytecode-native-raw-package-test--artifact (path entry)
  (let ((operation (if (string-match-p "cdr" entry) "cdr" "car")))
  (with-temp-file path
    (insert ";;; nelisp-private-nelr-v1\n")
    (prin1 (list :native
                 (list :exports (list (list :name entry :type 'func
                                            :signature "u64(u64)"))
                       :imports (list (concat "nl_native_" operation "_v2"))))
           (current-buffer)))))

(defun nelisp-bytecode-native-raw-package-test--compile (elc directory)
  (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
             (lambda () "runtime-test-sha"))
            ((symbol-function 'nelisp-bytecode-native-package-abi-fingerprint)
             (lambda () "abi-test"))
            ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
             (lambda () t))
            ((symbol-function 'nelisp-bytecode-native-compiler-build)
             (lambda (_fn artifact entry)
               (nelisp-bytecode-native-raw-package-test--artifact artifact entry)
               (list :status 'complete)))
            ((symbol-function 'nelisp-native-load-raw-v2-check)
             (lambda (_manifest _entry) nil)))
    (nelisp-bytecode-native-raw-package-compile-elc
     elc 'raw-package-fixture 'raw-package-fixture directory)))

(defun nelisp-bytecode-native-raw-package-test--open-stubs (thunk)
  (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
             (lambda () "runtime-test-sha"))
            ((symbol-function 'nelisp-bytecode-native-package-abi-fingerprint)
             (lambda () "abi-test"))
            ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
             (lambda () t))
            ((symbol-function 'nelisp-native-load-raw-v2-check)
             (lambda (manifest _entry)
               (let* ((native (plist-get manifest :native))
                      (export (car (plist-get native :exports)))
                      (operation (if (member "nl_native_cdr_v2"
                                              (plist-get native :imports))
                                     "cdr" "car")))
                 (unless (and (equal (plist-get export :name)
                                    (format "nl_native_%s_probe" operation))
                              (eq (plist-get export :type) 'func)
                              (equal (plist-get native :imports)
                                     (list (format "nl_native_%s_v2" operation))))
                   '(:raw-export-import-mismatch))))))
    (funcall thunk)))

(ert-deftest nelisp-bytecode-native-raw-package-rejects-before-effects-and-red-control-reaches-backend ()
  (let* ((root (make-temp-file "nelisp-raw-preflight-" t))
         (elc (nelisp-bytecode-native-raw-package-test--fixture root '(+ value 1)))
         (out (expand-file-name "unsupported" root)) (backend 0) (mkdirs 0)
         (mkdir-function (symbol-function 'make-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
                   (lambda () "runtime-test-sha"))
                  ((symbol-function 'nelisp-bytecode-native-package-abi-fingerprint)
                   (lambda () "abi-test"))
                  ((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                   (lambda () '(:status pinned :dialect "GNU Emacs 31.1")))
                  ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
                   (lambda () t))
                  ((symbol-function 'make-directory)
                   (lambda (&rest args) (setq mkdirs (1+ mkdirs))
                     (apply mkdir-function args)))
                  ((symbol-function 'nelisp-bytecode-native-compiler-build)
                   (lambda (_fn artifact entry) (setq backend (1+ backend))
                     (nelisp-bytecode-native-raw-package-test--artifact artifact entry)
                     (list :status 'complete))))
          (should-not (nelisp-bytecode-native-compiler-unary-template-operation
                       (nelisp-bytecode-compiler-input-build
                        (nelisp-bytecode-native-package-read-elc-function
                         elc 'raw-package-fixture))))
          (should-error (nelisp-bytecode-native-raw-package-compile-elc
                         elc 'raw-package-fixture 'raw-package-fixture out))
          (should (= backend 0))
          (should (= mkdirs 0))
          (cl-letf (((symbol-function 'nelisp-bytecode-native-compiler-unary-template-operation)
                     (lambda (_input) 'car)))
            (should (eq (plist-get (nelisp-bytecode-native-raw-package-compile-elc
                                   elc 'raw-package-fixture 'raw-package-fixture out)
                                  :status)
                        'complete)))
          (should (> backend 0)))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-raw-package-import-export-tampering-refuses-before-eval-or-mapping ()
  (let* ((root (make-temp-file "nelisp-raw-tamper-" t))
         (elc (nelisp-bytecode-native-raw-package-test--fixture root '(car value))))
    (unwind-protect
        (progn
          (dolist (mutation '(outer export import))
            (let* ((dir (expand-file-name (format "package-%s" mutation) root))
                   (result (nelisp-bytecode-native-raw-package-test--compile elc dir))
                   (manifest-path (plist-get result :manifest))
                   (outer (with-temp-buffer (insert-file-contents manifest-path)
                            (read (current-buffer))))
                   (artifact (expand-file-name (plist-get outer :artifact) dir))
                   (inner (nelisp-native-load-manifest artifact))
                   (native (plist-get inner :native)))
              (unless (eq mutation 'outer)
                (if (eq mutation 'export)
                    (setf (plist-get (car (plist-get native :exports)) :type) 'data)
                  (setf (plist-get native :imports) '("nl_native_cdr_v2")))
                (with-temp-file artifact
                  (insert ";;; nelisp-private-nelr-v1\n") (prin1 inner (current-buffer)))
                (setf (plist-get outer :artifact-sha256)
                      (nelisp-bytecode-native-package-raw-file-sha256 artifact)))
              (when (eq mutation 'outer)
                (setf (plist-get outer :operation) 'cdr))
              (with-temp-file manifest-path (prin1 outer (current-buffer)))
              (let ((nelisp-bytecode-native-raw-package-test--side-effect nil)
                    (nelisp-native-load-raw-mappings nil))
                (nelisp-bytecode-native-raw-package-test--open-stubs
                 (lambda ()
                   (should-error (nelisp-bytecode-native-raw-package-open manifest-path))))
                (should-not nelisp-bytecode-native-raw-package-test--side-effect)
                (should-not nelisp-native-load-raw-mappings))))
      (delete-directory root t)))))

(ert-deftest nelisp-bytecode-native-raw-package-preexisting-feature-is-vm-only ()
  (let* ((root (make-temp-file "nelisp-raw-hijack-" t))
         (elc (nelisp-bytecode-native-raw-package-test--fixture root '(car value)))
         (dir (expand-file-name "package" root)) (result nil)
         (feature 'raw-package-fixture)
         (nelisp-bytecode-native-raw-package--verified-features (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--handles (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--calls (make-hash-table :test 'eq))
         (nelisp-bytecode-native-package-native-enabled t)
         (features (cons feature (delq feature (copy-sequence features))))
         (old-cell (and (fboundp 'raw-package-fixture)
                        (symbol-function 'raw-package-fixture))))
    (unwind-protect
        (progn
          (fset 'raw-package-fixture (lambda (x) (list 'hijacked x)))
          (setq result (nelisp-bytecode-native-raw-package-test--compile elc dir))
          (nelisp-bytecode-native-raw-package-test--open-stubs
           (lambda ()
             (cl-letf (((symbol-function 'featurep) (lambda (_feature) t)))
             (let ((handle (nelisp-bytecode-native-raw-package-open
                            (plist-get result :manifest))))
               (should (equal (nelisp-bytecode-native-raw-package-call handle 'x)
                              '(hijacked x)))
               (should (= (nelisp-bytecode-native-raw-package-native-call-count handle) 0))
               (should-not (nth 4 handle))))))
      (if old-cell (fset 'raw-package-fixture old-cell)
        (fmakunbound 'raw-package-fixture))
      (delete-directory root t)))))

(ert-deftest nelisp-bytecode-native-raw-package-second-open-changed-cell-stays-vm-only ()
  (let* ((root (make-temp-file "nelisp-raw-provenance-" t))
         (elc (nelisp-bytecode-native-raw-package-test--fixture root '(car value)))
         (dir (expand-file-name "package" root)) (result nil)
         (nelisp-bytecode-native-raw-package--verified-features (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--handles (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--calls (make-hash-table :test 'eq))
         (nelisp-bytecode-native-package-native-enabled t)
         (features (delq 'raw-package-fixture (copy-sequence features)))
         (old-cell (and (fboundp 'raw-package-fixture)
                        (symbol-function 'raw-package-fixture)))
         (evals 0))
    (unwind-protect
        (progn
          (setq result (nelisp-bytecode-native-raw-package-test--compile elc dir))
          (nelisp-bytecode-native-raw-package-test--open-stubs
           (lambda ()
             (let ((feature-count 0)
                   (feature-function (symbol-function 'featurep))
                   (eval-function
                    (symbol-function 'nelisp-bytecode-native-package-raw-eval-elc-forms)))
             (cl-letf (((symbol-function 'featurep)
                        (lambda (feature)
                          (if (eq feature 'raw-package-fixture)
                              (prog1 (> feature-count 0) (setq feature-count (1+ feature-count)))
                            (funcall feature-function feature))))
                       ((symbol-function 'nelisp-bytecode-native-package-raw-eval-elc-forms)
                        (lambda (forms) (setq evals (1+ evals))
                          (funcall eval-function forms))))
             (let* ((path (plist-get result :manifest))
                    (first (nelisp-bytecode-native-raw-package-open path))
                    (second (nelisp-bytecode-native-raw-package-open path)))
               (should (= evals 1))
               (fset 'raw-package-fixture (lambda (x) (list 'changed x)))
               (should (equal (nelisp-bytecode-native-raw-package-call second 'x)
                              '(changed x)))
               (should (= (nelisp-bytecode-native-raw-package-native-call-count second) 0))
               (should-not (nth 4 second))
               (nelisp-bytecode-native-raw-package-close first)
               (nelisp-bytecode-native-raw-package-close second))))))
      (if old-cell (fset 'raw-package-fixture old-cell)
        (fmakunbound 'raw-package-fixture))
      (delete-directory root t)))))

(ert-deftest nelisp-bytecode-native-raw-package-close-refuses-active-and-blocks-post-close-call ()
  (let* ((unloads 0) (entries 0)
         (nelisp-bytecode-native-raw-package--handles (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--calls (make-hash-table :test 'eq))
         (nelisp-bytecode-native-package-native-enabled t)
         (original (lambda (_x) :vm))
         (handle (list nelisp-bytecode-native-raw-package--format "path"
                       '(:name raw-package-fixture) original 'mapping 1 nil)))
    (puthash handle t nelisp-bytecode-native-raw-package--handles)
    (cl-letf (((symbol-function 'nelisp-native-load-unload)
               (lambda (_mapping) (setq unloads (1+ unloads))))
              ((symbol-function 'nelisp-native-load-raw-v2-artifact)
               (lambda (&rest _) (setq entries (1+ entries))))
              ((symbol-function 'raw-package-fixture) original))
      (should-error (nelisp-bytecode-native-raw-package-close handle))
      (should (= unloads 0))
      (setcar (nthcdr 5 handle) 0)
      (should (nelisp-bytecode-native-raw-package-close handle))
      (should-error (nelisp-bytecode-native-raw-package-call handle 'x))
      (should (= entries 0))
      (should (= unloads 1)))))

(ert-deftest nelisp-bytecode-native-raw-package-different-template-same-feature-stays-vm-only ()
  (let* ((root (make-temp-file "nelisp-raw-provenance-" t))
         (nelisp-bytecode-native-raw-package--verified-features
          (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--handles (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--calls (make-hash-table :test 'eq))
         (nelisp-bytecode-native-package-native-enabled t)
         (car-dir (expand-file-name "car" root))
         (cdr-dir (expand-file-name "cdr" root))
         car-handle cdr-handle native-calls)
    (unwind-protect
        (progn
          (make-directory car-dir)
          (make-directory cdr-dir)
          (let ((car-elc (nelisp-bytecode-native-raw-package-test--fixture
                          car-dir '(car value)))
                (cdr-elc (nelisp-bytecode-native-raw-package-test--fixture
                          cdr-dir '(cdr value))))
            (nelisp-bytecode-native-raw-package-test--open-stubs
             (lambda ()
               (setq car-handle
                     (nelisp-bytecode-native-raw-package-open
                      (plist-get (nelisp-bytecode-native-raw-package-test--compile
                                  car-elc (expand-file-name "car-package" root))
                                 :manifest)))
               (setq cdr-handle
                     (nelisp-bytecode-native-raw-package-open
                      (plist-get (nelisp-bytecode-native-raw-package-test--compile
                                  cdr-elc (expand-file-name "cdr-package" root))
                                 :manifest)))))
            (cl-letf (((symbol-function 'nelisp-native-load-raw-v2-artifact)
                       (lambda (&rest _) :mapping))
                      ((symbol-function 'nelisp-native-load-raw-v2-car-call)
                       (lambda (&rest _) (setq native-calls (1+ (or native-calls 0)))))
                      ((symbol-function 'nelisp-native-load-raw-v2-cdr-call)
                       (lambda (&rest _) (setq native-calls (1+ (or native-calls 0))))))
              (should (equal (nelisp-bytecode-native-raw-package-call
                              cdr-handle '(head . tail))
                             'head))
              (should (= (nelisp-bytecode-native-raw-package-native-call-count
                          cdr-handle) 0))
              (should-not native-calls))
            (nelisp-bytecode-native-raw-package-close car-handle)
            (nelisp-bytecode-native-raw-package-close cdr-handle)))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-raw-package-refuses-artifact-change-before-first-map ()
  (let* ((root (make-temp-file "nelisp-raw-artifact-race-" t))
         (nelisp-bytecode-native-raw-package--verified-features
          (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--handles (make-hash-table :test 'eq))
         (nelisp-bytecode-native-raw-package--calls (make-hash-table :test 'eq))
         (nelisp-bytecode-native-package-native-enabled t)
         (features (delq 'raw-package-fixture (copy-sequence features)))
         (elc (nelisp-bytecode-native-raw-package-test--fixture root '(car value)))
         handle artifact maps)
    (unwind-protect
        (let ((feature-count 0)
              (feature-function (symbol-function 'featurep)))
          (cl-letf (((symbol-function 'featurep)
                     (lambda (feature)
                       (if (eq feature 'raw-package-fixture)
                           (prog1 (> feature-count 0) (setq feature-count (1+ feature-count)))
                         (funcall feature-function feature)))))
           (nelisp-bytecode-native-raw-package-test--open-stubs
            (lambda ()
           (setq handle
                 (nelisp-bytecode-native-raw-package-open
                  (plist-get (nelisp-bytecode-native-raw-package-test--compile
                              elc (expand-file-name "package" root)) :manifest)))
           (setq artifact (expand-file-name
                           (plist-get (nth 2 handle) :artifact)
                           (file-name-directory (nth 1 handle))))
           (with-temp-buffer
             (insert-file-contents artifact)
             (goto-char (point-max))
             (insert "tampered")
             (write-region (point-min) (point-max) artifact nil 'silent))
           (cl-letf (((symbol-function 'nelisp-native-load-raw-v2-artifact)
                      (lambda (&rest _) (setq maps (1+ (or maps 0)))))
                     ((symbol-function 'nelisp-native-load-raw-v2-car-call)
                      (lambda (&rest _) :native)))
             (should-error (nelisp-bytecode-native-raw-package-call handle '(head . tail)))
             (should-not maps)
             (should (= (nelisp-bytecode-native-raw-package-native-call-count handle) 0)))
           (nelisp-bytecode-native-raw-package-close handle)))))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-package-rejects-raw-unary-chain-before-effects ()
  (let* ((root (make-temp-file "nelisp-boxed-raw-chain-" t))
         (elc (nelisp-bytecode-native-raw-package-test--fixture
               root '(car (cdr value))))
         (function (nelisp-bytecode-native-package-read-elc-function
                    elc 'raw-package-fixture))
         (input (nelisp-bytecode-compiler-input-build function))
         (out (expand-file-name "package" root))
         (backend 0) (mkdirs 0)
         (mkdir-function (symbol-function 'make-directory)))
    (unwind-protect
        (progn
          (should (nelisp-bytecode-native-compiler-unary-chain-operations input))
          (cl-letf (((symbol-function 'nelisp-bytecode-native-package--abi-fingerprint)
                     (lambda () "abi-test"))
                    ((symbol-function 'make-directory)
                     (lambda (&rest args) (setq mkdirs (1+ mkdirs))
                       (apply mkdir-function args)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _) (setq backend (1+ backend))
                       (list :status 'unsupported))))
            (should-error
             (nelisp-bytecode-native-package-compile-elc
              elc 'raw-package-fixture '(raw-package-fixture) out))
            (should (= mkdirs 0))
            (should (= backend 0))
            (should-not (file-exists-p out))))
      (delete-directory root t))))

(provide 'nelisp-bytecode-native-raw-package-test)
;;; nelisp-bytecode-native-raw-package-test.el ends here
