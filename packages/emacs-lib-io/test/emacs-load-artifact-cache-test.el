;;; emacs-load-artifact-cache-test.el --- Artifact cache validity ERT  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst emacs-load-artifact-cache-test--root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name))))

(let ((host-load (symbol-function 'load))
      (host-load-file (symbol-function 'load-file)))
  (let ((emacs-version 1)
        (native-comp-enable-subr-trampolines nil))
    (funcall host-load-file
             (expand-file-name "src/emacs-load.el"
                               emacs-load-artifact-cache-test--root)))
  (fset 'load host-load)
  (fset 'load-file host-load-file))

(defconst emacs-load-artifact-cache-test--compiler-identity
  '(:path "/tmp/fake-nelisp" :size 42 :mtime (1 2 3 4)))

(defun emacs-load-artifact-cache-test--write-sidecar
    (path source-hash &optional compiler-identity)
  "Write a current cache record for SOURCE-HASH to PATH."
  (with-temp-file path
    (prin1 (list :cache-record-version 1
                 :source-sha256 source-hash
                 :compiler
                 (or compiler-identity
                     emacs-load-artifact-cache-test--compiler-identity))
           (current-buffer))))

(defun emacs-load-artifact-cache-test--write-manifest
    (artifact schema layout native-section-version)
  "Write ARTIFACT's manifest using the supplied contract versions."
  (with-temp-file (concat artifact ".manifest.el")
    (prin1
     (list :format 'nelisp-elisp-artifact-manifest-v1
           :kind 'neln
           :artifact-format 'nelisp-private-nelc-v2
           :artifact-class 'native
           :runtime-abi "nelisp-neln-aot-v1"
           :compiler
           (list :artifact-schema-version schema
                 :artifact-layout-version layout
                 :native-section-version native-section-version))
     (current-buffer))))

(defun emacs-load-artifact-cache-test--write-valid-artifact (artifact)
  "Write a minimal current-contract ARTIFACT and manifest."
  (with-temp-file artifact
    (insert ";;; nelisp-private-nelc-v2\n")
    (prin1 '(:format nelisp-private-nelc-v2
             :kind neln
             :module-init nil)
           (current-buffer)))
  (emacs-load-artifact-cache-test--write-manifest artifact 3 nil 2))

(defmacro emacs-load-artifact-cache-test--with-cache
    (source &rest body)
  "Create an isolated cache fixture for SOURCE and evaluate BODY."
  (declare (indent 1))
  `(let* ((temp-dir (make-temp-file "emacs-load-artifact-cache-test-" t))
          (resolved (expand-file-name "sample.el" temp-dir))
          (artifact (expand-file-name "cache/sample.neln" temp-dir))
          (sidecar (concat artifact ".source-sha256"))
          (source ,source)
          (source-hash (emacs-load--artifact-cache-source-digest source))
          (compiler-path "/tmp/fake-nelisp")
          (compiler-identity
           emacs-load-artifact-cache-test--compiler-identity))
     (unwind-protect
         (progn
           (make-directory (file-name-directory artifact) t)
           ,@body)
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

(ert-deftest emacs-load-artifact-cache-test/cache-miss-creates-shard-after-no-op-mkdir ()
  (let* ((temp-dir (make-temp-file "emacs-load-artifact-cache-test-" t))
         (cache-root (expand-file-name "cache/" temp-dir))
         (resolved (expand-file-name "sample.el" temp-dir))
         (source "(defun emacs-load-artifact-cache-test--mkdir () t)\n")
         (compiler-path "/tmp/fake-nelisp")
         (compiler-identity
          emacs-load-artifact-cache-test--compiler-identity)
         (real-make-directory (symbol-function 'make-directory))
         (mkdir-calls nil)
         (compile-count 0)
         (replay-count 0)
         (artifact nil))
    (unwind-protect
        (cl-letf (((symbol-function 'emacs-load--artifact-cache-directory)
                   (lambda () cache-root))
                  ((symbol-function 'emacs-load--artifact-compiler)
                   (lambda () compiler-path))
                  ((symbol-function 'emacs-load--artifact-compiler-identity)
                   (lambda (_compiler) compiler-identity))
                  ((symbol-function 'make-directory)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'nelisp--syscall-path-int)
                   (lambda (number directory mode)
                     (push (list number directory mode) mkdir-calls)
                     (funcall real-make-directory directory nil)
                     0))
                  ((symbol-function 'call-process)
                   (lambda (&rest args)
                     (setq compile-count (1+ compile-count))
                     (setq artifact (nth 10 args))
                     (should (file-directory-p
                              (file-name-directory artifact)))
                     (emacs-load-artifact-cache-test--write-valid-artifact
                      artifact)
                     0))
                  ((symbol-function 'emacs-load--artifact-replay-file)
                   (lambda (_path)
                     (setq replay-count (1+ replay-count))
                     t)))
          (let ((emacs-load-auto-native-compile t))
            (should (eq (emacs-load--artifact-load-or-compile resolved source)
                        t)))
          (should (= compile-count 1))
          (should (= replay-count 1))
          (should (= (length mkdir-calls) 2))
          (should (equal (mapcar #'car mkdir-calls) '(83 83)))
          (should (equal (mapcar (lambda (call) (nth 2 call)) mkdir-calls)
                         '(448 448)))
          (should (file-readable-p artifact))
          (should (file-readable-p (concat artifact ".manifest.el")))
          (should (file-readable-p (concat artifact ".source-sha256"))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-artifact-cache-test/stale-compiler-identity-is-recompiled ()
  (emacs-load-artifact-cache-test--with-cache
      "(defun emacs-load-artifact-cache-test--stale-compiler () t)\n"
    (let ((compile-count 0)
          (replay-count 0))
      (emacs-load-artifact-cache-test--write-valid-artifact artifact)
      (emacs-load-artifact-cache-test--write-sidecar
       sidecar source-hash
       '(:path "/tmp/fake-nelisp" :size 43 :mtime (1 2 3 5)))
      (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                 (lambda (_resolved) (list artifact sidecar)))
                ((symbol-function 'emacs-load--artifact-compiler)
                 (lambda () compiler-path))
                ((symbol-function 'emacs-load--artifact-compiler-identity)
                 (lambda (_compiler) compiler-identity))
                ((symbol-function 'call-process)
                 (lambda (&rest _args)
                   (setq compile-count (1+ compile-count))
                   (emacs-load-artifact-cache-test--write-valid-artifact artifact)
                   0))
                ((symbol-function 'emacs-load--artifact-replay-file)
                 (lambda (_path)
                   (setq replay-count (1+ replay-count))
                   t)))
        (let ((emacs-load-auto-native-compile t))
          (should (eq (emacs-load--artifact-load-or-compile resolved source) t))))
      (should (= compile-count 1))
      (should (= replay-count 1)))))

(ert-deftest emacs-load-artifact-cache-test/legacy-plain-digest-sidecar-is-recompiled ()
  (emacs-load-artifact-cache-test--with-cache
      "(defun emacs-load-artifact-cache-test--legacy-sidecar () t)\n"
    (let ((compile-count 0)
          (replay-count 0))
      (emacs-load-artifact-cache-test--write-valid-artifact artifact)
      (with-temp-file sidecar
        (insert source-hash))
      (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                 (lambda (_resolved) (list artifact sidecar)))
                ((symbol-function 'emacs-load--artifact-compiler)
                 (lambda () compiler-path))
                ((symbol-function 'emacs-load--artifact-compiler-identity)
                 (lambda (_compiler) compiler-identity))
                ((symbol-function 'call-process)
                 (lambda (&rest _args)
                   (setq compile-count (1+ compile-count))
                   (emacs-load-artifact-cache-test--write-valid-artifact artifact)
                   0))
                ((symbol-function 'emacs-load--artifact-replay-file)
                 (lambda (_path)
                   (setq replay-count (1+ replay-count))
                   t)))
        (let ((emacs-load-auto-native-compile t))
          (should (eq (emacs-load--artifact-load-or-compile resolved source) t))))
      (should (= compile-count 1))
      (should (= replay-count 1)))))

(ert-deftest emacs-load-artifact-cache-test/corrupt-hit-recompiles-once ()
  (emacs-load-artifact-cache-test--with-cache
      "(defun emacs-load-artifact-cache-test--corrupt () t)\n"
    (let ((compile-count 0)
          (replay-count 0))
      (emacs-load-artifact-cache-test--write-valid-artifact artifact)
      (emacs-load-artifact-cache-test--write-sidecar sidecar source-hash)
      (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                 (lambda (_resolved) (list artifact sidecar)))
                ((symbol-function 'emacs-load--artifact-compiler)
                 (lambda () compiler-path))
                ((symbol-function 'emacs-load--artifact-compiler-identity)
                 (lambda (_compiler) compiler-identity))
                ((symbol-function 'call-process)
                 (lambda (&rest _args)
                   (setq compile-count (1+ compile-count))
                   (emacs-load-artifact-cache-test--write-valid-artifact artifact)
                   0))
                ((symbol-function 'emacs-load--artifact-replay-file)
                 (lambda (_path)
                   (setq replay-count (1+ replay-count))
                   (if (= replay-count 1)
                       (error "corrupt cached artifact")
                     t))))
        (let ((emacs-load-auto-native-compile t))
          (should (eq (emacs-load--artifact-load-or-compile resolved source) t))))
      (should (= compile-count 1))
      (should (= replay-count 2)))))

(ert-deftest emacs-load-artifact-cache-test/matching-record-is-a-hit ()
  (emacs-load-artifact-cache-test--with-cache
      "(defun emacs-load-artifact-cache-test--matching () t)\n"
    (let ((compile-count 0)
          (replay-count 0))
      (emacs-load-artifact-cache-test--write-valid-artifact artifact)
      (emacs-load-artifact-cache-test--write-sidecar sidecar source-hash)
      (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                 (lambda (_resolved) (list artifact sidecar)))
                ((symbol-function 'emacs-load--artifact-compiler)
                 (lambda () compiler-path))
                ((symbol-function 'emacs-load--artifact-compiler-identity)
                 (lambda (_compiler) compiler-identity))
                ((symbol-function 'call-process)
                 (lambda (&rest _args)
                   (setq compile-count (1+ compile-count))
                   0))
                ((symbol-function 'emacs-load--artifact-replay-file)
                 (lambda (_path)
                   (setq replay-count (1+ replay-count))
                   t)))
        (let ((emacs-load-auto-native-compile t))
          (should (eq (emacs-load--artifact-load-or-compile resolved source) t))))
      (should (= compile-count 0))
      (should (= replay-count 1)))))

(provide 'emacs-load-artifact-cache-test)

;;; emacs-load-artifact-cache-test.el ends here
