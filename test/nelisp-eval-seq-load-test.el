;;; nelisp-eval-seq-load-test.el --- Artifact-load seq and Org chain in host eval  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'nelisp-artifact)

(defconst nelisp-eval-seq-load-test--vendor-root
  "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs/vendor/emacs-lisp")

(defconst nelisp-eval-seq-load-test--org-dir
  (expand-file-name "org" nelisp-eval-seq-load-test--vendor-root))

(defun nelisp-eval-seq-load-test--deps-available-p ()
  "Non-nil when the vendor seq/Org sources used by this test exist."
  (and (require 'cl-lib nil t)
       (file-readable-p
        (expand-file-name "emacs-lisp/seq.el"
                          nelisp-eval-seq-load-test--vendor-root))
       (file-readable-p
        (expand-file-name "format-spec.el"
                          nelisp-eval-seq-load-test--vendor-root))
       (file-readable-p
        (expand-file-name "org-version.el"
                          nelisp-eval-seq-load-test--org-dir))
       (file-readable-p
        (expand-file-name "org-macs.el"
                          nelisp-eval-seq-load-test--org-dir))
       (file-readable-p
        (expand-file-name "org-compat.el"
                          nelisp-eval-seq-load-test--org-dir))
       (file-readable-p
        (expand-file-name "org-fold-core.el"
                          nelisp-eval-seq-load-test--org-dir))
       (file-readable-p
        (expand-file-name "org-fold.el"
                          nelisp-eval-seq-load-test--org-dir))))

(defun nelisp-eval-seq-load-test--compile-artifact (src tmpdir load-paths)
  "Compile SRC into TMPDIR using LOAD-PATHS, returning the artifact path."
  (let ((art (expand-file-name
              (concat (file-name-nondirectory src) ".nelc")
              tmpdir)))
    (nelisp-artifact-compile-file src art nil nil load-paths nil nil 'nelc)
    art))

(ert-deftest nelisp-eval-seq-load-from-artifact ()
  "Compile vendored seq.el to `.nelc' and load only the artifact."
  (skip-unless (nelisp-eval-seq-load-test--deps-available-p))
  (let* ((tmpdir (make-temp-file "nelisp-seq-load-" t))
         (seq-src
          (expand-file-name "emacs-lisp/seq.el"
                            nelisp-eval-seq-load-test--vendor-root))
         (load-paths (list nelisp-eval-seq-load-test--vendor-root))
         (seq-art nil))
    (unwind-protect
        (progn
          (require 'cl-lib)
          (setq seq-art
                (nelisp-eval-seq-load-test--compile-artifact
                 seq-src tmpdir load-paths))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil
                nelisp-load-path (list tmpdir)
                nelisp-load-path-include-host nil)
          (nelisp-artifact-load-file seq-art)
          (should (nelisp-eval '(featurep 'seq)))
          (should (= (nelisp-eval '(seq-length '(a b c))) 3))
          (should (= (nelisp-eval '(seq-elt '(10 20 30) 1)) 20)))
      (nelisp--reset)
      (setq nelisp-artifact--loaded nil)
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest nelisp-eval-org-fold-chain-load-from-artifact ()
  "Compile the seq/Org compatibility chain to `.nelc' and load it in order."
  (skip-unless (nelisp-eval-seq-load-test--deps-available-p))
  (let* ((tmpdir (make-temp-file "nelisp-org-fold-chain-" t))
         (load-paths (list nelisp-eval-seq-load-test--vendor-root
                           nelisp-eval-seq-load-test--org-dir))
         (arts nil))
    (unwind-protect
        (progn
          (require 'cl-lib)
          (setq arts
                (list
                 (cons 'seq
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "emacs-lisp/seq.el"
                                          nelisp-eval-seq-load-test--vendor-root)
                        tmpdir load-paths))
                 (cons 'format-spec
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "format-spec.el"
                                          nelisp-eval-seq-load-test--vendor-root)
                        tmpdir load-paths))
                 (cons 'org-version
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "org-version.el"
                                          nelisp-eval-seq-load-test--org-dir)
                        tmpdir load-paths))
                 (cons 'org-macs
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "org-macs.el"
                                          nelisp-eval-seq-load-test--org-dir)
                        tmpdir load-paths))
                 (cons 'org-compat
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "org-compat.el"
                                          nelisp-eval-seq-load-test--org-dir)
                        tmpdir load-paths))
                 (cons 'org-fold-core
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "org-fold-core.el"
                                          nelisp-eval-seq-load-test--org-dir)
                        tmpdir load-paths))
                 (cons 'org-fold
                       (nelisp-eval-seq-load-test--compile-artifact
                        (expand-file-name "org-fold.el"
                                          nelisp-eval-seq-load-test--org-dir)
                        tmpdir load-paths))))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil
                nelisp-load-path (list tmpdir)
                nelisp-load-path-include-host nil)
          (dolist (feature '(seq format-spec org-version org-macs
                                 org-compat org-fold-core org-fold))
            (nelisp-artifact-load-file (alist-get feature arts)))
          (should (nelisp-eval '(featurep 'org-compat)))
          (should (nelisp-eval '(featurep 'org-fold-core)))
          (should (nelisp-eval '(featurep 'org-fold)))
          (should (eq (nelisp-eval '(org-xor t nil)) t))
          (should (nelisp-eval '(fboundp 'org-fold-core-initialize)))
          (should (nelisp-eval '(fboundp 'org-fold-initialize))))
      (nelisp--reset)
      (setq nelisp-artifact--loaded nil)
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

;;; nelisp-eval-seq-load-test.el ends here
