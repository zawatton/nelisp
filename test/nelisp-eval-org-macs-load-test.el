;;; nelisp-eval-org-macs-load-test.el --- Artifact-load org-macs in host eval  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'nelisp-artifact)

(defconst nelisp-eval-org-macs-load-test--vendor-root
  "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs/vendor/emacs-lisp")

(defconst nelisp-eval-org-macs-load-test--org-dir
  (expand-file-name "org" nelisp-eval-org-macs-load-test--vendor-root))

(defun nelisp-eval-org-macs-load-test--deps-available-p ()
  "Non-nil when the vendor Org sources needed by this test exist."
  (and (require 'cl-lib nil t)
       (file-readable-p
        (expand-file-name "format-spec.el"
                          nelisp-eval-org-macs-load-test--vendor-root))
       (file-readable-p
        (expand-file-name "org-version.el"
                          nelisp-eval-org-macs-load-test--org-dir))
       (file-readable-p
        (expand-file-name "org-macs.el"
                          nelisp-eval-org-macs-load-test--org-dir))))

(ert-deftest nelisp-eval-org-macs-load-from-artifact ()
  "Compile vendor Org helpers to `.nelc' and replay them in host eval."
  (skip-unless (nelisp-eval-org-macs-load-test--deps-available-p))
  (let* ((tmpdir (make-temp-file "nelisp-org-macs-load-" t))
         (load-paths (list nelisp-eval-org-macs-load-test--vendor-root
                           nelisp-eval-org-macs-load-test--org-dir))
         (format-src
          (expand-file-name "format-spec.el"
                            nelisp-eval-org-macs-load-test--vendor-root))
         (org-version-src
          (expand-file-name "org-version.el"
                            nelisp-eval-org-macs-load-test--org-dir))
         (org-macs-src
          (expand-file-name "org-macs.el"
                            nelisp-eval-org-macs-load-test--org-dir))
         (format-art (expand-file-name "format-spec.el.nelc" tmpdir))
         (org-version-art (expand-file-name "org-version.el.nelc" tmpdir))
         (org-macs-art (expand-file-name "org-macs.el.nelc" tmpdir)))
    (unwind-protect
        (progn
          (require 'cl-lib)
          (nelisp-artifact-compile-file
           format-src format-art nil nil load-paths nil nil 'nelc)
          (nelisp-artifact-compile-file
           org-version-src org-version-art nil nil load-paths nil nil 'nelc)
          (nelisp-artifact-compile-file
           org-macs-src org-macs-art nil nil load-paths nil nil 'nelc)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil
                nelisp-load-path (list tmpdir)
                nelisp-load-path-include-host nil)
          (nelisp-artifact-load-file format-art)
          (nelisp-artifact-load-file org-version-art)
          (nelisp-artifact-load-file org-macs-art)
          (should (nelisp-eval '(featurep 'org-macs)))
          (should (nelisp-eval '(fboundp 'org-string-nw-p)))
          (should (equal (nelisp-eval '(org-string-nw-p " x "))
                         " x ")))
      (nelisp--reset)
      (setq nelisp-artifact--loaded nil)
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

;;; nelisp-eval-org-macs-load-test.el ends here
