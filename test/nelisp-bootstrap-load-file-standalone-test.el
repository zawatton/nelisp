;;; nelisp-bootstrap-load-file-standalone-test.el --- early loader bootstrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The generated application bootstrap starts with a feature-registry
;; `require' before its later `emacs-fns.el' / `emacs-load.el' members define
;; the full loader.  This test exercises that order on the standalone binary:
;; load the prelude first, install only the registry-shaped `require', and
;; require a temporary feature whose source sets a marker.  A host ERT test
;; cannot catch the bug because host Emacs already supplies `load-file'.

;;; Code:

(require 'ert)

(defconst nelisp-bootstrap-load-file-standalone-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root derived from this test file.")

(defun nelisp-bootstrap-load-file-standalone-test--binary ()
  "Return the built standalone binary, or skip when it is unavailable."
  (let ((binary
         (or (let ((path (expand-file-name
                          "target/nelisp"
                          nelisp-bootstrap-load-file-standalone-test--repo-root)))
               (and (file-executable-p path) path))
             (let ((path (expand-file-name
                          "target/nelisp.exe"
                          nelisp-bootstrap-load-file-standalone-test--repo-root)))
               (and (file-executable-p path) path)))))
    (or binary
        (ert-skip "standalone target is not built; standalone-reader owns it"))))

(ert-deftest nelisp-bootstrap-load-file-standalone/early-require-loads-marker ()
  "Prelude-first standalone `require' can call its early `load-file' bridge."
  (let* ((binary (nelisp-bootstrap-load-file-standalone-test--binary))
         (prelude (expand-file-name
                   "scripts/nelisp-stdlib-prelude.el"
                   nelisp-bootstrap-load-file-standalone-test--repo-root))
         (marker-file (make-temp-file "nelisp-bootstrap-marker-" nil ".el"))
         (driver-file (make-temp-file "nelisp-bootstrap-driver-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file marker-file
            (insert "(setq nelisp-bootstrap-marker 42)\n"
                    "(provide 'nelisp-bootstrap-marker-feature)\n"))
          (with-temp-file driver-file
            ;; `load' is the standalone native primitive available before
            ;; this prelude and before any generated bundle member.
            (insert "(load " (prin1-to-string prelude) " nil nil t t)\n"
                    "(unless (boundp 'features) (defvar features nil))\n"
                    "(defun provide (feature &optional _subfeatures)\n"
                    "  (unless (memq feature features)\n"
                    "    (setq features (cons feature features)))\n"
                    "  feature)\n"
                    "(defun featurep (feature &optional _subfeature)\n"
                    "  (if (memq feature features) t nil))\n"
                    "(defun require (feature &optional filename noerror)\n"
                    "  (if (featurep feature) feature\n"
                    "    (if filename\n"
                    "        (progn\n"
                    "          (load-file filename)\n"
                    "          (if (featurep feature) feature\n"
                    "            (if noerror nil\n"
                    "              (error \"Required feature was not provided: %S\" feature))))\n"
                    "      (if noerror nil\n"
                    "        (error \"Cannot open load file: %S\" feature)))))\n"
                    "(require 'nelisp-bootstrap-marker-feature "
                    (prin1-to-string marker-file) ")\n"
                    "(nelisp--write-stdout-bytes\n"
                    " (format \"BOOTSTRAP-MARKER=%S FEATURE=%S\\n\"\n"
                    "        nelisp-bootstrap-marker\n"
                    "        (featurep 'nelisp-bootstrap-marker-feature)))\n"))
          (with-temp-buffer
            (let ((rc (call-process binary nil t nil "--load" driver-file)))
              (should (= rc 0))
              ;; `--load' also prints the loaded file's final nil value;
              ;; assert the marker line itself and reject any bootstrap
              ;; diagnostic before it.
              (should (equal (car (split-string (buffer-string) "\n" t))
                             "BOOTSTRAP-MARKER=42 FEATURE=t")))))
      (ignore-errors (delete-file marker-file))
      (ignore-errors (delete-file driver-file)))))

(provide 'nelisp-bootstrap-load-file-standalone-test)

;;; nelisp-bootstrap-load-file-standalone-test.el ends here
