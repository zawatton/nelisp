;;; nelisp-sys-extraction-test.el --- Boundary audit: extraction readiness -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 130 Stage 130.7 extraction rehearsal tests.
;;
;; These tests perform pure file scanning — no toolchain is required.
;; They verify two extraction criteria:
;;
;;   Criterion 6: no src/*.el file other than nelisp-sys-adapter-nelisp.el
;;   may reference any private NeLisp backend symbol prefix.
;;
;;   Criterion 5: the adapter exposes fewer than 20 public calls.
;;
;; Run with:
;;   emacs --batch -Q -L src -L packages/nelisp-sys/src \
;;         -L packages/nelisp-sys/test \
;;         --eval '(setq load-prefer-newer t)' \
;;         -l ert -l packages/nelisp-sys/test/nelisp-sys-extraction-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-sys-extraction-test--src
  (expand-file-name "../src/"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Absolute path to the nelisp-sys src/ directory, resolved at load time.")

(defconst nelisp-sys-extraction-test--private-prefixes
  '("nelisp-phase47-"
    "nelisp-asm-"
    "nelisp-elf-"
    "nelisp-mach-o-"
    "nelisp-pe-"
    "nelisp-link-"
    "nelisp-crt0"
    "nelisp-sexp-"
    "nelisp-syscall-")
  "Exact private NeLisp backend symbol prefixes forbidden outside the adapter.
Doc 130 extraction criterion 6.")

(ert-deftest nelisp-sys-extraction-no-private-leaks ()
  "Every src/*.el file except the adapter must contain no private backend prefix.
Doc 130 extraction criterion 6."
  (let ((src-dir nelisp-sys-extraction-test--src)
        (adapter "nelisp-sys-adapter-nelisp.el")
        (leaks nil))
    (dolist (el-file (directory-files src-dir t "\\.el\\'"))
      (let ((basename (file-name-nondirectory el-file)))
        (unless (string= basename adapter)
          (with-temp-buffer
            (insert-file-contents el-file)
            (let ((content (buffer-string)))
              (dolist (prefix nelisp-sys-extraction-test--private-prefixes)
                (when (string-match-p (regexp-quote prefix) content)
                  (push (format "%s contains private prefix %S" basename prefix)
                        leaks))))))))
    (should (null leaks))))

(ert-deftest nelisp-sys-extraction-adapter-call-budget ()
  "The adapter must expose >0 and <20 public calls.
Doc 130 extraction criterion 5."
  (let* ((adapter-file (expand-file-name
                        "nelisp-sys-adapter-nelisp.el"
                        nelisp-sys-extraction-test--src))
         (count 0))
    (with-temp-buffer
      (insert-file-contents adapter-file)
      (goto-char (point-min))
      (while (re-search-forward "^(defun nelisp-sys-adapter-" nil t)
        (cl-incf count)))
    (should (> count 0))
    (should (< count 20))))

(provide 'nelisp-sys-extraction-test)

;;; nelisp-sys-extraction-test.el ends here
