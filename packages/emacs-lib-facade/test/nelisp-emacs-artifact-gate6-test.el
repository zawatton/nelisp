;;; nelisp-emacs-artifact-gate6-test.el --- Doc 142 Gate 6 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nemacs-artifact-gate6)

(ert-deftest nemacs-artifact-gate6-format-spec-source-equals-artifact ()
  "Doc 142 gate 6: the real vendor source replay and cached `.neln'
artifact load must yield the same proof tuple."
  (let* ((source-proof
          (nemacs-artifact-gate6-run-subprocess
           'nemacs-artifact-gate6-batch-source-proof))
         (record (nemacs-artifact-gate6-compile-cache-record))
         (load-pair (nemacs-artifact-gate6-make-load-pair record))
         (artifact-proof nil))
    (unwind-protect
        (progn
          (setq artifact-proof
                (nemacs-artifact-gate6-run-subprocess
                 'nemacs-artifact-gate6-batch-artifact-proof
                 `((nemacs-artifact-gate6-artifact-path
                    . ,(plist-get load-pair :artifact-path)))))
          (princ (format "GATE6 cache-key=%s\n" (plist-get record :key)))
          (princ (format "GATE6 source=%S\n"
                         (nemacs-artifact-gate6-source-path)))
          (princ (format "GATE6 artifact=%S\n"
                         (plist-get record :artifact-path)))
          (princ (format "GATE6 manifest=%S\n"
                         (plist-get record :manifest-path)))
          (princ (format "GATE6 P1=%S\n" (plist-get source-proof :tuple)))
          (princ (format "GATE6 P2=%S\n" (plist-get artifact-proof :tuple)))
          (princ (format "GATE6 source-read-paths=%S\n"
                         (plist-get artifact-proof :source-read-paths)))
          (should-not (plist-get source-proof :before-feature))
          (should-not (plist-get source-proof :before-fboundp))
          (should-not (plist-get artifact-proof :before-feature))
          (should-not (plist-get artifact-proof :before-fboundp))
          (should (equal (plist-get source-proof :tuple)
                         (plist-get artifact-proof :tuple)))
          (should (equal (plist-get source-proof :tuple) '(t t "x")))
          (should-not (plist-get artifact-proof :source-read-paths)))
      (when (and load-pair (file-directory-p (plist-get load-pair :dir)))
        (delete-directory (plist-get load-pair :dir) t)))))

(provide 'nelisp-emacs-artifact-gate6-test)

;;; nelisp-emacs-artifact-gate6-test.el ends here
