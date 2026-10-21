;;; nelisp-version.el --- Build-time runtime identity -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Resolve identity from the source being built, never from the build host's
;; current directory or a previously installed runtime.  Exported sources can
;; supply NELISP_SOURCE_REVISION when their Git metadata is unavailable.

;;; Code:

(require 'subr-x)

(defun nelisp-build--git (root &rest arguments)
  "Return Git output for ROOT and ARGUMENTS, or nil on failure."
  (when (executable-find "git")
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory (expand-file-name root))))
        (condition-case nil
            (when (eq 0 (apply #'process-file "git" nil '(t nil) nil arguments))
              (string-trim (buffer-string)))
          (file-error nil))))))

(defun nelisp-build--git-root-p (root)
  "Whether ROOT itself is a Git worktree, rather than an exported subtree."
  (let ((top (nelisp-build--git root "rev-parse" "--show-toplevel")))
    (and top (file-equal-p root top))))

(defun nelisp-build-source-revision (root)
  "Return ROOT's source revision, if known."
  (or (and (nelisp-build--git-root-p root)
           (nelisp-build--git root "rev-parse" "HEAD"))
      (let ((revision (getenv "NELISP_SOURCE_REVISION")))
        (and revision (string-match-p "\\`[[:xdigit:]]\\{7,64\\}\\'" revision)
             revision))))

(defun nelisp-build-version (root)
  "Resolve ROOT's runtime version once on the build host.
NELISP_VERSION overrides Git describe.  Outside Git, use unknown, optionally
qualified by NELISP_SOURCE_REVISION.  Reject multiline metadata because the
CLI version contract is exactly one line."
  (let ((version
         (or (let ((override (getenv "NELISP_VERSION")))
               (and override (not (string-empty-p override)) override))
             (and (nelisp-build--git-root-p root)
                  (nelisp-build--git root "describe" "--tags" "--long"
                                     "--always" "--dirty"))
             (let ((revision (nelisp-build-source-revision root)))
               (if revision (concat "unknown-" revision) "unknown")))))
    (when (string-match-p "[\n\r\0]" version)
      (error "Runtime version must be a single line"))
    version))

(provide 'nelisp-version)
;;; nelisp-version.el ends here
