;;; nemacs-library-package-release-bundle-manifest.el --- package release bundle manifest -*- lexical-binding: t; -*-

;;; Commentary:

;; Retain generated package publication artifacts in one release bundle
;; directory and write a manifest for audit/release review.  Normal library
;; gates run in draft mode: package archives and evidence are copied, while
;; missing detached signatures are recorded as pending.  Release mode is
;; strict and fails when required signatures or release evidence are absent.

;;; Code:

(require 'subr-x)

(defvar nemacs-library-package-release-bundle-manifest-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-release-bundle-manifest-signature-policy
  (expand-file-name "build/nemacs-library-package-signature-policy.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Signature policy TSV.")

(defvar nemacs-library-package-release-bundle-manifest-signature-policy-summary
  (expand-file-name "build/nemacs-library-package-signature-policy.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Signature policy Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-release-key-policy
  (expand-file-name "build/nemacs-library-package-release-key-policy.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Release public key policy TSV.")

(defvar nemacs-library-package-release-bundle-manifest-release-key-policy-summary
  (expand-file-name "build/nemacs-library-package-release-key-policy.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Release public key policy Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-release-public-key-file
  (expand-file-name "docs/release/nemacs-library-release-public-key.asc"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Release public key file.")

(defvar nemacs-library-package-release-bundle-manifest-archive-checksum
  (expand-file-name "build/nemacs-library-package-archive-checksum.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Archive checksum TSV.")

(defvar nemacs-library-package-release-bundle-manifest-archive-checksum-summary
  (expand-file-name "build/nemacs-library-package-archive-checksum.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Archive checksum Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-archive-index
  (expand-file-name "build/nemacs-library-package-archive-index.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Archive index TSV.")

(defvar nemacs-library-package-release-bundle-manifest-archive-index-summary
  (expand-file-name "build/nemacs-library-package-archive-index.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Archive index Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-publication-policy
  (expand-file-name "build/nemacs-library-package-publication-policy.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Publication policy TSV.")

(defvar nemacs-library-package-release-bundle-manifest-publication-policy-summary
  (expand-file-name "build/nemacs-library-package-publication-policy.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Publication policy Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-dependency-policy
  (expand-file-name "build/nemacs-library-package-dependency-publication-policy.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Dependency publication policy TSV.")

(defvar nemacs-library-package-release-bundle-manifest-dependency-policy-summary
  (expand-file-name "build/nemacs-library-package-dependency-publication-policy.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Dependency publication policy Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-release-sign
  (expand-file-name "build/nemacs-library-package-signature-release-sign.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Release signing TSV.")

(defvar nemacs-library-package-release-bundle-manifest-release-sign-summary
  (expand-file-name "build/nemacs-library-package-signature-release-sign.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Release signing Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-release-verify
  (expand-file-name "build/nemacs-library-package-signature-release.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Strict release signature verification TSV.")

(defvar nemacs-library-package-release-bundle-manifest-release-verify-summary
  (expand-file-name "build/nemacs-library-package-signature-release.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Strict release signature verification Org summary.")

(defvar nemacs-library-package-release-bundle-manifest-bundle-root
  (expand-file-name "build/nemacs-library-package-release-bundle"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Release bundle retention root.")

(defvar nemacs-library-package-release-bundle-manifest-output
  (expand-file-name "build/nemacs-library-package-release-bundle-manifest.tsv"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-release-bundle-manifest-summary-output
  (expand-file-name "build/nemacs-library-package-release-bundle-manifest.org"
                    nemacs-library-package-release-bundle-manifest-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-release-bundle-manifest-strict-release nil
  "When non-nil, missing release signatures or release evidence fail.")

(defun nemacs-library-package-release-bundle-manifest--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-release-bundle-manifest--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-release-bundle-manifest--tsv-cell
             cells "\t"))

(defun nemacs-library-package-release-bundle-manifest--read-tsv (file)
  "Return rows from TSV FILE without the header."
  (let (rows)
    (unless (file-readable-p file)
      (error "missing readable TSV: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string= line "")
            (push (split-string line "\t") rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun nemacs-library-package-release-bundle-manifest--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file
                    nemacs-library-package-release-bundle-manifest-repo-root))

(defun nemacs-library-package-release-bundle-manifest--relative (file)
  "Return FILE relative to the repository root."
  (file-relative-name
   (expand-file-name file
                     nemacs-library-package-release-bundle-manifest-repo-root)
   nemacs-library-package-release-bundle-manifest-repo-root))

(defun nemacs-library-package-release-bundle-manifest--hash-file (file)
  "Return SHA-256 digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-release-bundle-manifest--file-bytes (file)
  "Return byte size for FILE."
  (number-to-string (nth 7 (file-attributes file))))

(defun nemacs-library-package-release-bundle-manifest--artifact-rows ()
  "Return signature policy artifact rows."
  (let (rows)
    (dolist (row
             (nemacs-library-package-release-bundle-manifest--read-tsv
              nemacs-library-package-release-bundle-manifest-signature-policy))
      (unless (string= (nth 0 row) "signing-key")
        (push row rows)))
    (nreverse rows)))

(defun nemacs-library-package-release-bundle-manifest--bundle-path
    (subdir source)
  "Return bundle path under SUBDIR for SOURCE."
  (expand-file-name
   (file-name-nondirectory source)
   (expand-file-name subdir
                     nemacs-library-package-release-bundle-manifest-bundle-root)))

(defun nemacs-library-package-release-bundle-manifest--retain
    (row-type subject source bundle required-for-release missing-ok)
  "Retain SOURCE at BUNDLE and return one manifest row.
ROW-TYPE and SUBJECT identify the row.  REQUIRED-FOR-RELEASE records release
policy.  MISSING-OK allows draft-mode pending rows instead of failures."
  (let* ((source-path
          (nemacs-library-package-release-bundle-manifest--absolute source))
         (bundle-path
          (nemacs-library-package-release-bundle-manifest--absolute bundle))
         (strict
          nemacs-library-package-release-bundle-manifest-strict-release)
         (status "ok")
         (details "retained")
         (retained "yes")
         (bytes "")
         (sha256 ""))
    (if (file-readable-p source-path)
        (progn
          (make-directory (file-name-directory bundle-path) t)
          (copy-file source-path bundle-path t t nil t)
          (setq bytes
                (nemacs-library-package-release-bundle-manifest--file-bytes
                 bundle-path))
          (setq sha256
                (nemacs-library-package-release-bundle-manifest--hash-file
                 bundle-path)))
      (setq retained "no")
      (cond
       ((and required-for-release (or strict (not missing-ok)))
        (setq status "fail")
        (setq details "required release bundle file is missing"))
       (t
        (setq status "pending")
        (setq details "not retained in draft bundle"))))
    (list row-type
          subject
          (nemacs-library-package-release-bundle-manifest--relative source)
          (nemacs-library-package-release-bundle-manifest--relative bundle)
          bytes
          sha256
          (if required-for-release "yes" "no")
          retained
          (if strict "yes" "no")
          status
          details)))

(defun nemacs-library-package-release-bundle-manifest--baseline-evidence ()
  "Return baseline evidence file descriptors."
  (list
   (list "archive-checksum"
         nemacs-library-package-release-bundle-manifest-archive-checksum)
   (list "archive-checksum-summary"
         nemacs-library-package-release-bundle-manifest-archive-checksum-summary)
   (list "archive-index"
         nemacs-library-package-release-bundle-manifest-archive-index)
   (list "archive-index-summary"
         nemacs-library-package-release-bundle-manifest-archive-index-summary)
   (list "publication-policy"
         nemacs-library-package-release-bundle-manifest-publication-policy)
   (list "publication-policy-summary"
         nemacs-library-package-release-bundle-manifest-publication-policy-summary)
   (list "release-key-policy"
         nemacs-library-package-release-bundle-manifest-release-key-policy)
   (list "release-key-policy-summary"
         nemacs-library-package-release-bundle-manifest-release-key-policy-summary)
   (list "signature-policy"
         nemacs-library-package-release-bundle-manifest-signature-policy)
   (list "signature-policy-summary"
         nemacs-library-package-release-bundle-manifest-signature-policy-summary)
   (list "dependency-publication-policy"
         nemacs-library-package-release-bundle-manifest-dependency-policy)
   (list "dependency-publication-policy-summary"
         nemacs-library-package-release-bundle-manifest-dependency-policy-summary)))

(defun nemacs-library-package-release-bundle-manifest--release-evidence ()
  "Return strict-release evidence file descriptors."
  (list
   (list "release-public-key"
         nemacs-library-package-release-bundle-manifest-release-public-key-file)
   (list "signature-release-sign"
         nemacs-library-package-release-bundle-manifest-release-sign)
   (list "signature-release-sign-summary"
         nemacs-library-package-release-bundle-manifest-release-sign-summary)
   (list "signature-release-verify"
         nemacs-library-package-release-bundle-manifest-release-verify)
   (list "signature-release-verify-summary"
         nemacs-library-package-release-bundle-manifest-release-verify-summary)))

(defun nemacs-library-package-release-bundle-manifest--clean-bundle-root ()
  "Remove and recreate the bundle root."
  (let ((repo
         (file-truename
          nemacs-library-package-release-bundle-manifest-repo-root))
        (root
         (file-truename
          nemacs-library-package-release-bundle-manifest-bundle-root)))
    (unless (and (file-in-directory-p root repo)
                 (not (string= root repo)))
      (error "refusing unsafe release bundle root: %s" root))
    (when (file-directory-p root)
      (dolist (entry (directory-files root t directory-files-no-dot-files-regexp))
        (cond
         ((file-directory-p entry)
          (delete-directory entry t))
         ((file-exists-p entry)
          (delete-file entry))))
      (ignore-errors (delete-directory root)))
    (make-directory root t)))

(defun nemacs-library-package-release-bundle-manifest--build-rows ()
  "Return release bundle manifest rows."
  (nemacs-library-package-release-bundle-manifest--clean-bundle-root)
  (let (rows)
    (dolist (policy-row
             (nemacs-library-package-release-bundle-manifest--artifact-rows))
      (let ((artifact-type (nth 0 policy-row))
            (subject (nth 1 policy-row))
            (artifact (nth 2 policy-row))
            (signature (nth 3 policy-row)))
        (push
         (nemacs-library-package-release-bundle-manifest--retain
          artifact-type
          subject
          artifact
          (nemacs-library-package-release-bundle-manifest--bundle-path
           "archives" artifact)
          t
          nil)
         rows)
        (push
         (nemacs-library-package-release-bundle-manifest--retain
          "detached-signature"
          subject
          signature
          (nemacs-library-package-release-bundle-manifest--bundle-path
           "archives" signature)
          t
          t)
         rows)))
    (dolist (evidence
             (nemacs-library-package-release-bundle-manifest--baseline-evidence))
      (push
       (nemacs-library-package-release-bundle-manifest--retain
        "evidence"
        (nth 0 evidence)
        (nth 1 evidence)
        (nemacs-library-package-release-bundle-manifest--bundle-path
         "evidence" (nth 1 evidence))
        t
        nil)
       rows))
    (dolist (evidence
             (nemacs-library-package-release-bundle-manifest--release-evidence))
      (let ((bundle
             (nemacs-library-package-release-bundle-manifest--bundle-path
              "evidence" (nth 1 evidence))))
        (push
         (if nemacs-library-package-release-bundle-manifest-strict-release
             (nemacs-library-package-release-bundle-manifest--retain
              "release-evidence"
              (nth 0 evidence)
              (nth 1 evidence)
              bundle
              t
              t)
           (list
            "release-evidence"
            (nth 0 evidence)
            (nemacs-library-package-release-bundle-manifest--relative
             (nth 1 evidence))
            (nemacs-library-package-release-bundle-manifest--relative bundle)
            ""
            ""
            "yes"
            "no"
            "no"
            "pending"
            "release evidence is retained only in strict release bundle"))
         rows)))
    (nreverse rows)))

(defun nemacs-library-package-release-bundle-manifest--write-tsv (rows)
  "Write manifest ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-release-bundle-manifest-output)
   t)
  (with-temp-file nemacs-library-package-release-bundle-manifest-output
    (insert
     (nemacs-library-package-release-bundle-manifest--row
      "artifact_type" "subject" "source_file" "bundle_file" "bytes"
      "sha256" "required_for_release" "retained" "strict_release"
      "status" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-release-bundle-manifest--row row)
       "\n"))))

(defun nemacs-library-package-release-bundle-manifest--count-if (predicate rows)
  "Return number of ROWS where PREDICATE returns non-nil."
  (let ((count 0))
    (dolist (row rows count)
      (when (funcall predicate row)
        (setq count (1+ count))))))

(defun nemacs-library-package-release-bundle-manifest--write-summary (rows)
  "Write manifest ROWS to Org summary output."
  (let ((retained
         (nemacs-library-package-release-bundle-manifest--count-if
          (lambda (row) (string= (nth 7 row) "yes"))
          rows))
        (pending
         (nemacs-library-package-release-bundle-manifest--count-if
          (lambda (row) (string= (nth 9 row) "pending"))
          rows))
        (failures
         (nemacs-library-package-release-bundle-manifest--count-if
          (lambda (row) (string= (nth 9 row) "fail"))
          rows))
        (signatures
         (nemacs-library-package-release-bundle-manifest--count-if
          (lambda (row) (string= (nth 0 row) "detached-signature"))
          rows))
        (retained-signatures
         (nemacs-library-package-release-bundle-manifest--count-if
          (lambda (row)
            (and (string= (nth 0 row) "detached-signature")
                 (string= (nth 7 row) "yes")))
          rows)))
    (make-directory
     (file-name-directory
      nemacs-library-package-release-bundle-manifest-summary-output)
     t)
    (with-temp-file
        nemacs-library-package-release-bundle-manifest-summary-output
      (insert "#+TITLE: nemacs library package release bundle manifest\n\n")
      (insert "* Summary\n\n")
      (insert (format "- rows: %d\n" (length rows)))
      (insert (format "- retained files: %d\n" retained))
      (insert (format "- pending files: %d\n" pending))
      (insert (format "- failures: %d\n" failures))
      (insert (format "- detached signatures retained: %d/%d\n"
                      retained-signatures signatures))
      (insert
       (format "- strict release: %s\n"
               (if nemacs-library-package-release-bundle-manifest-strict-release
                   "yes"
                 "no")))
      (insert
       (format "- bundle root: =%s=\n\n"
               (nemacs-library-package-release-bundle-manifest--relative
                nemacs-library-package-release-bundle-manifest-bundle-root)))
      (insert "* Files\n\n")
      (insert "| Type | Subject | Retained | Status | Bundle File |\n")
      (insert "|------+---------+----------+--------+-------------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | %s | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 7 row)
                 (nth 9 row)
                 (nth 3 row))))
      (insert "\n* Notes\n\n")
      (insert "- Draft mode records missing release signatures as pending.\n")
      (insert "- Strict release mode fails unless signatures and release evidence are retained.\n"))))

;;;###autoload
(defun nemacs-library-package-release-bundle-manifest-batch ()
  "Write the release bundle manifest."
  (let* ((rows
          (nemacs-library-package-release-bundle-manifest--build-rows))
         (failures
          (nemacs-library-package-release-bundle-manifest--count-if
           (lambda (row) (string= (nth 9 row) "fail"))
           rows)))
    (unless rows
      (error "empty release bundle manifest rows"))
    (nemacs-library-package-release-bundle-manifest--write-tsv rows)
    (nemacs-library-package-release-bundle-manifest--write-summary rows)
    (princ
     (format
      "nemacs-library-package-release-bundle-manifest: rows=%d failures=%d strict=%s output=%s summary=%s\n"
      (length rows)
      failures
      (if nemacs-library-package-release-bundle-manifest-strict-release
          "yes"
        "no")
      nemacs-library-package-release-bundle-manifest-output
      nemacs-library-package-release-bundle-manifest-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-release-bundle-manifest)

;;; nemacs-library-package-release-bundle-manifest.el ends here
