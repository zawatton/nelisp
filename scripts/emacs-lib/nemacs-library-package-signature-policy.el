;;; nemacs-library-package-signature-policy.el --- release signature policy gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify the release detached-signature policy for generated package
;; artifacts.  Normal library gates run in non-strict draft mode and record the
;; exact artifacts that must be signed.  Release verification can enable strict
;; mode to require a configured signing key fingerprint, detached signature
;; files, and GnuPG verification for archive-contents and every package tarball.

;;; Code:

(require 'subr-x)

(defvar nemacs-library-package-signature-policy-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-signature-policy-checksum
  (expand-file-name "build/nemacs-library-package-archive-checksum.tsv"
                    nemacs-library-package-signature-policy-repo-root)
  "Package archive checksum TSV.")

(defvar nemacs-library-package-signature-policy-index
  (expand-file-name "build/nemacs-library-package-archive-index.tsv"
                    nemacs-library-package-signature-policy-repo-root)
  "Package archive index TSV.")

(defvar nemacs-library-package-signature-policy-archive-root
  (expand-file-name "build/nemacs-library-package-archives"
                    nemacs-library-package-signature-policy-repo-root)
  "Directory containing package tar archives and archive-contents.")

(defvar nemacs-library-package-signature-policy-output
  (expand-file-name "build/nemacs-library-package-signature-policy.tsv"
                    nemacs-library-package-signature-policy-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-signature-policy-summary-output
  (expand-file-name "build/nemacs-library-package-signature-policy.org"
                    nemacs-library-package-signature-policy-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-signature-policy-release-output
  (expand-file-name "build/nemacs-library-package-signature-release.tsv"
                    nemacs-library-package-signature-policy-repo-root)
  "Strict release TSV output path.")

(defvar nemacs-library-package-signature-policy-release-summary-output
  (expand-file-name "build/nemacs-library-package-signature-release.org"
                    nemacs-library-package-signature-policy-repo-root)
  "Strict release Org summary output path.")

(defvar nemacs-library-package-signature-policy-release-strict nil
  "When non-nil, require concrete release signatures and GPG verification.")

(defvar nemacs-library-package-signature-policy-key-fingerprint
  (or (getenv "NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT") "")
  "Expected release signing key fingerprint.")

(defvar nemacs-library-package-signature-policy-signature-suffix ".sig"
  "Detached signature file suffix.")

(defvar nemacs-library-package-signature-policy-gpg-program "gpg"
  "GnuPG executable used for strict release signature verification.")

(defconst nemacs-library-package-signature-policy--policy
  "release-detached-signature-verified-by-configured-key"
  "Policy name for strict release detached signatures.")

(defun nemacs-library-package-signature-policy--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-signature-policy--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-signature-policy--tsv-cell
             cells "\t"))

(defun nemacs-library-package-signature-policy--read-tsv (file)
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

(defun nemacs-library-package-signature-policy--relative-file (file)
  "Return FILE relative to the repository root."
  (file-relative-name
   (expand-file-name file nemacs-library-package-signature-policy-repo-root)
   nemacs-library-package-signature-policy-repo-root))

(defun nemacs-library-package-signature-policy--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file nemacs-library-package-signature-policy-repo-root))

(defun nemacs-library-package-signature-policy--archive-contents ()
  "Return generated archive-contents path relative to the repository."
  (nemacs-library-package-signature-policy--relative-file
   (expand-file-name "archive-contents"
                     nemacs-library-package-signature-policy-archive-root)))

(defun nemacs-library-package-signature-policy--normalize-fingerprint
    (fingerprint)
  "Return normalized uppercase FINGERPRINT hex."
  (upcase
   (replace-regexp-in-string "[^[:xdigit:]]" "" (or fingerprint ""))))

(defun nemacs-library-package-signature-policy--fingerprint-valid-p
    (fingerprint)
  "Return non-nil when FINGERPRINT is a full 40-hex OpenPGP fingerprint."
  (string-match-p
   "\\`[[:xdigit:]]\\{40\\}\\'"
   (nemacs-library-package-signature-policy--normalize-fingerprint
    fingerprint)))

(defun nemacs-library-package-signature-policy--signature-file (artifact)
  "Return expected detached signature path for ARTIFACT."
  (concat artifact
          nemacs-library-package-signature-policy-signature-suffix))

(defun nemacs-library-package-signature-policy--find-row
    (package rows column)
  "Return row for PACKAGE from ROWS comparing COLUMN."
  (let (found)
    (dolist (row rows found)
      (when (string= package (nth column row))
        (setq found row)))))

(defun nemacs-library-package-signature-policy--strict-verify
    (artifact signature fingerprint)
  "Verify SIGNATURE for ARTIFACT against FINGERPRINT.
Return a cons cell (STATUS . DETAILS)."
  (let ((artifact-path
         (nemacs-library-package-signature-policy--absolute artifact))
        (signature-path
         (nemacs-library-package-signature-policy--absolute signature))
        (expected
         (nemacs-library-package-signature-policy--normalize-fingerprint
          fingerprint)))
    (cond
     ((not (nemacs-library-package-signature-policy--fingerprint-valid-p
            fingerprint))
      (cons "fail" "release signing key fingerprint must be a 40-hex OpenPGP fingerprint"))
     ((not (file-readable-p artifact-path))
      (cons "fail" "artifact file is not readable"))
     ((not (file-readable-p signature-path))
      (cons "fail" "detached signature file is not readable"))
     ((not (executable-find
            nemacs-library-package-signature-policy-gpg-program))
      (cons "fail" "gpg executable is not available"))
     (t
      (with-temp-buffer
        (let ((exit-code
               (call-process
                nemacs-library-package-signature-policy-gpg-program
                nil t nil
                "--batch" "--status-fd" "1"
                "--verify" signature-path artifact-path))
              (validsig nil))
          (goto-char (point-min))
          (while (re-search-forward
                  "^\\[GNUPG:\\] VALIDSIG \\([[:xdigit:]]+\\)" nil t)
            (setq validsig (upcase (match-string 1))))
          (if (and (eq exit-code 0)
                   validsig
                   (string= validsig expected))
              (cons "ok" "detached signature verified")
            (cons
             "fail"
             (format "gpg verification failed or signer mismatch: exit=%s signer=%s"
                     exit-code
                     (or validsig ""))))))))))

(defun nemacs-library-package-signature-policy--draft-status
    (artifact signature)
  "Return draft status for ARTIFACT and expected SIGNATURE."
  (if (file-readable-p
       (nemacs-library-package-signature-policy--absolute artifact))
      (cons "ok"
            (format "draft mode records expected release signature %s"
                    signature))
    (cons "fail" "artifact file is not readable")))

(defun nemacs-library-package-signature-policy--artifact-row
    (artifact-type subject artifact sha256)
  "Return signature policy row for ARTIFACT-TYPE SUBJECT ARTIFACT SHA256."
  (let* ((signature
          (nemacs-library-package-signature-policy--signature-file artifact))
         (fingerprint
          (nemacs-library-package-signature-policy--normalize-fingerprint
           nemacs-library-package-signature-policy-key-fingerprint))
         (result
          (if nemacs-library-package-signature-policy-release-strict
              (nemacs-library-package-signature-policy--strict-verify
               artifact signature fingerprint)
            (nemacs-library-package-signature-policy--draft-status
             artifact signature))))
    (list artifact-type
          subject
          artifact
          signature
          sha256
          fingerprint
          (if nemacs-library-package-signature-policy-release-strict
              "strict-release"
            "draft-policy")
          (car result)
          (cdr result))))

(defun nemacs-library-package-signature-policy--key-row ()
  "Return release signing key policy row."
  (let* ((fingerprint
          (nemacs-library-package-signature-policy--normalize-fingerprint
           nemacs-library-package-signature-policy-key-fingerprint))
         (valid
          (nemacs-library-package-signature-policy--fingerprint-valid-p
           fingerprint)))
    (list
     "signing-key"
     "public-release"
     ""
     ""
     ""
     fingerprint
     (if nemacs-library-package-signature-policy-release-strict
         "strict-release"
       "draft-policy")
     (if (or valid
             (not nemacs-library-package-signature-policy-release-strict))
         "ok"
       "fail")
     (cond
      (valid "release signing key fingerprint configured")
      (nemacs-library-package-signature-policy-release-strict
       "strict release verification requires NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT")
      (t "draft mode records that release verification requires a signing key fingerprint")))))

(defun nemacs-library-package-signature-policy--build-rows ()
  "Return release signature policy rows."
  (let* ((checksum-rows
          (nemacs-library-package-signature-policy--read-tsv
           nemacs-library-package-signature-policy-checksum))
         (index-rows
          (nemacs-library-package-signature-policy--read-tsv
           nemacs-library-package-signature-policy-index))
         (archive-contents
          (nemacs-library-package-signature-policy--archive-contents))
         rows)
    (push (nemacs-library-package-signature-policy--key-row) rows)
    (push
     (nemacs-library-package-signature-policy--artifact-row
      "archive-index"
      "archive-contents"
      archive-contents
      "")
     rows)
    (dolist (checksum checksum-rows)
      (let* ((package (nth 0 checksum))
             (archive (nth 2 checksum))
             (sha256 (nth 3 checksum))
             (index
              (nemacs-library-package-signature-policy--find-row
               package index-rows 0)))
        (unless index
          (error "missing archive index row for %s" package))
        (unless (and (string= "ok" (nth 7 checksum))
                     (string= "yes" (nth 6 checksum))
                     (string= "ok" (nth 7 index))
                     (string= archive (nth 4 index)))
          (error "archive checksum/index rows are not publishable for %s"
                 package))
        (push
         (nemacs-library-package-signature-policy--artifact-row
          "package-tarball"
          package
          archive
          sha256)
         rows)))
    (sort
     (nreverse rows)
     (lambda (a b)
       (string< (mapconcat #'identity (list (nth 0 a) (nth 1 a)) "\t")
                (mapconcat #'identity (list (nth 0 b) (nth 1 b)) "\t"))))))

(defun nemacs-library-package-signature-policy--write-tsv (rows)
  "Write signature policy ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-signature-policy-output) t)
  (with-temp-file nemacs-library-package-signature-policy-output
    (insert
     (nemacs-library-package-signature-policy--row
      "artifact_type" "subject" "artifact_file" "signature_file"
      "artifact_sha256" "signing_key_fingerprint" "verification_mode"
      "status" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-signature-policy--row row)
       "\n"))))

(defun nemacs-library-package-signature-policy--count-if
    (predicate rows)
  "Return count of ROWS matching PREDICATE."
  (let ((count 0))
    (dolist (row rows count)
      (when (funcall predicate row)
        (setq count (1+ count))))))

(defun nemacs-library-package-signature-policy--write-summary (rows)
  "Write signature policy ROWS to Org summary output."
  (let* ((artifacts
          (nemacs-library-package-signature-policy--count-if
           (lambda (row) (not (string= (nth 0 row) "signing-key")))
           rows))
         (signature-files
          (nemacs-library-package-signature-policy--count-if
           (lambda (row)
             (and (not (string= (nth 0 row) "signing-key"))
                  (file-readable-p
                   (nemacs-library-package-signature-policy--absolute
                    (nth 3 row)))))
           rows))
         (verified
          (nemacs-library-package-signature-policy--count-if
           (lambda (row)
             (and (string= (nth 6 row) "strict-release")
                  (string= (nth 7 row) "ok")
                  (not (string= (nth 0 row) "signing-key"))))
           rows))
         (failures
          (nemacs-library-package-signature-policy--count-if
           (lambda (row) (string= (nth 7 row) "fail"))
           rows)))
    (make-directory
     (file-name-directory
      nemacs-library-package-signature-policy-summary-output)
     t)
    (with-temp-file nemacs-library-package-signature-policy-summary-output
      (insert "#+TITLE: nemacs library package signature policy\n\n")
      (insert "* Summary\n\n")
      (insert (format "- artifacts requiring signatures: %d\n" artifacts))
      (insert (format "- signature files present: %d\n" signature-files))
      (insert (format "- verified signatures: %d\n" verified))
      (insert (format "- failures: %d\n" failures))
      (insert
       (format "- strict release: %s\n"
               (if nemacs-library-package-signature-policy-release-strict
                   "yes"
                 "no")))
      (insert
       (format "- signing key configured: %s\n\n"
               (if (nemacs-library-package-signature-policy--fingerprint-valid-p
                    nemacs-library-package-signature-policy-key-fingerprint)
                   "yes"
                 "no")))
      (insert "* Policy\n\n")
      (insert "- Public releases require a 40-hex OpenPGP signing key fingerprint.\n")
      (insert "- Public releases require detached signatures for =archive-contents= and every package tarball.\n")
      (insert "- Strict release verification uses =gpg --verify= and requires the signer fingerprint to match the configured key.\n")
      (insert "- Draft mode records expected signature paths without requiring local signing keys or signature artifacts.\n\n")
      (insert "* Artifacts\n\n")
      (insert "| Type | Subject | Status | Signature |\n")
      (insert "|------+---------+--------+-----------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 7 row)
                 (nth 3 row))))
      (insert "\n* Notes\n\n")
      (insert
       (format "- Policy: =%s=.\n"
               nemacs-library-package-signature-policy--policy))
      (insert
       "- Run =make nemacs-library-package-signature-release-sign= to create detached signatures with the release key.\n")
      (insert
       "- Run =make nemacs-library-package-signature-release-verify= for strict release verification after signing.\n"))))

;;;###autoload
(defun nemacs-library-package-signature-policy-batch ()
  "Generate and verify release signature policy artifacts."
  (let* ((rows (nemacs-library-package-signature-policy--build-rows))
         (failures 0))
    (unless rows
      (error "empty package signature policy rows"))
    (nemacs-library-package-signature-policy--write-tsv rows)
    (nemacs-library-package-signature-policy--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 7 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-signature-policy: artifacts=%d failures=%d strict=%s output=%s summary=%s\n"
      (nemacs-library-package-signature-policy--count-if
       (lambda (row) (not (string= (nth 0 row) "signing-key")))
       rows)
      failures
      (if nemacs-library-package-signature-policy-release-strict "yes" "no")
      nemacs-library-package-signature-policy-output
      nemacs-library-package-signature-policy-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-signature-policy)

;;; nemacs-library-package-signature-policy.el ends here
