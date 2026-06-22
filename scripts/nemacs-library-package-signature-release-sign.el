;;; nemacs-library-package-signature-release-sign.el --- sign release package artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Create detached signatures for the release artifacts recorded by
;; `nemacs-library-package-signature-policy'.  This target is intentionally
;; release-only: it requires a configured OpenPGP fingerprint and a local
;; private key available to GnuPG.  Normal library gates verify the signature
;; policy in draft mode but do not create signatures.

;;; Code:

(require 'subr-x)
(require 'seq)

(defvar nemacs-library-package-signature-release-sign-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-signature-release-sign-policy
  (expand-file-name "build/nemacs-library-package-signature-policy.tsv"
                    nemacs-library-package-signature-release-sign-repo-root)
  "Draft signature policy TSV used as signing input.")

(defvar nemacs-library-package-signature-release-sign-output
  (expand-file-name "build/nemacs-library-package-signature-release-sign.tsv"
                    nemacs-library-package-signature-release-sign-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-signature-release-sign-summary-output
  (expand-file-name "build/nemacs-library-package-signature-release-sign.org"
                    nemacs-library-package-signature-release-sign-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-signature-release-sign-key-fingerprint
  (or (getenv "NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT") "")
  "OpenPGP fingerprint used to sign release artifacts.")

(defvar nemacs-library-package-signature-release-sign-gpg-program "gpg"
  "GnuPG executable used for release signing.")

(defvar nemacs-library-package-signature-release-sign-armor nil
  "When non-nil, create armored detached signatures.")

(defun nemacs-library-package-signature-release-sign--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-signature-release-sign--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-signature-release-sign--tsv-cell
             cells "\t"))

(defun nemacs-library-package-signature-release-sign--read-tsv (file)
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

(defun nemacs-library-package-signature-release-sign--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file
                    nemacs-library-package-signature-release-sign-repo-root))

(defun nemacs-library-package-signature-release-sign--normalize-fingerprint
    (fingerprint)
  "Return normalized uppercase FINGERPRINT hex."
  (upcase
   (replace-regexp-in-string "[^[:xdigit:]]" "" (or fingerprint ""))))

(defun nemacs-library-package-signature-release-sign--fingerprint-valid-p
    (fingerprint)
  "Return non-nil when FINGERPRINT is a full 40-hex OpenPGP fingerprint."
  (string-match-p
   "\\`[[:xdigit:]]\\{40\\}\\'"
   (nemacs-library-package-signature-release-sign--normalize-fingerprint
    fingerprint)))

(defun nemacs-library-package-signature-release-sign--hash-file (file)
  "Return SHA-256 digest for FILE."
  (unless (file-readable-p file)
    (error "missing readable file: %s" file))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-signature-release-sign--artifact-rows ()
  "Return signature policy artifact rows to sign."
  (seq-filter
   (lambda (row)
     (not (string= (nth 0 row) "signing-key")))
   (nemacs-library-package-signature-release-sign--read-tsv
    nemacs-library-package-signature-release-sign-policy)))

(defun nemacs-library-package-signature-release-sign--gpg-args
    (fingerprint signature artifact)
  "Return GnuPG arguments for FINGERPRINT SIGNATURE ARTIFACT."
  (append
   (list "--batch"
         "--yes"
         "--local-user" fingerprint
         "--output" signature
         "--detach-sign")
   (and nemacs-library-package-signature-release-sign-armor
        (list "--armor"))
   (list artifact)))

(defun nemacs-library-package-signature-release-sign--sign-one (row)
  "Sign one artifact policy ROW and return report row."
  (let* ((artifact-type (nth 0 row))
         (subject (nth 1 row))
         (artifact (nth 2 row))
         (signature (nth 3 row))
         (artifact-path
          (nemacs-library-package-signature-release-sign--absolute artifact))
         (signature-path
          (nemacs-library-package-signature-release-sign--absolute signature))
         (fingerprint
          (nemacs-library-package-signature-release-sign--normalize-fingerprint
           nemacs-library-package-signature-release-sign-key-fingerprint))
         (status "ok")
         (details "detached signature created"))
    (cond
     ((not (nemacs-library-package-signature-release-sign--fingerprint-valid-p
            fingerprint))
      (setq status "fail")
      (setq details
            "NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT must be a 40-hex OpenPGP fingerprint"))
     ((not (executable-find
            nemacs-library-package-signature-release-sign-gpg-program))
      (setq status "fail")
      (setq details "gpg executable is not available"))
     ((not (file-readable-p artifact-path))
      (setq status "fail")
      (setq details "artifact file is not readable"))
     (t
      (make-directory (file-name-directory signature-path) t)
      (with-temp-buffer
        (let ((exit-code
               (apply
                #'call-process
                nemacs-library-package-signature-release-sign-gpg-program
                nil t nil
                (nemacs-library-package-signature-release-sign--gpg-args
                 fingerprint signature-path artifact-path))))
          (unless (and (eq exit-code 0)
                       (file-readable-p signature-path))
            (setq status "fail")
            (setq details
                  (format "gpg detach-sign failed: exit=%s output=%s"
                          exit-code
                          (string-trim
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))))))
    (list artifact-type
          subject
          artifact
          signature
          fingerprint
          (if nemacs-library-package-signature-release-sign-armor
              "yes"
            "no")
          (if (file-readable-p signature-path)
              (number-to-string
               (nth 7 (file-attributes signature-path)))
            "")
          (if (file-readable-p signature-path)
              (nemacs-library-package-signature-release-sign--hash-file
               signature-path)
            "")
          status
          details)))

(defun nemacs-library-package-signature-release-sign--write-tsv (rows)
  "Write signing ROWS to TSV output."
  (make-directory
   (file-name-directory
    nemacs-library-package-signature-release-sign-output)
   t)
  (with-temp-file nemacs-library-package-signature-release-sign-output
    (insert
     (nemacs-library-package-signature-release-sign--row
      "artifact_type" "subject" "artifact_file" "signature_file"
      "signing_key_fingerprint" "armored" "signature_bytes"
      "signature_sha256" "status" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-signature-release-sign--row row)
       "\n"))))

(defun nemacs-library-package-signature-release-sign--write-summary (rows)
  "Write signing ROWS to Org summary output."
  (let ((ok 0)
        (fail 0))
    (dolist (row rows)
      (if (string= (nth 8 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail))))
    (make-directory
     (file-name-directory
      nemacs-library-package-signature-release-sign-summary-output)
     t)
    (with-temp-file
        nemacs-library-package-signature-release-sign-summary-output
      (insert "#+TITLE: nemacs library package release signing\n\n")
      (insert "* Summary\n\n")
      (insert (format "- artifacts: %d\n" (length rows)))
      (insert (format "- signed: %d\n" ok))
      (insert (format "- failures: %d\n" fail))
      (insert
       (format "- armored: %s\n"
               (if nemacs-library-package-signature-release-sign-armor
                   "yes"
                 "no")))
      (insert
       (format "- signing key configured: %s\n\n"
               (if (nemacs-library-package-signature-release-sign--fingerprint-valid-p
                    nemacs-library-package-signature-release-sign-key-fingerprint)
                   "yes"
                 "no")))
      (insert "* Signed Artifacts\n\n")
      (insert "| Type | Subject | Status | Signature | SHA-256 |\n")
      (insert "|------+---------+--------+-----------+---------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | =%s= | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 8 row)
                 (nth 3 row)
                 (nth 7 row))))
      (insert "\n* Notes\n\n")
      (insert "- Run =make nemacs-library-package-signature-release-verify= after signing.\n")
      (insert "- Signing is release-only and requires local GnuPG private key access.\n"))))

;;;###autoload
(defun nemacs-library-package-signature-release-sign-batch ()
  "Create detached signatures for release package artifacts."
  (let* ((rows
          (mapcar
           #'nemacs-library-package-signature-release-sign--sign-one
           (nemacs-library-package-signature-release-sign--artifact-rows)))
         (failures 0))
    (unless rows
      (error "empty release signature artifact rows"))
    (nemacs-library-package-signature-release-sign--write-tsv rows)
    (nemacs-library-package-signature-release-sign--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 8 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-signature-release-sign: artifacts=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-package-signature-release-sign-output
      nemacs-library-package-signature-release-sign-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-signature-release-sign)

;;; nemacs-library-package-signature-release-sign.el ends here
