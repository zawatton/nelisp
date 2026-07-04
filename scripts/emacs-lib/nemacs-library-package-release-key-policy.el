;;; nemacs-library-package-release-key-policy.el --- release public key policy -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify the public release key material used to validate signed package
;; release bundles.  Normal library gates run in draft mode and can pass
;; without committed public key material.  Strict release mode requires an
;; explicit public key file, a 40-hex OpenPGP fingerprint, and a GnuPG import
;; proof that the key material contains the expected fingerprint.

;;; Code:

(require 'subr-x)

(defvar nemacs-library-package-release-key-policy-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-release-key-policy-public-key-file
  (expand-file-name "docs/release/nemacs-library-release-public-key.asc"
                    nemacs-library-package-release-key-policy-repo-root)
  "Release public key file.")

(defvar nemacs-library-package-release-key-policy-key-fingerprint
  (or (getenv "NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT") "")
  "Expected release signing key fingerprint.")

(defvar nemacs-library-package-release-key-policy-output
  (expand-file-name "build/nemacs-library-package-release-key-policy.tsv"
                    nemacs-library-package-release-key-policy-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-release-key-policy-summary-output
  (expand-file-name "build/nemacs-library-package-release-key-policy.org"
                    nemacs-library-package-release-key-policy-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-release-key-policy-strict nil
  "When non-nil, require concrete public key material and fingerprint match.")

(defvar nemacs-library-package-release-key-policy-gpg-program "gpg"
  "GnuPG executable used to inspect the release public key.")

(defun nemacs-library-package-release-key-policy--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-release-key-policy--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-release-key-policy--tsv-cell
             cells "\t"))

(defun nemacs-library-package-release-key-policy--relative (file)
  "Return FILE relative to the repository root."
  (file-relative-name
   (expand-file-name file nemacs-library-package-release-key-policy-repo-root)
   nemacs-library-package-release-key-policy-repo-root))

(defun nemacs-library-package-release-key-policy--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file
                    nemacs-library-package-release-key-policy-repo-root))

(defun nemacs-library-package-release-key-policy--normalize-fingerprint
    (fingerprint)
  "Return normalized uppercase FINGERPRINT hex."
  (upcase
   (replace-regexp-in-string "[^[:xdigit:]]" "" (or fingerprint ""))))

(defun nemacs-library-package-release-key-policy--fingerprint-valid-p
    (fingerprint)
  "Return non-nil when FINGERPRINT is a full 40-hex OpenPGP fingerprint."
  (string-match-p
   "\\`[[:xdigit:]]\\{40\\}\\'"
   (nemacs-library-package-release-key-policy--normalize-fingerprint
    fingerprint)))

(defun nemacs-library-package-release-key-policy--hash-file (file)
  "Return SHA-256 digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-release-key-policy--file-bytes (file)
  "Return byte size for FILE."
  (number-to-string (nth 7 (file-attributes file))))

(defun nemacs-library-package-release-key-policy--read-key-fingerprints
    (key-file)
  "Import KEY-FILE into a temporary GnuPG home and return (STATUS DETAILS FPRS)."
  (cond
   ((not (executable-find nemacs-library-package-release-key-policy-gpg-program))
    (list "fail" "gpg executable is not available" nil))
   ((not (file-readable-p key-file))
    (list "pending" "public key file is not readable" nil))
   (t
    (let ((home (make-temp-file "nemacs-release-key-gnupg-" t))
          (old-home (getenv "GNUPGHOME"))
          (status "ok")
          (details "public key imported")
          fingerprints)
      (unwind-protect
          (progn
            (set-file-modes home #o700)
            (setenv "GNUPGHOME" home)
            (with-temp-buffer
              (let ((exit-code
                     (call-process
                      nemacs-library-package-release-key-policy-gpg-program
                      nil t nil
                      "--batch" "--import" key-file)))
                (unless (eq exit-code 0)
                  (setq status "fail")
                  (setq details
                        (format "gpg import failed: exit=%s output=%s"
                                exit-code
                                (string-trim
                                 (buffer-substring-no-properties
                                  (point-min) (point-max))))))))
            (when (string= status "ok")
              (with-temp-buffer
                (let ((exit-code
                       (call-process
                        nemacs-library-package-release-key-policy-gpg-program
                        nil t nil
                        "--batch" "--with-colons" "--fingerprint"
                        "--list-keys")))
                  (if (eq exit-code 0)
                      (progn
                        (goto-char (point-min))
                        (while (not (eobp))
                          (let* ((line (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
                                 (fields (split-string line ":")))
                            (when (string= (car fields) "fpr")
                              (let ((fpr (nth 9 fields)))
                                (when (and fpr (not (string= fpr "")))
                                  (push (upcase fpr) fingerprints)))))
                          (forward-line 1))
                        (setq fingerprints
                              (sort (delete-dups fingerprints) #'string<))
                        (unless fingerprints
                          (setq status "fail")
                          (setq details "no fingerprints found in public key")))
                    (setq status "fail")
                    (setq details
                          (format "gpg list-keys failed: exit=%s output=%s"
                                  exit-code
                                  (string-trim
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))))))))
        (setenv "GNUPGHOME" old-home)
        (delete-directory home t))
      (list status details fingerprints)))))

(defun nemacs-library-package-release-key-policy--build-rows ()
  "Return release public key policy rows."
  (let* ((key-file
          (nemacs-library-package-release-key-policy--absolute
           nemacs-library-package-release-key-policy-public-key-file))
         (fingerprint
          (nemacs-library-package-release-key-policy--normalize-fingerprint
           nemacs-library-package-release-key-policy-key-fingerprint))
         (fingerprint-valid
          (nemacs-library-package-release-key-policy--fingerprint-valid-p
           fingerprint))
         (key-readable (file-readable-p key-file))
         (inspection
          (nemacs-library-package-release-key-policy--read-key-fingerprints
           key-file))
         (inspection-status (nth 0 inspection))
         (inspection-details (nth 1 inspection))
         (fingerprints (nth 2 inspection))
         (mode
          (if nemacs-library-package-release-key-policy-strict
              "strict-release"
            "draft-policy"))
         rows)
    (push
     (list "expected-fingerprint"
           "public-release"
           (nemacs-library-package-release-key-policy--relative key-file)
           fingerprint
           ""
           ""
           mode
           (cond
            (fingerprint-valid "ok")
            (nemacs-library-package-release-key-policy-strict "fail")
            (t "pending"))
           (if fingerprint-valid
               "expected release fingerprint configured"
             "NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT is not a 40-hex OpenPGP fingerprint"))
     rows)
    (push
     (list "public-key-file"
           "public-release"
           (nemacs-library-package-release-key-policy--relative key-file)
           fingerprint
           (if key-readable
               (nemacs-library-package-release-key-policy--file-bytes key-file)
             "")
           (if key-readable
               (nemacs-library-package-release-key-policy--hash-file key-file)
             "")
           mode
           (cond
            (key-readable "ok")
            (nemacs-library-package-release-key-policy-strict "fail")
            (t "pending"))
           (if key-readable
               "release public key file is readable"
             "release public key file is not configured"))
     rows)
    (push
     (list "public-key-import"
           "public-release"
           (nemacs-library-package-release-key-policy--relative key-file)
           fingerprint
           ""
           (mapconcat #'identity fingerprints ",")
           mode
           (cond
            ((string= inspection-status "ok") "ok")
            ((and (not key-readable)
                  (not nemacs-library-package-release-key-policy-strict))
             "pending")
            (t "fail"))
           inspection-details)
     rows)
    (push
     (list "fingerprint-match"
           "public-release"
           (nemacs-library-package-release-key-policy--relative key-file)
           fingerprint
           ""
           (mapconcat #'identity fingerprints ",")
           mode
           (cond
            ((and fingerprint-valid (member fingerprint fingerprints)) "ok")
            ((and (not nemacs-library-package-release-key-policy-strict)
                  (or (not fingerprint-valid) (not key-readable)))
             "pending")
            (t "fail"))
           (cond
            ((and fingerprint-valid (member fingerprint fingerprints))
             "public key material contains expected fingerprint")
            ((not fingerprint-valid)
             "expected fingerprint is not configured")
            (t "public key material does not contain expected fingerprint")))
     rows)
    (nreverse rows)))

(defun nemacs-library-package-release-key-policy--write-tsv (rows)
  "Write key policy ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-release-key-policy-output) t)
  (with-temp-file nemacs-library-package-release-key-policy-output
    (insert
     (nemacs-library-package-release-key-policy--row
      "check" "subject" "public_key_file" "expected_fingerprint"
      "public_key_bytes" "public_key_sha256_or_fingerprints"
      "verification_mode" "status" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-release-key-policy--row row)
       "\n"))))

(defun nemacs-library-package-release-key-policy--count-if
    (predicate rows)
  "Return count of ROWS matching PREDICATE."
  (let ((count 0))
    (dolist (row rows count)
      (when (funcall predicate row)
        (setq count (1+ count))))))

(defun nemacs-library-package-release-key-policy--write-summary (rows)
  "Write key policy ROWS to Org summary output."
  (let ((ok
         (nemacs-library-package-release-key-policy--count-if
          (lambda (row) (string= (nth 7 row) "ok"))
          rows))
        (pending
         (nemacs-library-package-release-key-policy--count-if
          (lambda (row) (string= (nth 7 row) "pending"))
          rows))
        (failures
         (nemacs-library-package-release-key-policy--count-if
          (lambda (row) (string= (nth 7 row) "fail"))
          rows)))
    (make-directory
     (file-name-directory
      nemacs-library-package-release-key-policy-summary-output)
     t)
    (with-temp-file
        nemacs-library-package-release-key-policy-summary-output
      (insert "#+TITLE: nemacs library package release key policy\n\n")
      (insert "* Summary\n\n")
      (insert (format "- checks: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- pending: %d\n" pending))
      (insert (format "- failures: %d\n" failures))
      (insert
       (format "- strict release: %s\n"
               (if nemacs-library-package-release-key-policy-strict
                   "yes"
                 "no")))
      (insert
       (format "- public key file: =%s=\n\n"
               (nemacs-library-package-release-key-policy--relative
                nemacs-library-package-release-key-policy-public-key-file)))
      (insert "* Checks\n\n")
      (insert "| Check | Status | Details |\n")
      (insert "|-------+--------+---------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | %s | %s |\n"
                 (nth 0 row)
                 (nth 7 row)
                 (nth 8 row))))
      (insert "\n* Notes\n\n")
      (insert "- Draft mode records missing public release key material as pending.\n")
      (insert "- Strict release mode requires the public key file and expected fingerprint to match.\n"))))

;;;###autoload
(defun nemacs-library-package-release-key-policy-batch ()
  "Generate and verify release public key policy artifacts."
  (let* ((rows
          (nemacs-library-package-release-key-policy--build-rows))
         (failures
          (nemacs-library-package-release-key-policy--count-if
           (lambda (row) (string= (nth 7 row) "fail"))
           rows)))
    (unless rows
      (error "empty release key policy rows"))
    (nemacs-library-package-release-key-policy--write-tsv rows)
    (nemacs-library-package-release-key-policy--write-summary rows)
    (princ
     (format
      "nemacs-library-package-release-key-policy: checks=%d failures=%d strict=%s output=%s summary=%s\n"
      (length rows)
      failures
      (if nemacs-library-package-release-key-policy-strict "yes" "no")
      nemacs-library-package-release-key-policy-output
      nemacs-library-package-release-key-policy-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-release-key-policy)

;;; nemacs-library-package-release-key-policy.el ends here
