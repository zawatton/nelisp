;;; nemacs-library-package-release-publication-policy.el --- release bundle publication gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that a retained release bundle is suitable to hand to a publication
;; step.  Draft mode records the unsigned/local-development state while still
;; proving manifest hashes, smoke rows, and absence of manifest-external files.
;; Strict mode additionally requires no pending rows, retained signatures, and
;; retained release evidence.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar nemacs-library-package-release-publication-policy-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-release-publication-policy-manifest
  (expand-file-name "build/nemacs-library-package-release-bundle-manifest.tsv"
                    nemacs-library-package-release-publication-policy-repo-root)
  "Release bundle manifest TSV.")

(defvar nemacs-library-package-release-publication-policy-smoke
  (expand-file-name "build/nemacs-library-package-release-bundle-smoke.tsv"
                    nemacs-library-package-release-publication-policy-repo-root)
  "Release bundle smoke TSV.")

(defvar nemacs-library-package-release-publication-policy-bundle-root
  (expand-file-name "build/nemacs-library-package-release-bundle"
                    nemacs-library-package-release-publication-policy-repo-root)
  "Release bundle root.")

(defvar nemacs-library-package-release-publication-policy-output
  (expand-file-name "build/nemacs-library-package-release-publication-policy.tsv"
                    nemacs-library-package-release-publication-policy-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-release-publication-policy-summary-output
  (expand-file-name "build/nemacs-library-package-release-publication-policy.org"
                    nemacs-library-package-release-publication-policy-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-release-publication-policy-strict-release nil
  "When non-nil, require a fully publishable signed release bundle.")

(defconst nemacs-library-package-release-publication-policy--contract
  "release-bundle-publication-contract"
  "Policy name used for publishable release bundle checks.")

(defun nemacs-library-package-release-publication-policy--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-release-publication-policy--row (&rest cells)
  "Return CELLS as a TSV row."
  (mapconcat #'nemacs-library-package-release-publication-policy--tsv-cell
             cells "\t"))

(defun nemacs-library-package-release-publication-policy--read-tsv (file)
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

(defun nemacs-library-package-release-publication-policy--absolute (file)
  "Return FILE as an absolute path rooted at the repository."
  (expand-file-name file
                    nemacs-library-package-release-publication-policy-repo-root))

(defun nemacs-library-package-release-publication-policy--relative (file)
  "Return FILE relative to the repository root."
  (file-relative-name
   (expand-file-name
    file
    nemacs-library-package-release-publication-policy-repo-root)
   nemacs-library-package-release-publication-policy-repo-root))

(defun nemacs-library-package-release-publication-policy--hash-file (file)
  "Return SHA-256 digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-release-publication-policy--file-bytes (file)
  "Return byte size for FILE."
  (number-to-string (nth 7 (file-attributes file))))

(defun nemacs-library-package-release-publication-policy--bundle-files ()
  "Return all regular files under the release bundle root as relative paths."
  (let* ((root
          (file-name-as-directory
           (expand-file-name
            nemacs-library-package-release-publication-policy-bundle-root)))
         files)
    (when (file-directory-p root)
      (dolist (file (directory-files-recursively root ".*" nil))
        (when (file-regular-p file)
          (push
           (nemacs-library-package-release-publication-policy--relative file)
           files))))
    (sort files #'string<)))

(defun nemacs-library-package-release-publication-policy--count-if
    (predicate rows)
  "Return number of ROWS where PREDICATE returns non-nil."
  (let ((count 0))
    (dolist (row rows count)
      (when (funcall predicate row)
        (setq count (1+ count))))))

(defun nemacs-library-package-release-publication-policy--ok
    (check subject evidence details)
  "Return an ok policy row."
  (list check subject
        nemacs-library-package-release-publication-policy--contract
        "ok" evidence details))

(defun nemacs-library-package-release-publication-policy--fail
    (check subject evidence details)
  "Return a failing policy row."
  (list check subject
        nemacs-library-package-release-publication-policy--contract
        "fail" evidence details))

(defun nemacs-library-package-release-publication-policy--status-row
    (check subject status evidence details)
  "Return a policy row with explicit STATUS."
  (list check subject
        nemacs-library-package-release-publication-policy--contract
        status evidence details))

(defun nemacs-library-package-release-publication-policy--retained-files
    (manifest-rows)
  "Return retained bundle files from MANIFEST-ROWS."
  (let (files)
    (dolist (row manifest-rows (sort files #'string<))
      (when (string= (nth 7 row) "yes")
        (push (nth 3 row) files)))))

(defun nemacs-library-package-release-publication-policy--duplicate-files
    (files)
  "Return duplicate entries in FILES."
  (let ((seen (make-hash-table :test #'equal))
        duplicates)
    (dolist (file files (sort (delete-dups duplicates) #'string<))
      (if (gethash file seen)
          (push file duplicates)
        (puthash file t seen)))))

(defun nemacs-library-package-release-publication-policy--manifest-hash-rows
    (manifest-rows)
  "Return verification rows for retained files in MANIFEST-ROWS."
  (let (rows)
    (dolist (row manifest-rows (nreverse rows))
      (when (string= (nth 7 row) "yes")
        (let* ((bundle-file (nth 3 row))
               (path
                (nemacs-library-package-release-publication-policy--absolute
                 bundle-file))
               (expected-bytes (nth 4 row))
               (expected-sha256 (nth 5 row)))
          (push
           (cond
            ((not (file-readable-p path))
             (nemacs-library-package-release-publication-policy--fail
              "retained-file-readable" bundle-file bundle-file
              "retained manifest file is not readable"))
            ((not (string= expected-bytes
                           (nemacs-library-package-release-publication-policy--file-bytes
                            path)))
             (nemacs-library-package-release-publication-policy--fail
              "retained-file-bytes" bundle-file bundle-file
              "retained manifest file byte count mismatch"))
            ((not (string= expected-sha256
                           (nemacs-library-package-release-publication-policy--hash-file
                            path)))
             (nemacs-library-package-release-publication-policy--fail
              "retained-file-sha256" bundle-file bundle-file
              "retained manifest file SHA-256 mismatch"))
            (t
             (nemacs-library-package-release-publication-policy--ok
              "retained-file-hash" bundle-file bundle-file
              "retained manifest file bytes and SHA-256 verified")))
           rows))))))

(defun nemacs-library-package-release-publication-policy--build-rows ()
  "Return release publication policy rows."
  (let* ((manifest-rows
          (nemacs-library-package-release-publication-policy--read-tsv
           nemacs-library-package-release-publication-policy-manifest))
         (smoke-rows
          (nemacs-library-package-release-publication-policy--read-tsv
           nemacs-library-package-release-publication-policy-smoke))
         (strict
          nemacs-library-package-release-publication-policy-strict-release)
         (manifest-failures
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 9 row) "fail"))
           manifest-rows))
         (manifest-pending
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 9 row) "pending"))
           manifest-rows))
         (retained
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 7 row) "yes"))
           manifest-rows))
         (archive-index
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row)
             (and (string= (nth 0 row) "archive-index")
                  (string= (nth 7 row) "yes")))
           manifest-rows))
         (tarballs
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row)
             (and (string= (nth 0 row) "package-tarball")
                  (string= (nth 7 row) "yes")))
           manifest-rows))
         (signature-rows
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 0 row) "detached-signature"))
           manifest-rows))
         (retained-signatures
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row)
             (and (string= (nth 0 row) "detached-signature")
                  (string= (nth 7 row) "yes")))
           manifest-rows))
         (release-evidence
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 0 row) "release-evidence"))
           manifest-rows))
         (retained-release-evidence
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row)
             (and (string= (nth 0 row) "release-evidence")
                  (string= (nth 7 row) "yes")))
           manifest-rows))
         (smoke-ok
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 1 row) "ok"))
           smoke-rows))
         (smoke-fail
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (not (string= (nth 1 row) "ok")))
           smoke-rows))
         (retained-files
          (nemacs-library-package-release-publication-policy--retained-files
           manifest-rows))
         (duplicate-retained-files
          (nemacs-library-package-release-publication-policy--duplicate-files
           retained-files))
         (bundle-files
          (nemacs-library-package-release-publication-policy--bundle-files))
         (unexpected
          (seq-difference bundle-files retained-files #'string=))
         rows)
    (push
     (nemacs-library-package-release-publication-policy--ok
      "manifest-readable" "release-bundle"
      (nemacs-library-package-release-publication-policy--relative
       nemacs-library-package-release-publication-policy-manifest)
      (format "manifest rows=%d" (length manifest-rows)))
     rows)
    (push
     (nemacs-library-package-release-publication-policy--ok
      "bundle-smoke-readable" "release-bundle"
      (nemacs-library-package-release-publication-policy--relative
       nemacs-library-package-release-publication-policy-smoke)
      (format "smoke rows=%d" (length smoke-rows)))
     rows)
    (push
     (if (file-directory-p
          nemacs-library-package-release-publication-policy-bundle-root)
         (nemacs-library-package-release-publication-policy--ok
          "bundle-root-readable" "release-bundle"
          (nemacs-library-package-release-publication-policy--relative
           nemacs-library-package-release-publication-policy-bundle-root)
          "bundle root exists")
       (nemacs-library-package-release-publication-policy--fail
        "bundle-root-readable" "release-bundle"
        (nemacs-library-package-release-publication-policy--relative
         nemacs-library-package-release-publication-policy-bundle-root)
        "bundle root is not a directory"))
     rows)
    (push
     (if (zerop manifest-failures)
         (nemacs-library-package-release-publication-policy--ok
          "manifest-failure-rows" "release-bundle" manifest-failures
          "manifest has no failure rows")
       (nemacs-library-package-release-publication-policy--fail
        "manifest-failure-rows" "release-bundle" manifest-failures
        "manifest contains failure rows"))
     rows)
    (push
     (cond
      ((zerop manifest-pending)
       (nemacs-library-package-release-publication-policy--ok
        "manifest-pending-rows" "release-bundle" manifest-pending
        "manifest has no pending rows"))
      (strict
       (nemacs-library-package-release-publication-policy--fail
        "manifest-pending-rows" "release-bundle" manifest-pending
        "strict publication requires zero pending rows"))
      (t
       (nemacs-library-package-release-publication-policy--status-row
        "manifest-pending-rows" "release-bundle" "pending" manifest-pending
        "draft bundle records release-only files as pending")))
     rows)
    (push
     (if (and (= archive-index 1) (= tarballs 8))
         (nemacs-library-package-release-publication-policy--ok
          "archive-artifact-retention" "release-bundle"
          (format "archive-index=%d tarballs=%d" archive-index tarballs)
          "archive contents and package tarballs are retained")
       (nemacs-library-package-release-publication-policy--fail
        "archive-artifact-retention" "release-bundle"
        (format "archive-index=%d tarballs=%d" archive-index tarballs)
        "expected one archive-contents row and 8 package tarballs"))
     rows)
    (push
     (cond
      ((= retained-signatures signature-rows 9)
       (nemacs-library-package-release-publication-policy--ok
        "detached-signature-retention" "release-bundle"
        (format "%d/%d" retained-signatures signature-rows)
        "all detached signatures are retained"))
      (strict
       (nemacs-library-package-release-publication-policy--fail
        "detached-signature-retention" "release-bundle"
        (format "%d/%d" retained-signatures signature-rows)
        "strict publication requires all detached signatures"))
      (t
       (nemacs-library-package-release-publication-policy--status-row
        "detached-signature-retention" "release-bundle" "pending"
        (format "%d/%d" retained-signatures signature-rows)
        "draft bundle records missing detached signatures as pending")))
     rows)
    (push
     (cond
      ((= retained-release-evidence release-evidence 5)
       (nemacs-library-package-release-publication-policy--ok
        "release-evidence-retention" "release-bundle"
        (format "%d/%d" retained-release-evidence release-evidence)
        "all strict release evidence files are retained"))
      (strict
       (nemacs-library-package-release-publication-policy--fail
        "release-evidence-retention" "release-bundle"
        (format "%d/%d" retained-release-evidence release-evidence)
        "strict publication requires all release evidence"))
      (t
       (nemacs-library-package-release-publication-policy--status-row
        "release-evidence-retention" "release-bundle" "pending"
        (format "%d/%d" retained-release-evidence release-evidence)
        "draft bundle records release evidence as pending")))
     rows)
    (push
     (if (and (= smoke-ok 8) (zerop smoke-fail))
         (nemacs-library-package-release-publication-policy--ok
          "bundle-smoke-ok" "release-bundle"
          (format "ok=%d failures=%d" smoke-ok smoke-fail)
          "all reusable packages install from the retained bundle")
       (nemacs-library-package-release-publication-policy--fail
        "bundle-smoke-ok" "release-bundle"
        (format "ok=%d failures=%d" smoke-ok smoke-fail)
        "expected 8 package smoke rows and zero failures"))
     rows)
    (push
     (if duplicate-retained-files
         (nemacs-library-package-release-publication-policy--fail
          "manifest-duplicate-bundle-files" "release-bundle"
          (mapconcat #'identity duplicate-retained-files ",")
          "retained manifest rows must not share the same bundle file")
       (nemacs-library-package-release-publication-policy--ok
        "manifest-duplicate-bundle-files" "release-bundle"
        (format "retained=%d unique=%d"
                retained
                (length (delete-dups (copy-sequence retained-files))))
        "retained manifest bundle paths are unique"))
     rows)
    (push
     (if unexpected
         (nemacs-library-package-release-publication-policy--fail
          "manifest-external-files" "release-bundle"
          (mapconcat #'identity unexpected ",")
          "bundle root contains files not retained by manifest")
       (nemacs-library-package-release-publication-policy--ok
        "manifest-external-files" "release-bundle"
        (format "bundle-files=%d retained=%d"
                (length bundle-files) retained)
        "all bundle files are retained manifest rows"))
     rows)
    (setq rows
          (append
           (nreverse rows)
           (nemacs-library-package-release-publication-policy--manifest-hash-rows
            manifest-rows)))
    rows))

(defun nemacs-library-package-release-publication-policy--write-tsv (rows)
  "Write policy ROWS to TSV."
  (make-directory
   (file-name-directory
    nemacs-library-package-release-publication-policy-output)
   t)
  (with-temp-file nemacs-library-package-release-publication-policy-output
    (insert
     (nemacs-library-package-release-publication-policy--row
      "check" "subject" "policy" "status" "evidence" "details")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-release-publication-policy--row row)
       "\n"))))

(defun nemacs-library-package-release-publication-policy--write-summary
    (rows)
  "Write policy ROWS as an Org summary."
  (let ((ok
         (nemacs-library-package-release-publication-policy--count-if
          (lambda (row) (string= (nth 3 row) "ok"))
          rows))
        (pending
         (nemacs-library-package-release-publication-policy--count-if
          (lambda (row) (string= (nth 3 row) "pending"))
          rows))
        (failures
         (nemacs-library-package-release-publication-policy--count-if
          (lambda (row) (string= (nth 3 row) "fail"))
          rows)))
    (make-directory
     (file-name-directory
      nemacs-library-package-release-publication-policy-summary-output)
     t)
    (with-temp-file
        nemacs-library-package-release-publication-policy-summary-output
      (insert "#+TITLE: nemacs library package release publication policy\n\n")
      (insert "* Summary\n\n")
      (insert (format "- checks: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- pending: %d\n" pending))
      (insert (format "- failures: %d\n" failures))
      (insert
       (format "- strict release: %s\n\n"
               (if nemacs-library-package-release-publication-policy-strict-release
                   "yes"
                 "no")))
      (insert "* Checks\n\n")
      (insert "| Check | Subject | Status | Evidence | Details |\n")
      (insert "|-------+---------+--------+----------+---------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | =%s= | %s |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 3 row)
                 (nth 4 row)
                 (nth 5 row))))
      (insert "\n* Notes\n\n")
      (insert "- Draft mode allows pending rows only for release-only signatures and release evidence.\n")
      (insert "- Strict mode requires zero pending rows, no manifest-external files, verified retained hashes, and successful bundle smoke.\n"))))

;;;###autoload
(defun nemacs-library-package-release-publication-policy-batch ()
  "Write the release publication policy."
  (let* ((rows
          (nemacs-library-package-release-publication-policy--build-rows))
         (failures
          (nemacs-library-package-release-publication-policy--count-if
           (lambda (row) (string= (nth 3 row) "fail"))
           rows)))
    (unless rows
      (error "empty release publication policy rows"))
    (nemacs-library-package-release-publication-policy--write-tsv rows)
    (nemacs-library-package-release-publication-policy--write-summary rows)
    (princ
     (format
      "nemacs-library-package-release-publication-policy: checks=%d failures=%d strict=%s output=%s summary=%s\n"
      (length rows)
      failures
      (if nemacs-library-package-release-publication-policy-strict-release
          "yes"
        "no")
      nemacs-library-package-release-publication-policy-output
      nemacs-library-package-release-publication-policy-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-release-publication-policy)

;;; nemacs-library-package-release-publication-policy.el ends here
