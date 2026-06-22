;;; nemacs-library-package-vendor-lock.el --- vendor dependency lock artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a release-review lock for vendored package dependencies used by
;; reusable nelisp-emacs packages.  The lock records the containing vendor Git
;; HEAD and a deterministic content hash for the package directory.  Daily
;; library gates can run this in draft mode; release verification can set
;; `nemacs-library-package-vendor-lock-release-strict' to require clean package
;; paths.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-library-package-vendor-lock-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-vendor-lock-dependency-policy
  (expand-file-name "build/nemacs-library-package-dependency-publication-policy.tsv"
                    nemacs-library-package-vendor-lock-repo-root)
  "Dependency publication policy TSV.")

(defvar nemacs-library-package-vendor-lock-output
  (expand-file-name "build/nemacs-library-package-vendor-lock.tsv"
                    nemacs-library-package-vendor-lock-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-vendor-lock-summary-output
  (expand-file-name "build/nemacs-library-package-vendor-lock.org"
                    nemacs-library-package-vendor-lock-repo-root)
  "Org summary output path.")

(defvar nemacs-library-package-vendor-lock-release-strict nil
  "When non-nil, fail if any vendored package path has uncommitted changes.")

(defun nemacs-library-package-vendor-lock--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-vendor-lock--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-vendor-lock--tsv-cell cells "\t"))

(defun nemacs-library-package-vendor-lock--read-tsv (file)
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

(defun nemacs-library-package-vendor-lock--git-lines (directory &rest args)
  "Run git in DIRECTORY with ARGS and return (EXIT . LINES)."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil
                       "-C" directory args)))
      (cons exit
            (split-string
             (string-trim
              (buffer-substring-no-properties (point-min) (point-max)))
             "\n" t)))))

(defun nemacs-library-package-vendor-lock--git-string (directory &rest args)
  "Run git in DIRECTORY with ARGS and return output string, or nil."
  (let ((result
         (apply #'nemacs-library-package-vendor-lock--git-lines
                directory args)))
    (and (= (car result) 0)
         (car (cdr result)))))

(defun nemacs-library-package-vendor-lock--git-count (directory &rest args)
  "Run git in DIRECTORY with ARGS and return output line count, or nil."
  (let ((result
         (apply #'nemacs-library-package-vendor-lock--git-lines
                directory args)))
    (and (= (car result) 0)
         (length (cdr result)))))

(defun nemacs-library-package-vendor-lock--regular-files (directory)
  "Return sorted regular files under DIRECTORY."
  (sort
   (cl-remove-if-not
    #'file-regular-p
    (directory-files-recursively directory ".*" nil nil t))
   #'string<))

(defun nemacs-library-package-vendor-lock--file-sha256 (file)
  "Return SHA-256 digest for FILE contents."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nemacs-library-package-vendor-lock--directory-sha256 (directory)
  "Return deterministic SHA-256 digest for DIRECTORY contents."
  (let ((files (nemacs-library-package-vendor-lock--regular-files directory)))
    (with-temp-buffer
      (dolist (file files)
        (let ((relative (file-relative-name file directory)))
          (insert relative "\t"
                  (number-to-string
                   (file-attribute-size (file-attributes file)))
                  "\t"
                  (nemacs-library-package-vendor-lock--file-sha256 file)
                  "\n")))
      (list (length files)
            (secure-hash 'sha256 (current-buffer))))))

(defun nemacs-library-package-vendor-lock--vendor-policy-rows ()
  "Return vendored package rows from dependency publication policy."
  (cl-remove-if-not
   (lambda (row)
     (and (string= (nth 0 row) "vendor-package")
          (string= (nth 4 row) "ok")))
   (nemacs-library-package-vendor-lock--read-tsv
    nemacs-library-package-vendor-lock-dependency-policy)))

(defun nemacs-library-package-vendor-lock--build-row (policy-row)
  "Return a vendor lock row for POLICY-ROW."
  (let* ((package (nth 1 policy-row))
         (feature (nth 2 policy-row))
         (directory (nth 5 policy-row))
         (absolute-directory
          (expand-file-name directory
                            nemacs-library-package-vendor-lock-repo-root)))
    (cond
     ((not (file-directory-p absolute-directory))
      (list package feature directory "" "" "0" "" "" "" "missing"
            "fail" "vendored package directory is missing"))
     (t
      (let* ((repo-root
              (nemacs-library-package-vendor-lock--git-string
               absolute-directory "rev-parse" "--show-toplevel"))
             (repo-head
              (and repo-root
                   (nemacs-library-package-vendor-lock--git-string
                    repo-root "rev-parse" "HEAD")))
             (repo-dirty
              (and repo-root
                   (nemacs-library-package-vendor-lock--git-count
                    repo-root "status" "--porcelain" "--untracked-files=all")))
             (package-relative
              (and repo-root
                   (file-relative-name absolute-directory repo-root)))
             (package-dirty
              (and repo-root package-relative
                   (nemacs-library-package-vendor-lock--git-count
                    repo-root "status" "--porcelain" "--untracked-files=all"
                    "--" package-relative)))
             (digest
              (nemacs-library-package-vendor-lock--directory-sha256
               absolute-directory))
             (file-count (number-to-string (car digest)))
             (package-sha256 (cadr digest))
             (package-clean-p (equal package-dirty 0))
             (head-valid-p
              (and repo-head
                   (string-match-p "\\`[0-9a-f]\\{40\\}\\'" repo-head)))
             (strict nemacs-library-package-vendor-lock-release-strict)
             (status
              (if (and head-valid-p
                       (or (not strict) package-clean-p))
                  "ok"
                "fail"))
             (release-status
              (cond
               ((not head-valid-p) "missing-git-head")
               ((not package-clean-p) "package-dirty")
               (strict "release-ok")
               (t "draft-ok")))
             (details
              (cond
               ((not head-valid-p)
                "vendored package must be inside a Git repository with a pinned HEAD")
               ((and strict (not package-clean-p))
                "release strict mode requires a clean vendored package path")
               ((and repo-dirty (> repo-dirty 0))
                "vendor repository has unrelated dirty paths; package path lock is clean")
               (t "vendored package path is lockable"))))
        (list package feature directory
              (or repo-root "")
              (or repo-head "")
              (number-to-string (or repo-dirty 0))
              (number-to-string (or package-dirty 0))
              file-count
              package-sha256
              release-status
              status
              details))))))

(defun nemacs-library-package-vendor-lock--build-rows ()
  "Return vendor lock rows."
  (mapcar #'nemacs-library-package-vendor-lock--build-row
          (nemacs-library-package-vendor-lock--vendor-policy-rows)))

(defun nemacs-library-package-vendor-lock--write-tsv (rows)
  "Write vendor lock ROWS to TSV output."
  (make-directory
   (file-name-directory nemacs-library-package-vendor-lock-output)
   t)
  (with-temp-file nemacs-library-package-vendor-lock-output
    (insert
     (nemacs-library-package-vendor-lock--row
      "package_id" "feature" "vendor_directory" "vendor_repo_root"
      "vendor_repo_head" "vendor_repo_dirty_paths" "vendor_package_dirty_paths"
      "vendor_package_files" "vendor_package_sha256" "release_status"
      "status" "details")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-vendor-lock--row row) "\n"))))

(defun nemacs-library-package-vendor-lock--write-summary (rows)
  "Write vendor lock ROWS to Org summary output."
  (let ((ok 0)
        (fail 0)
        (package-dirty 0)
        (repo-dirty 0))
    (dolist (row rows)
      (if (string= (nth 10 row) "ok")
          (setq ok (1+ ok))
        (setq fail (1+ fail)))
      (when (> (string-to-number (nth 5 row)) 0)
        (setq repo-dirty (1+ repo-dirty)))
      (when (> (string-to-number (nth 6 row)) 0)
        (setq package-dirty (1+ package-dirty))))
    (make-directory
     (file-name-directory nemacs-library-package-vendor-lock-summary-output)
     t)
    (with-temp-file nemacs-library-package-vendor-lock-summary-output
      (insert "#+TITLE: nemacs library package vendor lock\n\n")
      (insert "* Summary\n\n")
      (insert (format "- vendor package rows: %d\n" (length rows)))
      (insert (format "- ok: %d\n" ok))
      (insert (format "- failures: %d\n" fail))
      (insert (format "- release strict: %s\n"
                      (if nemacs-library-package-vendor-lock-release-strict
                          "yes"
                        "no")))
      (insert (format "- dirty vendor repositories: %d\n" repo-dirty))
      (insert (format "- dirty vendor package paths: %d\n\n" package-dirty))
      (insert "* Policy\n\n")
      (insert "- Vendored dependencies are locked by vendor Git HEAD and package directory SHA-256.\n")
      (insert "- Draft mode records dirty state without failing clean package paths.\n")
      (insert "- Release strict mode fails when a vendored package path has uncommitted changes.\n\n")
      (insert "* Locks\n\n")
      (insert "| Package | Feature | Status | Release Status | Package SHA-256 |\n")
      (insert "|---------+---------+--------+----------------+----------------|\n")
      (dolist (row rows)
        (insert
         (format "| =%s= | =%s= | %s | =%s= | =%s= |\n"
                 (nth 0 row)
                 (nth 1 row)
                 (nth 10 row)
                 (nth 9 row)
                 (nth 8 row)))))))

;;;###autoload
(defun nemacs-library-package-vendor-lock-batch ()
  "Generate and verify vendored package lock artifacts."
  (let* ((rows (nemacs-library-package-vendor-lock--build-rows))
         (failures 0))
    (unless rows
      (error "empty vendor lock rows"))
    (nemacs-library-package-vendor-lock--write-tsv rows)
    (nemacs-library-package-vendor-lock--write-summary rows)
    (dolist (row rows)
      (unless (string= (nth 10 row) "ok")
        (setq failures (1+ failures))))
    (princ
     (format
      "nemacs-library-package-vendor-lock: rows=%d failures=%d strict=%s output=%s summary=%s\n"
      (length rows)
      failures
      (if nemacs-library-package-vendor-lock-release-strict "yes" "no")
      nemacs-library-package-vendor-lock-output
      nemacs-library-package-vendor-lock-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-package-vendor-lock)

;;; nemacs-library-package-vendor-lock.el ends here
