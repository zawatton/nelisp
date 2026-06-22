;;; nemacs-library-app-boundary.el --- verify app scaffold boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that app/bootstrap policy files are staged in the app scaffold and
;; do not leak into reusable package scaffolds.  This complements
;; `nemacs-library-package-verify', which checks reusable package artifacts.

;;; Code:

(require 'cl-lib)

(defvar nemacs-library-app-boundary-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-app-boundary-package-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-app-boundary-repo-root)
  "Reusable package scaffold index.")

(defvar nemacs-library-app-boundary-app-scaffold
  (expand-file-name "build/nemacs-library-app-scaffold.tsv"
                    nemacs-library-app-boundary-repo-root)
  "App scaffold index.")

(defvar nemacs-library-app-boundary-output
  (expand-file-name "build/nemacs-library-app-boundary.tsv"
                    nemacs-library-app-boundary-repo-root)
  "TSV output path.")

(defvar nemacs-library-app-boundary-summary-output
  (expand-file-name "build/nemacs-library-app-boundary.org"
                    nemacs-library-app-boundary-repo-root)
  "Org summary output path.")

(defconst nemacs-library-app-boundary--app-files
  '(("app-bootstrap" "src/emacs-init.el"
     "packages/nelisp-emacs-app-gui/lisp/emacs-init.el")
    ("app-bootstrap" "src/nemacs-loadup.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-loadup.el")
    ("app-entry" "src/nemacs-main.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-main.el")
    ("app-utility" "src/image-baker.el"
     "packages/nelisp-emacs-app-gui/lisp/image-baker.el"))
  "App/bootstrap policy files that must stay outside reusable packages.")

(defconst nemacs-library-app-boundary--obsolete-app-files
  '("src/emacs-dump.el"
    "src/image-loader.el")
  "Reusable files that must no longer be staged as app-only files.")

(defun nemacs-library-app-boundary--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-app-boundary--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-app-boundary--tsv-cell cells "\t"))

(defun nemacs-library-app-boundary--scaffold-rows (index)
  "Return file rows from scaffold INDEX."
  (let (rows)
    (when (file-readable-p index)
      (with-temp-buffer
        (insert-file-contents index)
        (goto-char (point-min))
        (forward-line 1)
        (while (not (eobp))
          (let ((fields (split-string
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         "\t")))
            (when (equal (nth 0 fields) "file")
              (push fields rows)))
          (forward-line 1))))
    (nreverse rows)))

(defun nemacs-library-app-boundary--find-source (source rows)
  "Return row for SOURCE in ROWS, or nil."
  (cl-find source rows :key (lambda (row) (nth 3 row)) :test #'equal))

(defun nemacs-library-app-boundary--app-row-ok-p (expected rows)
  "Return non-nil when EXPECTED app row is present in ROWS."
  (let* ((role (nth 0 expected))
         (source (nth 1 expected))
         (target (nth 2 expected))
         (row (nemacs-library-app-boundary--find-source source rows)))
    (and row
         (equal (nth 2 row) role)
         (equal (nth 4 row) target))))

(defun nemacs-library-app-boundary--check-rows ()
  "Return app boundary check rows."
  (let ((app-rows (nemacs-library-app-boundary--scaffold-rows
                   nemacs-library-app-boundary-app-scaffold))
        (package-rows (nemacs-library-app-boundary--scaffold-rows
                       nemacs-library-app-boundary-package-scaffold))
        rows)
    (dolist (expected nemacs-library-app-boundary--app-files)
      (let ((source (nth 1 expected)))
        (push (list "app-file-staged"
                    source
                    (if (nemacs-library-app-boundary--app-row-ok-p
                         expected app-rows)
                        "ok" "fail")
                    (nth 0 expected)
                    (nth 2 expected))
              rows)
        (push (list "app-file-not-in-package"
                    source
                    (if (not (nemacs-library-app-boundary--find-source
                              source package-rows))
                        "ok" "fail")
                    ""
                    "")
              rows)))
    (dolist (source nemacs-library-app-boundary--obsolete-app-files)
      (push (list "reusable-file-not-app-only"
                  source
                  (if (not (nemacs-library-app-boundary--find-source
                            source app-rows))
                      "ok" "fail")
                  ""
                  "")
            rows))
    (nreverse rows)))

(defun nemacs-library-app-boundary--write-tsv (rows)
  "Write ROWS to `nemacs-library-app-boundary-output'."
  (make-directory (file-name-directory nemacs-library-app-boundary-output) t)
  (with-temp-file nemacs-library-app-boundary-output
    (insert
     (nemacs-library-app-boundary--row
      "check" "source_file" "status" "expected_role" "expected_target")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-app-boundary--row row) "\n"))))

(defun nemacs-library-app-boundary--write-summary (rows)
  "Write ROWS summary to `nemacs-library-app-boundary-summary-output'."
  (let ((failures (cl-count "fail" rows :key (lambda (row) (nth 2 row))
                            :test #'equal)))
    (make-directory
     (file-name-directory nemacs-library-app-boundary-summary-output) t)
    (with-temp-file nemacs-library-app-boundary-summary-output
      (insert "#+TITLE: nemacs library app boundary verification\n\n")
      (insert "* Summary\n\n")
      (insert (format "- checks: %d\n" (length rows)))
      (insert (format "- failures: %d\n\n" failures))
      (insert "* Checks\n\n")
      (insert "| Check | Source | Status | Expected role | Expected target |\n")
      (insert "|-------+--------+--------+---------------+-----------------|\n")
      (dolist (row rows)
        (insert (format "| =%s= | =%s= | %s | =%s= | =%s= |\n"
                        (nth 0 row)
                        (nth 1 row)
                        (nth 2 row)
                        (nth 3 row)
                        (nth 4 row))))
      (insert "\n* Notes\n\n")
      (insert "- APP bootstrap policy may be staged by the app scaffold.\n")
      (insert "- APP bootstrap policy must not enter reusable package scaffolds.\n")
      (insert "- Reusable dump/loader helpers remain package-owned lazy IO features.\n"))))

;;;###autoload
(defun nemacs-library-app-boundary-batch ()
  "Verify APP bootstrap/package scaffold separation."
  (let* ((rows (nemacs-library-app-boundary--check-rows))
         (failures (cl-count "fail" rows :key (lambda (row) (nth 2 row))
                             :test #'equal)))
    (nemacs-library-app-boundary--write-tsv rows)
    (nemacs-library-app-boundary--write-summary rows)
    (princ
     (format "nemacs-library-app-boundary: checks=%d failures=%d output=%s summary=%s\n"
             (length rows)
             failures
             nemacs-library-app-boundary-output
             nemacs-library-app-boundary-summary-output))
    (when (/= failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-app-boundary)

;;; nemacs-library-app-boundary.el ends here
