;;; nemacs-ownership-coverage.el --- inventory Doc 18 file ownership coverage -*- lexical-binding: t; -*-

;;; Commentary:

;; File-level ownership coverage for the library-first phase.  This checks
;; that every repository Elisp file under `src/' and `gui/' is covered by
;; Doc 18, including simple prefix rows such as `gui/spikes/*'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-ownership-coverage-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-ownership-coverage-doc
  (expand-file-name "docs/design/18-library-package-ownership-inventory.org"
                    nemacs-ownership-coverage-repo-root)
  "Doc 18 ownership inventory path.")

(defvar nemacs-ownership-coverage-output
  (expand-file-name "build/nemacs-ownership-coverage.tsv"
                    nemacs-ownership-coverage-repo-root)
  "TSV output path.")

(defvar nemacs-ownership-coverage-summary-output
  (expand-file-name "build/nemacs-ownership-coverage-summary.org"
                    nemacs-ownership-coverage-repo-root)
  "Org summary output path.")

(defun nemacs-ownership-coverage--relative (path)
  "Return PATH relative to repository root."
  (file-relative-name path nemacs-ownership-coverage-repo-root))

(defun nemacs-ownership-coverage--primary-group (group)
  "Return primary ownership group from GROUP."
  (car (split-string group "/" t)))

(defun nemacs-ownership-coverage--entry-relative (item)
  "Return repo-relative path for Doc 18 ITEM, or nil when irrelevant."
  (cond
   ((string-prefix-p "gui/" item) item)
   ((string-prefix-p "src/" item) item)
   ((string-suffix-p ".el" item) (concat "src/" item))
   (t nil)))

(defun nemacs-ownership-coverage--ownership ()
  "Return ownership entries as (RELATIVE GROUP SOURCE)."
  (let (entries)
    (with-temp-buffer
      (insert-file-contents nemacs-ownership-coverage-doc)
      (goto-char (point-min))
      (while (re-search-forward "^| =\\([^=]+\\)= | \\([^ |]+\\)" nil t)
        (let* ((item (match-string 1))
               (relative (nemacs-ownership-coverage--entry-relative item))
               (group (nemacs-ownership-coverage--primary-group
                       (match-string 2))))
          (when relative
            (push (list relative group item) entries)))))
    (nreverse entries)))

(defun nemacs-ownership-coverage--elisp-files ()
  "Return repository Elisp files covered by this inventory."
  (sort
   (append
    (directory-files-recursively
     (expand-file-name "src" nemacs-ownership-coverage-repo-root)
     "\\.el\\'")
    (let ((gui (expand-file-name "gui" nemacs-ownership-coverage-repo-root)))
      (and (file-directory-p gui)
           (directory-files-recursively gui "\\.el\\'"))))
   #'string<))

(defun nemacs-ownership-coverage--wildcard-prefix (relative)
  "Return wildcard prefix for RELATIVE when it is a simple `path/*' row."
  (and (string-suffix-p "/*" relative)
       (substring relative 0 -1)))

(defun nemacs-ownership-coverage--owner-for (relative entries)
  "Return ownership entry for RELATIVE from ENTRIES."
  (or (cl-find relative entries :key #'car :test #'equal)
      (cl-find-if
       (lambda (entry)
         (let ((prefix (nemacs-ownership-coverage--wildcard-prefix
                        (car entry))))
           (and prefix (string-prefix-p prefix relative))))
       entries)))

(defun nemacs-ownership-coverage--file-exists-p (relative)
  "Return non-nil when RELATIVE exists under the repository root."
  (file-exists-p (expand-file-name relative nemacs-ownership-coverage-repo-root)))

(defun nemacs-ownership-coverage--rows ()
  "Return ownership coverage rows."
  (let* ((entries (nemacs-ownership-coverage--ownership))
         (seen (make-hash-table :test 'equal))
         rows)
    (dolist (file (nemacs-ownership-coverage--elisp-files))
      (let* ((relative (nemacs-ownership-coverage--relative file))
             (entry (nemacs-ownership-coverage--owner-for relative entries)))
        (if entry
            (progn
              (puthash (car entry) t seen)
              (push (list "owned" (cadr entry) relative (caddr entry)) rows))
          (push (list "unowned" "" relative "") rows))))
    (dolist (entry entries)
      (let ((relative (car entry)))
        (unless (or (gethash relative seen)
                    (nemacs-ownership-coverage--wildcard-prefix relative)
                    (nemacs-ownership-coverage--file-exists-p relative))
          (push (list "stale" (cadr entry) relative (caddr entry)) rows))))
    (sort (nreverse rows)
          (lambda (a b)
            (let ((status-a (car a))
                  (status-b (car b)))
              (if (equal status-a status-b)
                  (string< (nth 2 a) (nth 2 b))
                (string< status-a status-b)))))))

(defun nemacs-ownership-coverage--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-ownership-coverage--sorted-counts (table)
  "Return TABLE counts sorted by key."
  (let (items)
    (maphash (lambda (k v) (push (cons k v) items)) table)
    (sort items (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-ownership-coverage--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-ownership-coverage--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-ownership-coverage--tsv-cell cells "\t"))

(defun nemacs-ownership-coverage--write-summary (rows output)
  "Write ROWS summary to OUTPUT."
  (let ((by-status (make-hash-table :test 'equal))
        (by-group (make-hash-table :test 'equal)))
    (dolist (row rows)
      (nemacs-ownership-coverage--inc by-status (nth 0 row))
      (when (equal (nth 0 row) "owned")
        (nemacs-ownership-coverage--inc by-group (nth 1 row))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs ownership coverage summary\n\n")
      (insert "* Counts by status\n\n")
      (insert "| Status | Count |\n|--------+-------|\n")
      (dolist (item (nemacs-ownership-coverage--sorted-counts by-status))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Owned files by group\n\n")
      (insert "| Group | Count |\n|-------+-------|\n")
      (dolist (item (nemacs-ownership-coverage--sorted-counts by-group))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Notes\n\n")
      (insert "- `unowned' means an existing src/gui Elisp file is missing from Doc 18.\n")
      (insert "- `stale' means Doc 18 names an exact Elisp file that no longer exists.\n"))))

;;;###autoload
(defun nemacs-ownership-coverage-batch ()
  "Write Doc 18 ownership coverage artifacts."
  (let ((rows (nemacs-ownership-coverage--rows)))
    (make-directory (file-name-directory nemacs-ownership-coverage-output) t)
    (with-temp-file nemacs-ownership-coverage-output
      (insert
       (nemacs-ownership-coverage--row
        "status" "group" "file" "source")
       "\n")
      (dolist (row rows)
        (insert (apply #'nemacs-ownership-coverage--row row) "\n")))
    (nemacs-ownership-coverage--write-summary
     rows nemacs-ownership-coverage-summary-output)
    (princ
     (format
      "nemacs-ownership-coverage: rows=%d output=%s summary=%s\n"
      (length rows)
      nemacs-ownership-coverage-output
      nemacs-ownership-coverage-summary-output))))

(provide 'nemacs-ownership-coverage)

;;; nemacs-ownership-coverage.el ends here
