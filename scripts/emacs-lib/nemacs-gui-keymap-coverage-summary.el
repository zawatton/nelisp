;;; nemacs-gui-keymap-coverage-summary.el --- summarize GUI keymap coverage TSV -*- lexical-binding: t; -*-

;;; Commentary:

;; Post-processes `scripts/nemacs-gui-keymap-coverage.el' output into stable
;; build artifacts used by docs/design/08-command-coverage-plan.org.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-gui-keymap-coverage-summary-input
  (expand-file-name
   "../build/nemacs-gui-keymap-coverage.tsv"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Raw coverage TSV input path.")

(defvar nemacs-gui-keymap-coverage-summary-output
  (expand-file-name
   "../build/nemacs-gui-keymap-coverage-summary.org"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Org summary output path.")

(defvar nemacs-gui-keymap-coverage-missing-output
  (expand-file-name
   "../build/nemacs-gui-keymap-coverage-missing.tsv"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Filtered TSV output for missing key bindings.")

(defvar nemacs-gui-keymap-coverage-command-missing-output
  (expand-file-name
   "../build/nemacs-gui-keymap-coverage-command-missing.tsv"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Filtered TSV output for missing runtime commands.")

(defvar nemacs-gui-keymap-coverage-different-output
  (expand-file-name
   "../build/nemacs-gui-keymap-coverage-different.tsv"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Filtered TSV output for different command bindings.")

(defconst nemacs-gui-keymap-coverage-summary--families
  '("basic-edit" "file-buffer" "help-info" "dired" "shell-process"
    "project-vc" "package-custom" "window-tab-frame" "other"))

(defconst nemacs-gui-keymap-coverage-summary--statuses
  '("implemented" "different" "command-missing" "missing" "runtime-only"))

(defun nemacs-gui-keymap-coverage-summary--slurp (file)
  "Return FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nemacs-gui-keymap-coverage-summary--command (row)
  "Return the best command field for ROW."
  (or (plist-get row :emacs-command)
      (plist-get row :nemacs-command)
      ""))

(defun nemacs-gui-keymap-coverage-summary--matches-any-p (regexp-list value)
  "Return non-nil when VALUE matches any regexp in REGEXP-LIST."
  (cl-some (lambda (regexp) (string-match-p regexp value)) regexp-list))

(defun nemacs-gui-keymap-coverage-summary--family (row)
  "Return command family for coverage ROW."
  (let ((key (or (plist-get row :key) ""))
        (command (nemacs-gui-keymap-coverage-summary--command row)))
    (cond
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("\\`C-h" "\\`<help>" "help\\|describe-\\|apropos\\|Info\\|info")
       (concat key " " command))
      "help-info")
     ((string-match-p "dired" command) "dired")
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("shell\\|process\\|compile\\|grep\\|async\\|term\\|eshell")
       command)
      "shell-process")
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("\\`project-" "\\`vc-" "\\`magit" "diff")
       command)
      "project-vc")
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("package\\|custom\\|customize")
       command)
      "package-custom")
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("window\\|frame\\|tab-\\|split-window\\|delete-other-windows"
         "\\`C-x [1-9o0]\\'" "\\`C-x 5" "\\`C-x t")
       (concat key " " command))
      "window-tab-frame")
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("file\\|buffer\\|save\\|write\\|revert\\|find-file\\|switch-to-buffer"
         "\\`C-x C-[fsw]\\'" "\\`C-x b\\'")
       (concat key " " command))
      "file-buffer")
     ((nemacs-gui-keymap-coverage-summary--matches-any-p
       '("forward\\|backward\\|beginning\\|end-of\\|next-line\\|previous-line"
         "delete\\|kill\\|yank\\|undo\\|set-mark\\|mark-\\|newline\\|open-line"
         "indent\\|transpose\\|fill\\|query-replace\\|isearch\\|keyboard-quit")
       command)
      "basic-edit")
     (t "other"))))

(defun nemacs-gui-keymap-coverage-summary--parse-row (line)
  "Parse one TSV LINE into a plist."
  (let ((fields (split-string line "\t")))
    (list :status (nth 0 fields)
          :key (nth 1 fields)
          :emacs-command (unless (string-empty-p (or (nth 2 fields) ""))
                           (nth 2 fields))
          :nemacs-command (unless (string-empty-p (or (nth 3 fields) ""))
                            (nth 3 fields))
          :nemacs-command-implemented (nth 4 fields)
          :line line)))

(defun nemacs-gui-keymap-coverage-summary--rows (file)
  "Return parsed coverage rows from FILE."
  (let* ((lines (split-string
                 (nemacs-gui-keymap-coverage-summary--slurp file) "\n" t))
         (body (cdr lines)))
    (mapcar #'nemacs-gui-keymap-coverage-summary--parse-row body)))

(defun nemacs-gui-keymap-coverage-summary--write-filtered
    (header rows status output)
  "Write HEADER and ROWS with STATUS to OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert header "\n")
    (dolist (row rows)
      (when (equal (plist-get row :status) status)
        (insert (plist-get row :line) "\n")))))

(defun nemacs-gui-keymap-coverage-summary--inc (table family status)
  "Increment TABLE counter for FAMILY and STATUS."
  (let ((key (cons family status)))
    (puthash key (1+ (gethash key table 0)) table)))

(defun nemacs-gui-keymap-coverage-summary--write-summary (rows output)
  "Write Org summary for ROWS to OUTPUT."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (row rows)
      (nemacs-gui-keymap-coverage-summary--inc
       counts
       (nemacs-gui-keymap-coverage-summary--family row)
       (plist-get row :status)))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: Nemacs GUI Keymap Coverage Summary\n\n")
      (insert "| family | implemented | different | command-missing | missing | runtime-only | total |\n")
      (insert "|-+-------------+-----------+-----------------+---------+--------------+-------|\n")
      (dolist (family nemacs-gui-keymap-coverage-summary--families)
        (let ((total 0))
          (dolist (status nemacs-gui-keymap-coverage-summary--statuses)
            (setq total (+ total (gethash (cons family status) counts 0))))
          (insert
           (format "| %s | %d | %d | %d | %d | %d | %d |\n"
                   family
                   (gethash (cons family "implemented") counts 0)
                   (gethash (cons family "different") counts 0)
                   (gethash (cons family "command-missing") counts 0)
                   (gethash (cons family "missing") counts 0)
                   (gethash (cons family "runtime-only") counts 0)
                   total)))))))

(defun nemacs-gui-keymap-coverage-summary-batch ()
  "Generate coverage summary artifacts."
  (let* ((text (nemacs-gui-keymap-coverage-summary--slurp
                nemacs-gui-keymap-coverage-summary-input))
         (header (car (split-string text "\n" t)))
         (rows (nemacs-gui-keymap-coverage-summary--rows
                nemacs-gui-keymap-coverage-summary-input)))
    (nemacs-gui-keymap-coverage-summary--write-filtered
     header rows "missing" nemacs-gui-keymap-coverage-missing-output)
    (nemacs-gui-keymap-coverage-summary--write-filtered
     header rows "command-missing"
     nemacs-gui-keymap-coverage-command-missing-output)
    (nemacs-gui-keymap-coverage-summary--write-filtered
     header rows "different" nemacs-gui-keymap-coverage-different-output)
    (nemacs-gui-keymap-coverage-summary--write-summary
     rows nemacs-gui-keymap-coverage-summary-output)
    (princ
     (format
      "nemacs-gui-keymap-coverage-summary: rows=%d summary=%s missing=%s command-missing=%s different=%s\n"
      (length rows)
      nemacs-gui-keymap-coverage-summary-output
      nemacs-gui-keymap-coverage-missing-output
      nemacs-gui-keymap-coverage-command-missing-output
      nemacs-gui-keymap-coverage-different-output))))

(provide 'nemacs-gui-keymap-coverage-summary)

;;; nemacs-gui-keymap-coverage-summary.el ends here
