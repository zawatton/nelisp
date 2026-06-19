;;; nemacs-stub-fallback-skip-inventory.el --- inventory stub/fallback/skip sites -*- lexical-binding: t; -*-

;;; Commentary:

;; Static source inventory for completion-gate review.  This intentionally
;; scans repository-owned source, tests, scripts, and design docs, while
;; excluding generated output, vendor code, and historical worklogs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-stub-fallback-skip-inventory-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-stub-fallback-skip-inventory-output
  (expand-file-name "build/nemacs-stub-fallback-skip-inventory.tsv"
                    nemacs-stub-fallback-skip-inventory-repo-root)
  "TSV output path.")

(defvar nemacs-stub-fallback-skip-inventory-summary-output
  (expand-file-name "build/nemacs-stub-fallback-skip-summary.org"
                    nemacs-stub-fallback-skip-inventory-repo-root)
  "Org summary output path.")

(defconst nemacs-stub-fallback-skip-inventory--roots
  '("Makefile" "README.org" "docs" "scripts" "src" "test")
  "Repo-relative files/directories scanned by the inventory.")

(defconst nemacs-stub-fallback-skip-inventory--extensions
  '("el" "sh" "org" "md")
  "File extensions included in recursive directory scans.")

(defun nemacs-stub-fallback-skip-inventory--relative (path)
  "Return PATH relative to the repository root."
  (file-relative-name path nemacs-stub-fallback-skip-inventory-repo-root))

(defun nemacs-stub-fallback-skip-inventory--excluded-p (relative)
  "Return non-nil when RELATIVE should not be scanned."
  (or (string-suffix-p "~" relative)
      (string-match-p
       (rx string-start
           (or "build/" "target/" "vendor/" "tmp-diag/"
               "docs/worklog/"))
       relative)
      (string-match-p (rx "/." (or "git" "cache") "/") relative)
      (string-match-p (rx ".elc" string-end) relative)))

(defun nemacs-stub-fallback-skip-inventory--interesting-file-p (path)
  "Return non-nil when PATH is an inventory source file."
  (let ((relative (nemacs-stub-fallback-skip-inventory--relative path)))
    (and (file-regular-p path)
         (not (nemacs-stub-fallback-skip-inventory--excluded-p relative))
         (or (member (file-name-nondirectory path) '("Makefile" "README"))
             (member (file-name-extension path)
                     nemacs-stub-fallback-skip-inventory--extensions)))))

(defun nemacs-stub-fallback-skip-inventory--root-files (root)
  "Return source files under repo-relative ROOT."
  (let ((path (expand-file-name root nemacs-stub-fallback-skip-inventory-repo-root)))
    (cond
     ((and (file-regular-p path)
           (nemacs-stub-fallback-skip-inventory--interesting-file-p path))
      (list path))
     ((file-directory-p path)
      (cl-remove-if-not
       #'nemacs-stub-fallback-skip-inventory--interesting-file-p
       (directory-files-recursively path ".*" nil)))
     (t nil))))

(defun nemacs-stub-fallback-skip-inventory--files ()
  "Return all source files scanned by the inventory."
  (sort
   (delete-dups
    (apply #'append
           (mapcar #'nemacs-stub-fallback-skip-inventory--root-files
                   nemacs-stub-fallback-skip-inventory--roots)))
   #'string<))

(defun nemacs-stub-fallback-skip-inventory--family (line)
  "Return inventory family for LINE, or nil when it is uninteresting."
  (cond
   ((string-match-p (rx word-start (or "stub" "stubs" "stubbed") word-end) line)
    "stub")
   ((string-match-p (rx word-start (or "fallback" "fallbacks") word-end) line)
    "fallback")
   ((string-match-p
     (rx (or "ert-skip" "skip-unless" "SKIP" "skipped"
             (seq word-start (or "skip" "skips") word-end)))
     line)
    "skip")
   (t nil)))

(defun nemacs-stub-fallback-skip-inventory--scope (relative)
  "Return broad ownership scope for RELATIVE path."
  (cond
   ((string-match-p (rx string-start "test/") relative) "test")
   ((string-match-p (rx string-start "docs/") relative) "documentation")
   ((string-match-p (rx string-start "scripts/verify-nemacs-tui.sh") relative)
    "daily-driver")
   ((string-match-p (rx string-start "scripts/") relative) "tooling")
   ((string-match-p (rx string-start "src/nemacs-main.el") relative) "tui-entry")
   ((string-match-p (rx string-start "src/nemacs-gui-file-bridge-runtime.el") relative)
    "gui-bridge-runtime")
   ((string-match-p (rx string-start "src/") relative) "runtime")
   ((equal relative "Makefile") "gate")
   (t "repo")))

(defun nemacs-stub-fallback-skip-inventory--owner (relative line)
  "Return likely owner module for RELATIVE and LINE."
  (cond
   ((string-match-p "dired" (concat relative " " line)) "dired")
   ((string-match-p "\\bhelp\\|describe" (concat relative " " line)) "help")
   ((string-match-p "\\binfo\\b\\|Info" (concat relative " " line)) "info")
   ((string-match-p "shell\\|process\\|compile\\|grep" (concat relative " " line))
    "process-shell")
   ((string-match-p "minibuffer\\|read-string\\|completing-read"
                    (concat relative " " line))
    "minibuffer")
   ((string-match-p "command\\|keymap\\|keyboard\\|interactive"
                    (concat relative " " line))
    "command-loop")
   ((string-match-p "file\\|buffer\\|save\\|write\\|directory"
                    (concat relative " " line))
    "file-buffer")
   ((string-match-p "window\\|frame\\|tab" (concat relative " " line))
    "window-frame")
   ((string-match-p "package\\|pkg\\|custom" (concat relative " " line))
    "package-custom")
   (t "general")))

(defun nemacs-stub-fallback-skip-inventory--disposition
    (family relative line)
  "Return review disposition for FAMILY at RELATIVE LINE."
  (let ((scope (nemacs-stub-fallback-skip-inventory--scope relative)))
    (cond
     ((equal scope "documentation") "documented-boundary")
     ((and (equal family "skip")
           (string-match-p "ert-skip\\|skip-unless\\|SKIP" line))
      "conditional-test-skip")
     ((and (equal family "skip") (equal scope "daily-driver"))
      "daily-driver-observability")
     ((and (equal family "fallback") (string-match-p "host_fallback" line))
      "host-fallback-sentinel")
     ((and (equal family "fallback")
           (string-match-p "runtime-process-preload" relative))
      "runtime-image-preload")
     ((and (equal family "fallback") (equal scope "gui-bridge-runtime"))
      "bridge-local-fallback")
     ((and (equal family "fallback") (equal scope "runtime"))
      "runtime-compatibility")
     ((and (equal family "stub") (equal scope "test"))
      "test-fixture")
     ((and (equal family "stub")
           (string-match-p "bootstrap\\|preload\\|standalone" line))
      "bootstrap-compatibility")
     ((and (equal family "stub") (member scope '("runtime" "tooling")))
      "implementation-debt")
     ((equal scope "test") "test-fixture")
     ((equal scope "tooling") "tooling-compatibility")
     (t "review-classified"))))

(defun nemacs-stub-fallback-skip-inventory--clean-line (line)
  "Return a single-line TSV-safe copy of LINE."
  (replace-regexp-in-string
   "[\t\r\n]+" " "
   (string-trim (substring-no-properties line))))

(defun nemacs-stub-fallback-skip-inventory--scan-file (path)
  "Return inventory rows for PATH."
  (let ((relative (nemacs-stub-fallback-skip-inventory--relative path))
        rows)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((line-number 1))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (family
                  (nemacs-stub-fallback-skip-inventory--family line)))
            (when family
              (push
               (list family
                     (nemacs-stub-fallback-skip-inventory--scope relative)
                     (nemacs-stub-fallback-skip-inventory--owner relative line)
                     (nemacs-stub-fallback-skip-inventory--disposition
                      family relative line)
                     relative
                     (number-to-string line-number)
                     (nemacs-stub-fallback-skip-inventory--clean-line line))
               rows)))
          (forward-line 1)
          (setq line-number (1+ line-number)))))
    (nreverse rows)))

(defun nemacs-stub-fallback-skip-inventory--rows ()
  "Return all inventory rows."
  (apply #'append
         (mapcar #'nemacs-stub-fallback-skip-inventory--scan-file
                 (nemacs-stub-fallback-skip-inventory--files))))

(defun nemacs-stub-fallback-skip-inventory--inc (table keys)
  "Increment TABLE counter for KEYS."
  (puthash keys (1+ (gethash keys table 0)) table))

(defun nemacs-stub-fallback-skip-inventory--sorted-counts (table)
  "Return sorted count alist from TABLE."
  (let (items)
    (maphash (lambda (key value)
               (push (cons key value) items))
             table)
    (sort items
          (lambda (a b)
            (string< (format "%S" (car a)) (format "%S" (car b)))))))

(defun nemacs-stub-fallback-skip-inventory--write-summary (rows output)
  "Write Org summary for ROWS to OUTPUT."
  (let ((by-family (make-hash-table :test 'equal))
        (by-scope (make-hash-table :test 'equal))
        (by-disposition (make-hash-table :test 'equal)))
    (dolist (row rows)
      (nemacs-stub-fallback-skip-inventory--inc by-family (nth 0 row))
      (nemacs-stub-fallback-skip-inventory--inc by-scope (nth 1 row))
      (nemacs-stub-fallback-skip-inventory--inc by-disposition (nth 3 row)))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: Nemacs Stub/Fallback/Skip Inventory Summary\n\n")
      (insert (format "- total rows: %d\n\n" (length rows)))
      (insert "* By family\n\n| family | count |\n|-+-------|\n")
      (dolist (item (nemacs-stub-fallback-skip-inventory--sorted-counts
                     by-family))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* By scope\n\n| scope | count |\n|-+-------|\n")
      (dolist (item (nemacs-stub-fallback-skip-inventory--sorted-counts
                     by-scope))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* By disposition\n\n| disposition | count |\n|-+-------|\n")
      (dolist (item (nemacs-stub-fallback-skip-inventory--sorted-counts
                     by-disposition))
        (insert (format "| %s | %d |\n" (car item) (cdr item)))))))

;;;###autoload
(defun nemacs-stub-fallback-skip-inventory-batch ()
  "Write stub/fallback/skip inventory artifacts."
  (let ((rows (nemacs-stub-fallback-skip-inventory--rows)))
    (make-directory
     (file-name-directory nemacs-stub-fallback-skip-inventory-output)
     t)
    (with-temp-file nemacs-stub-fallback-skip-inventory-output
      (insert "family\tscope\towner\tdisposition\tfile\tline\ttext\n")
      (dolist (row rows)
        (insert (mapconcat #'identity row "\t") "\n")))
    (nemacs-stub-fallback-skip-inventory--write-summary
     rows nemacs-stub-fallback-skip-inventory-summary-output)
    (princ
     (format
      "nemacs-stub-fallback-skip-inventory: rows=%d output=%s summary=%s\n"
      (length rows)
      nemacs-stub-fallback-skip-inventory-output
      nemacs-stub-fallback-skip-inventory-summary-output))))

(provide 'nemacs-stub-fallback-skip-inventory)

;;; nemacs-stub-fallback-skip-inventory.el ends here
