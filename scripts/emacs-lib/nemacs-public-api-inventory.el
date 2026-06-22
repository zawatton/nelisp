;;; nemacs-public-api-inventory.el --- inventory reusable public APIs -*- lexical-binding: t; -*-

;;; Commentary:

;; Static inventory for the library-first phase.  It reads Doc 18's package
;; ownership inventory, scans repository Elisp definitions, and writes a
;; package-group view of exported-looking symbols.
;;
;; This is not a stability promise.  It separates prefixed reusable API,
;; Emacs-compatible public shim names, private helpers, and app/GUI-owned
;; adapters so package owners can review the surface before extraction.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-public-api-inventory-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-public-api-inventory-ownership-doc
  (expand-file-name "docs/design/18-library-package-ownership-inventory.org"
                    nemacs-public-api-inventory-repo-root)
  "Doc 18 ownership inventory path.")

(defvar nemacs-public-api-inventory-output
  (expand-file-name "build/nemacs-public-api-inventory.tsv"
                    nemacs-public-api-inventory-repo-root)
  "TSV output path.")

(defvar nemacs-public-api-inventory-summary-output
  (expand-file-name "build/nemacs-public-api-summary.org"
                    nemacs-public-api-inventory-repo-root)
  "Org summary output path.")

(defconst nemacs-public-api-inventory--definition-re
  "^(\\s-*\\(defun\\|defmacro\\|defsubst\\|cl-defun\\|cl-defmacro\\|defvar\\|defconst\\|defcustom\\|define-minor-mode\\|define-derived-mode\\)\\s-+\\([^][() \t\n]+\\)"
  "Regexp matching top-level definitions for inventory.")

(defconst nemacs-public-api-inventory--reusable-groups
  '("FND" "TXT" "BUF" "CORE" "IO" "DSP" "FEAT" "PKG")
  "Ownership groups considered reusable library package groups.")

(defun nemacs-public-api-inventory--relative (path)
  "Return PATH relative to repository root."
  (file-relative-name path nemacs-public-api-inventory-repo-root))

(defun nemacs-public-api-inventory--primary-group (group)
  "Return primary ownership group from GROUP."
  (car (split-string group "/" t)))

(defun nemacs-public-api-inventory--ownership ()
  "Return a hash table mapping repo-relative paths to primary group."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents nemacs-public-api-inventory-ownership-doc)
      (goto-char (point-min))
      (while (re-search-forward "^| =\\([^=]+\\)= | \\([^ |]+\\)" nil t)
        (let* ((item (match-string 1))
               (group (nemacs-public-api-inventory--primary-group
                       (match-string 2)))
               (relative
                (cond
                 ((string-prefix-p "gui/" item) item)
                 ((string-suffix-p ".el" item) (concat "src/" item))
                 (t item))))
          (puthash relative group table))))
    table))

(defun nemacs-public-api-inventory--elisp-files ()
  "Return repository Elisp files relevant to API inventory."
  (sort
   (append
    (directory-files-recursively
     (expand-file-name "src" nemacs-public-api-inventory-repo-root)
     "\\.el\\'")
    (let ((gui (expand-file-name "gui" nemacs-public-api-inventory-repo-root)))
      (and (file-directory-p gui)
           (directory-files-recursively gui "\\.el\\'"))))
   #'string<))

(defun nemacs-public-api-inventory--line-number-at (pos)
  "Return 1-based line number at POS in current buffer."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun nemacs-public-api-inventory--file-group (ownership relative)
  "Return ownership group for RELATIVE using OWNERSHIP."
  (or (gethash relative ownership)
      (and (string-prefix-p "gui/" relative) "GUI")
      "UNOWNED"))

(defun nemacs-public-api-inventory--kind (definer)
  "Return coarse definition kind for DEFINER."
  (cond
   ((member definer '("defvar" "defconst" "defcustom")) "variable")
   ((member definer '("defmacro" "cl-defmacro")) "macro")
   ((member definer '("define-minor-mode" "define-derived-mode")) "mode")
   (t "function")))

(defun nemacs-public-api-inventory--prefixed-symbol-p (symbol)
  "Return non-nil when SYMBOL has a project/library prefix."
  (string-match-p
   (rx string-start
       (or "emacs-" "nelisp-" "nemacs-runtime-" "files-runtime-"
           "generator-" "map-" "seq-" "subr-" "range-" "regi-"
           "json-" "hex-"))
   symbol))

(defun nemacs-public-api-inventory--api-class (group relative symbol)
  "Return public API class for GROUP RELATIVE SYMBOL."
  (ignore relative)
  (cond
   ((string-match-p "--" symbol) "private-helper")
   ((not (member group nemacs-public-api-inventory--reusable-groups))
    "adapter-or-app")
   ((nemacs-public-api-inventory--prefixed-symbol-p symbol)
    "public-prefixed")
   (t "compat-global")))

(defun nemacs-public-api-inventory--surface (api-class)
  "Return high-level surface bucket for API-CLASS."
  (cond
   ((equal api-class "public-prefixed") "library")
   ((equal api-class "compat-global") "compat")
   ((equal api-class "adapter-or-app") "adapter")
   (t "private")))

(defun nemacs-public-api-inventory--scan-file (ownership file)
  "Return inventory rows for FILE using OWNERSHIP."
  (let* ((relative (nemacs-public-api-inventory--relative file))
         (group (nemacs-public-api-inventory--file-group ownership relative))
         rows)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              nemacs-public-api-inventory--definition-re nil t)
        (let* ((definer (match-string 1))
               (symbol (match-string 2))
               (line (nemacs-public-api-inventory--line-number-at
                      (match-beginning 2)))
               (api-class
                (nemacs-public-api-inventory--api-class
                 group relative symbol))
               (surface
                (nemacs-public-api-inventory--surface api-class)))
          (push (list group surface api-class
                      (nemacs-public-api-inventory--kind definer)
                      relative line symbol)
                rows))))
    (nreverse rows)))

(defun nemacs-public-api-inventory--rows ()
  "Return all public API inventory rows."
  (let ((ownership (nemacs-public-api-inventory--ownership))
        rows)
    (dolist (file (nemacs-public-api-inventory--elisp-files))
      (setq rows
            (append rows
                    (nemacs-public-api-inventory--scan-file
                     ownership file))))
    rows))

(defun nemacs-public-api-inventory--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-public-api-inventory--sorted-counts (table)
  "Return TABLE counts sorted by key."
  (let (items)
    (maphash (lambda (k v) (push (cons k v) items)) table)
    (sort items (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-public-api-inventory--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-public-api-inventory--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-public-api-inventory--tsv-cell cells "\t"))

(defun nemacs-public-api-inventory--write-summary (rows output)
  "Write ROWS summary to OUTPUT."
  (let ((by-group-surface (make-hash-table :test 'equal))
        (by-class (make-hash-table :test 'equal))
        (compat 0))
    (dolist (row rows)
      (nemacs-public-api-inventory--inc
       by-group-surface (format "%s/%s" (nth 0 row) (nth 1 row)))
      (nemacs-public-api-inventory--inc by-class (nth 2 row))
      (when (equal (nth 2 row) "compat-global")
        (setq compat (1+ compat))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs public API inventory summary\n\n")
      (insert "* Counts by group/surface\n\n")
      (insert "| Group/Surface | Count |\n|---------------+-------|\n")
      (dolist (item (nemacs-public-api-inventory--sorted-counts
                     by-group-surface))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Counts by class\n\n")
      (insert "| Class | Count |\n|-------+-------|\n")
      (dolist (item (nemacs-public-api-inventory--sorted-counts by-class))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Notes\n\n")
      (insert "- This inventory is advisory; it does not promise API stability.\n")
      (insert "- `public-prefixed' is the preferred reusable library surface.\n")
      (insert "- `compat-global' is intentional Emacs-compatible global API.\n")
      (insert "- App/GUI rows are adapter surfaces, not reusable package API.\n")
      (insert (format "- Current compat-global count: %d.\n" compat)))))

;;;###autoload
(defun nemacs-public-api-inventory-batch ()
  "Write public API inventory artifacts."
  (let ((rows (nemacs-public-api-inventory--rows)))
    (make-directory (file-name-directory nemacs-public-api-inventory-output) t)
    (with-temp-file nemacs-public-api-inventory-output
      (insert
       (nemacs-public-api-inventory--row
        "group" "surface" "class" "kind" "file" "line" "symbol")
       "\n")
      (dolist (row rows)
        (insert (apply #'nemacs-public-api-inventory--row row) "\n")))
    (nemacs-public-api-inventory--write-summary
     rows nemacs-public-api-inventory-summary-output)
    (princ
     (format
      "nemacs-public-api-inventory: symbols=%d output=%s summary=%s\n"
      (length rows)
      nemacs-public-api-inventory-output
      nemacs-public-api-inventory-summary-output))))

(provide 'nemacs-public-api-inventory)

;;; nemacs-public-api-inventory.el ends here
