;;; nemacs-library-api-promotion-queue.el --- package API promotion queue -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a package-oriented API promotion queue from the package-scoped
;; API inventory.  This does not make symbols stable.  It highlights which
;; public-looking package APIs already have test/doc evidence and which ones
;; still need package-owner review before external consumers should rely on
;; them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-emacs)
(require 'nemacs-library-package-api)

(defvar nemacs-library-api-promotion-queue-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-api-promotion-queue-output
  (expand-file-name "build/nemacs-library-api-promotion-queue.tsv"
                    nemacs-library-api-promotion-queue-repo-root)
  "TSV output path.")

(defvar nemacs-library-api-promotion-queue-summary-output
  (expand-file-name "build/nemacs-library-api-promotion-queue.org"
                    nemacs-library-api-promotion-queue-repo-root)
  "Org summary output path.")

(defconst nemacs-library-api-promotion-queue--api-classes
  '("public-prefixed" "compat-global")
  "Package API classes that need consumer-surface promotion review.")

(defconst nemacs-library-api-promotion-queue--symbol-char-re
  "[:alnum:]_!$%&*+./:<=>?@^|~-"
  "Character class fragment treated as symbol-name characters.")

(defun nemacs-library-api-promotion-queue--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-api-promotion-queue--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-api-promotion-queue--tsv-cell cells "\t"))

(defun nemacs-library-api-promotion-queue--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-api-promotion-queue--sort-strings (values)
  "Return sorted unique string VALUES."
  (sort (delete-dups
         (mapcar #'nemacs-library-api-promotion-queue--symbol-name
                 (copy-sequence values)))
        #'string<))

(defun nemacs-library-api-promotion-queue--join (values)
  "Return VALUES as a stable comma-separated string."
  (mapconcat #'identity
             (nemacs-library-api-promotion-queue--sort-strings values)
             ","))

(defun nemacs-library-api-promotion-queue--relative (path)
  "Return PATH relative to repository root."
  (file-relative-name path nemacs-library-api-promotion-queue-repo-root))

(defun nemacs-library-api-promotion-queue--files (directory regexp)
  "Return files under DIRECTORY matching REGEXP."
  (let ((root (expand-file-name directory
                                nemacs-library-api-promotion-queue-repo-root)))
    (and (file-directory-p root)
         (directory-files-recursively root regexp))))

(defun nemacs-library-api-promotion-queue--evidence-files ()
  "Return evidence file groups for API promotion review."
  `(("test" . ,(nemacs-library-api-promotion-queue--files "test" "\\.el\\'"))
    ("doc" . ,(sort
               (append
                (list (expand-file-name
                       "README.org"
                       nemacs-library-api-promotion-queue-repo-root)
                      (expand-file-name
                       "AGENTS.md"
                       nemacs-library-api-promotion-queue-repo-root)
                      (expand-file-name
                       "CLAUDE.md"
                       nemacs-library-api-promotion-queue-repo-root))
                (nemacs-library-api-promotion-queue--files
                 "docs/design" "\\.org\\'"))
               #'string<))))

(defun nemacs-library-api-promotion-queue--read-evidence-files (files)
  "Return readable FILES as (RELATIVE . CONTENT) entries."
  (let (entries)
    (dolist (file files)
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (push (cons (nemacs-library-api-promotion-queue--relative file)
                      (buffer-string))
                entries))))
    (sort entries (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-api-promotion-queue--symbol-regexp (symbol)
  "Return a regexp that matches SYMBOL as a whole Elisp-like name."
  (concat "\\(?:\\`\\|[^"
          nemacs-library-api-promotion-queue--symbol-char-re
          "]\\)"
          (regexp-quote symbol)
          "\\(?:\\'\\|[^"
          nemacs-library-api-promotion-queue--symbol-char-re
          "]\\)"))

(defun nemacs-library-api-promotion-queue--symbol-index (entries)
  "Return hash table mapping symbols to relative files from ENTRIES."
  (let ((table (make-hash-table :test 'equal))
        (token-re
         (concat "[" nemacs-library-api-promotion-queue--symbol-char-re
                 "]+")))
    (dolist (entry entries)
      (with-temp-buffer
        (insert (cdr entry))
        (goto-char (point-min))
        (while (re-search-forward token-re nil t)
          (let ((token (match-string 0)))
            (puthash token
                     (cons (car entry) (gethash token table))
                     table)))))
    table))

(defun nemacs-library-api-promotion-queue--symbol-refs (index symbol)
  "Return relative files from INDEX that mention SYMBOL."
  (nemacs-library-api-promotion-queue--sort-strings
   (gethash symbol index)))

(defun nemacs-library-api-promotion-queue--stable-symbols ()
  "Return stable package API symbols from the facade manifest."
  (append
   (and (fboundp 'nelisp-emacs-library-stable-api-symbols)
        (nelisp-emacs-library-stable-api-symbols))
   (and (fboundp 'nelisp-emacs-library-stable-lazy-api-symbols)
        (nelisp-emacs-library-stable-lazy-api-symbols))))

(defun nemacs-library-api-promotion-queue--shim-helper-p (class file)
  "Return non-nil if CLASS/FILE is a prefixed compatibility shim helper.
Symbols defined in `emacs-*-builtins.el' can be prefixed implementation
helpers for installing or backing unprefixed Emacs-compatible names.  They
are reviewable shim surface, but not preferred stable library APIs."
  (and (equal class "public-prefixed")
       (stringp file)
       (string-match-p "\\`src/emacs-.*-builtins\\.el\\'" file)))

(defconst nemacs-library-api-promotion-queue--internal-shim-helpers
  '("emacs-buffer-builtins-buffer-modified-tick"
    "emacs-line-eol-pos"
    "emacs-edit-canonicalize-fill-text"
    "emacs-edit-empty-line-at-p"
    "emacs-edit-line-bounds-at"
    "emacs-edit-line-commented-p"
    "emacs-edit-line-start-position-for-number"
    "emacs-edit-mark-paragraph-bounds"
    "emacs-edit-next-line-bol"
    "emacs-edit-overwrite-mode-active-p"
    "emacs-edit-previous-line-bol"
    "emacs-edit-register-point-value-p"
    "emacs-edit-register-put"
    "emacs-edit-sexp-skip-backward-ws"
    "emacs-edit-wrap-fill-text"
    "emacs-fileio-buffer-file-direct"
    "emacs-fileio-buffer-name-for-file"
    "emacs-fileio-file-exists-direct-p"
    "emacs-fileio-make-temp-name"
    "emacs-fileio-read-file-text-direct"
    "emacs-fileio-record-buffer-file"
    "emacs-fileio-visit-file-direct")
  "Reviewed prefixed shim helpers that remain internal implementation API.")

(defun nemacs-library-api-promotion-queue--internal-shim-helper-p (symbol)
  "Return non-nil when SYMBOL is a reviewed internal shim helper."
  (member symbol nemacs-library-api-promotion-queue--internal-shim-helpers))

(defconst nemacs-library-api-promotion-queue--internal-validation-helpers
  '("nelisp-ec-check-live")
  "Reviewed public-prefixed validation helpers that remain internal API.")

(defun nemacs-library-api-promotion-queue--internal-validation-helper-p (symbol)
  "Return non-nil when SYMBOL is a reviewed internal validation helper."
  (member symbol nemacs-library-api-promotion-queue--internal-validation-helpers))

(defun nemacs-library-api-promotion-queue--status
    (class file symbol test-refs doc-refs stable-symbols)
  "Return promotion status for CLASS FILE SYMBOL.
TEST-REFS and DOC-REFS are evidence references.  STABLE-SYMBOLS is the
facade stable package API manifest symbol list."
  (cond
   ((memq (intern symbol) stable-symbols) "stable-contract")
   ((equal class "compat-global")
    (if (or test-refs doc-refs) "compat-shim-covered" "compat-shim-review"))
   ((nemacs-library-api-promotion-queue--shim-helper-p class file)
    (cond
     ((nemacs-library-api-promotion-queue--internal-shim-helper-p symbol)
      "compat-shim-helper-internal")
     ((or test-refs doc-refs)
      "compat-shim-helper-covered")
     (t "compat-shim-helper-review")))
   ((string-suffix-p "-check-live" symbol)
    (cond
     ((nemacs-library-api-promotion-queue--internal-validation-helper-p symbol)
      "validation-helper-internal")
     ((or test-refs doc-refs)
      "validation-helper-covered")
     (t "validation-helper-review")))
   ((string-suffix-p "-features" symbol) "loader-manifest")
   ((and test-refs doc-refs) "promote-ready")
   (test-refs "needs-doc")
   (doc-refs "needs-test")
   (t "needs-review")))

(defun nemacs-library-api-promotion-queue--reason
    (class status)
  "Return human review reason for CLASS and STATUS."
  (cond
   ((equal status "loader-manifest")
    "package membership manifest; stable through loader contract")
   ((equal status "stable-contract")
    "listed in facade stable or stable lazy package API manifest")
   ((equal status "promote-ready")
    "has test and doc references")
   ((equal status "needs-doc")
    "has test reference but no consumer-facing doc reference")
   ((equal status "needs-test")
    "has doc reference but no test reference")
   ((equal status "compat-shim-covered")
    "compat-global shim has test or doc evidence")
   ((equal status "compat-shim-review")
    "compat-global shim needs explicit compatibility review")
   ((equal status "compat-shim-helper-covered")
    "prefixed builtins shim helper has test or doc evidence")
   ((equal status "compat-shim-helper-internal")
    "reviewed prefixed builtins shim helper remains internal implementation API")
   ((equal status "compat-shim-helper-review")
    "prefixed builtins shim helper needs explicit compatibility review")
   ((equal status "validation-helper-covered")
    "public-prefixed validation helper has test or doc evidence")
   ((equal status "validation-helper-internal")
    "reviewed public-prefixed validation helper remains internal implementation API")
   ((equal status "validation-helper-review")
    "public-prefixed validation helper needs explicit internal API review")
   ((equal class "public-prefixed")
    "public-prefixed candidate lacks test/doc evidence")
   (t "not a promotion candidate")))

(defun nemacs-library-api-promotion-queue--candidate-row-p (row)
  "Return non-nil if package API ROW belongs in the promotion queue."
  (member (nth 7 row) nemacs-library-api-promotion-queue--api-classes))

(defun nemacs-library-api-promotion-queue--rows ()
  "Return API promotion queue rows."
  (let* ((evidence (nemacs-library-api-promotion-queue--evidence-files))
         (test-entries
          (nemacs-library-api-promotion-queue--read-evidence-files
           (cdr (assoc "test" evidence))))
         (doc-entries
          (nemacs-library-api-promotion-queue--read-evidence-files
           (cdr (assoc "doc" evidence))))
         (test-index
          (nemacs-library-api-promotion-queue--symbol-index test-entries))
         (doc-index
          (nemacs-library-api-promotion-queue--symbol-index doc-entries))
         (stable-symbols
          (nemacs-library-api-promotion-queue--stable-symbols))
         rows)
    (dolist (api-row (nemacs-library-package-api--rows))
      (when (nemacs-library-api-promotion-queue--candidate-row-p api-row)
        (let* ((class (nth 7 api-row))
               (symbol (nth 11 api-row))
               (test-refs
                (nemacs-library-api-promotion-queue--symbol-refs
                 test-index symbol))
               (doc-refs
                (nemacs-library-api-promotion-queue--symbol-refs
                 doc-index symbol))
               (status
                (nemacs-library-api-promotion-queue--status
                 class (nth 9 api-row) symbol test-refs doc-refs
                 stable-symbols)))
          (push (list (nth 0 api-row)
                      (nth 1 api-row)
                      (nth 2 api-row)
                      (nth 3 api-row)
                      (nth 4 api-row)
                      class
                      (nth 8 api-row)
                      (nth 9 api-row)
                      (nth 10 api-row)
                      symbol
                      status
                      test-refs
                      doc-refs
                      (nemacs-library-api-promotion-queue--reason
                       class status))
                rows))))
    (sort (nreverse rows)
          (lambda (a b)
            (string<
             (mapconcat
              #'nemacs-library-api-promotion-queue--symbol-name a "\t")
             (mapconcat
              #'nemacs-library-api-promotion-queue--symbol-name b "\t"))))))

(defun nemacs-library-api-promotion-queue--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-library-api-promotion-queue--sorted-counts (table)
  "Return TABLE counts sorted by key."
  (let (items)
    (maphash (lambda (key value) (push (cons key value) items)) table)
    (sort items (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-api-promotion-queue--write-tsv (rows output)
  "Write promotion queue ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-api-promotion-queue--row
      "package_id" "facade_name" "owner" "loader_feature" "role"
      "class" "kind" "file" "line" "symbol" "promotion_status"
      "test_refs" "doc_refs" "reason")
     "\n")
    (dolist (row rows)
      (insert
       (nemacs-library-api-promotion-queue--row
        (nth 0 row)
        (nth 1 row)
        (nth 2 row)
        (nth 3 row)
        (nth 4 row)
        (nth 5 row)
        (nth 6 row)
        (nth 7 row)
        (nth 8 row)
        (nth 9 row)
        (nth 10 row)
        (nemacs-library-api-promotion-queue--join (nth 11 row))
        (nemacs-library-api-promotion-queue--join (nth 12 row))
        (nth 13 row))
       "\n"))))

(defun nemacs-library-api-promotion-queue--write-summary (rows output)
  "Write promotion queue ROWS summary to Org OUTPUT."
  (let ((by-status (make-hash-table :test 'equal))
        (by-package-status (make-hash-table :test 'equal)))
    (dolist (row rows)
      (nemacs-library-api-promotion-queue--inc by-status (nth 10 row))
      (nemacs-library-api-promotion-queue--inc
       by-package-status (format "%s/%s" (nth 0 row) (nth 10 row))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library API promotion queue\n\n")
      (insert (format "* Summary\n\n- rows: %d\n\n" (length rows)))
      (insert "* Counts by status\n\n")
      (insert "| Status | Count |\n|--------+-------|\n")
      (dolist (item (nemacs-library-api-promotion-queue--sorted-counts
                     by-status))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Counts by package/status\n\n")
      (insert "| Package/Status | Count |\n|----------------+-------|\n")
      (dolist (item (nemacs-library-api-promotion-queue--sorted-counts
                     by-package-status))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Review queue\n\n")
      (insert "| Package | Status | Symbol | Reason |\n")
      (insert "|---------+--------+--------+--------|\n")
      (dolist (row rows)
        (when (member (nth 10 row)
                      '("needs-review" "needs-doc" "needs-test"
                        "compat-shim-review"
                        "compat-shim-helper-review"
                        "validation-helper-review"))
          (insert (format "| %s | %s | =%s= | %s |\n"
                          (nth 0 row)
                          (nth 10 row)
                          (nth 9 row)
                          (nth 13 row)))))
      (insert "\n* Notes\n\n")
      (insert "- This queue is generated from package-scoped API rows and evidence references in =test/=, =README.org=, =AGENTS.md=, =CLAUDE.md=, and =docs/design/=.\n")
      (insert "- =promote-ready= means the candidate has both test and doc references; package owners still decide stability.\n")
      (insert "- =stable-contract= means the symbol is listed in the facade stable package API manifest or stable lazy package API manifest.\n")
      (insert "- =needs-doc= and =needs-test= identify the next concrete work before relying on a symbol externally.\n")
      (insert "- =needs-review= has no test/doc evidence yet and should not be treated as a consumer contract.\n")
      (insert "- =compat-shim-*= rows are Emacs-compatible shim names, not preferred new integration APIs.\n")
      (insert "- =compat-shim-helper-internal= rows are reviewed prefixed helpers inside =emacs-*-builtins.el= shim modules that remain internal implementation API.\n")
      (insert "- Other =compat-shim-helper-*= rows are prefixed helpers inside =emacs-*-builtins.el= shim modules; they support compatibility installs but are not preferred stable library APIs.\n")
      (insert "- =validation-helper-internal= rows are reviewed public-prefixed guard helpers that remain internal implementation API.\n")
      (insert "- Other =validation-helper-*= rows are public-prefixed guard helpers; keep them internal unless a consumer use case justifies facade promotion.\n"))))

;;;###autoload
(defun nemacs-library-api-promotion-queue-batch ()
  "Write package API promotion queue artifacts."
  (let ((rows (nemacs-library-api-promotion-queue--rows)))
    (unless rows
      (error "empty nelisp-emacs API promotion queue"))
    (nemacs-library-api-promotion-queue--write-tsv
     rows nemacs-library-api-promotion-queue-output)
    (nemacs-library-api-promotion-queue--write-summary
     rows nemacs-library-api-promotion-queue-summary-output)
    (princ
     (format
      "nemacs-library-api-promotion-queue: rows=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-api-promotion-queue-output
      nemacs-library-api-promotion-queue-summary-output))))

(provide 'nemacs-library-api-promotion-queue)

;;; nemacs-library-api-promotion-queue.el ends here
