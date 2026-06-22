;;; nemacs-library-boundary-report.el --- advisory library boundary report -*- lexical-binding: t; -*-

;;; Commentary:

;; Advisory report for the library-first phase.  It reads Doc 18's package
;; ownership inventory, then reports:
;;
;; - cross-owner calls to private `--' symbols;
;; - APP/GUI definitions that look like reusable command semantics, separated
;;   from known adapter/transport glue;
;; - the package groups pulled by `src/emacs-init.el'.
;;
;; This is intentionally non-failing.  It produces review input before any
;; check graduates into a gate.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-library-boundary-report-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-boundary-report-ownership-doc
  (expand-file-name "docs/design/18-library-package-ownership-inventory.org"
                    nemacs-library-boundary-report-repo-root)
  "Doc 18 ownership inventory path.")

(defvar nemacs-library-boundary-report-output
  (expand-file-name "build/nemacs-library-boundary-report.tsv"
                    nemacs-library-boundary-report-repo-root)
  "TSV report output path.")

(defvar nemacs-library-boundary-report-summary-output
  (expand-file-name "build/nemacs-library-boundary-summary.org"
                    nemacs-library-boundary-report-repo-root)
  "Org summary output path.")

(defconst nemacs-library-boundary-report--semantic-name-re
  (rx (or "command" "interactive" "execute" "dispatch"
          "find-file" "save-buffer" "write-file" "kill-buffer"
          "switch-to-buffer" "list-buffers" "dired"
          "shell-command" "query-replace" "describe-key"
          "minibuffer" "keymap" "undo" "yank" "kill-ring"))
  "Names matching likely reusable editor semantics.")

(defconst nemacs-library-boundary-report--runtime-adapter-name-re
  (rx (or "-adapter" "-backend-" (seq "-backend" eos)))
  "Names matching known runtime adapter glue.")

(defconst nemacs-library-boundary-report--main-tui-frontend-symbols
  '("nemacs-main--keymap-slot-vector"
    "nemacs-main--make-full-keymap"
    "nemacs-main--init-keymap"
    "nemacs-main--ensure-keymap-after-feature-load"
    "nemacs-main--install-keymap-host"
    "nemacs-main--uninstall-keymap-host"
    "nemacs-main--direct-tui-command-p"
    "nemacs-main--execute-printable-self-insert"
    "nemacs-main--dispatch-printable-self-insert-direct"
    "nemacs-main--dispatch-key-code"
    "nemacs-main--dispatch-key-event"
    "nemacs-main--tui-dired-list-directory"
    "nemacs-main--tui-help-keymap-source")
  "Nemacs main definitions that belong to the concrete TUI frontend.
This includes event loop definitions and backend callbacks.")

(defconst nemacs-library-boundary-report--main-tui-adapter-symbols
  nil
  "Nemacs main definitions that adapt TUI prompts to shared command APIs.")

(defun nemacs-library-boundary-report--semantic-row-type (relative symbol)
  "Return advisory row type for RELATIVE defining SYMBOL."
  (cond
   ((and (equal relative "src/nemacs-gui-file-bridge-runtime.el")
         (or (string-prefix-p "files--minibuffer-gui-backend" symbol)
             (equal symbol "files--minibuffer-gui-install-backend")))
    "app-gui-file-bridge-runtime-definition")
   ((string-match-p nemacs-library-boundary-report--runtime-adapter-name-re
                    symbol)
    "app-gui-runtime-adapter-definition")
   ((or (string-prefix-p "gui/" relative)
        (string-match-p (rx (or "transport" "render" "draw"
                                (seq (or bos "-" "_") "patch"
                                     (or eos "-" "_"))))
                        symbol))
    "app-gui-transport-definition")
   ((and (equal relative "src/nemacs-main.el")
         (member symbol
                 nemacs-library-boundary-report--main-tui-frontend-symbols))
    "app-tui-frontend-definition")
   ((and (equal relative "src/nemacs-main.el")
         (or (member symbol
                     nemacs-library-boundary-report--main-tui-adapter-symbols)
             (string-suffix-p "-interactive" symbol)))
    "app-tui-command-adapter-definition")
   (t
    "app-gui-semantic-definition")))

(defun nemacs-library-boundary-report--relative (path)
  "Return PATH relative to repository root."
  (file-relative-name path nemacs-library-boundary-report-repo-root))

(defun nemacs-library-boundary-report--primary-group (group)
  "Return primary ownership group from GROUP."
  (car (split-string group "/" t)))

(defun nemacs-library-boundary-report--ownership ()
  "Return a hash table mapping repo-relative paths to primary group."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents nemacs-library-boundary-report-ownership-doc)
      (goto-char (point-min))
      (while (re-search-forward "^| =\\([^=]+\\)= | \\([^ |]+\\)" nil t)
        (let* ((item (match-string 1))
               (group (nemacs-library-boundary-report--primary-group
                       (match-string 2)))
               (relative
                (cond
                 ((string-prefix-p "gui/" item) item)
                 ((string-suffix-p ".el" item)
                  (concat "src/" item))
                 (t item))))
          (puthash relative group table))))
    table))

(defun nemacs-library-boundary-report--elisp-files ()
  "Return repository Elisp files relevant to boundary analysis."
  (sort
   (append
    (directory-files-recursively
     (expand-file-name "src" nemacs-library-boundary-report-repo-root)
     "\\.el\\'")
    (let ((gui (expand-file-name "gui" nemacs-library-boundary-report-repo-root)))
      (and (file-directory-p gui)
           (directory-files-recursively gui "\\.el\\'"))))
   #'string<))

(defun nemacs-library-boundary-report--line-number-at (pos)
  "Return 1-based line number at POS in current buffer."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun nemacs-library-boundary-report--file-group (ownership relative)
  "Return ownership group for RELATIVE using OWNERSHIP."
  (or (gethash relative ownership)
      (and (string-prefix-p "gui/" relative) "GUI")
      "UNOWNED"))

(defun nemacs-library-boundary-report--collect-definitions (ownership)
  "Return hash private symbol -> list of (RELATIVE GROUP LINE).
Same-named helpers exist in a few compatibility/adapter pairs.  Keep all
definitions so the call scanner can prefer a definition in the same file
or ownership group instead of reporting a false cross-owner edge."
  (let ((defs (make-hash-table :test 'equal)))
    (dolist (file (nemacs-library-boundary-report--elisp-files))
      (let* ((relative (nemacs-library-boundary-report--relative file))
             (group (nemacs-library-boundary-report--file-group
                     ownership relative)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "^(\\s-*\\(defun\\|defmacro\\|defsubst\\|cl-defun\\|cl-defmacro\\)\\s-+\\([^][() \t\n]+\\)"
                  nil t)
            (let ((symbol (match-string 2)))
              (when (string-match-p "--" symbol)
                (push (list relative group
                            (nemacs-library-boundary-report--line-number-at
                             (match-beginning 2)))
                      (gethash symbol defs))))))))
    defs))

(defun nemacs-library-boundary-report--resolve-definition
    (defs symbol caller-file caller-group)
  "Resolve SYMBOL in DEFS for CALLER-FILE and CALLER-GROUP.
Prefer same-file definitions, then same-owner definitions, then the first
known definition.  This keeps duplicate helper names from looking like
cross-owner private calls."
  (let ((candidates (gethash symbol defs)))
    (or (cl-find caller-file candidates :key #'car :test #'equal)
        (cl-find caller-group candidates :key #'cadr :test #'equal)
        (car candidates))))

(defun nemacs-library-boundary-report--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (let ((s (format "%s" (or value ""))))
    (setq s (replace-regexp-in-string "[\t\n\r]+" " " s))
    s))

(defun nemacs-library-boundary-report--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-boundary-report--tsv-cell cells "\t"))

(defun nemacs-library-boundary-report--private-call-rows (ownership defs)
  "Return rows for cross-owner private calls using OWNERSHIP and DEFS."
  (let (rows)
    (dolist (file (nemacs-library-boundary-report--elisp-files))
      (let* ((relative (nemacs-library-boundary-report--relative file))
             (caller-group (nemacs-library-boundary-report--file-group
                            ownership relative)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "\\_<[A-Za-z0-9+*/:<=>?!_$%&~.^-]+--[A-Za-z0-9+*/:<=>?!_$%&~.^-]+\\_>"
                  nil t)
            (let* ((symbol (match-string 0))
                   (def (nemacs-library-boundary-report--resolve-definition
                         defs symbol relative caller-group))
                   (provider-file (nth 0 def))
                   (provider-group (nth 1 def))
                   (line (nemacs-library-boundary-report--line-number-at
                          (match-beginning 0))))
              (when (and def
                         (not (equal relative provider-file))
                         (not (equal caller-group provider-group)))
                (push (list "private-cross-owner"
                            caller-group provider-group relative line symbol
                            provider-file
                            "Private -- helper crosses ownership boundary")
                      rows)))))))
    (nreverse rows)))

(defun nemacs-library-boundary-report--semantic-definition-rows (ownership)
  "Return APP/GUI rows defining names that look like reusable semantics."
  (let (rows)
    (dolist (file (nemacs-library-boundary-report--elisp-files))
      (let* ((relative (nemacs-library-boundary-report--relative file))
             (group (nemacs-library-boundary-report--file-group
                     ownership relative)))
        (when (member group '("APP" "GUI"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward
                    "^(\\s-*\\(defun\\|defmacro\\|defsubst\\|cl-defun\\|cl-defmacro\\)\\s-+\\([^][() \t\n]+\\)"
                    nil t)
              (let ((symbol (match-string 2))
                    (line (nemacs-library-boundary-report--line-number-at
                           (match-beginning 2))))
                (when (string-match-p
                       nemacs-library-boundary-report--semantic-name-re
                       symbol)
                  (push (list (nemacs-library-boundary-report--semantic-row-type
                               relative symbol)
                              group "" relative line symbol ""
                              "Likely reusable semantics in app/gui owner")
                        rows))))))))
    (nreverse rows)))

(defun nemacs-library-boundary-report--init-require-rows (ownership)
  "Return rows describing package groups required by emacs-init."
  (let ((file (expand-file-name "src/emacs-init.el"
                                nemacs-library-boundary-report-repo-root))
        rows)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(require '\\([^][() \t\n]+\\))" nil t)
          (let* ((feature (match-string 1))
                 (relative (concat "src/" feature ".el"))
                 (group (nemacs-library-boundary-report--file-group
                         ownership relative))
                 (line (nemacs-library-boundary-report--line-number-at
                        (match-beginning 1))))
            (unless (string= feature "nelisp-emacs")
              (push (list "emacs-init-require"
                          "APP" group "src/emacs-init.el" line feature relative
                          "Current app assembly load dependency")
                    rows))))))
    (nreverse rows)))

(defun nemacs-library-boundary-report--rows ()
  "Return all advisory report rows."
  (let* ((ownership (nemacs-library-boundary-report--ownership))
         (defs (nemacs-library-boundary-report--collect-definitions ownership)))
    (append
     (nemacs-library-boundary-report--private-call-rows ownership defs)
     (nemacs-library-boundary-report--semantic-definition-rows ownership)
     (nemacs-library-boundary-report--init-require-rows ownership))))

(defun nemacs-library-boundary-report--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-library-boundary-report--sorted-counts (table)
  "Return TABLE counts sorted by descending count then key."
  (let (items)
    (maphash (lambda (k v) (push (cons k v) items)) table)
    (sort items
          (lambda (a b)
            (if (= (cdr a) (cdr b))
                (string< (car a) (car b))
              (> (cdr a) (cdr b)))))))

(defun nemacs-library-boundary-report--write-summary (rows output)
  "Write ROWS summary to OUTPUT."
  (let ((by-type (make-hash-table :test 'equal))
        (by-owner-pair (make-hash-table :test 'equal)))
    (dolist (row rows)
      (nemacs-library-boundary-report--inc by-type (nth 0 row))
      (when (equal (nth 0 row) "private-cross-owner")
        (nemacs-library-boundary-report--inc
         by-owner-pair (format "%s -> %s" (nth 1 row) (nth 2 row)))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library boundary summary\n\n")
      (insert "* Counts by type\n\n")
      (insert "| Type | Count |\n|------+-------|\n")
      (dolist (item (nemacs-library-boundary-report--sorted-counts by-type))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Private cross-owner pairs\n\n")
      (insert "| Pair | Count |\n|------+-------|\n")
      (dolist (item (nemacs-library-boundary-report--sorted-counts
                     by-owner-pair))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Notes\n\n")
      (insert "- This report is advisory and does not fail the build.\n")
      (insert "- Use it to choose small API-promotion and adapter-cleanup tasks.\n"))))

;;;###autoload
(defun nemacs-library-boundary-report-batch ()
  "Write the advisory library boundary report."
  (let ((rows (nemacs-library-boundary-report--rows)))
    (make-directory (file-name-directory nemacs-library-boundary-report-output)
                    t)
    (with-temp-file nemacs-library-boundary-report-output
      (insert
       (nemacs-library-boundary-report--row
        "type" "caller_group" "provider_group" "file" "line" "symbol"
        "provider_or_target" "note")
       "\n")
      (dolist (row rows)
        (insert (apply #'nemacs-library-boundary-report--row row) "\n")))
    (nemacs-library-boundary-report--write-summary
     rows nemacs-library-boundary-report-summary-output)
    (princ
     (format
      "nemacs-library-boundary-report: rows=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-boundary-report-output
      nemacs-library-boundary-report-summary-output))))

(provide 'nemacs-library-boundary-report)

;;; nemacs-library-boundary-report.el ends here
