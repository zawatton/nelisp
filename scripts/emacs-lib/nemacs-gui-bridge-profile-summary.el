;;; nemacs-gui-bridge-profile-summary.el --- summarize GUI bridge profile logs -*- lexical-binding: t; -*-

;;; Commentary:

;; Post-processes NEMACS_GUI_BRIDGE_PROFILE=1 output from
;; `test/nemacs-gui-file-bridge-runtime-test.el' into a stable Org summary.
;; The transport snapshot is captured before each runner form executes, so
;; compound `(progn ...)' forms are grouped separately instead of trusting
;; potentially stale cmd/keys fields.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-gui-bridge-profile-summary-input
  (expand-file-name
   "../build/nemacs-gui-bridge-profile.log"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Raw NEMACS_GUI_BRIDGE_PROFILE=1 log input path.")

(defvar nemacs-gui-bridge-profile-summary-output
  (expand-file-name
   "../build/nemacs-gui-bridge-profile-summary.org"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Org summary output path.")

(defvar nemacs-gui-bridge-profile-summary-top-limit 30
  "Maximum rows to write in top-command tables.")

(defun nemacs-gui-bridge-profile-summary--slurp (file)
  "Return FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nemacs-gui-bridge-profile-summary--runner-form-p (form)
  "Return non-nil when FORM is the plain bridge runner form."
  (equal form "\"(nemacs-gui-file-bridge-run)\""))

(defun nemacs-gui-bridge-profile-summary--name (row)
  "Return display name for ROW."
  (if (plist-get row :compound)
      "compound-form"
    (let ((cmd (plist-get row :cmd))
          (keys (plist-get row :keys)))
      (if (string-empty-p cmd)
          (format "keys:%s" keys)
        (format "cmd:%s" cmd)))))

(defun nemacs-gui-bridge-profile-summary--matches-any-p (regexps value)
  "Return non-nil when VALUE matches any regexp in REGEXPS."
  (cl-some (lambda (regexp) (string-match-p regexp value)) regexps))

(defun nemacs-gui-bridge-profile-summary--family (row)
  "Return coarse command family for ROW."
  (if (plist-get row :compound)
      "compound-form"
    (let* ((cmd (plist-get row :cmd))
           (keys (plist-get row :keys))
           (name (if (string-empty-p cmd) (format "KEY:%s" keys) cmd)))
      (cond
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`find-file" "\\`find-alternate-file" "\\`insert-file"
           "\\`save-" "\\`basic-save-buffer" "\\`write-file"
           "\\`revert-buffer" "\\`read-only-mode" "\\`toggle-read-only")
         name)
        "file/save")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`project" "\\`KEY:C-x p")
         name)
        "project")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`dired" "\\`list-directory")
         name)
        "dired")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`switch-to-buffer" "\\`display-buffer" "\\`kill-buffer"
           "\\`rename-" "\\`clone-" "\\`list-buffers" "\\`compose-mail"
           "\\`KEY:C-x 4 0")
         name)
        "buffer")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`isearch" "\\`replace" "\\`query-replace" "\\`KEY:M-s"
           "\\`KEY:C-M-%" "\\`KEY:C-M-s" "\\`KEY:C-M-r" "\\`KEY:y"
           "\\`KEY:M-ESC" "\\`KEY:C-M-c" "\\`KEY:C-\\]")
         name)
        "search/replace")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`split-" "\\`delete-window" "\\`delete-other-windows"
           "\\`other-window" "\\`enlarge-window" "\\`shrink-window"
           "\\`balance-windows" "\\`fit-window" "\\`quit-window"
           "\\`tear-off-window" "\\`window-" "\\`toggle-window"
           "\\`recenter" "\\`scroll" "\\`move-to-window"
           "\\`KEY:C-x <" "\\`KEY:M-r" "\\`KEY:C-M-v"
           "\\`KEY:C-M-S-v" "\\`KEY:C-M-l" "\\`KEY:C-M-S-l"
           "\\`KEY:C-x w")
         name)
        "window/scroll")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`set-mark" "\\`pop-global-mark" "\\`exchange-point-and-mark"
           "\\`mark-whole-buffer" "\\`rectangle" "\\`KEY:C-@"
           "\\`KEY:C-x C-SPC" "\\`KEY:C-x SPC" "\\`KEY:C-g"
           "\\`KEY:C-x x t")
         name)
        "region/state")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`forward-char" "\\`backward-char" "\\`beginning-"
           "\\`end-" "\\`next-line" "\\`previous-line"
           "\\`set-goal-column" "\\`move-" "\\`back-to-indentation"
           "\\`goto-" "\\`forward-word" "\\`backward-word"
           "\\`forward-sexp" "\\`backward-sexp" "\\`down-list"
           "\\`forward-list" "\\`backward-list" "\\`backward-up-list"
           "\\`forward-sentence" "\\`backward-sentence" "\\`forward-page"
           "\\`backward-page" "\\`mark-page" "\\`mark-word"
           "\\`mark-sexp" "\\`mark-defun" "\\`KEY:C-M" "\\`KEY:M-m"
           "\\`KEY:M-@")
         name)
        "motion/mark")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`delete" "\\`kill" "\\`yank" "\\`append-next-kill"
           "\\`self-insert" "\\`undo" "\\`quoted-insert" "\\`indent"
           "\\`tab-to" "\\`newline" "\\`electric" "\\`default-indent"
           "\\`open-line" "\\`split-line" "\\`upcase" "\\`downcase"
           "\\`capitalize" "\\`sort-lines" "\\`transpose"
           "\\`cycle-spacing" "\\`just-one-space" "\\`comment"
           "\\`fill" "\\`not-modified" "\\`KEY:C-q" "\\`KEY:M-i"
           "\\`KEY:C-M-\\\\" "\\`KEY:C-x TAB" "\\`KEY:C-j"
           "\\`KEY:C-M-j" "\\`KEY:M-j" "\\`KEY:C-M-o"
           "\\`KEY:C-x C-o" "\\`KEY:M-y" "\\`KEY:C-M-w"
           "\\`KEY:M-;" "\\`KEY:M-SPC" "\\`KEY:M-~")
         name)
        "editing")
       ((nemacs-gui-bridge-profile-summary--matches-any-p
         '("\\`keyboard" "\\`exit-recursive" "\\`abort-recursive")
         name)
        "control")
       (t "other")))))

(defun nemacs-gui-bridge-profile-summary--parse-line (line)
  "Parse one profile LINE into a plist, or nil."
  (when (string-match
         (concat "persistent-runner id=\\([0-9]+\\) "
                 "seconds=\\([0-9.]+\\).*"
                 "form=\\(\".*\"\\) transport=cmd=\"\\([^\"]*\\)\" "
                 "keys=\"\\([^\"]*\\)\"")
         line)
    (let ((form (match-string 3 line)))
      (list :id (string-to-number (match-string 1 line))
            :seconds (string-to-number (match-string 2 line))
            :form form
            :compound (not
                       (nemacs-gui-bridge-profile-summary--runner-form-p
                        form))
            :cmd (match-string 4 line)
            :keys (match-string 5 line)))))

(defun nemacs-gui-bridge-profile-summary--rows (file)
  "Return parsed profile rows from FILE."
  (let (rows)
    (dolist (line (split-string
                   (nemacs-gui-bridge-profile-summary--slurp file) "\n" t))
      (let ((row (nemacs-gui-bridge-profile-summary--parse-line line)))
        (when row
          (push row rows))))
    (nreverse rows)))

(defun nemacs-gui-bridge-profile-summary--hash-keys (table)
  "Return keys from hash TABLE."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) table)
    keys))

(defun nemacs-gui-bridge-profile-summary--add (table key seconds)
  "Add one observation for KEY with SECONDS into TABLE."
  (let ((cell (gethash key table)))
    (if cell
        (progn
          (setcar cell (1+ (car cell)))
          (setcdr cell (+ (cdr cell) seconds)))
      (puthash key (cons 1 seconds) table))))

(defun nemacs-gui-bridge-profile-summary--pct (part total)
  "Return PART / TOTAL as a percentage."
  (if (> total 0.0)
      (* 100.0 (/ part total))
    0.0))

(defun nemacs-gui-bridge-profile-summary--write-count-table
    (title table total output-order)
  "Insert TITLE and rows from TABLE.
TOTAL is the denominator for percentage.  OUTPUT-ORDER is either
`seconds' or `count'."
  (insert (format "\n** %s\n\n" title))
  (insert "| name | count | seconds | pct | avg |\n")
  (insert "|-+-------+---------+-----+-----|\n")
  (let ((sorted (nemacs-gui-bridge-profile-summary--hash-keys table))
        (limit nemacs-gui-bridge-profile-summary-top-limit)
        (index 0))
    (setq sorted
          (sort sorted
                (lambda (a b)
                  (let ((ca (gethash a table))
                        (cb (gethash b table)))
                    (if (eq output-order 'count)
                        (or (> (car ca) (car cb))
                            (and (= (car ca) (car cb))
                                 (> (cdr ca) (cdr cb))))
                      (> (cdr ca) (cdr cb)))))))
    (dolist (key sorted)
      (when (or (null limit) (< index limit))
        (let* ((cell (gethash key table))
               (count (car cell))
               (seconds (cdr cell)))
          (insert
           (format "| %s | %d | %.3f | %.1f | %.3f |\n"
                   key count seconds
                   (nemacs-gui-bridge-profile-summary--pct seconds total)
                   (/ seconds count)))))
      (setq index (1+ index)))))

(defun nemacs-gui-bridge-profile-summary--write-slow-table (rows)
  "Insert slow request table for ROWS."
  (insert "\n** Slow Requests\n\n")
  (insert "| id | seconds | name | form |\n")
  (insert "|-+---------+------+------|\n")
  (dolist (row (sort (cl-copy-list rows)
                     (lambda (a b)
                       (> (plist-get a :seconds)
                          (plist-get b :seconds)))))
    (when (>= (plist-get row :seconds) 1.0)
      (insert
       (format "| %d | %.3f | %s | %s |\n"
               (plist-get row :id)
               (plist-get row :seconds)
               (nemacs-gui-bridge-profile-summary--name row)
               (plist-get row :form))))))

(defun nemacs-gui-bridge-profile-summary--write-summary (rows output)
  "Write Org summary for ROWS to OUTPUT."
  (let ((by-name (make-hash-table :test 'equal))
        (by-family (make-hash-table :test 'equal))
        (by-lane (make-hash-table :test 'equal))
        (total 0.0))
    (dolist (row rows)
      (let ((seconds (plist-get row :seconds)))
        (setq total (+ total seconds))
        (nemacs-gui-bridge-profile-summary--add
         by-name (nemacs-gui-bridge-profile-summary--name row) seconds)
        (nemacs-gui-bridge-profile-summary--add
         by-family (nemacs-gui-bridge-profile-summary--family row) seconds)
        (nemacs-gui-bridge-profile-summary--add
         by-lane
         (cond
          ((plist-get row :compound) "compound-form")
          ((string-empty-p (plist-get row :cmd)) "key-or-empty")
          (t "direct-cmd"))
         seconds)))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: Nemacs GUI Bridge Profile Summary\n\n")
      (insert (format "- source: =%s=\n" nemacs-gui-bridge-profile-summary-input))
      (insert (format "- parsed requests: %d\n" (length rows)))
      (insert (format "- summed seconds: %.3f\n" total))
      (insert "- note: compound forms are grouped separately because transport is sampled before form execution.\n")
      (nemacs-gui-bridge-profile-summary--write-count-table
       "Lane" by-lane total 'seconds)
      (nemacs-gui-bridge-profile-summary--write-count-table
       "Family" by-family total 'seconds)
      (nemacs-gui-bridge-profile-summary--write-count-table
       "Top By Total Seconds" by-name total 'seconds)
      (nemacs-gui-bridge-profile-summary--write-count-table
       "Top By Count" by-name total 'count)
      (nemacs-gui-bridge-profile-summary--write-slow-table rows))))

(defun nemacs-gui-bridge-profile-summary-batch ()
  "Generate profile summary artifact."
  (let ((rows (nemacs-gui-bridge-profile-summary--rows
               nemacs-gui-bridge-profile-summary-input)))
    (nemacs-gui-bridge-profile-summary--write-summary
     rows nemacs-gui-bridge-profile-summary-output)
    (princ
     (format
      "nemacs-gui-bridge-profile-summary: rows=%d summary=%s\n"
      (length rows)
      nemacs-gui-bridge-profile-summary-output))))

(provide 'nemacs-gui-bridge-profile-summary)

;;; nemacs-gui-bridge-profile-summary.el ends here
