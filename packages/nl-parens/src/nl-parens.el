;;; nl-parens.el --- Locate unbalanced top-level Lisp forms -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `check-parens' identifies where the reader stopped, which can be much later
;; than the form that needs a closing paren.  This package reports the form,
;; its name, its swallowed physical forms, and an indentation-derived repair.
;; It has no dependencies so it can also run on the standalone NeLisp runtime.

;;; Code:

(defvar nl-parens-report-style 'compact
  "Default style for `nl-parens-report'.  It is `compact' or `rich'.")

(defun nl-parens--definition-head-p (head)
  "Return non-nil when HEAD conventionally gives its name as argument two."
  (and (symbolp head)
       (let ((name (symbol-name head)))
         (or (eq head 'ert-deftest)
             (= 0 (string-match "\\`def" name))))))

(defun nl-parens--form-name (start)
  "Return the defined symbol at START, or nil when the form has no such name."
  (save-excursion
    (goto-char (1+ start))
    (condition-case nil
        (let ((head (read (current-buffer))))
          (when (nl-parens--definition-head-p head)
            (let ((name (read (current-buffer))))
              (and (symbolp name) name))))
      (error nil))))

(defun nl-parens--form-starts ()
  "Return physical top-level form boundaries in the current buffer.

A top-level form begins with a `(' at column 0 that is not inside a
string or comment.  That alone is not enough, twice over:

- A data block can begin a line with `(' while nested inside a larger
  form.  This tree really does that, so requiring depth 0 matters.
- But a form that is MISSING a closer never returns the depth to 0, and
  the forms after it must still be found -- reporting only the first
  break in a file would defeat the purpose.

Both hold if a start is a column-0 `(' whose enclosing parens, if any,
were themselves opened at column 0.  A still-open column-0 paren means a
previous top-level form failed to close; anything opened deeper means we
are genuinely inside one."
  (goto-char (point-min))
  (let ((starts nil) (columns nil) (state nil) (scan (point-min)))
    (while (and (re-search-forward "[()]" nil t))
      (let* ((pos (1- (point)))
             (char (char-after pos)))
        ;; Let the parser consume ordinary text in one span.  Its state is
        ;; still authoritative at each delimiter (including escapes, strings,
        ;; comments, and multiline forms).
        (setq state (parse-partial-sexp scan pos nil nil state))
        (goto-char pos)
        (let ((outside (not (or (nth 3 state) (nth 4 state)))))
          (when (and outside (eq char ?\() (= (current-column) 0)
                     (not (let ((rest columns) (nested nil))
                            (while rest
                              (when (> (car rest) 0) (setq nested t))
                              (setq rest (cdr rest)))
                            nested)))
            (setq starts (cons pos starts)))
          (cond
           ((and outside (eq char ?\())
            (setq columns (cons (current-column) columns)))
           ((and outside (eq char ?\)) columns)
            (setq columns (cdr columns)))))
        (setq state (parse-partial-sexp pos (1+ pos) nil nil state))
        (setq scan (1+ pos))
        (goto-char scan)))
    (setq state (parse-partial-sexp scan (point-max) nil nil state))
    (nreverse starts)))

(defun nl-parens--guard-wrapper-boundaries (starts)
  "Return (OPEN . CLOSE) boundaries for balanced fboundp guard forms in STARTS.
Only inspect forms with a plausible `when' or `unless' guard head.  Reading
the form validates its balance without evaluating it."
  (let ((wrappers nil))
    (dolist (start starts)
      (save-excursion
        (goto-char start)
        (when (looking-at "(\\(?:when\\|unless\\)[ \t\n]+(fboundp[ \t\n]+'[^() \t\n;]+")
          (condition-case nil
              (let* ((form (read (current-buffer)))
                     (guard (cadr form))
                     (quoted (cadr guard)))
                (when (and (memq (car-safe form) '(when unless))
                           (eq (car-safe guard) 'fboundp)
                           (eq (car-safe quoted) 'quote)
                           (symbolp (cadr quoted))
                           (eq (char-before) ?\)))
                  (setq wrappers (cons (cons start (1- (point))) wrappers))))
            (error nil)))))
    (nreverse wrappers)))

(defun nl-parens--form-end (start limit)
  "Return the insertion point after the final non-whitespace text before LIMIT."
  (save-excursion
    (goto-char limit)
    (skip-chars-backward " \t\r\n" start)
    (point)))

(defun nl-parens--comment-only-line-p ()
  "Return non-nil when the current line contains only an Emacs Lisp comment."
  (looking-at "^[ \t]*;"))

(defun nl-parens--next-significant-line (pos limit)
  "Return the next nonblank, non-comment line after POS, before LIMIT."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (while (and (< (point) limit)
                (or (looking-at "^[ \t]*$")
                    (nl-parens--comment-only-line-p)))
      (forward-line 1))
    (and (< (point) limit) (point))))

(defun nl-parens--line-code-end (line-start)
  "Return LINE-START's last code position, before whitespace or a comment."
  (save-excursion
    (goto-char line-start)
    (let ((line-end (line-end-position)) (comment-start nil))
      (while (and (not comment-start) (search-forward ";" line-end t))
        (let ((semicolon (1- (point))))
          (when (nth 4 (parse-partial-sexp (point-min) (1+ semicolon)))
            (setq comment-start semicolon))))
      (goto-char (or comment-start line-end))
      (skip-chars-backward " \t\r" line-start)
      (point))))

(defun nl-parens--last-code-end (start limit)
  "Return the last significant line's code end between START and LIMIT."
  (save-excursion
    (goto-char start)
    (let ((end (nl-parens--line-code-end start)))
      (while (< (point) limit)
        (unless (or (looking-at "^[ \t]*$")
                    (nl-parens--comment-only-line-p))
          (setq end (nl-parens--line-code-end (point))))
        (forward-line 1))
      end)))

(defun nl-parens--line-rows (start limit)
  "Return a row per significant line between START and LIMIT.
Each row is (INDENT CODE-END OPEN-COLUMNS), where OPEN-COLUMNS is the
stack of paren columns still open when the line begins, innermost first.
Blank and comment-only lines are skipped: they neither continue a body
nor end one.  Parens inside strings, comments and character literals do
not count, because the syntax state decides what is code."
  (let ((rows nil) (columns nil) (state nil) (pos start))
    (while (< pos limit)
      (goto-char pos)
      (let ((line-start (line-beginning-position))
            (line-end (min limit (line-end-position)))
            (entry columns))
        (back-to-indentation)
        (unless (or (eolp) (nl-parens--comment-only-line-p))
          (setq rows (cons (list (current-column)
                                 (nl-parens--line-code-end line-start)
                                 entry)
                           rows)))
        ;; Advance the open-column stack across this line.
        (let ((scan pos))
          (while (< scan line-end)
            (goto-char scan)
            (let ((outside (not (or (nth 3 state) (nth 4 state))))
                  (char (char-after)))
              (cond
               ((and outside (eq char ?\())
                (setq columns (cons (current-column) columns)))
               ((and outside (eq char ?\)) columns)
                (setq columns (cdr columns)))))
            (setq state (parse-partial-sexp scan (1+ scan) nil nil state))
            (setq scan (1+ scan))))
        (when (< line-end limit)
          (setq state (parse-partial-sexp line-end (1+ line-end) nil nil state)))
        (setq pos (min limit (1+ line-end)))))
    (nreverse rows)))

(defun nl-parens--repair-plan (start limit depth)
  "Return (POSITION . INFERRED) insertions for positive DEPTH at START.

The rule, and it is the whole design: when a line's indentation is no
deeper than a paren that is still open, that paren should already have
closed, and its closer belongs at the end of the previous significant
line.  Indentation is how the author said where the body ends, and a
missing closer leaves the reader with no other way to know which opener
was meant -- paren counting alone cannot tell.

Appending at the end of the form instead is not a repair.  It can make
the file parse while binding the following code into the wrong form,
which turns a loud error into a silent one.

When indentation gives no signal -- a one-liner, or nothing follows --
the closer goes at the end of the form and INFERRED is nil, so the
report can call the placement a guess."
  (let ((rows (nl-parens--line-rows start limit))
        (end (nl-parens--last-code-end start limit))
        (previous nil)
        (plan nil)
        (remaining depth))
    (while (and rows (> remaining 0))
      (let* ((row (car rows))
             (indent (car row))
             (columns (nth 2 row)))
        (while (and columns (> remaining 0) previous
                    (>= (car columns) indent))
          (setq plan (cons (cons previous t) plan))
          (setq columns (cdr columns))
          (setq remaining (1- remaining)))
        (setq previous (nth 1 row)))
      (setq rows (cdr rows)))
    (while (> remaining 0)
      (setq plan (cons (cons end nil) plan))
      (setq remaining (1- remaining)))
    (nreverse plan)))

(defun nl-parens--source-line (position)
  "Return the source line containing POSITION without its trailing newline."
  (save-excursion
    (goto-char position)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun nl-parens--scan-buffer (file)
  "Return (FINDINGS . REPAIRS) for the current Emacs Lisp buffer.
REPAIRS contains (POSITION . COUNT) pairs for positive-depth findings only."
  (emacs-lisp-mode)
  (let* ((starts (nl-parens--form-starts))
         (wrappers (nl-parens--guard-wrapper-boundaries starts))
         (findings nil) (repairs nil))
    (while starts
      (let* ((start (car starts))
             (following (cdr starts))
             (limit (or (car following) (point-max)))
             (depth (condition-case nil
                        (car (parse-partial-sexp start limit))
                      (scan-error 0))))
        ;; A valid runtime-marker guard spans several physical candidate
        ;; intervals.  Balance only its known boundary parens; leave every
        ;; body interval intact so broken definitions remain visible.
        (when (assq start wrappers) (setq depth (1- depth)))
        (dolist (wrapper wrappers)
          (when (and (<= start (cdr wrapper)) (< (cdr wrapper) limit))
            (setq depth (1+ depth))))
        (when (/= depth 0)
          (let* ((end (nl-parens--form-end start limit))
                 (plan (and (> depth 0) (nl-parens--repair-plan start limit depth)))
                 (first (car plan))
                 (insert (or (car first) end))
                 (absorbed nil))
            (when (> depth 0)
              (dolist (other following)
                (let ((name (nl-parens--form-name other)))
                  (when name (setq absorbed (cons name absorbed)))))
              (setq absorbed (nreverse absorbed)))
            (setq findings
                  (cons (list :kind (if (> depth 0) 'parens-missing 'parens-extra)
                              :file file :line (line-number-at-pos start)
                              :name (nl-parens--form-name start) :depth depth
                              :end-line (line-number-at-pos (max (point-min) (1- end)))
                              :insert-line (line-number-at-pos insert)
                              :insert-column (save-excursion
                                               (goto-char insert) (current-column))
                              :inferred (and first (cdr first))
                              :source-line (nl-parens--source-line insert)
                              :absorbed absorbed)
                        findings))
            (when plan
              (let ((positions nil))
                (dolist (repair plan)
                  (let ((old (assoc (car repair) positions)))
                    (if old
                        (setcdr old (1+ (cdr old)))
                      (setq positions (cons (cons (car repair) 1) positions)))))
                (setq repairs (append positions repairs))))))
        (setq starts (cdr starts))))
    (cons (nreverse findings) repairs)))

(defun nl-parens-check-buffer ()
  "Return parenthesis findings for the current buffer without modifying it."
  (car (nl-parens--scan-buffer (or buffer-file-name (buffer-name)))))

(defun nl-parens-check-file (path)
  "Return parenthesis findings for Emacs Lisp file PATH without evaluating it."
  (with-temp-buffer
    (insert-file-contents path)
    (car (nl-parens--scan-buffer path))))

(defun nl-parens-check-files (paths)
  "Return the concatenated parenthesis findings for PATHS."
  (let ((findings nil))
    (dolist (path paths)
      (setq findings (append findings (nl-parens-check-file path))))
    findings))

(defun nl-parens--plural (count)
  "Return the word `paren' or `parens' for COUNT."
  (if (= count 1) "paren" "parens"))

(defun nl-parens--message (finding)
  "Return the human-readable diagnostic message for FINDING."
  (let* ((depth (plist-get finding :depth))
         (count (abs depth))
         (name (or (plist-get finding :name) "<unnamed form>"))
         (absorbed (plist-get finding :absorbed)))
    (concat (format "%s is %s %d closing %s" name
                    (if (> depth 0) "missing" "extra") count
                    (nl-parens--plural count))
            (if absorbed
                (format "; it has absorbed the %d top-level forms that follow it (%s)"
                        (length absorbed)
                        (mapconcat (lambda (item) (format "%s" item)) absorbed ", "))
              ""))))

(defun nl-parens-report (findings &optional style)
  "Return FINDINGS in STYLE, either `compact', `rich', or `sexp'."
  (let ((style (or style nl-parens-report-style)) (lines nil))
    (dolist (finding findings)
      (if (eq style 'sexp)
          (setq lines (cons (prin1-to-string finding) lines))
        (let* ((line (plist-get finding :line))
               (column (if (eq (plist-get finding :kind) 'parens-missing)
                           (1+ (plist-get finding :insert-column)) 1))
               (header (format "%s:%d:%d: error: %s"
                               (plist-get finding :file) line column
                               (nl-parens--message finding))))
          (setq lines
                (cons (if (eq style 'rich)
                          (concat header "\n"
                                  (format "  %d | %s\n"
                                          (plist-get finding :insert-line)
                                          (plist-get finding :source-line))
                                  (format "    | %s^ %s"
                                          (make-string (plist-get finding :insert-column) ?\s)
                                          (if (eq (plist-get finding :kind)
                                                  'parens-missing)
                                              "insert `)' here"
                                            "extra closer reported here")))
                        header)
                      lines)))))
    (if lines (concat (mapconcat #'identity (nreverse lines) "\n") "\n") "")))

(defun nl-parens--fix-current-buffer ()
  "Insert missing closers in the current buffer and return their count."
  (let* ((scan (nl-parens--scan-buffer (or buffer-file-name (buffer-name))))
         (repairs (cdr scan)) (count 0))
    ;; REPAIRS are in reverse source order, so positions remain valid.
    (dolist (repair repairs)
      (goto-char (car repair))
      (insert (make-string (cdr repair) ?\)))
      (setq count (+ count (cdr repair))))
    count))

(defun nl-parens-fix-file (path &optional dry-run)
  "Fix missing closers in PATH, or return its insertion plan when DRY-RUN.
After a write, scan again.  If any finding remains, restore the original file
and signal an error; extra closers are deliberately never edited."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((original (buffer-string))
           (scan (nl-parens--scan-buffer path))
           (repairs (cdr scan)))
      (if dry-run
          (let ((result nil))
            (dolist (repair repairs)
              (goto-char (car repair))
              (setq result
                    (cons (list :position (car repair)
                                :line (line-number-at-pos (car repair))
                                :column (current-column)
                                :text (make-string (cdr repair) ?\)))
                          result)))
            (nreverse result))
        (let ((count (nl-parens--fix-current-buffer)))
          (when (> count 0)
            (write-region (point-min) (point-max) path nil 'silent)
            (when (nl-parens-check-file path)
              (with-temp-file path (insert original))
              (error "nl-parens refused a partial repair of %s" path)))
          count)))))

(provide 'nl-parens)

;;; nl-parens.el ends here
