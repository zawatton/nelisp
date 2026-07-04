;;; emacs-org-todo.el --- Org TODO subset for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 daily-driver §3.3.2.
;;
;; This module implements the M3.2 org TODO subset:
;;
;; - heading TODO keyword cycling (`org-todo')
;; - DONE / CANCEL logging via lightweight CLOSED timestamps
;; - context-aware `org-toggle-todo' for headings and checkboxes
;; - TODO keyword faces wired through the existing font-lock substrate
;;
;; Heading structure / detection intentionally delegates to
;; `emacs-org-outline.el'.  We reuse its heading helpers rather than
;; re-parsing star prefixes here.

;;; Code:

(require 'cl-lib)
(require 'emacs-faces-builtins)
(require 'emacs-minibuffer-builtins)
(require 'emacs-org-outline)

;;;; Customization

(defgroup org nil
  "Minimal Org subset."
  :group 'text)

(defcustom org-todo-keywords
  '((sequence "INBOX" "NEXT" "WAIT" "PROJECTS" "SCHEDULED" "SOMEDAY"
              "|" "DONE" "CANCEL"))
  "Keyword sequences used by `org-todo'.
Only the `sequence' form is supported in the v0.1 subset.  The
string \"|\" separates active from done states."
  :type 'sexp
  :group 'org)

(defcustom org-log-done 'time
  "Control CLOSED timestamp logging when entering a done keyword.
nil disables logging.  The symbol `time' inserts a lightweight
`CLOSED: [timestamp]' line."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Timestamp" time))
  :group 'org)

(defcustom org-highest-priority ?A
  "Highest priority cookie supported by `org-priority'."
  :type 'character
  :group 'org)

(defcustom org-lowest-priority ?C
  "Lowest priority cookie supported by `org-priority'."
  :type 'character
  :group 'org)

(defcustom org-default-priority ?B
  "Default priority used by compatibility callers.
The lightweight `org-priority' cycle starts at `org-highest-priority' to
match the existing nemacs bridge behavior."
  :type 'character
  :group 'org)

(defcustom org-priority-faces nil
  "Face alist for priority cookies.
The lightweight Org subset exposes this variable for compatibility; full
font-lock face application is intentionally minimal."
  :type 'sexp
  :group 'org)

;;;; Faces

(defface org-todo-keyword-todo
  '((t :weight bold))
  "Face used for active Org TODO keywords."
  :group 'org)

(defface org-todo-keyword-done
  '((t :foreground "gray50" :strike-through t))
  "Face used for done Org TODO keywords."
  :group 'org)

;;;; Keyword parsing

(defconst org-todo--sequence-separator "|"
  "Separator token in `org-todo-keywords'.")

(defun org-todo--keyword-spec ()
  "Return normalized TODO keyword metadata.
The result is a plist containing `:active', `:done' and `:all'."
  (let ((active nil)
        (done nil)
        (seen-separator nil))
    (dolist (entry org-todo-keywords)
      (when (and (consp entry) (eq (car entry) 'sequence))
        (dolist (token (cdr entry))
          (cond
           ((equal token org-todo--sequence-separator)
            (setq seen-separator t))
           ((stringp token)
            (if seen-separator
                (push token done)
              (push token active)))))))
    (setq active (nreverse active))
    (setq done (nreverse done))
    (list :active active
          :done done
          :all (append active done))))

(defun org-todo--active-keywords ()
  "Return the configured active TODO keywords."
  (plist-get (org-todo--keyword-spec) :active))

(defun org-todo--done-keywords ()
  "Return the configured done TODO keywords."
  (plist-get (org-todo--keyword-spec) :done))

(defun org-todo--all-keywords ()
  "Return all configured TODO keywords in cycle order."
  (plist-get (org-todo--keyword-spec) :all))

(defun org-todo--keyword-member-p (keyword)
  "Return non-nil when KEYWORD is in `org-todo-keywords'."
  (and keyword (member keyword (org-todo--all-keywords))))

(defun org-todo--done-keyword-p (keyword)
  "Return non-nil when KEYWORD is a done-state keyword."
  (and keyword (member keyword (org-todo--done-keywords))))

(defun org-todo--todo-keyword-face (keyword)
  "Return the face symbol for KEYWORD."
  (if (org-todo--done-keyword-p keyword)
      'org-todo-keyword-done
    'org-todo-keyword-todo))

(defun org-todo--regexp-opt (keywords)
  "Return a safe regexp matching KEYWORDS, or nil when empty."
  (when keywords
    (regexp-opt keywords t)))

;;;; Heading text extraction / rewriting

(defun org-todo--trim-left-space (text)
  "Return TEXT with leading spaces removed."
  (if (string-match "\\`[ \t\n\r]+" text)
      (substring text (match-end 0))
    text))

(defun org-todo--heading-components ()
  "Return plist describing the current heading line.
Signals `user-error' unless point is on a heading.  The result contains:

- `:prefix'         leading stars plus trailing space
- `:keyword'        recognized TODO keyword, or nil
- `:rest'           heading text with the TODO keyword removed
- `:line-start'     current line beginning
- `:line-end'       current line end (without newline)"
  (org-outline--require-heading)
  (let* ((line (org-outline--line-string))
         (all-keywords (org-todo--all-keywords))
         (keyword nil)
         (rest ""))
    (unless (string-match org-outline--heading-regexp line)
      (user-error "Not on an Org heading"))
    (let* ((prefix-end (match-end 0))
           (prefix (substring line 0 prefix-end))
           (body (substring line prefix-end))
           (matched nil))
      (dolist (candidate all-keywords)
        (when (and (null matched)
                   (string-match
                    (concat "\\`" (regexp-quote candidate) "\\(?:\\'\\| \\)")
                    body))
          (setq keyword candidate)
          (setq rest
                (org-todo--trim-left-space
                 (substring body (match-end 0))))
          (setq matched t)))
      (unless matched
        (setq rest body))
      (list :prefix prefix
            :keyword keyword
            :rest rest
            :line-start (line-beginning-position)
            :line-end (line-end-position)))))

(defun org-todo--heading-string (prefix keyword rest)
  "Build a heading string from PREFIX, KEYWORD and REST."
  (concat prefix
          (cond
           ((and keyword (> (length rest) 0))
            (concat keyword " " rest))
           (keyword
            keyword)
           ((> (length rest) 0)
            rest)
           (t
            ""))))

(defun org-todo--replace-heading-keyword (keyword)
  "Replace the current heading's TODO keyword with KEYWORD.
KEYWORD may be nil to clear the heading's TODO state.  Return a plist
with `:old-keyword' and `:new-keyword'."
  (let* ((parts (org-todo--heading-components))
         (old-keyword (plist-get parts :keyword))
         (line-start (plist-get parts :line-start))
         (line-end (plist-get parts :line-end))
         (new-line (org-todo--heading-string
                    (plist-get parts :prefix)
                    keyword
                    (plist-get parts :rest))))
    (delete-region line-start line-end)
    (goto-char line-start)
    (insert new-line)
    (list :old-keyword old-keyword
          :new-keyword keyword)))

(defun org-todo--priority-chars ()
  "Return the configured priority character cycle."
  (let ((chars nil)
        (current org-highest-priority))
    (while (<= current org-lowest-priority)
      (push current chars)
      (setq current (1+ current)))
    (nreverse chars)))

(defun org-todo--split-priority (text)
  "Return plist for priority cookie and title in TEXT.
The result contains `:priority' as a character or nil and `:title' with
the leading priority cookie removed."
  (if (string-match "\\`[ \t]*\\[#\\(.\\)\\][ \t]*" text)
      (list :priority (aref (match-string 1 text) 0)
            :title (substring text (match-end 0)))
    (list :priority nil
          :title (org-todo--trim-left-space text))))

(defun org-todo--normalize-priority-action (action)
  "Return priority character requested by ACTION, or nil to clear it."
  (cond
   ((null action) nil)
   ((memq action '(none reset remove clear))
    nil)
   ((and (integerp action)
         (or (= action ?\s) (= action ?0)))
    nil)
   ((integerp action)
    action)
   ((and (stringp action) (= (length action) 0))
    nil)
   ((and (stringp action) (= (length action) 1))
    (aref action 0))
   ((symbolp action)
    (let ((name (symbol-name action)))
      (if (= (length name) 1)
          (aref name 0)
        (user-error "Invalid Org priority action: %S" action))))
   (t
    (user-error "Invalid Org priority action: %S" action))))

(defun org-todo--next-priority (priority)
  "Return the next priority after PRIORITY, or nil after the last one."
  (let ((cycle (org-todo--priority-chars)))
    (if priority
        (cadr (memq priority cycle))
      (car cycle))))

(defun org-todo--priority-cookie (priority)
  "Return Org priority cookie for PRIORITY, or an empty string."
  (if priority
      (format "[#%c]" priority)
    ""))

(defun org-todo--priority-rest (priority title)
  "Return heading rest containing PRIORITY cookie and TITLE."
  (let ((cookie (org-todo--priority-cookie priority)))
    (cond
     ((and (> (length cookie) 0) (> (length title) 0))
      (concat cookie " " title))
     ((> (length cookie) 0)
      cookie)
     (t
      title))))

(defun org-todo--replace-priority (priority)
  "Replace current heading priority with PRIORITY.
PRIORITY is a character or nil.  Return PRIORITY."
  (let* ((cycle (org-todo--priority-chars))
         (parts (org-todo--heading-components))
         (split (org-todo--split-priority (plist-get parts :rest)))
         (title (plist-get split :title))
         (line-start (plist-get parts :line-start))
         (line-end (plist-get parts :line-end)))
    (when (and priority (not (memq priority cycle)))
      (user-error "Priority %c is outside Org priority range" priority))
    (delete-region line-start line-end)
    (goto-char line-start)
    (insert
     (org-todo--heading-string
      (plist-get parts :prefix)
      (plist-get parts :keyword)
      (org-todo--priority-rest priority title)))
    priority))

(defun org-todo--next-keyword (keyword)
  "Return the next keyword in the configured cycle after KEYWORD."
  (let ((cycle (append (list nil) (org-todo--all-keywords) (list nil))))
    (cadr (member keyword cycle))))

(defun org-todo--read-keyword ()
  "Prompt for a configured TODO keyword and return it."
  (let ((reader (cond
                 ((fboundp 'emacs-minibuffer-completing-read)
                  #'emacs-minibuffer-completing-read)
                 ((fboundp 'completing-read)
                  #'completing-read)
                 (t
                  nil))))
    (unless reader
      (user-error "No completing-read available"))
    (funcall reader
             "TODO keyword: "
             (org-todo--all-keywords)
             nil t nil nil)))

;;;; Logging

(defun org-todo--timestamp-string ()
  "Return a lightweight Org CLOSED timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun org-todo--planning-timestamp-string (&optional time)
  "Return a lightweight active Org planning timestamp for TIME."
  (format-time-string "<%Y-%m-%d %a>" time))

(defun org-todo--normalize-planning-time (time)
  "Return an Emacs time value for TIME.
TIME may be nil, an Emacs time value, or a string accepted as YYYY-MM-DD
or an Org timestamp."
  (cond
   ((null time)
    (current-time))
   ((stringp time)
    (let ((clean time))
      (when (string-match "\\`[<[]\\([^]>]+\\)[]>]\\'" clean)
        (setq clean (match-string 1 clean)))
      (date-to-time clean)))
   (t
    time)))

(defun org-todo--planning-timestamp-from-input (&optional time)
  "Return an active Org planning timestamp for TIME."
  (org-todo--planning-timestamp-string
   (org-todo--normalize-planning-time time)))

(defun org-todo--planning-line-range ()
  "Return (START . END) for the direct planning line after heading."
  (save-excursion
    (org-outline--require-heading)
    (forward-line 1)
    (when (looking-at
           "\\(?:DEADLINE:\\|SCHEDULED:\\)[ \t]*<[^>\n]+>")
      (cons (line-beginning-position)
            (org-outline--line-end-with-newline)))))

(defun org-todo--planning-line-components (&optional line)
  "Return planning plist parsed from LINE or the current planning line."
  (let ((text (or line
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        deadline
        scheduled)
    (when (string-match "DEADLINE:[ \t]*\\(<[^>\n]+>\\)" text)
      (setq deadline (match-string 1 text)))
    (when (string-match "SCHEDULED:[ \t]*\\(<[^>\n]+>\\)" text)
      (setq scheduled (match-string 1 text)))
    (list :deadline deadline :scheduled scheduled)))

(defun org-todo--planning-line-string (components)
  "Return a normalized planning line string from COMPONENTS."
  (let ((deadline (plist-get components :deadline))
        (scheduled (plist-get components :scheduled))
        parts)
    (when deadline
      (push (concat "DEADLINE: " deadline) parts))
    (when scheduled
      (push (concat "SCHEDULED: " scheduled) parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun org-todo--set-planning (keyword time)
  "Set planning KEYWORD to TIME on the current heading."
  (org-outline--require-heading)
  (let* ((timestamp (org-todo--planning-timestamp-from-input time))
         (range (org-todo--planning-line-range))
         (components
          (if range
              (org-todo--planning-line-components
               (buffer-substring-no-properties (car range) (cdr range)))
            (list :deadline nil :scheduled nil)))
         (message-prefix (if (equal keyword "SCHEDULED")
                             "Scheduled to"
                           "Deadline on"))
         line)
    (cond
     ((equal keyword "SCHEDULED")
      (setq components (plist-put components :scheduled timestamp)))
     ((equal keyword "DEADLINE")
      (setq components (plist-put components :deadline timestamp)))
     (t
      (user-error "Unsupported planning keyword: %s" keyword)))
    (setq line (concat (org-todo--planning-line-string components) "\n"))
    (save-excursion
      (if range
          (progn
            (delete-region (car range) (cdr range))
            (goto-char (car range)))
        (goto-char (org-outline--line-end-with-newline)))
      (insert line))
    (message "%s %s" message-prefix timestamp)))

(defun org-todo--get-planning-time (keyword)
  "Return the Emacs time value for planning KEYWORD at current heading."
  (org-outline--require-heading)
  (let ((range (org-todo--planning-line-range)))
    (when range
      (let* ((components
              (org-todo--planning-line-components
               (buffer-substring-no-properties (car range) (cdr range))))
             (timestamp (plist-get components keyword)))
        (when timestamp
          (org-todo--normalize-planning-time timestamp))))))

(defun org-todo--direct-logbook-range ()
  "Return plist describing an immediate LOGBOOK drawer, or nil.
The drawer must appear directly after the heading line."
  (let* ((bounds (org-outline--subtree-bounds))
         (content-start (plist-get bounds :content-start))
         (subtree-end (plist-get bounds :subtree-end)))
    (save-excursion
      (goto-char content-start)
      (when (and (< (point) subtree-end)
                 (looking-at ":LOGBOOK:[ \t]*$"))
        (let ((drawer-start (line-beginning-position))
              (drawer-content-start (org-outline--line-end-with-newline))
              (drawer-end nil))
          (forward-line 1)
          (while (and (null drawer-end) (< (point) subtree-end))
            (when (looking-at ":END:[ \t]*$")
              (setq drawer-end (line-beginning-position)))
            (unless drawer-end
              (forward-line 1)))
          (when drawer-end
            (list :drawer-start drawer-start
                  :drawer-content-start drawer-content-start
                  :drawer-end drawer-end)))))))

(defun org-todo--existing-closed-line-range (start end)
  "Return (LINE-START . LINE-END) for an existing CLOSED line in START..END.
END is exclusive.  Return nil when none exists."
  (save-excursion
    (goto-char start)
    (catch 'found
      (while (< (point) end)
        (when (looking-at "CLOSED:[ \t]*\\[[^]\n]+\\][ \t]*$")
          (throw 'found
                 (cons (line-beginning-position)
                       (org-outline--line-end-with-newline))))
        (forward-line 1))
      nil)))

(defun org-todo--upsert-closed-line (start end)
  "Insert or update a CLOSED line within START..END.
END is exclusive.  Return non-nil when a change was made."
  (let ((existing (org-todo--existing-closed-line-range start end))
        (line (concat "CLOSED: " (org-todo--timestamp-string) "\n")))
    (if existing
        (progn
          (delete-region (car existing) (cdr existing))
          (goto-char (car existing))
          (insert line)
          t)
      (goto-char start)
      (insert line)
      t)))

(defun org-todo--maybe-log-done-transition (old-keyword new-keyword)
  "Insert a CLOSED timestamp for OLD-KEYWORD -> NEW-KEYWORD transitions."
  (when (and (eq org-log-done 'time)
             (org-todo--done-keyword-p new-keyword)
             (not (org-todo--done-keyword-p old-keyword)))
    (save-excursion
      (let ((logbook (org-todo--direct-logbook-range)))
        (if logbook
            (org-todo--upsert-closed-line
             (plist-get logbook :drawer-content-start)
             (plist-get logbook :drawer-end))
          (let* ((bounds (org-outline--subtree-bounds))
                 (content-start (plist-get bounds :content-start))
                 (end-limit
                  (save-excursion
                    (goto-char content-start)
                    (if (org-outline--heading-at-point-p)
                        content-start
                      (org-outline--line-end-with-newline)))))
            (org-todo--upsert-closed-line content-start end-limit)))))))

;;;; Checkboxes

(defconst org-todo--checkbox-regexp
  "^\\([ \t]*[-+*][ \t]+\\)\\(\\[ \\]\\|\\[X\\]\\)\\(.*\\)$"
  "Regexp matching a minimal Org checkbox line.")

(defun org-todo--checkbox-at-point-p ()
  "Return non-nil when the current line is a checkbox item."
  (save-excursion
    (beginning-of-line)
    (looking-at org-todo--checkbox-regexp)))

(defun org-todo--toggle-checkbox ()
  "Toggle the checkbox on the current line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at org-todo--checkbox-regexp)
      (replace-match
       (concat (match-string 1)
               (if (equal (match-string 2) "[ ]") "[X]" "[ ]")
               (match-string 3))
       t t)
      t)))

;;;; Font-lock integration

(defvar org-todo--font-lock-keywords nil
  "Font-lock keyword definitions installed by `org-todo'.")

(defun org-todo--font-lock-keywords ()
  "Return font-lock keyword specs for current TODO configuration."
  (let ((active (org-todo--regexp-opt (org-todo--active-keywords)))
        (done (org-todo--regexp-opt (org-todo--done-keywords))))
    (delq nil
          (list
           (and active
                (list (concat "^\\*+ \\(" active "\\)\\(?:\\'\\| \\)")
                      '(1 'org-todo-keyword-todo)))
           (and done
                (list (concat "^\\*+ \\(" done "\\)\\(?:\\'\\| \\)")
                      '(1 'org-todo-keyword-done)))))))

(defun org-todo-refresh-font-lock (&optional buf)
  "Install or refresh Org TODO font-lock rules on BUF."
  (when (and (fboundp 'font-lock-add-keywords)
             (fboundp 'font-lock-mode))
    (let ((buffer (or buf (current-buffer))))
      (with-current-buffer buffer
        (setq org-todo--font-lock-keywords (org-todo--font-lock-keywords))
        (setq-local font-lock-defaults '(nil))
        (font-lock-add-keywords nil org-todo--font-lock-keywords 'set)
        (font-lock-mode 1)))))

(defun org-todo--refontify-line ()
  "Re-fontify the current line when font-lock is active."
  (when (and (boundp 'font-lock-mode)
             font-lock-mode
             (fboundp 'font-lock-unfontify-region)
             (fboundp 'font-lock-fontify-region))
    (let ((start (line-beginning-position))
          (end (org-outline--line-end-with-newline)))
      (font-lock-unfontify-region start end)
      (font-lock-fontify-region start end))))

(defun org-todo--org-mode-setup ()
  "Attach TODO-specific UI to the current Org buffer."
  (org-todo-refresh-font-lock (current-buffer)))

(when (fboundp 'add-hook)
  (when (boundp 'org-mode-hook)
    (add-hook 'org-mode-hook #'org-todo--org-mode-setup))
  (when (boundp 'emacs-mode-org-mode-hook)
    (add-hook 'emacs-mode-org-mode-hook #'org-todo--org-mode-setup)))

(define-key org-mode-map (kbd "C-c C-t") #'org-todo)
(define-key org-mode-map (kbd "C-c C-c") #'org-toggle-todo)
(define-key org-mode-map (kbd "C-c ,") #'org-priority)
(define-key org-mode-map (kbd "C-c C-s") #'org-schedule)
(define-key org-mode-map (kbd "C-c C-d") #'org-deadline)

;;;; Public commands

;;;###autoload
(defun org-todo (&optional arg)
  "Cycle or set the TODO keyword on the current heading.
Without ARG, cycle through the configured TODO sequence.
With ARG, prompt for a specific keyword and jump directly to it."
  (interactive "P")
  (org-outline--require-heading)
  (let* ((parts (org-todo--heading-components))
         (current (plist-get parts :keyword))
         (target (if arg
                     (org-todo--read-keyword)
                   (org-todo--next-keyword current)))
         (result nil))
    (save-excursion
      (setq result (org-todo--replace-heading-keyword target))
      (org-todo--maybe-log-done-transition
       (plist-get result :old-keyword)
       (plist-get result :new-keyword))
      (org-todo--refontify-line))
    target))

;;;###autoload
(defun org-toggle-todo ()
  "Toggle Org TODO state at point.
On a heading line this delegates to `org-todo'.  On a checkbox item it
toggles `[ ]' and `[X]'."
  (interactive)
  (cond
   ((org-outline--heading-at-point-p)
    (org-todo))
   ((org-todo--checkbox-at-point-p)
    (org-todo--toggle-checkbox)
    (org-todo--refontify-line))
   (t
    (user-error "Nothing to toggle"))))

;;;###autoload
(defun org-priority (&optional action)
  "Cycle or set the priority cookie on the current Org heading.
Without ACTION, cycle priority as none -> A -> B -> C -> none by default.
When ACTION is a priority character or one-character string, set that
priority.  ACTION values `none', `reset', `remove', `clear', SPC, or 0
remove the priority cookie."
  (interactive)
  (org-outline--require-heading)
  (let* ((parts (org-todo--heading-components))
         (split (org-todo--split-priority (plist-get parts :rest)))
         (target (if action
                     (org-todo--normalize-priority-action action)
                   (org-todo--next-priority (plist-get split :priority)))))
    (save-excursion
      (org-todo--replace-priority target)
      (org-todo--refontify-line))
    target))

;;;###autoload
(defun org-schedule (&optional _arg time)
  "Set the SCHEDULED planning timestamp on the current Org heading."
  (interactive)
  (org-todo--set-planning "SCHEDULED" time))

;;;###autoload
(defun org-deadline (&optional _arg time)
  "Set the DEADLINE planning timestamp on the current Org heading."
  (interactive)
  (org-todo--set-planning "DEADLINE" time))

(defun org-get-scheduled-time (&optional _pom)
  "Return the scheduled time for the current Org heading."
  (org-todo--get-planning-time :scheduled))

(defun org-get-deadline-time (&optional _pom)
  "Return the deadline time for the current Org heading."
  (org-todo--get-planning-time :deadline))

(provide 'emacs-org-todo)

;;; emacs-org-todo.el ends here
