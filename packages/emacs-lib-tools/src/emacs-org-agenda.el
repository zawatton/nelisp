;;; emacs-org-agenda.el --- Lightweight org-agenda for nelisp-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 `docs/design/02-v01-daily-driver.org' §3.3.4 asks for a
;; lightweight org-agenda implementation that is good enough for the
;; v0.1 daily-driver gate:
;;
;; - a shared `*Org Agenda*' buffer
;; - dispatcher commands for agenda / todo / tag match
;; - source jumps back to the originating Org line
;; - simple rescan and day navigation
;;
;; This module deliberately stays narrow.  It reads `org-agenda-files'
;; directly with `insert-file-contents', scans headings plus basic Org
;; metadata, and renders the result into a single buffer.  Advanced
;; dispatcher commands (`L', `B', search views, category filters, etc.)
;; are intentionally out of scope for v0.1.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'emacs-buffer-builtins)
(require 'emacs-command-loop-builtins)
(require 'emacs-fileio)
(require 'emacs-fileio-builtins)
(require 'emacs-keymap-builtins)
(require 'emacs-minibuffer-builtins)
(require 'emacs-mode-builtins)
(require 'emacs-org-outline)

(defgroup org-agenda nil
  "Lightweight Org agenda for nelisp-emacs."
  :group 'applications)

(defcustom org-agenda-files
  (let ((journal (expand-file-name "~/Notes/Cowork/Notes/capture/journals-2026.org"))
        (todo (expand-file-name "~/Cowork/Notes/capture/todo.org")))
    (if (and (file-exists-p journal) (file-exists-p todo))
        (list journal todo)
      nil))
  "List of Org files scanned by `org-agenda'."
  :type '(repeat file)
  :group 'org-agenda)

(defconst org-agenda--buffer-name "*Org Agenda*"
  "Name of the shared agenda buffer.")

(defconst org-agenda--timestamp-regexp
  "<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\(?: [A-Za-z][A-Za-z][A-Za-z]\\)?\\(?: \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?>"
  "Regexp matching the timestamp subset supported by M3.4.")

(defconst org-agenda--tags-regexp
  "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
  "Regexp matching an Org headline tag suffix.")

(defvar org-todo-keywords nil
  "Optional TODO keyword specification from M3.2.
When nil or unbound, `org-agenda' simply reports no TODO headings.")

(defvar org-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-agenda-goto)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "f") #'org-agenda-forward-day)
    (define-key map (kbd "b") #'org-agenda-backward-day)
    map)
  "Keymap for `org-agenda-mode'.")

(defvar org-agenda--state (make-hash-table :test 'eq :weakness nil)
  "Hash table mapping agenda buffers to render metadata.
Each value is a plist with keys:
- `:view'        one of `agenda', `todo', or `match'
- `:start-day'   absolute day number for agenda view
- `:span'        number of days in the rendered agenda span
- `:tag'         normalized tag string for match view
- `:previous'    previously-selected buffer
- `:files'       snapshot of `org-agenda-files' during render")

(defun org-agenda--agenda-buffer ()
  "Return the shared `*Org Agenda*' buffer."
  (get-buffer-create org-agenda--buffer-name))

(defun org-agenda--display-buffer (buffer)
  "Display BUFFER in the current window and return it."
  (if (fboundp 'switch-to-buffer)
      (switch-to-buffer buffer)
    (set-buffer buffer))
  buffer)

(defun org-agenda--normalize-file (path)
  "Return PATH expanded to an absolute filename."
  (expand-file-name path))

(defun org-agenda--agenda-files ()
  "Return the list of readable agenda files."
  (let ((files nil))
    (dolist (file org-agenda-files)
      (when (and (stringp file)
                 (> (length file) 0))
        (push (org-agenda--normalize-file file) files)))
    (nreverse files)))

(defun org-agenda--normalize-tag (tag)
  "Normalize TAG into bare `work' form."
  (let ((text (if (symbolp tag) (symbol-name tag) (or tag ""))))
    (replace-regexp-in-string "\\`:+\\|:+\\'" "" text)))

(defun org-agenda--parse-tags (headline)
  "Return a list of tags parsed from HEADLINE."
  (when (string-match org-agenda--tags-regexp headline)
    (split-string (match-string 1 headline) ":" t)))

(defun org-agenda--trim-right (text)
  "Return TEXT without trailing spaces or tabs."
  (replace-regexp-in-string "[ \t]+\\'" "" text))

(defun org-agenda--heading-text (headline)
  "Return HEADLINE content without stars or trailing tags."
  (let ((text headline))
    (when (string-match org-outline--heading-regexp text)
      (setq text (substring text (match-end 0))))
    (when (string-match org-agenda--tags-regexp text)
      (setq text (substring text 0 (match-beginning 0))))
    (org-agenda--trim-right text)))

(defun org-agenda--strip-todo-prefix (title todo)
  "Return TITLE with leading TODO keyword TODO removed."
  (if (and todo
           (string-match
            (concat "\\`" (regexp-quote todo) "\\(?:[ \t]+\\|\\'\\)") title))
      (org-agenda--trim-right (substring title (match-end 0)))
    title))

(defun org-agenda--flatten-todo-keywords (spec)
  "Flatten Org TODO keyword SPEC into a simple string list."
  (cond
   ((null spec) nil)
   ((stringp spec)
    (unless (string= spec "|")
      (list spec)))
   ((symbolp spec)
    nil)
   ((consp spec)
    (append (org-agenda--flatten-todo-keywords (car spec))
            (org-agenda--flatten-todo-keywords (cdr spec))))
   (t nil)))

(defun org-agenda--todo-keywords ()
  "Return the configured TODO keywords, or nil when unavailable."
  (when (boundp 'org-todo-keywords)
    (delete-dups
     (org-agenda--flatten-todo-keywords org-todo-keywords))))

(defun org-agenda--extract-todo-state (headline)
  "Return HEADLINE's TODO keyword, or nil."
  (let* ((title (org-agenda--heading-text headline))
         (keywords (org-agenda--todo-keywords))
         (word (car (split-string title "[ \t]+" t))))
    (when (and word keywords (member word keywords))
      word)))

(defun org-agenda--today-components ()
  "Return today's Gregorian date as (YEAR MONTH DAY)."
  (let* ((decoded (decode-time (current-time))))
    (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded))))

(defun org-agenda--absolute-day (year month day)
  "Return absolute day count for YEAR MONTH DAY."
  (calendar-absolute-from-gregorian (list month day year)))

(defun org-agenda--today-absolute-day ()
  "Return the absolute day number for today."
  (pcase-let ((`(,year ,month ,day) (org-agenda--today-components)))
    (org-agenda--absolute-day year month day)))

(defun org-agenda--absolute-day-components (absolute-day)
  "Return Gregorian (YEAR MONTH DAY) for ABSOLUTE-DAY."
  (pcase-let ((`(,month ,day ,year)
               (calendar-gregorian-from-absolute absolute-day)))
    (list year month day)))

(defun org-agenda--format-day (absolute-day)
  "Return ABSOLUTE-DAY as YYYY-MM-DD."
  (pcase-let ((`(,year ,month ,day)
               (org-agenda--absolute-day-components absolute-day)))
    (format "%04d-%02d-%02d" year month day)))

(defun org-agenda--format-time-fragment (hour minute)
  "Return a display suffix for HOUR and MINUTE."
  (if (and hour minute)
      (format "%02d:%02d " hour minute)
    ""))

(defun org-agenda--line-number-at-pos (&optional pos)
  "Return 1-based line number at POS."
  (save-excursion
    (goto-char (or pos (point)))
    (1+ (count-lines (point-min) (line-beginning-position)))))

(defun org-agenda--parse-timestamp-match (text)
  "Return a plist for the current `org-agenda--timestamp-regexp' match in TEXT."
  (let* ((year (string-to-number (match-string 1 text)))
         (month (string-to-number (match-string 2 text)))
         (day (string-to-number (match-string 3 text)))
         (hour-str (match-string 4 text))
         (minute-str (match-string 5 text))
         (hour (and hour-str (string-to-number hour-str)))
         (minute (and minute-str (string-to-number minute-str))))
    (list :year year
          :month month
          :day day
          :hour hour
          :minute minute
          :absolute-day (org-agenda--absolute-day year month day))))

(defun org-agenda--collect-timestamps (text)
  "Return timestamp plists parsed from TEXT."
  (let ((start 0)
        (items nil))
    (while (and text
                (string-match org-agenda--timestamp-regexp text start))
      (push (org-agenda--parse-timestamp-match text) items)
      (setq start (match-end 0)))
    (nreverse items)))

(defun org-agenda--heading-plist (file line headline)
  "Return normalized heading metadata for FILE LINE HEADLINE."
  (let* ((todo (org-agenda--extract-todo-state headline))
         (title (org-agenda--strip-todo-prefix
                 (org-agenda--heading-text headline)
                 todo)))
    (list :file file
          :line line
          :headline headline
          :title title
          :todo todo
          :tags (org-agenda--parse-tags headline))))

(defun org-agenda--add-agenda-entry (entries heading kind timestamp)
  "Push agenda entry into ENTRIES from HEADING KIND and TIMESTAMP."
  (push (list :file (plist-get heading :file)
              :line (plist-get heading :line)
              :headline (plist-get heading :headline)
              :title (plist-get heading :title)
              :todo (plist-get heading :todo)
              :tags (plist-get heading :tags)
              :kind kind
              :year (plist-get timestamp :year)
              :month (plist-get timestamp :month)
              :day (plist-get timestamp :day)
              :hour (plist-get timestamp :hour)
              :minute (plist-get timestamp :minute)
              :absolute-day (plist-get timestamp :absolute-day))
        entries)
  entries)

(defun org-agenda--scan-file (file)
  "Scan FILE and return a plist of `:agenda', `:todos', and `:headings'."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((agenda nil)
          (todos nil)
          (headings nil)
          (current-heading nil))
      (while (< (point) (point-max))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-start line-end))
               (line-number (org-agenda--line-number-at-pos line-start)))
          (cond
           ((string-match org-outline--heading-regexp line)
            (setq current-heading
                  (org-agenda--heading-plist file line-number line))
            (push current-heading headings)
            (when (plist-get current-heading :todo)
              (push current-heading todos))
            (when (string-match "SCHEDULED:[ \t]*" line)
              (dolist (timestamp (org-agenda--collect-timestamps line))
                (setq agenda
                      (org-agenda--add-agenda-entry
                       agenda current-heading 'scheduled timestamp))))
            (when (string-match "DEADLINE:[ \t]*" line)
              (dolist (timestamp (org-agenda--collect-timestamps line))
                (setq agenda
                      (org-agenda--add-agenda-entry
                       agenda current-heading 'deadline timestamp)))))
           ((and current-heading (string-match "SCHEDULED:[ \t]*" line))
            (dolist (timestamp (org-agenda--collect-timestamps line))
              (setq agenda
                    (org-agenda--add-agenda-entry
                     agenda current-heading 'scheduled timestamp))))
           ((and current-heading (string-match "DEADLINE:[ \t]*" line))
            (dolist (timestamp (org-agenda--collect-timestamps line))
              (setq agenda
                    (org-agenda--add-agenda-entry
                     agenda current-heading 'deadline timestamp))))))
        (forward-line 1))
      (list :agenda (nreverse agenda)
            :todos (nreverse todos)
            :headings (nreverse headings)))))

(defun org-agenda--scan-files ()
  "Scan `org-agenda-files' and return combined agenda data."
  (let ((agenda nil)
        (todos nil)
        (headings nil))
    (dolist (file (org-agenda--agenda-files))
      (when (file-readable-p file)
        (let ((result (org-agenda--scan-file file)))
          (setq agenda (nconc agenda (plist-get result :agenda)))
          (setq todos (nconc todos (plist-get result :todos)))
          (setq headings (nconc headings (plist-get result :headings))))))
    (list :agenda agenda :todos todos :headings headings)))

(defun org-agenda--entry-time-key (entry)
  "Return a sortable time key for ENTRY."
  (if (plist-get entry :hour)
      (+ (* 60 (plist-get entry :hour))
         (plist-get entry :minute))
    100000))

(defun org-agenda--sort-agenda-entries (entries)
  "Return ENTRIES sorted by day, then time, then title."
  (sort (copy-sequence entries)
        (lambda (left right)
          (let ((day-left (plist-get left :absolute-day))
                (day-right (plist-get right :absolute-day)))
            (if (/= day-left day-right)
                (< day-left day-right)
              (let ((time-left (org-agenda--entry-time-key left))
                    (time-right (org-agenda--entry-time-key right)))
                (if (/= time-left time-right)
                    (< time-left time-right)
                  (string-lessp (plist-get left :title)
                                (plist-get right :title)))))))))

(defun org-agenda--filter-agenda-window (entries start-day span)
  "Return ENTRIES whose dates lie in START-DAY .. START-DAY+SPAN-1."
  (let ((end-day (+ start-day (1- span)))
        (filtered nil))
    (dolist (entry entries)
      (let ((day (plist-get entry :absolute-day)))
        (when (and (>= day start-day) (<= day end-day))
          (push entry filtered))))
    (org-agenda--sort-agenda-entries (nreverse filtered))))

(defun org-agenda--group-by-day (entries)
  "Return ENTRIES grouped as ((ABSOLUTE-DAY ENTRY...) ...)."
  (let ((groups nil)
        (current-day nil)
        (current-items nil))
    (dolist (entry entries)
      (let ((day (plist-get entry :absolute-day)))
        (if (equal day current-day)
            (push entry current-items)
          (when current-day
            (push (list current-day (nreverse current-items)) groups))
          (setq current-day day)
          (setq current-items (list entry)))))
    (when current-day
      (push (list current-day (nreverse current-items)) groups))
    (nreverse groups)))

(defun org-agenda--insert-entry-line (prefix entry)
  "Insert PREFIX followed by a rendered ENTRY line."
  (let ((start (point))
        (time-text (org-agenda--format-time-fragment
                    (plist-get entry :hour)
                    (plist-get entry :minute))))
    (insert prefix time-text)
    (when (plist-get entry :todo)
      (insert (plist-get entry :todo) " "))
    (insert (plist-get entry :title)
            "  ["
            (capitalize (symbol-name (plist-get entry :kind)))
            "]"
            "\n")
    (put-text-property start (point) 'org-agenda-entry entry)))

(defun org-agenda--render-empty (message-text)
  "Insert MESSAGE-TEXT followed by a newline."
  (insert message-text "\n"))

(defun org-agenda--render-agenda-view (data start-day span)
  "Render agenda DATA for START-DAY and SPAN days."
  (let ((entries (org-agenda--filter-agenda-window
                  (plist-get data :agenda) start-day span)))
    (insert (format "Org Agenda: %s +%d days\n\n"
                    (org-agenda--format-day start-day)
                    (1- span)))
    (if entries
        (dolist (group (org-agenda--group-by-day entries))
          (insert (format "%s\n" (org-agenda--format-day (car group))))
          (dolist (entry (cadr group))
            (org-agenda--insert-entry-line "  " entry))
          (insert "\n"))
      (org-agenda--render-empty "No agenda items in range."))))

(defun org-agenda--sort-headings (headings)
  "Return HEADINGS sorted by file, line, and title."
  (sort (copy-sequence headings)
        (lambda (left right)
          (let ((file-left (plist-get left :file))
                (file-right (plist-get right :file))
                (line-left (plist-get left :line))
                (line-right (plist-get right :line)))
            (cond
             ((not (equal file-left file-right))
              (string-lessp file-left file-right))
             ((/= line-left line-right)
              (< line-left line-right))
             (t
              (string-lessp (plist-get left :title)
                            (plist-get right :title))))))))

(defun org-agenda--render-todo-view (data)
  "Render TODO headings from DATA."
  (let ((headings (org-agenda--sort-headings (plist-get data :todos))))
    (insert "Org Agenda: TODOs\n\n")
    (if headings
        (dolist (heading headings)
          (let ((start (point)))
            (insert (or (plist-get heading :todo) "")
                    (if (plist-get heading :todo) " " "")
                    (plist-get heading :title)
                    "\n")
            (put-text-property start (point) 'org-agenda-entry heading)))
      (org-agenda--render-empty "No TODO headings found."))))

(defun org-agenda--matching-headings (data tag)
  "Return headings from DATA that contain TAG."
  (let ((needle (org-agenda--normalize-tag tag))
        (matches nil))
    (dolist (heading (plist-get data :headings))
      (when (member needle (plist-get heading :tags))
        (push heading matches)))
    (org-agenda--sort-headings (nreverse matches))))

(defun org-agenda--render-match-view (data tag)
  "Render tag match headings from DATA for TAG."
  (let ((matches (org-agenda--matching-headings data tag)))
    (insert (format "Org Agenda: tag match %s\n\n" tag))
    (if matches
        (dolist (heading matches)
          (let ((start (point)))
            (insert (plist-get heading :title) "\n")
            (put-text-property start (point) 'org-agenda-entry heading)))
      (org-agenda--render-empty "No matching headings found."))))

(defun org-agenda--record-state (buffer state)
  "Store BUFFER render STATE."
  (puthash buffer state org-agenda--state)
  state)

(defun org-agenda--current-state ()
  "Return render state for the current agenda buffer."
  (let* ((buffer (current-buffer))
         (state (and buffer (gethash buffer org-agenda--state))))
    (or state
        (user-error "Current buffer is not an Org Agenda buffer"))))

(defun org-agenda--render-into-buffer (buffer state)
  "Render agenda BUFFER using STATE and return BUFFER."
  (let ((data (org-agenda--scan-files)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-agenda-mode)
        (setq-local revert-buffer-function #'org-agenda-redo)
        (pcase (plist-get state :view)
          ('agenda
           (org-agenda--render-agenda-view
            data
            (plist-get state :start-day)
            (or (plist-get state :span) 7)))
          ('todo
           (org-agenda--render-todo-view data))
          ('match
           (org-agenda--render-match-view
            data
            (plist-get state :tag)))
          (_
           (user-error "Unknown agenda view")))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (org-agenda--record-state
         buffer
         (plist-put (plist-put state :files (copy-sequence org-agenda-files))
                    :data data))))
    buffer))

(defun org-agenda--open-view (view &optional start-day tag)
  "Open agenda VIEW and render it.
START-DAY is only used for `agenda'.  TAG is only used for `match'."
  (let* ((buffer (org-agenda--agenda-buffer))
         (previous (current-buffer))
         (state (list :view view
                      :start-day (or start-day (org-agenda--today-absolute-day))
                      :span 7
                      :tag (and tag (org-agenda--normalize-tag tag))
                      :previous previous
                      :files (copy-sequence org-agenda-files))))
    (org-agenda--render-into-buffer buffer state)
    (org-agenda--display-buffer buffer)))

(defun org-agenda--read-dispatch-key ()
  "Read and return the agenda dispatcher key."
  (let ((key (read-key "Org agenda [a/t/m]: ")))
    (cond
     ((integerp key) key)
     ((and (vectorp key) (> (length key) 0))
      (aref key 0))
     (t key))))

(defun org-agenda--read-match-tag ()
  "Prompt for a tag string."
  (let ((tag (read-string "Match tag: ")))
    (if (and (stringp tag) (> (length tag) 0))
        tag
      (user-error "Tag must not be empty"))))

(defun org-agenda--ensure-agenda-view ()
  "Return current agenda state, ensuring the view is `agenda'."
  (let ((state (org-agenda--current-state)))
    (unless (eq (plist-get state :view) 'agenda)
      (user-error "Day navigation is only available in agenda view"))
    state))

(defun org-agenda--goto-entry (entry)
  "Visit source ENTRY and move point to its originating line."
  (let ((file (plist-get entry :file))
        (line (plist-get entry :line)))
    (unless (and file line)
      (user-error "Agenda entry has no source location"))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))
    (back-to-indentation)
    (current-buffer)))

(defun org-agenda--entry-at-point ()
  "Return agenda entry plist at point."
  (or (get-text-property (point) 'org-agenda-entry)
      (get-text-property (line-beginning-position) 'org-agenda-entry)
      (user-error "No agenda entry on this line")))

;;;###autoload
(define-derived-mode org-agenda-mode special-mode "Org Agenda"
  "Major mode for the shared `*Org Agenda*' buffer."
  (use-local-map org-agenda-mode-map)
  (setq truncate-lines t))

;;;###autoload
(defun org-agenda (&optional dispatch-key)
  "Dispatch to a lightweight Org agenda view.
When DISPATCH-KEY is non-nil, it should be one of `?a', `?t', or `?m'."
  (interactive)
  (pcase (or dispatch-key (org-agenda--read-dispatch-key))
    ((or ?a "a")
     (org-agenda--open-view 'agenda (org-agenda--today-absolute-day)))
    ((or ?t "t")
     (org-agenda--open-view 'todo))
    ((or ?m "m")
     (org-agenda--open-view 'match nil (org-agenda--read-match-tag)))
    (_
     (user-error "Unsupported agenda command"))))

;;;###autoload
(defun org-agenda-goto ()
  "Jump to the source entry at point."
  (interactive)
  (org-agenda--goto-entry (org-agenda--entry-at-point)))

;;;###autoload
(defun org-agenda-redo (&optional _ignore-auto _noconfirm _preserve-modes)
  "Rebuild the current agenda buffer."
  (interactive)
  (org-agenda--render-into-buffer (current-buffer) (org-agenda--current-state))
  nil)

;;;###autoload
(defun org-agenda-forward-day (&optional n)
  "Move the agenda start date forward by N days."
  (interactive "p")
  (let* ((state (org-agenda--ensure-agenda-view))
         (delta (or n 1)))
    (org-agenda--render-into-buffer
     (current-buffer)
     (plist-put (copy-sequence state)
                :start-day (+ (plist-get state :start-day) delta)))))

;;;###autoload
(defun org-agenda-backward-day (&optional n)
  "Move the agenda start date backward by N days."
  (interactive "p")
  (org-agenda-forward-day (- (or n 1))))

(defun org-agenda--ensure-global-binding ()
  "Install the global `C-c a' binding."
  (let ((map (or (and (fboundp 'current-global-map) (current-global-map))
                 (and (fboundp 'make-sparse-keymap) (make-sparse-keymap)))))
    (when (and map (fboundp 'use-global-map))
      (use-global-map map)
      (define-key map (kbd "C-c a") #'org-agenda))))

(org-agenda--ensure-global-binding)

(provide 'emacs-org-agenda)

;;; emacs-org-agenda.el ends here
