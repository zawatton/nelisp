;;; emacs-org-capture.el --- Lightweight org-capture for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; `docs/design/02-v01-daily-driver.org' §3.3.5 asks for a narrow
;; `org-capture' subset suitable for the v0.1 daily-driver gate.
;;
;; Scope for this module:
;; - template selection from `org-capture-templates'
;; - `entry' templates only
;; - targets: `(file FILE)', `(file+headline FILE HEADLINE)',
;;   `(file+olp FILE OLP...)', `(file+regexp FILE REGEXP)', and
;;   `(file+olp+datetree FILE)'
;; - a small placeholder expander for `%?' / `%%' / `%<...>' / `%t' /
;;   `%T' / `%u' / `%U' / `%a' / `%A' / `%l' / `%L' / `%f' /
;;   `%F' / `%i' / `%n' / `%c' / `%x' / `%k' / `%K'
;; - finalize / abort workflow via a dedicated `*Capture*' buffer
;; - minimal template properties: `:immediate-finish',
;;   `:prepend', `:empty-lines-before', `:empty-lines-after',
;;   and `:empty-lines'
;;
;; The implementation stays deliberately conservative:
;; - file visiting and persistence go through `find-file-noselect' and
;;   `save-buffer' from `emacs-fileio'
;; - heading discovery and subtree bounds reuse `emacs-org-outline'
;; - capture-session metadata lives in a side table keyed by buffer
;;
;; Non-goals for v0.1:
;; - non-`entry' capture types
;; - template property handling beyond the documented minimal subset
;; - clipboard integration beyond the supported source placeholders
;; - the full Org target DSL

;;; Code:

(require 'cl-lib)
(require 'emacs-fileio)
(require 'emacs-keymap-builtins)
(require 'emacs-minibuffer-builtins)
(require 'emacs-mode-builtins)
(require 'emacs-org-outline)
(require 'emacs-time)

(defgroup org-capture nil
  "Lightweight org-capture for nelisp-emacs."
  :group 'applications)

(defcustom org-capture-templates nil
  "Capture template alist.
Each entry has the shape:

  (KEY DESCRIPTION TYPE TARGET TEMPLATE-STRING &rest PROPERTIES)

v0.1 supports TYPE = `entry' only."
  :type '(repeat sexp)
  :group 'org-capture)

(defconst org-capture--buffer-name "*Capture*"
  "Name of the transient capture buffer.")

(defvar org-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-capture-finalize)
    (define-key map (kbd "C-c C-k") #'org-capture-kill)
    map)
  "Local keymap for `org-capture-mode'.")

(defvar org-capture--state (make-hash-table :test 'eq :weakness nil)
  "Hash table mapping capture buffers to session metadata.
Each value is a plist with keys:
- `:template'       chosen template entry
- `:target'         parsed target form
- `:target-file'    expanded target path
- `:parent-level'   level under which the entry will be inserted
- `:origin-buffer'  source buffer at capture start
- `:origin-file'    source file for `%a', if any")

(defun org-capture--buffer ()
  "Return the capture buffer, creating it when needed."
  (get-buffer-create org-capture--buffer-name))

(defun org-capture--menu-prompt ()
  "Return the template-selection prompt string."
  (concat
   "Org capture template:\n"
   (mapconcat
    (lambda (template)
      (format "%s %s"
              (nth 0 template)
              (nth 1 template)))
    org-capture-templates
    "\n")
   "\n"))

(defun org-capture--lookup-template (key)
  "Return the capture template whose KEY matches KEY."
  (cl-find-if
   (lambda (template)
     (equal (nth 0 template) key))
   org-capture-templates))

(defun org-capture--read-template ()
  "Prompt for a capture template and return its template entry."
  (unless org-capture-templates
    (user-error "No org-capture templates configured"))
  (let* ((event (read-key (org-capture--menu-prompt)))
         (key (char-to-string event))
         (template (org-capture--lookup-template key)))
    (or template
        (user-error "No capture template for key %s" key))))

(defun org-capture--template-key (template)
  "Return TEMPLATE's key string."
  (nth 0 template))

(defun org-capture--template-description (template)
  "Return TEMPLATE's description string."
  (nth 1 template))

(defun org-capture--template-type (template)
  "Return TEMPLATE's type symbol."
  (nth 2 template))

(defun org-capture--template-target (template)
  "Return TEMPLATE's target form."
  (nth 3 template))

(defun org-capture--template-string (template)
  "Return TEMPLATE's body string."
  (nth 4 template))

(defun org-capture--template-properties (template)
  "Return TEMPLATE's trailing property list."
  (nthcdr 5 template))

(defun org-capture--template-property (template property)
  "Return TEMPLATE's PROPERTY value from its trailing property list."
  (plist-get (org-capture--template-properties template) property))

(defun org-capture--entry-type-p (template)
  "Return non-nil when TEMPLATE is an `entry' template."
  (eq (org-capture--template-type template) 'entry))

(defun org-capture--ensure-supported-template (template)
  "Validate TEMPLATE for the v0.1 subset."
  (unless (org-capture--entry-type-p template)
    (user-error "Unsupported capture type: %S"
                (org-capture--template-type template)))
  (unless (stringp (org-capture--template-string template))
    (user-error "Capture template must provide a template string"))
  template)

(defun org-capture--target-file (target)
  "Return TARGET's expanded file path."
  (expand-file-name (nth 1 target)))

(defun org-capture--line-number-at-pos (pos)
  "Return the 1-based line number for POS."
  (save-excursion
    (goto-char pos)
    (1+ (count-lines (point-min) (line-beginning-position)))))

(defun org-capture--nelisp-buffer-p (buffer)
  "Return non-nil when BUFFER is a live `nelisp-ec-buffer'."
  (and (fboundp 'nelisp-ec-buffer-p)
       (ignore-errors (nelisp-ec-buffer-p buffer))
       (not (and (fboundp 'nelisp-ec-buffer-killed-p)
                 (ignore-errors
                   (nelisp-ec-buffer-killed-p buffer))))))

(defun org-capture--source-buffer ()
  "Return the best source buffer for capture annotation."
  (or (and (fboundp 'nelisp-ec-current-buffer)
           (let ((buffer (nelisp-ec-current-buffer)))
             (and (org-capture--nelisp-buffer-p buffer)
                  buffer)))
      (current-buffer)))

(defun org-capture--buffer-file-name (buffer)
  "Return BUFFER's visited file name, or nil."
  (cond
   ((org-capture--nelisp-buffer-p buffer)
    (or (and (fboundp 'emacs-fileio--direct-buffer-file-name)
             (emacs-fileio--direct-buffer-file-name buffer))
        (and (boundp 'emacs-fileio--buffer-files)
             (cdr (assq buffer emacs-fileio--buffer-files)))
        (ignore-errors (buffer-file-name buffer))))
   ((bufferp buffer)
    (with-current-buffer buffer
      (buffer-file-name buffer)))
   (t nil)))

(defun org-capture--nelisp-line-number (buffer)
  "Return the current line number in nelisp BUFFER."
  (nelisp-ec-with-current-buffer buffer
    (let* ((point (nelisp-ec-buffer-point buffer))
           (prefix (nelisp-ec-buffer-substring (nelisp-ec-point-min) point))
           (line 1)
           (index 0))
      (while (< index (length prefix))
        (when (= (aref prefix index) ?\n)
          (setq line (1+ line)))
        (setq index (1+ index)))
      line)))

(defun org-capture--buffer-raw-link (buffer)
  "Return a best-effort raw file or buffer link for BUFFER."
  (if (org-capture--nelisp-buffer-p buffer)
      (let ((file (org-capture--buffer-file-name buffer)))
        (if (and (stringp file) (> (length file) 0))
            (format "file:%s::%d"
                    (expand-file-name file)
                    (org-capture--nelisp-line-number buffer))
          (format "buffer:%s::%d"
                  (nelisp-ec-buffer-name buffer)
                  (org-capture--nelisp-line-number buffer))))
    (with-current-buffer buffer
      (let ((file (org-capture--buffer-file-name (current-buffer))))
        (if (and (stringp file) (> (length file) 0))
            (format "file:%s::%d"
                    (expand-file-name file)
                    (org-capture--line-number-at-pos (point)))
          (format "buffer:%s::%d"
                  (buffer-name)
                  (org-capture--line-number-at-pos (point))))))))

(defun org-capture--format-link (raw-link &optional description)
  "Return an Org link for RAW-LINK, optionally with DESCRIPTION."
  (cond
   ((not (and (stringp raw-link) (> (length raw-link) 0))) "")
   ((and (stringp description) (> (length description) 0))
    (format "[[%s][%s]]" raw-link description))
   (t
    (format "[[%s]]" raw-link))))

(defun org-capture--buffer-backlink (buffer)
  "Return a best-effort file or buffer Org link for BUFFER."
  (org-capture--format-link (org-capture--buffer-raw-link buffer)))

(defun org-capture--raw-link ()
  "Return a best-effort raw source link for capture placeholders."
  (let ((source (org-capture--source-buffer)))
    (if (org-capture--nelisp-buffer-p source)
        (org-capture--buffer-raw-link source)
      (let ((link (and (fboundp 'org-store-link)
                       (org-store-link nil nil))))
        (if (and (stringp link) (> (length link) 0))
            link
          (org-capture--buffer-raw-link source))))))

(defun org-capture--backlink ()
  "Return a best-effort source backlink for `%a'."
  (org-capture--format-link (org-capture--raw-link)))

(defun org-capture--prompted-backlink ()
  "Return a best-effort source backlink with prompted description for `%A'."
  (let ((raw-link (org-capture--raw-link)))
    (org-capture--format-link
     raw-link
     (if (and (stringp raw-link) (> (length raw-link) 0))
         (read-string "Link description: ")
       ""))))

(defun org-capture--source-file-basename ()
  "Return the source buffer file basename for `%f', or an empty string."
  (let ((file (org-capture--buffer-file-name (org-capture--source-buffer))))
    (if (and (stringp file) (> (length file) 0))
        (file-name-nondirectory file)
      "")))

(defun org-capture--source-file-path ()
  "Return the source buffer absolute file path for `%F', or an empty string."
  (let ((file (org-capture--buffer-file-name (org-capture--source-buffer))))
    (if (and (stringp file) (> (length file) 0))
        (expand-file-name file)
      "")))

(defun org-capture--source-region-text ()
  "Return active source region text for `%i', or an empty string."
  (let ((source (org-capture--source-buffer)))
    (cond
     ((org-capture--nelisp-buffer-p source)
      "")
     ((bufferp source)
      (with-current-buffer source
        (let ((mark-position (and (boundp 'mark-active)
                                  mark-active
                                  (mark t))))
          (if (and (integerp mark-position)
                   (/= mark-position (point)))
              (buffer-substring-no-properties
               (min mark-position (point))
               (max mark-position (point)))
            ""))))
     (t ""))))

(defun org-capture--user-full-name ()
  "Return `user-full-name' for `%n', or an empty string."
  (if (and (boundp 'user-full-name)
           (stringp user-full-name))
      user-full-name
    ""))

(defun org-capture--current-kill ()
  "Return the current kill text for `%c', or an empty string."
  (let ((text (cond
               ((fboundp 'current-kill)
                (condition-case nil
                    (current-kill 0 t)
                  (error nil)))
               ((and (boundp 'kill-ring)
                     (consp kill-ring))
                (car kill-ring)))))
    (if (stringp text) text "")))

(defun org-capture--clipboard-text ()
  "Return external clipboard text for `%x', or an empty string."
  (let ((text (cond
               ((and (boundp 'interprogram-paste-function)
                     (functionp interprogram-paste-function))
                (condition-case nil
                    (funcall interprogram-paste-function)
                  (error nil)))
               ((fboundp 'gui-get-selection)
                (condition-case nil
                    (gui-get-selection 'CLIPBOARD 'STRING)
                  (error nil))))))
    (if (stringp text) text "")))

(defun org-capture--clock-buffer ()
  "Return the buffer for the active lightweight Org clock, or nil."
  (and (boundp 'org-clock-marker)
       (markerp org-clock-marker)
       (marker-buffer org-clock-marker)))

(defun org-capture--clock-heading ()
  "Return the currently clocked heading for `%k', or an empty string."
  (let ((buffer (org-capture--clock-buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char org-clock-marker)
            (if (org-at-heading-p)
                (org-get-heading t t t)
              "")))
      "")))

(defun org-capture--clock-link ()
  "Return the currently clocked heading link for `%K', or an empty string."
  (let ((buffer (org-capture--clock-buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char org-clock-marker)
            (if (org-at-heading-p)
                (org-capture--format-link
                 (org-capture--buffer-raw-link buffer))
              "")))
      "")))

(defun org-capture--time-string (format-string)
  "Return a time string for FORMAT-STRING using the current time."
  (format-time-string format-string (current-time)))

(defun org-capture--replacement-for-code (code)
  "Return replacement text for placeholder CODE."
  (pcase code
    (?% "%")
    (?t (concat "<" (org-capture--time-string "%Y-%m-%d %a") ">"))
    (?T (concat "<" (org-capture--time-string "%Y-%m-%d %a %H:%M") ">"))
    (?u (concat "[" (org-capture--time-string "%Y-%m-%d %a") "]"))
    (?U (concat "[" (org-capture--time-string "%Y-%m-%d %a %H:%M") "]"))
    (?a (org-capture--backlink))
    (?A (org-capture--prompted-backlink))
    (?l (org-capture--format-link (org-capture--raw-link)))
    (?L (org-capture--raw-link))
    (?f (org-capture--source-file-basename))
    (?F (org-capture--source-file-path))
    (?i (org-capture--source-region-text))
    (?n (org-capture--user-full-name))
    (?c (org-capture--current-kill))
    (?x (org-capture--clipboard-text))
    (?k (org-capture--clock-heading))
    (?K (org-capture--clock-link))
    (?? "")
    (_ "")))

(defun org-capture--expand-placeholders (template-string)
  "Expand placeholders in TEMPLATE-STRING.
Return a plist with keys:
- `:text'   expanded text with `%?' removed
- `:point'  1-based point position to use inside the capture buffer"
  (let ((index 0)
        (length (length template-string))
        (chunks nil)
        (point-pos nil)
        (output-length 0))
    (while (< index length)
      (let ((char (aref template-string index)))
        (if (and (= char ?%)
                 (< (1+ index) length))
            (let ((code (aref template-string (1+ index))))
              (cond
               ((= code ??)
                (unless point-pos
                  (setq point-pos (1+ output-length)))
                (setq index (+ index 2)))
               ((= code ?<)
                (let ((end (cl-position ?> template-string
                                        :start (+ index 2))))
                  (if end
                      (let ((replacement
                             (org-capture--time-string
                              (substring template-string
                                         (+ index 2)
                                         end))))
                        (push replacement chunks)
                        (setq output-length (+ output-length
                                               (length replacement)))
                        (setq index (1+ end)))
                    (let ((replacement
                           (org-capture--replacement-for-code code)))
                      (push replacement chunks)
                      (setq output-length (+ output-length
                                             (length replacement)))
                      (setq index (+ index 2))))))
               (t
                (let ((replacement
                       (org-capture--replacement-for-code code)))
                  (push replacement chunks)
                  (setq output-length (+ output-length
                                         (length replacement))))
                (setq index (+ index 2)))))
          (push (char-to-string char) chunks)
          (setq output-length (1+ output-length))
          (setq index (1+ index)))))
    (list :text (apply #'concat (nreverse chunks))
          :point (or point-pos (1+ output-length)))))

(defun org-capture--shift-entry-template (text parent-level)
  "Shift Org heading lines in TEXT beneath PARENT-LEVEL."
  (let* ((trailing-newline (string-suffix-p "\n" text))
         (body (if trailing-newline
                   (substring text 0 -1)
                 text))
         (lines (if (> (length body) 0)
                    (split-string body "\n")
                  (list "")))
         (shifted
          (mapcar
           (lambda (line)
             (if (string-match "^\\(\\*+\\) \\(.*\\)$" line)
                 (concat
                  (make-string (+ parent-level
                                  (length (match-string 1 line)))
                               ?*)
                  " "
                  (match-string 2 line))
               line))
           lines)))
    (concat (mapconcat #'identity shifted "\n")
            (if trailing-newline "\n" ""))))

(defun org-capture--normalize-entry-template (text point parent-level)
  "Return normalized entry data for TEXT and POINT beneath PARENT-LEVEL.
The result is a plist with keys `:text' and `:point'."
  (let* ((safe-point (max 1 point))
         (prefix (substring text 0 (min (1- safe-point) (length text))))
         (shifted-text (org-capture--shift-entry-template text parent-level))
         (shifted-prefix (org-capture--shift-entry-template prefix parent-level)))
    (list :text shifted-text
          :point (1+ (length shifted-prefix)))))

(defun org-capture--find-headline (headline)
  "Move point to HEADLINE in the current Org buffer and return its level."
  (goto-char (point-min))
  (let ((regexp (format "^\\(\\*+\\) %s$" (regexp-quote headline)))
        (level nil))
    (while (and (not level) (re-search-forward regexp nil t))
      (goto-char (line-beginning-position))
      (setq level (org-outline--heading-level-at-point)))
    level))

(defun org-capture--resolve-headline-parent-level (file headline)
  "Return the level of HEADLINE inside FILE."
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (save-excursion
      (or (org-capture--find-headline headline)
          (user-error "Headline not found in %s: %s" file headline)))))

(defun org-capture--parent-heading-level-at-point ()
  "Return the current or preceding heading level, or 0 before any heading."
  (save-excursion
    (beginning-of-line)
    (cond
     ((org-outline--heading-level-at-point))
     ((re-search-backward "^\\*+ " nil t)
      (org-outline--heading-level-at-point))
     (t 0))))

(defun org-capture--regexp-context (file regexp)
  "Return insertion context for REGEXP in FILE.
The result is a plist with keys `:point' and `:parent-level'."
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (goto-char (point-min))
    (unless (re-search-forward regexp nil t)
      (user-error "Regexp not found in %s: %s" file regexp))
    (list :point (match-beginning 0)
          :parent-level (org-capture--parent-heading-level-at-point))))

(defun org-capture--resolve-target-context (template)
  "Return a plist describing TEMPLATE's target context."
  (let* ((target (org-capture--template-target template))
         (file (org-capture--target-file target)))
    (pcase (car-safe target)
      ('file+headline
       (unless (= (length target) 3)
         (user-error "Invalid file+headline target: %S" target))
       (list :target target
             :target-file file
             :parent-level
             (org-capture--resolve-headline-parent-level file (nth 2 target))))
      ('file+olp
       (unless (>= (length target) 3)
         (user-error "Invalid file+olp target: %S" target))
       (list :target target
             :target-file file
             :parent-level (length (cddr target))))
      ('file+regexp
       (unless (= (length target) 3)
         (user-error "Invalid file+regexp target: %S" target))
       (list :target target
             :target-file file
             :parent-level
             (plist-get
              (org-capture--regexp-context file (nth 2 target))
              :parent-level)))
      ('file+olp+datetree
       (unless (= (length target) 2)
         (user-error "Invalid file+olp+datetree target: %S" target))
       (list :target target
             :target-file file
             :parent-level 3))
      ('file
       (unless (= (length target) 2)
         (user-error "Invalid file target: %S" target))
       (list :target target
             :target-file file
             :parent-level 0))
      (_
       (user-error "Unsupported capture target: %S" target)))))

(defun org-capture--insert-buffer (buffer)
  "Display BUFFER in the current window and return it."
  (if (fboundp 'switch-to-buffer)
      (switch-to-buffer buffer)
    (set-buffer buffer))
  buffer)

(defun org-capture--store-state (buffer plist)
  "Associate BUFFER with capture-state PLIST."
  (puthash buffer plist org-capture--state)
  plist)

(defun org-capture--current-state ()
  "Return the current capture state or signal `user-error'."
  (let* ((buffer (current-buffer))
         (state (gethash buffer org-capture--state)))
    (or state
        (user-error "Current buffer is not a capture buffer"))))

(defun org-capture--kill-capture-buffer (buffer)
  "Kill BUFFER and drop its capture-session state."
  (remhash buffer org-capture--state)
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun org-capture--target-headline-insertion-point (headline &optional properties)
  "Return insertion metadata for HEADLINE in the current buffer.
The result is a plist with keys `:point' and `:parent-level'."
  (save-excursion
    (let ((level (org-capture--find-headline headline)))
      (unless level
        (user-error "Headline not found: %s" headline))
      (let ((bounds (org-outline--subtree-bounds)))
        (list :point (if (plist-get properties :prepend)
                         (plist-get bounds :content-start)
                       (plist-get bounds :subtree-end))
              :parent-level level)))))

(defun org-capture--find-child-heading (title level limit)
  "Return point of child heading TITLE at LEVEL before LIMIT, or nil."
  (let ((regexp (format "^%s %s$"
                        (make-string level ?*)
                        (regexp-quote title)))
        (found nil))
    (save-excursion
      (while (and (not found)
                  (< (point) limit)
                  (re-search-forward regexp limit t))
        (goto-char (line-beginning-position))
        (setq found (point))))
    found))

(defun org-capture--insert-heading-at (position level title)
  "Insert a heading with LEVEL and TITLE at POSITION."
  (goto-char position)
  (unless (or (= (point) (point-min))
              (eq (char-before) ?\n))
    (insert "\n"))
  (let ((start (point)))
    (insert (make-string level ?*) " " title "\n")
    start))

(defun org-capture--ensure-child-heading (title level parent-point)
  "Return point of child heading TITLE with LEVEL under PARENT-POINT.
When absent, create it at the end of the parent subtree."
  (save-excursion
    (goto-char parent-point)
    (let* ((bounds (org-outline--subtree-bounds))
           (content-start (plist-get bounds :content-start))
           (limit (plist-get bounds :subtree-end))
           (found nil))
      (goto-char content-start)
      (setq found (org-capture--find-child-heading title level limit))
      (or found
          (org-capture--insert-heading-at limit level title)))))

(defun org-capture--ensure-top-level-heading (title)
  "Return point of top-level heading TITLE, creating it if needed."
  (save-excursion
    (goto-char (point-min))
    (let ((found (org-capture--find-child-heading title 1 (point-max))))
      (or found
          (org-capture--insert-heading-at (point-max) 1 title)))))

(defun org-capture--ensure-olp (path &optional properties)
  "Return insertion metadata for outline PATH, creating missing headings."
  (let ((level 1)
        (parent-point (org-capture--ensure-top-level-heading (car path))))
    (dolist (title (cdr path))
      (setq level (1+ level))
      (setq parent-point
            (org-capture--ensure-child-heading title level parent-point)))
    (save-excursion
      (goto-char parent-point)
      (let ((bounds (org-outline--subtree-bounds)))
        (list :point (if (plist-get properties :prepend)
                         (plist-get bounds :content-start)
                       (plist-get bounds :subtree-end))
              :parent-level level)))))

(defun org-capture--datetree-components ()
  "Return today's datetree components as a plist."
  (list :year (org-capture--time-string "%Y")
        :month (org-capture--time-string "%Y-%m")
        :day (org-capture--time-string "%Y-%m-%d")))

(defun org-capture--ensure-datetree-day (&optional properties)
  "Return insertion metadata for today's datetree day node."
  (let* ((parts (org-capture--datetree-components))
         (year-point (org-capture--ensure-top-level-heading
                      (plist-get parts :year)))
         (month-point (org-capture--ensure-child-heading
                       (plist-get parts :month)
                       2
                       year-point))
         (day-point (org-capture--ensure-child-heading
                     (plist-get parts :day)
                     3
                     month-point)))
    (save-excursion
      (goto-char day-point)
      (let ((bounds (org-outline--subtree-bounds)))
        (list :point (if (plist-get properties :prepend)
                         (plist-get bounds :content-start)
                       (plist-get bounds :subtree-end))
              :parent-level 3)))))

(defun org-capture--target-insertion-metadata (target &optional properties)
  "Return insertion metadata for TARGET in the current buffer."
  (pcase (car-safe target)
    ('file
     (list :point (if (plist-get properties :prepend)
                      (point-min)
                    (point-max))
           :parent-level 0))
    ('file+headline
     (org-capture--target-headline-insertion-point (nth 2 target) properties))
    ('file+olp
     (org-capture--ensure-olp (cddr target) properties))
    ('file+regexp
     (org-capture--regexp-context (org-capture--target-file target)
                                  (nth 2 target)))
    ('file+olp+datetree
     (org-capture--ensure-datetree-day properties))
    (_
     (user-error "Unsupported capture target: %S" target))))

(defun org-capture--nonnegative-integer (value)
  "Return VALUE as a non-negative integer, or 0 when unsupported."
  (if (and (integerp value)
           (>= value 0))
      value
    0))

(defun org-capture--empty-lines-before (properties)
  "Return blank line count to insert before a captured entry."
  (org-capture--nonnegative-integer
   (plist-get properties :empty-lines-before)))

(defun org-capture--empty-lines-after (properties)
  "Return blank line count to insert after a captured entry."
  (org-capture--nonnegative-integer
   (or (plist-get properties :empty-lines-after)
       (plist-get properties :empty-lines))))

(defun org-capture--insert-entry-at (position text cursor-offset &optional properties)
  "Insert TEXT at POSITION and leave point at CURSOR-OFFSET within it.
PROPERTIES may request lightweight blank-line padding."
  (goto-char position)
  (unless (or (= (point) (point-min))
              (eq (char-before) ?\n))
    (insert "\n"))
  (let ((before (org-capture--empty-lines-before properties)))
    (when (> before 0)
      (insert (make-string before ?\n))))
  (let ((start (point)))
    (insert text)
    (let ((after (org-capture--empty-lines-after properties)))
      (when (> after 0)
        (insert (make-string after ?\n))))
    (goto-char (+ start (max 0 (1- cursor-offset))))
    start))

(defun org-capture--prepare-capture-buffer (template context)
  "Populate and return the `*Capture*' buffer for TEMPLATE and CONTEXT."
  (let* ((expanded (org-capture--expand-placeholders
                    (org-capture--template-string template)))
         (normalized (org-capture--normalize-entry-template
                      (plist-get expanded :text)
                      (plist-get expanded :point)
                      (plist-get context :parent-level)))
         (entry-text (let ((text (plist-get normalized :text)))
                       (if (string-suffix-p "\n" text)
                           text
                         (concat text "\n"))))
         (source-buffer (org-capture--source-buffer))
         (buffer (org-capture--buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert entry-text)
      (goto-char (min (point-max)
                      (plist-get normalized :point)))
      (org-capture-mode)
      (setq buffer-offer-save nil))
    (org-capture--store-state
     buffer
     (list :template template
           :target (plist-get context :target)
           :target-file (plist-get context :target-file)
           :parent-level (plist-get context :parent-level)
           :origin-buffer source-buffer
           :origin-file (org-capture--buffer-file-name source-buffer)
           :properties (org-capture--template-properties template)))
    buffer))

(defun org-capture--capture-buffer-p (buffer)
  "Return non-nil when BUFFER is a live capture buffer."
  (and (buffer-live-p buffer)
       (gethash buffer org-capture--state)))

;;;###autoload
(defun org-capture-mode ()
  "Major mode for the transient capture buffer."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'org-capture-mode)
  (setq mode-name "Org-Capture")
  (use-local-map org-capture-mode-map)
  nil)

;;;###autoload
(defun org-capture (&optional keys)
  "Create a new capture buffer from a configured template.
When KEYS is non-nil, use it as the template key instead of prompting."
  (interactive)
  (let* ((template
          (org-capture--ensure-supported-template
           (or (and keys
                    (org-capture--lookup-template
                     (if (characterp keys)
                         (char-to-string keys)
                       keys)))
               (org-capture--read-template))))
         (context (org-capture--resolve-target-context template))
         (buffer (org-capture--prepare-capture-buffer template context)))
    (if (org-capture--template-property template :immediate-finish)
        (with-current-buffer buffer
          (org-capture-finalize))
      (org-capture--insert-buffer buffer)
      buffer)))

;;;###autoload
(defun org-capture-finalize ()
  "Insert the capture buffer into its target and save the target file."
  (interactive)
  (let* ((capture-buffer (current-buffer))
         (state (org-capture--current-state))
         (target-file (plist-get state :target-file))
         (target (plist-get state :target))
         (properties (plist-get state :properties))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (cursor-offset (point)))
    (with-temp-buffer
      (when (file-exists-p target-file)
        (insert-file-contents target-file))
      (let ((metadata (org-capture--target-insertion-metadata target properties)))
        (org-capture--insert-entry-at
         (plist-get metadata :point)
         text
         cursor-offset
         properties)
        (write-region (point-min) (point-max) target-file nil 'silent)))
    (org-capture--kill-capture-buffer capture-buffer)
    target-file))

;;;###autoload
(defun org-capture-kill ()
  "Abort the active capture session without touching its target."
  (interactive)
  (let* ((buffer (current-buffer))
         (state (org-capture--current-state))
         (origin (plist-get state :origin-buffer)))
    (org-capture--kill-capture-buffer buffer)
    (when (buffer-live-p origin)
      (org-capture--insert-buffer origin))
    nil))

(defun org-capture--ensure-global-bindings ()
  "Install the global `C-c c' binding."
  (let ((map (or (and (fboundp 'current-global-map) (current-global-map))
                 (make-sparse-keymap))))
    (when (and map (fboundp 'use-global-map))
      (use-global-map map)
      (define-key map (kbd "C-c c") #'org-capture))))

(org-capture--ensure-global-bindings)

(provide 'emacs-org-capture)

;;; emacs-org-capture.el ends here
