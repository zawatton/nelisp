;;; emacs-org-outline.el --- Org outline subset for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 daily-driver §3.3.1.
;;
;; This module implements the M3.1 org outline subset:
;;
;; - `org-mode' major mode with `.org' auto-dispatch
;; - heading detection and level parsing
;; - heading insertion / promotion / demotion
;; - local subtree visibility cycling (`org-cycle')
;; - global visibility cycling (`org-shifttab' / `org-global-cycle')
;;
;; The folding backend uses the `invisible' text property consistently.
;; We prefer text properties over overlays here because the repo already
;; ships a text-property substrate in `emacs-buffer.el', while overlay
;; semantics are deferred to later phases.

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-buffer-builtins)
(require 'emacs-keymap-builtins)
(require 'emacs-mode-builtins)

;;;; Constants and state

(defconst org-outline--heading-regexp "^\\(\\*+\\) "
  "Regexp matching an Org heading line.
Group 1 is the raw star prefix, whose length is the heading level.")

(defconst org-outline--invisible-spec 'org-outline
  "Value stored in the `invisible' text property for folded Org text.")

(defvar org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'org-cycle)
    (define-key map (kbd "<tab>") #'org-cycle)
    (define-key map (kbd "<backtab>") #'org-shifttab)
    (define-key map (kbd "S-TAB") #'org-shifttab)
    (define-key map (kbd "M-RET") #'org-insert-heading)
    (define-key map (kbd "M-<return>") #'org-insert-heading)
    (define-key map (kbd "M-<left>") #'org-promote)
    (define-key map (kbd "M-<right>") #'org-demote)
    map)
  "Keymap for `org-mode'.")

(defvar-local org-outline--global-cycle-state 'overview
  "Current buffer-wide visibility state for `org-global-cycle'.")

;;;; Low-level text-property helpers

(defun org-outline--put-invisible (start end)
  "Hide text between START and END with the Org outline invisibility spec."
  (when (< start end)
    (if (and (fboundp 'emacs-buffer-put-text-property)
             (fboundp 'nelisp-ec-buffer-p)
             (ignore-errors (nelisp-ec-buffer-p (current-buffer))))
        (emacs-buffer-put-text-property
         start end 'invisible org-outline--invisible-spec (current-buffer))
      (put-text-property start end 'invisible org-outline--invisible-spec))))

(defun org-outline--remove-invisible (start end)
  "Remove Org outline invisibility from START to END."
  (when (< start end)
    (if (and (fboundp 'emacs-buffer-remove-text-properties)
             (fboundp 'nelisp-ec-buffer-p)
             (ignore-errors (nelisp-ec-buffer-p (current-buffer))))
        (emacs-buffer-remove-text-properties
         start end '(invisible) (current-buffer))
      (remove-text-properties start end '(invisible nil)))))

(defun org-outline--invisible-p (pos)
  "Return non-nil when POS is hidden by Org outline folding."
  (let ((value (if (and (fboundp 'emacs-buffer-get-text-property)
                        (fboundp 'nelisp-ec-buffer-p)
                        (ignore-errors (nelisp-ec-buffer-p (current-buffer))))
                   (emacs-buffer-get-text-property
                    pos 'invisible (current-buffer))
                 (get-text-property pos 'invisible))))
    (or (eq value org-outline--invisible-spec)
        (and (listp value) (memq org-outline--invisible-spec value)))))

(defun org-outline--show-all ()
  "Remove all Org outline invisibility from the current buffer."
  (org-outline--remove-invisible (point-min) (point-max)))

(defun org-outline--ensure-visibility-spec ()
  "Ensure `buffer-invisibility-spec' recognizes Org outline folds."
  (cond
   ((eq buffer-invisibility-spec t)
    t)
   ((null buffer-invisibility-spec)
    (setq-local buffer-invisibility-spec (list org-outline--invisible-spec)))
   ((listp buffer-invisibility-spec)
    (unless (memq org-outline--invisible-spec buffer-invisibility-spec)
      (setq-local buffer-invisibility-spec
                  (cons org-outline--invisible-spec
                        buffer-invisibility-spec))))
   (t
    (setq-local buffer-invisibility-spec
                (list org-outline--invisible-spec buffer-invisibility-spec)))))

;;;; Heading parsing

(defun org-outline--line-string ()
  "Return the current line as a string without text properties."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun org-outline--match-heading-on-line ()
  "Return the match data result for the current line's heading match.
The caller should use `match-string' immediately when this returns
non-nil."
  (string-match org-outline--heading-regexp (org-outline--line-string)))

(defun org-outline--heading-level-at-point ()
  "Return the current line's heading level, or nil when not at a heading."
  (when (org-outline--match-heading-on-line)
    (length (match-string 1 (org-outline--line-string)))))

(defun org-outline--heading-at-point-p ()
  "Return non-nil when point is on a heading line."
  (not (null (org-outline--heading-level-at-point))))

(defun org-outline--require-heading ()
  "Signal `user-error' unless point is on a heading line.
Returns the current heading level."
  (or (org-outline--heading-level-at-point)
      (user-error "Not on an Org heading")))

(defun org-outline--line-end-with-newline ()
  "Return the position just after the current line's terminating newline.
If the current line is the last line without a trailing newline, return
`line-end-position'."
  (let ((line-end (line-end-position)))
    (if (and (< line-end (point-max))
             (eq (char-after line-end) ?\n))
        (1+ line-end)
      line-end)))

(defun org-outline--heading-line-range ()
  "Return (START END) for the current heading line.
END includes the terminating newline when one exists."
  (list (line-beginning-position)
        (org-outline--line-end-with-newline)))

(defun org-outline--next-heading ()
  "Move to the next heading line at any level.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil))
    (forward-line 1)
    (while (and (not found) (< (point) (point-max)))
      (when (org-outline--heading-at-point-p)
        (setq found (point)))
      (unless found
        (forward-line 1)))
    (unless found
      (goto-char origin))
    found))

(defun org-outline--find-next-heading-at-or-above (level)
  "Return point of the next heading whose level is <= LEVEL.
Leaves point at the matching heading when found, else returns nil and
keeps point at the original location."
  (let ((origin (point))
        (found nil))
    (forward-line 1)
    (while (and (not found) (< (point) (point-max)))
      (let ((other-level (org-outline--heading-level-at-point)))
        (when (and other-level (<= other-level level))
          (setq found (point))))
      (unless found
        (forward-line 1)))
    (unless found
      (goto-char origin))
    found))

(defun org-outline--subtree-bounds ()
  "Return plist describing the subtree rooted at the current heading.
The result contains:
- `:heading-start'
- `:heading-end'
- `:content-start'
- `:subtree-end'
- `:level'

`subtree-end' is exclusive."
  (let* ((level (org-outline--require-heading))
         (heading-start (line-beginning-position))
         (heading-end (org-outline--line-end-with-newline))
         (content-start heading-end)
         (subtree-end
          (save-excursion
            (or (and (org-outline--find-next-heading-at-or-above level)
                     (line-beginning-position))
                (point-max)))))
    (list :heading-start heading-start
          :heading-end heading-end
          :content-start content-start
          :subtree-end subtree-end
          :level level)))

(defun org-outline--collect-direct-children (bounds)
  "Return a list of direct child subtree plists for BOUNDS."
  (let ((parent-level (plist-get bounds :level))
        (limit (plist-get bounds :subtree-end))
        (children nil))
    (save-excursion
      (goto-char (plist-get bounds :content-start))
      (while (< (point) limit)
        (let ((level (org-outline--heading-level-at-point)))
          (cond
           ((and level (= level (1+ parent-level)))
            (let ((child (org-outline--subtree-bounds)))
              (push child children)
              (goto-char (plist-get child :subtree-end))))
           (t
            (forward-line 1))))))
    (nreverse children)))

(defun org-outline--line-hidden-p (pos)
  "Return non-nil when POS lies on a hidden line."
  (org-outline--invisible-p pos))

(defun org-outline--subtree-folded-p (bounds)
  "Return non-nil when the subtree described by BOUNDS is folded."
  (let ((content-start (plist-get bounds :content-start))
        (subtree-end (plist-get bounds :subtree-end)))
    (and (< content-start subtree-end)
         (org-outline--line-hidden-p content-start))))

(defun org-outline--subtree-children-visible-p (bounds)
  "Return non-nil when BOUNDS appears to be in the children-visible state."
  (let* ((children (org-outline--collect-direct-children bounds))
         (content-start (plist-get bounds :content-start))
         (subtree-end (plist-get bounds :subtree-end)))
    (and children
         (< content-start subtree-end)
         (org-outline--line-hidden-p content-start)
         (cl-every
          (lambda (child)
            (let ((child-start (plist-get child :heading-start))
                  (child-end (plist-get child :subtree-end))
                  (child-content (plist-get child :content-start)))
              (and (not (org-outline--line-hidden-p child-start))
                   (or (>= child-content child-end)
                       (org-outline--line-hidden-p child-content)))))
          children))))

(defun org-outline--subtree-visible-p (bounds)
  "Return non-nil when BOUNDS appears fully visible."
  (let ((content-start (plist-get bounds :content-start))
        (subtree-end (plist-get bounds :subtree-end)))
    (or (>= content-start subtree-end)
        (not (org-outline--line-hidden-p content-start)))))

;;;; Visibility application

(defun org-outline--fold-subtree (bounds)
  "Fold the subtree described by BOUNDS."
  (org-outline--ensure-visibility-spec)
  (org-outline--remove-invisible
   (plist-get bounds :heading-start)
   (plist-get bounds :subtree-end))
  (org-outline--put-invisible
   (plist-get bounds :content-start)
   (plist-get bounds :subtree-end)))

(defun org-outline--show-subtree (bounds)
  "Show the subtree described by BOUNDS."
  (org-outline--ensure-visibility-spec)
  (org-outline--remove-invisible
   (plist-get bounds :heading-start)
   (plist-get bounds :subtree-end)))

(defun org-outline--show-children (bounds)
  "Show the direct children of the subtree described by BOUNDS.
Body text remains folded."
  (org-outline--ensure-visibility-spec)
  (org-outline--remove-invisible
   (plist-get bounds :heading-start)
   (plist-get bounds :subtree-end))
  (let ((cursor (plist-get bounds :content-start))
        (limit (plist-get bounds :subtree-end))
        (children (org-outline--collect-direct-children bounds)))
    (if (null children)
        (org-outline--fold-subtree bounds)
      (dolist (child children)
        (let ((child-start (plist-get child :heading-start))
              (child-content (plist-get child :content-start))
              (child-end (plist-get child :subtree-end)))
          (org-outline--put-invisible cursor child-start)
          (org-outline--put-invisible child-content child-end)
          (setq cursor child-end)))
      (org-outline--put-invisible cursor limit))))

(defun org-outline--hide-body-lines ()
  "Hide every non-heading line in the current buffer, leaving headings visible."
  (save-excursion
    (goto-char (point-min))
    (let ((segment-start (point-min)))
      (while (< (point) (point-max))
        (if (org-outline--heading-at-point-p)
            (progn
              (org-outline--put-invisible segment-start (line-beginning-position))
              (setq segment-start (org-outline--line-end-with-newline))
              (forward-line 1))
          (forward-line 1)))
      (org-outline--put-invisible segment-start (point-max)))))

(defun org-outline--hide-to-next-heading-for-all-headings ()
  "Hide heading bodies while keeping every heading line visible."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (org-outline--heading-at-point-p)
          (let* ((line-end (org-outline--line-end-with-newline))
                 (next-start
                  (save-excursion
                    (or (and (org-outline--next-heading)
                             (line-beginning-position))
                        (point-max)))))
            (org-outline--put-invisible line-end next-start)
            (goto-char next-start))
        (forward-line 1)))))

(defun org-outline--apply-overview ()
  "Show top-level headings only."
  (org-outline--show-all)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((level (org-outline--heading-level-at-point)))
        (cond
         ((and level (= level 1))
          (org-outline--fold-subtree (org-outline--subtree-bounds))
          (goto-char (plist-get (org-outline--subtree-bounds) :subtree-end)))
         (t
          (forward-line 1)))))))

(defun org-outline--apply-contents ()
  "Show all headings but hide body text."
  (org-outline--show-all)
  (org-outline--hide-to-next-heading-for-all-headings)

  ;; Leading non-heading text before the first heading should stay hidden.
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (catch 'found
             (while (< (point) (point-max))
               (when (org-outline--heading-at-point-p)
                 (throw 'found (line-beginning-position)))
               (forward-line 1))
             nil)))
      (when first-heading
        (org-outline--put-invisible (point-min) first-heading)))))

(defun org-outline--apply-show-all ()
  "Show the full buffer."
  (org-outline--show-all))

(defun org-outline--current-global-cycle-state ()
  "Infer the current global visibility state."
  (or org-outline--global-cycle-state 'overview))

;;;; Editing helpers

(defun org-outline--replace-heading-stars (count)
  "Replace the current heading's star prefix with COUNT stars."
  (let ((text (org-outline--line-string)))
    (unless (string-match org-outline--heading-regexp text)
      (user-error "Not on an Org heading"))
    (let ((new-text (concat (make-string count ?*)
                            (substring text (match-end 1)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert new-text)
      new-text)))

(defun org-outline--move-to-heading-text ()
  "Move point to the text portion of the current heading."
  (beginning-of-line)
  (when (looking-at org-outline--heading-regexp)
    (goto-char (match-end 0))))

;;;; Public commands

;;;###autoload
(define-derived-mode org-mode text-mode "Org"
  "Minimal Org major mode for the v0.1 outline subset."
  (use-local-map org-mode-map)
  (org-outline--ensure-visibility-spec)
  (setq-local org-outline--global-cycle-state 'overview))

;;;###autoload
(defun org-cycle ()
  "Cycle visibility of the subtree at point.
The cycle is:

1. folded
2. children visible
3. subtree visible
4. folded"
  (interactive)
  (let ((bounds (org-outline--subtree-bounds)))
    (cond
     ((org-outline--subtree-children-visible-p bounds)
      (org-outline--show-subtree bounds))
     ((org-outline--subtree-folded-p bounds)
      (org-outline--show-children bounds))
     ((org-outline--subtree-visible-p bounds)
      (org-outline--fold-subtree bounds))
     (t
      (org-outline--fold-subtree bounds)))))

(defalias 'org-global-cycle #'org-shifttab)

;;;###autoload
(defun org-shifttab ()
  "Cycle the visibility of the whole Org buffer.
The cycle is:

1. overview
2. contents
3. show-all
4. overview"
  (interactive)
  (pcase (org-outline--current-global-cycle-state)
    ('overview
     (org-outline--apply-overview)
     (setq org-outline--global-cycle-state 'contents))
    ('contents
     (org-outline--apply-contents)
     (setq org-outline--global-cycle-state 'show-all))
    ('show-all
     (org-outline--apply-show-all)
     (setq org-outline--global-cycle-state 'overview))
    (_
     (org-outline--apply-overview)
     (setq org-outline--global-cycle-state 'contents))))

;;;###autoload
(defun org-insert-heading ()
  "Insert a new heading at the current heading's level.
The new heading is inserted immediately after the current heading line."
  (interactive)
  (let* ((level (org-outline--require-heading))
         (marker (make-string level ?*))
         (insert-at (org-outline--line-end-with-newline)))
    (goto-char insert-at)
    (insert marker " \n")
    (backward-char 1)))

;;;###autoload
(defun org-promote ()
  "Promote the current heading by one level.
Signals `user-error' when called on a top-level heading."
  (interactive)
  (let ((level (org-outline--require-heading)))
    (when (= level 1)
      (user-error "Cannot promote a level-1 heading"))
    (save-excursion
      (org-outline--replace-heading-stars (1- level)))
    (org-outline--move-to-heading-text)))

;;;###autoload
(defun org-demote ()
  "Demote the current heading by one level."
  (interactive)
  (let ((level (org-outline--require-heading)))
    (save-excursion
      (org-outline--replace-heading-stars (1+ level)))
    (org-outline--move-to-heading-text)))

;;;; Auto-mode registration

(defun org-outline--install-auto-mode ()
  "Register `.org' to open in `org-mode' through `auto-mode-alist'."
  (let ((entry '("\\.org\\'" . org-mode)))
    (unless (member entry auto-mode-alist)
      (setq auto-mode-alist (cons entry auto-mode-alist)))))

(org-outline--install-auto-mode)

;;;; Org AST callable shims

(defvar org-footnote-section "Footnotes")
(defvar org-footnote-define-inline nil)
(defvar org-footnote-auto-label t)
(defvar org-footnote-auto-adjust nil)
(defvar org-footnote-fill-after-inline-note-extraction nil)
(defconst org-footnote-re "\\[fn:\\(?:[-_[:word:]]+\\)?[]:]")
(defconst org-footnote-definition-re "^\\[fn:\\([-_[:word:]]+\\)\\]")
(defconst org-footnote-forbidden-blocks '("comment" "example" "export" "src"))

(defvar org-cycle-include-plain-lists t)
(defvar org-list-demote-modify-bullet nil)
(defvar org-plain-list-ordered-item-terminator t)
(defvar org-list-allow-alphabetical nil)
(defvar org-list-two-spaces-after-bullet-regexp nil)
(defvar org-list-automatic-rules '((checkbox . t) (indent . t)))
(defvar org-list-use-circular-motion nil)
(defvar org-checkbox-statistics-hook nil)
(defvar org-checkbox-hierarchical-statistics t)
(defvar org-list-indent-offset 0)
(defvar org-list-forbidden-blocks '("example" "verse" "src" "export"))
(defvar org--item-re-cache nil)
(defvar org-list-checkbox-radio-mode nil)
(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))
(defvar org-entities-user nil)
(defconst org-entities nil)
(defvar org-macro-templates nil)
(defvar org-macro--counter-table nil)
(defvar org-babel-error-buffer-name "*Org-Babel Error Output*")
(defvar org-todo-keyword-faces nil)
(defvar org-tag-faces nil)
(defvar org-level-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8))
(defvar org-tags-special-faces-re nil)
(defconst org-list-end-re "^[ \t]*\n[ \t]*\n")
(defconst org-list-full-item-re
  (concat "^[ \t]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ \t]+\\|$\\)\\)"
          "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
          "\\(?:\\(\\[[ X-]\\]\\)\\(?:[ \t]+\\|$\\)\\)?"
          "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?"))

(unless (fboundp 'org-item-re)
  (defun org-item-re ()
    "Return the correct regular expression for plain lists."
    (or (plist-get
         (plist-get org--item-re-cache org-list-allow-alphabetical)
         org-plain-list-ordered-item-terminator)
        (let* ((term (cond
                      ((eq org-plain-list-ordered-item-terminator t) "[.)]")
                      ((= org-plain-list-ordered-item-terminator ?\)) ")")
                      ((= org-plain-list-ordered-item-terminator ?.) "\\.")
                      (t "[.)]")))
               (alpha (if org-list-allow-alphabetical "\\|[A-Za-z]" ""))
               (re (concat "\\([ \t]*\\([-+]\\|\\(\\([0-9]+" alpha "\\)" term
                           "\\)\\)\\|[ \t]+\\*\\)\\([ \t]+\\|$\\)")))
          (setq org--item-re-cache
                (plist-put
                 org--item-re-cache
                 org-list-allow-alphabetical
                 (plist-put
                  (plist-get org--item-re-cache org-list-allow-alphabetical)
                  org-plain-list-ordered-item-terminator
                  re)))
          re))))

(unless (fboundp 'org-item-beginning-re)
  (defun org-item-beginning-re ()
    "Regexp matching the beginning of a plain list item."
    (concat "^" (org-item-re))))

(unless (fboundp 'org-entities--user-safe-p)
  (defun org-entities--user-safe-p (value)
    "Return non-nil when VALUE has a lightweight Org entity shape."
    (or (null value)
        (and (listp value)
             (cl-every
              (lambda (entry)
                (and (listp entry)
                     (= (length entry) 7)
                     (stringp (nth 0 entry))
                     (string-match-p "\\`[A-Za-z][A-Za-z0-9]*\\'"
                                     (nth 0 entry))
                     (stringp (nth 1 entry))
                     (memq (nth 2 entry) '(nil t))
                     (stringp (nth 3 entry))
                     (stringp (nth 4 entry))
                     (stringp (nth 5 entry))
                     (stringp (nth 6 entry))))
              value)))))

(unless (fboundp 'org-entity-get)
  (defun org-entity-get (name)
    "Get the entity association for NAME from user or built-in lists."
    (or (assoc name org-entities-user)
        (assoc name org-entities))))

(unless (fboundp 'org-entities-create-table)
  (defun org-entities-create-table (&rest _args)
    "Compatibility shim: ignore Org entity table generation."
    nil))

(unless (fboundp 'org-entities-help)
  (defun org-entities-help (&rest _args)
    "Compatibility shim: ignore Org entity help generation."
    nil))

(unless (fboundp 'org-macro--makeargs)
  (defun org-macro--makeargs (template)
    "Compute a lightweight formal arglist for Org macro TEMPLATE."
    (let ((max 0)
          (index 0))
      (while (and (stringp template)
                  (string-match "\\$\\([0-9]+\\)" template index))
        (setq index (match-end 0))
        (setq max (max max (string-to-number (match-string 1 template)))))
      (let ((args '(&rest _)))
        (if (< max 1)
            args
          (while (> max 0)
            (push (intern (format "$%d" max)) args)
            (setq max (1- max)))
          (cons '&optional args))))))

(unless (fboundp 'org-macro--set-templates)
  (defun org-macro--set-templates (templates)
    "Return TEMPLATES with later macro definitions overriding earlier ones."
    (let (new)
      (dolist (entry templates)
        (when (consp entry)
          (let* ((name (car entry))
                 (value (cdr entry))
                 (old (assoc name new)))
            (if old
                (when value
                  (setcdr old value))
              (push (cons name (or value "")) new)))))
      (nreverse new))))

(unless (fboundp 'org-macro--collect-macros)
  (defun org-macro--collect-macros ()
    "Compatibility shim: return no buffer-local Org macro templates."
    nil))

(unless (fboundp 'org-macro--counter-initialize)
  (defun org-macro--counter-initialize ()
    "Initialize the lightweight Org macro counter table."
    (setq org-macro--counter-table (make-hash-table :test #'equal))))

(unless (fboundp 'org-macro--trim)
  (defun org-macro--trim (value)
    "Return VALUE without surrounding spaces when it is a string."
    (when (stringp value)
      (replace-regexp-in-string
       "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" value))))

(unless (fboundp 'org-macro--counter-increment)
  (defun org-macro--counter-increment (name &optional action)
    "Increment Org macro counter NAME according to ACTION."
    (unless (hash-table-p org-macro--counter-table)
      (org-macro--counter-initialize))
    (let* ((key (or (org-macro--trim name) ""))
           (action (org-macro--trim action))
           (value (cond
                   ((or (null action) (string= action ""))
                    (1+ (gethash key org-macro--counter-table 0)))
                   ((string= action "-")
                    (gethash key org-macro--counter-table 1))
                   ((string-match-p "\\`[0-9]+\\'" action)
                    (string-to-number action))
                   (t 1))))
      (puthash key value org-macro--counter-table))))

(unless (fboundp 'org-macro-initialize-templates)
  (defun org-macro-initialize-templates (&optional default)
    "Initialize lightweight Org macro templates from DEFAULT."
    (org-macro--counter-initialize)
    (setq org-macro-templates
          (append (org-macro--set-templates
                   (append default (org-macro--collect-macros)))
                  '(("n" . (lambda (&optional arg1 arg2 &rest _)
                             (org-macro--counter-increment arg1 arg2)))
                    ("time" . (lambda (arg1 &rest _)
                                (format-time-string arg1))))))))

(unless (fboundp 'org-macro--node-property)
  (defun org-macro--node-property (property node)
    "Return PROPERTY from NODE using Org helpers or plist fallback."
    (cond
     ((fboundp 'org-element-property)
      (org-element-property property node))
     ((and (consp node) (plistp (cdr node)))
      (plist-get (cdr node) property))
     ((plistp node)
      (plist-get node property))
     (t nil))))

(unless (fboundp 'org-macro-expand)
  (defun org-macro-expand (macro templates)
    "Expand MACRO using lightweight Org macro TEMPLATES."
    (let ((template (cdr (assoc-string
                          (org-macro--node-property :key macro)
                          templates
                          t))))
      (when template
        (let* ((args (or (org-macro--node-property :args macro) nil))
               (value
                (if (functionp template)
                    (apply template args)
                  (replace-regexp-in-string
                   "\\$[0-9]+"
                   (lambda (placeholder)
                     (or (nth (1- (string-to-number
                                   (substring placeholder 1)))
                              args)
                         ""))
                   template nil 'literal))))
          (format "%s" (or value "")))))))

(unless (fboundp 'org-macro-replace-all)
  (defun org-macro-replace-all (&rest _args)
    "Compatibility shim: ignore in-buffer Org macro replacement."
    nil))

(unless (fboundp 'org-macro-escape-arguments)
  (defun org-macro-escape-arguments (&rest args)
    "Return a comma-separated lightweight macro argument string."
    (mapconcat #'identity args ",")))

(unless (fboundp 'org-macro-extract-arguments)
  (defun org-macro-extract-arguments (s)
    "Split lightweight Org macro argument string S."
    (split-string (or s "") ",")))

(unless (fboundp 'org-macro--get-property)
  (defun org-macro--get-property (property _location)
    "Compatibility shim for Org macro PROPERTY lookup."
    (if (fboundp 'org-entry-get)
        (org-entry-get nil property 'selective)
      nil)))

(dolist (symbol
         '(org-macro--find-keyword-value
           org-macro--find-date
           org-macro--vc-modified-time))
  (unless (fboundp symbol)
    (fset symbol (lambda (&rest _args) nil))))

(unless (fboundp 'org-babel-eval-error-notify)
  (defun org-babel-eval-error-notify (exit-code stderr)
    "Record a lightweight Org Babel evaluation error notification."
    (when (and (stringp stderr)
               (not (string= stderr "")))
      (message "Babel evaluation exited%s"
               (if exit-code
                   (format " with code %S" exit-code)
                 " abnormally")))))

(unless (fboundp 'org-babel-eval)
  (defun org-babel-eval (command query)
    "Run COMMAND on QUERY and return standard output."
    (if (fboundp 'call-process-region)
        (with-temp-buffer
          (insert (or query "") "\n")
          (let ((status (call-process-region
                         (point-min)
                         (point-max)
                         (org-babel--get-shell-file-name)
                         t
                         t
                         nil
                         shell-command-switch
                         command)))
            (when (and (numberp status) (> status 0))
              (org-babel-eval-error-notify status ""))
            (buffer-string)))
      "")))

(unless (fboundp 'org-babel-eval-read-file)
  (defun org-babel-eval-read-file (file)
    "Return FILE contents as a string."
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(unless (fboundp 'org-babel--get-shell-file-name)
  (defun org-babel--get-shell-file-name ()
    "Return a shell executable name for lightweight Babel evaluation."
    (or (and (boundp 'shell-file-name)
             (stringp shell-file-name)
             shell-file-name)
        "/bin/sh")))

(unless (fboundp 'org-babel--write-temp-buffer-input-file)
  (defun org-babel--write-temp-buffer-input-file (input-file)
    "Write the current buffer contents to INPUT-FILE."
    (write-region (point-min) (point-max) input-file nil 'silent)))

(unless (fboundp 'org-babel--shell-command-on-region)
  (defun org-babel--shell-command-on-region (command _error-buffer)
    "Run shell COMMAND with the current buffer as input."
    (if (fboundp 'call-process-region)
        (call-process-region
         (point-min)
         (point-max)
         (org-babel--get-shell-file-name)
         nil
         t
         nil
         shell-command-switch
         command)
      0)))

(unless (fboundp 'org-babel-eval-wipe-error-buffer)
  (defun org-babel-eval-wipe-error-buffer ()
    "Delete contents of the lightweight Org Babel error buffer."
    (when (get-buffer org-babel-error-buffer-name)
      (with-current-buffer org-babel-error-buffer-name
        (delete-region (point-min) (point-max))))))

(unless (fboundp 'org-set-tag-faces)
  (defun org-set-tag-faces (var value)
    "Set lightweight Org tag face VALUE on VAR and cache matching tags."
    (set-default-toplevel-value var value)
    (if value
        (setq org-tags-special-faces-re
              (concat ":" (regexp-opt (mapcar #'car value) t) ":"))
      (setq org-tags-special-faces-re nil))))

(unless (fboundp 'org-footnote-new)
  (defun org-footnote-new (&rest _args)
    "Compatibility shim: ignore Org footnote insertion."
    nil))

(unless (fboundp 'org-footnote-action)
  (defun org-footnote-action (&rest _args)
    "Compatibility shim: ignore Org footnote action."
    nil))

(dolist (symbol
         '(org-footnote-in-valid-context-p
           org-footnote-at-reference-p
           org-footnote-at-definition-p
           org-footnote--allow-reference-p
           org-footnote--clear-footnote-section
           org-footnote--set-label
           org-footnote--collect-references
           org-footnote--collect-definitions
           org-footnote--goto-local-insertion-point
           org-footnote-get-next-reference
           org-footnote-next-reference-or-definition
           org-footnote-goto-definition
           org-footnote-goto-previous-reference
           org-footnote-normalize-label
           org-footnote-get-definition
           org-footnote-all-labels
           org-footnote-unique-label
           org-footnote-new
           org-footnote-create-definition
           org-footnote-delete-references
           org-footnote-delete-definitions
           org-footnote-delete
           org-footnote-renumber-fn:N
           org-footnote-sort
           org-footnote-normalize
           org-footnote-auto-adjust-maybe
           org-footnote-action))
  (unless (fboundp symbol)
    (fset symbol (lambda (&rest _args) nil))))

(dolist (symbol
         '(org-list-at-regexp-after-bullet-p
           org-list-in-valid-context-p
           org-in-item-p
           org-at-item-p
           org-at-item-bullet-p
           org-at-item-timer-p
           org-at-item-description-p
           org-at-item-checkbox-p
           org-at-item-counter-p
           org-list-context
           org-list-struct
           org-list-struct-assoc-end
           org-list-prevs-alist
           org-list-parents-alist
           org-list--delete-metadata
           org-list-get-nth
           org-list-set-nth
           org-list-get-ind
           org-list-set-ind
           org-list-get-bullet
           org-list-set-bullet
           org-list-get-counter
           org-list-get-checkbox
           org-list-set-checkbox
           org-list-get-tag
           org-list-get-item-end
           org-list-get-item-end-before-blank
           org-list-get-parent
           org-list-has-child-p
           org-list-get-next-item
           org-list-get-prev-item
           org-list-get-subtree
           org-list-get-all-items
           org-list-get-children
           org-list-get-top-point
           org-list-get-bottom-point
           org-list-get-first-item
           org-list-get-list-begin
           org-list-get-last-item
           org-list-get-list-end
           org-list-get-list-type
           org-list-get-item-number
           org-list-search-generic
           org-list-search-backward
           org-list-search-forward
           org-list-bullet-string
           org-list-swap-items
           org-list-separating-blank-lines-number
           org-list-insert-item
           org-list-delete-item
           org-list-send-item
           org-list-struct-outdent
           org-list-struct-indent
           org-list-use-alpha-bul-p
           org-list-inc-bullet-maybe
           org-list-struct-fix-bul
           org-list-struct-fix-ind
           org-list-struct-fix-box
           org-list-struct-fix-item-end
           org-list-struct-apply-struct
           org-list-write-struct
           org-apply-on-list
           org-list-set-item-visibility
           org-list-item-body-column
           org-list-get-item-begin
           org-list-checkbox-radio-mode
           org-beginning-of-item
           org-beginning-of-item-list
           org-end-of-item-list
           org-end-of-item
           org-previous-item
           org-next-item
           org-move-item-down
           org-move-item-up
           org-insert-item
           org-list-repair
           org-cycle-list-bullet
           org-toggle-radio-button
           org-at-radio-list-p
           org-toggle-checkbox
           org-reset-checkbox-state-subtree
           org-update-checkbox-count
           org-get-checkbox-statistics-face
           org-update-checkbox-count-maybe
           org-list-indent-item-generic
           org-outdent-item
           org-indent-item
           org-outdent-item-tree
           org-indent-item-tree
           org-cycle-item-indentation
           org-sort-list
           org-toggle-item
           org-list-to-lisp
           org-list-make-subtree
           org-list-to-generic
           org-list--depth
           org-list--trailing-newlines
           org-list--generic-eval
           org-list--to-generic-plain-list
           org-list--to-generic-item
           org-list-to-latex
           org-list-to-html
           org-list-to-texinfo
           org-list-to-org
           org-list-to-subtree))
  (unless (fboundp symbol)
    (fset symbol (lambda (&rest _args) nil))))

(unless (fboundp 'org-element-properties-resolve)
  (defun org-element-properties-resolve (node &optional _force-undefer)
    "Compatibility shim: return NODE with deferred Org AST properties ignored.
The full vendored Org AST resolver is elided during standalone replay because
its deferred-property helpers exceed the current evaluator envelope.  Keeping
this callable preserves the runtime surface for lightweight Org consumers."
    node))

(unless (fboundp 'org-element-set-contents)
  (defun org-element-set-contents (node &rest contents)
    "Compatibility shim: set NODE's contents to CONTENTS.
For typed Org AST nodes, replace the cddr contents and return NODE.  For
anonymous nodes, replace the list contents.  For plain strings or nil, return
CONTENTS, matching the non-mutating fallback shape."
    (cond
     ((or (stringp node) (null node))
      contents)
     ((and (consp node) (symbolp (car node)))
      (setcdr (cdr node) contents)
      node)
     ((consp node)
      (setcar node (car contents))
      (setcdr node (cdr contents))
      node)
     (t contents))))

(unless (fboundp 'org-element-create)
  (defun org-element-create (type &optional props &rest children)
    "Compatibility shim: create a lightweight Org AST node."
    (if (or (eq type 'plain-text) (stringp type))
        (car children)
      (cons type (cons props children)))))

(unless (fboundp 'org-element-copy)
  (defun org-element-copy (datum &optional _keep-contents)
    "Compatibility shim: return a structural copy of DATUM."
    (cond
     ((consp datum) (copy-tree datum))
     ((stringp datum) (substring datum 0))
     (t datum))))

(unless (fboundp 'org-element--properties-mapc)
  (defun org-element--properties-mapc (_fun _node &optional _collect)
    "Compatibility shim: ignore Org AST property traversal."
    nil))

(unless (fboundp 'org-element-properties-mapc)
  (defun org-element-properties-mapc (fun node &optional undefer)
    "Compatibility shim: map FUN over NODE properties."
    (when undefer
      (org-element-properties-resolve node (eq 'force undefer)))
    (org-element--properties-mapc fun node)))

(unless (fboundp 'org-element-properties-map)
  (defun org-element-properties-map (fun node &optional undefer)
    "Compatibility shim: collect FUN over NODE properties."
    (when undefer
      (org-element-properties-resolve node (eq 'force undefer)))
    (org-element--properties-mapc fun node 'collect)))

(unless (fboundp 'org-element-property-raw)
  (defun org-element-property-raw (property node &optional dflt)
    "Compatibility shim: return PROPERTY from NODE plist."
    (cond
     ((and (listp node) (keywordp (car node)) (plist-member node property))
      (plist-get node property))
     ((and (consp node) (consp (cdr node)))
      (let ((props (cadr node)))
        (if (and (listp props) (plist-member props property))
            (plist-get props property)
          dflt)))
     (t dflt))))

(unless (fboundp 'org-element--property)
  (defun org-element--property
      (property node &optional dflt _force-undefer)
    "Compatibility shim: return PROPERTY from NODE."
    (org-element-property-raw property node dflt)))

(unless (fboundp 'org-element-property)
  (defun org-element-property (property node &optional dflt force-undefer)
    "Compatibility shim: return PROPERTY from NODE."
    (org-element--property property node dflt force-undefer)))

(unless (fboundp 'org-element-put-property)
  (defun org-element-put-property (node property value)
    "Compatibility shim: set PROPERTY to VALUE in NODE plist."
    (when (and (consp node) (consp (cdr node)) (listp (cadr node)))
      (setcar (cdr node) (plist-put (cadr node) property value)))
    node))

(unless (fboundp 'org-element-put-property-2)
  (defun org-element-put-property-2 (property value node)
    "Compatibility shim: set PROPERTY to VALUE in NODE plist."
    (org-element-put-property node property value)))

(unless (fboundp 'org-element-parent)
  (defun org-element-parent (node)
    "Compatibility shim: return NODE parent."
    (org-element-property :parent node)))

(unless (fboundp 'org-element-contents)
  (defun org-element-contents (node)
    "Compatibility shim: return NODE contents."
    (cond
     ((not (consp node)) nil)
     ((symbolp (car node)) (nthcdr 2 node))
     (t node))))

(unless (fboundp 'org-element-resolve-deferred)
  (defalias 'org-element-resolve-deferred 'org-element-properties-resolve))

(unless (fboundp 'org-element-ast-map)
  (defun org-element-ast-map
      (data types fun &optional _ignore first-match _no-recursion
            _with-properties _no-secondary _no-undefer)
    "Compatibility shim: map FUN over DATA nodes whose type is in TYPES."
    (let ((types-list (cond ((eq types t) t)
                            ((listp types) types)
                            (t (list types))))
          (call (if (functionp fun) fun (lambda (_node) (eval fun t))))
          results)
      (cl-labels
          ((walk (node)
             (when (consp node)
               (let ((type (and (symbolp (car node)) (car node))))
                 (when (and type
                            (or (eq types-list t)
                                (memq type types-list)))
                   (let ((value (funcall call node)))
                     (when first-match
                       (cl-return-from org-element-ast-map value))
                     (push value results)))
                 (dolist (child (if type (cddr node) node))
                   (walk child))))))
        (walk data))
      (nreverse results))))

(unless (fboundp 'org-element-lineage)
  (defun org-element-lineage (datum &optional types with-self)
    "Compatibility shim: return DATUM ancestry using `:parent' properties."
    (let ((types-list (and types (if (listp types) types (list types))))
          (node (if with-self datum (and (fboundp 'org-element-property)
                                         (org-element-property :parent datum))))
          results)
      (while node
        (when (or (not types-list)
                  (and (consp node)
                       (memq (car node) types-list)))
          (push node results))
        (setq node (and (fboundp 'org-element-property)
                        (org-element-property :parent node))))
      (nreverse results))))

(unless (fboundp 'org-element-lineage-map)
  (defun org-element-lineage-map
      (datum fun &optional types with-self first-match)
    "Compatibility shim: map FUN across `org-element-lineage'."
    (let ((call (if (functionp fun) fun (lambda (_node) (eval fun t))))
          results)
      (dolist (node (org-element-lineage datum types with-self))
        (let ((value (funcall call node)))
          (when first-match
            (cl-return-from org-element-lineage-map value))
          (when value
            (push value results))))
      (nreverse results))))

(unless (fboundp 'org-element-property-inherited)
  (defun org-element-property-inherited
      (property node &optional with-self _accumulate _literal-nil _include-nil)
    "Compatibility shim: find PROPERTY on NODE or its parents."
    (let ((props (if (listp property) property (list property)))
          (cur (if with-self node
                 (and (fboundp 'org-element-property)
                      (org-element-property :parent node))))
          found)
      (while (and cur (not found))
        (dolist (prop props)
          (let ((value (and (fboundp 'org-element-property)
                            (org-element-property prop cur))))
            (when value
              (setq found value))))
        (setq cur (and (not found)
                       (fboundp 'org-element-property)
                       (org-element-property :parent cur))))
      found)))

(unless (fboundp 'org-element-adopt)
  (defun org-element-adopt (parent &rest children)
    "Compatibility shim: append CHILDREN to PARENT contents."
    (if (null parent)
        children
      (apply #'org-element-set-contents
             parent
             (append (org-element-contents parent) children))
      parent)))

(unless (fboundp 'org-element-adopt-elements)
  (defalias 'org-element-adopt-elements 'org-element-adopt))

(unless (fboundp 'org-element-extract)
  (defun org-element-extract (node)
    "Compatibility shim: return NODE without mutating its parent tree."
    node))

(unless (fboundp 'org-element-extract-element)
  (defalias 'org-element-extract-element 'org-element-extract))

(unless (fboundp 'org-element-insert-before)
  (defun org-element-insert-before (node _location)
    "Compatibility shim: return NODE without mutating the parent tree."
    node))

(unless (fboundp 'org-element-set)
  (defun org-element-set (_old new &optional _keep-props)
    "Compatibility shim: return NEW without mutating the parent tree."
    new))

(unless (fboundp 'org-element-set-element)
  (defalias 'org-element-set-element 'org-element-set))

(unless (featurep 'org-element-ast)
  (provide 'org-element-ast))

(unless (featurep 'org-footnote)
  (provide 'org-footnote))

(unless (featurep 'org-list)
  (provide 'org-list))

(unless (featurep 'org-entities)
  (provide 'org-entities))

(unless (featurep 'org-macro)
  (provide 'org-macro))

(unless (featurep 'ob-eval)
  (provide 'ob-eval))

(provide 'emacs-org-outline)

;;; emacs-org-outline.el ends here
