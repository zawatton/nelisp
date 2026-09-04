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

(declare-function org-get-todo-state "emacs-org-outline")
(declare-function org-element-create "emacs-org-outline")
(declare-function outline-back-to-heading "emacs-org-outline")
(declare-function outline-level "emacs-org-outline")

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
    (define-key map (kbd "M-<up>") #'org-move-subtree-up)
    (define-key map (kbd "M-<down>") #'org-move-subtree-down)
    (define-key map (kbd "M-<left>") #'org-promote)
    (define-key map (kbd "M-<right>") #'org-demote)
    (define-key map (kbd "C-c <") #'org-promote-subtree)
    (define-key map (kbd "C-c >") #'org-demote-subtree)
    (define-key map (kbd "C-c C-n") #'org-next-visible-heading)
    (define-key map (kbd "C-c C-p") #'org-previous-visible-heading)
    (define-key map (kbd "C-c C-f") #'org-forward-heading-same-level)
    (define-key map (kbd "C-c C-b") #'org-backward-heading-same-level)
    (define-key map (kbd "C-c C-u") #'outline-up-heading)
    (define-key map (kbd "C-c C-o") #'org-open-at-point)
    (define-key map (kbd "C-c C-l") #'org-insert-link)
    (define-key map (kbd "C-c C-q") #'org-set-tags-command)
    (define-key map (kbd "C-c C-w") #'org-refile)
    (define-key map (kbd "C-c ^") #'org-sort)
    (define-key map (kbd "C-c C-x p") #'org-set-property)
    (define-key map (kbd "C-x n s") #'org-narrow-to-subtree)
    map)
  "Keymap for `org-mode'.")

(defvar-local org-outline--global-cycle-state 'overview
  "Current buffer-wide visibility state for `org-global-cycle'.")

(defvar org-map-continue-from nil
  "Position from which `org-map-entries' should continue scanning.")

(defvar org-stored-links nil
  "Lightweight list of links stored by `org-store-link'.
Entries have the shape (LINK DESCRIPTION).")

(defcustom org-archive-heading "Archive"
  "Heading title used by lightweight `org-archive-subtree'."
  :type 'string
  :group 'org)

(defcustom org-refile-targets nil
  "Lightweight compatibility variable for Org refile targets."
  :type 'sexp
  :group 'org)

(defvar org-clock-marker nil
  "Marker for the currently clocked lightweight Org heading.")

(defvar org-clock-start-time nil
  "Start time for the currently running lightweight Org clock.")

(defvar org-clock-start-line-marker nil
  "Marker pointing at the active lightweight CLOCK line.")

(defvar org-columns-buffer-name "*Org Columns*"
  "Buffer name used by lightweight `org-columns'.")

(defvar org-export-buffer-name "*Org Export*"
  "Buffer name used by lightweight `org-export-dispatch'.")

;;;; Generic outline substrate

(defvar-local outline-regexp "\\*+ "
  "Regexp matching the start of an outline heading line.")

(defvar outline-level #'outline-level
  "Function returning the level of the current outline heading.")

(defun emacs-outline--line-string ()
  "Return the current line as a string without text properties."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun emacs-outline--match-heading-on-line ()
  "Return non-nil if the current line matches `outline-regexp'."
  (let ((line (emacs-outline--line-string)))
    (and (stringp outline-regexp)
         (string-match outline-regexp line)
         (= (match-beginning 0) 0))))

(defun emacs-outline--heading-level-at-point ()
  "Return current generic outline level, or nil when not on a heading."
  (when (emacs-outline--match-heading-on-line)
    (let ((line (emacs-outline--line-string)))
      (cond
       ((match-beginning 1)
        (length (match-string 1 line)))
       ((match-string 0 line)
        (length (replace-regexp-in-string "[ \t]+\\'" "" (match-string 0 line))))
       (t nil)))))

(defun emacs-outline--heading-at-point-p ()
  "Return non-nil when point is on a generic outline heading line."
  (not (null (emacs-outline--heading-level-at-point))))

(defun emacs-outline--next-heading ()
  "Move to the next generic outline heading line.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil))
    (forward-line 1)
    (while (and (not found) (< (point) (point-max)))
      (when (emacs-outline--heading-at-point-p)
        (setq found (point)))
      (unless found
        (forward-line 1)))
    (unless found
      (goto-char origin))
    found))

(defun emacs-outline--previous-heading ()
  "Move to the previous generic outline heading line.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil)
        (done nil))
    (beginning-of-line)
    (unless (bobp)
      (forward-line -1)
      (while (and (not found) (not done))
        (when (emacs-outline--heading-at-point-p)
          (setq found (point)))
        (unless found
          (if (bobp)
              (setq done t)
            (forward-line -1)))))
    (if found
        (goto-char found)
      (goto-char origin))
    found))

(defun emacs-outline--back-to-heading ()
  "Move to current or preceding generic outline heading.
Return point when found, else nil and restore point."
  (let ((origin (point)))
    (beginning-of-line)
    (cond
     ((emacs-outline--heading-at-point-p)
      (point))
     ((emacs-outline--previous-heading)
      (point))
     (t
      (goto-char origin)
      nil))))

(defun emacs-outline--find-next-heading-at-or-above (level)
  "Return point of the next generic heading whose level is <= LEVEL."
  (let ((origin (point))
        (found nil))
    (forward-line 1)
    (while (and (not found) (< (point) (point-max)))
      (let ((other-level (emacs-outline--heading-level-at-point)))
        (when (and other-level (<= other-level level))
          (setq found (point))))
      (unless found
        (forward-line 1)))
    (unless found
      (goto-char origin))
    found))

(defun emacs-outline--goto-parent-heading ()
  "Move to the parent generic outline heading.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil))
    (unless (emacs-outline--heading-at-point-p)
      (emacs-outline--previous-heading))
    (let ((level (emacs-outline--heading-level-at-point)))
      (when level
        (while (and (not found)
                    (emacs-outline--previous-heading))
          (let ((other-level (emacs-outline--heading-level-at-point)))
            (when (and other-level (< other-level level))
              (setq found (point)))))))
    (unless found
      (goto-char origin))
    found))

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

(defun org-outline--visible-heading-at-point-p ()
  "Return non-nil when point is on a visible Org heading line."
  (and (org-outline--heading-at-point-p)
       (not (org-outline--line-hidden-p (line-beginning-position)))))

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

(defun org-outline--previous-heading ()
  "Move to the previous heading line at any level.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil)
        (done nil))
    (beginning-of-line)
    (unless (bobp)
      (forward-line -1)
      (while (and (not found) (not done))
        (when (org-outline--heading-at-point-p)
          (setq found (point)))
        (unless found
          (if (bobp)
              (setq done t)
            (forward-line -1)))))
    (if found
        (goto-char found)
      (goto-char origin))
    found))

(defun org-outline--next-visible-heading ()
  "Move to the next visible heading line at any level.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil))
    (forward-line 1)
    (while (and (not found) (< (point) (point-max)))
      (when (org-outline--visible-heading-at-point-p)
        (setq found (point)))
      (unless found
        (forward-line 1)))
    (unless found
      (goto-char origin))
    found))

(defun org-outline--previous-visible-heading ()
  "Move to the previous visible heading line at any level.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil)
        (done nil))
    (beginning-of-line)
    (unless (bobp)
      (forward-line -1)
      (while (and (not found) (not done))
        (when (org-outline--visible-heading-at-point-p)
          (setq found (point)))
        (unless found
          (if (bobp)
              (setq done t)
            (forward-line -1)))))
    (if found
        (goto-char found)
      (goto-char origin))
    found))

(defun org-outline--move-visible-heading (count)
  "Move COUNT visible headings and return point.
Negative COUNT moves backward.  Return nil and leave point unchanged when
there are fewer than ABS(COUNT) visible headings in that direction."
  (let ((origin (point))
        (remaining (abs count))
        (mover (if (< count 0)
                   #'org-outline--previous-visible-heading
                 #'org-outline--next-visible-heading))
        (ok t))
    (while (and ok (> remaining 0))
      (setq ok (funcall mover))
      (setq remaining (1- remaining)))
    (unless ok
      (goto-char origin))
    ok))

(defun org-outline--goto-heading-same-level (count)
  "Move COUNT visible headings at the current heading's level.
Negative COUNT moves backward."
  (let ((origin (point))
        (level (org-outline--require-heading))
        (remaining (abs count))
        (mover (if (< count 0)
                   #'org-outline--previous-visible-heading
                 #'org-outline--next-visible-heading)))
    (while (and (> remaining 0) (funcall mover))
      (when (= (org-outline--require-heading) level)
        (setq remaining (1- remaining))))
    (if (= remaining 0)
        (point)
      (goto-char origin)
      nil)))

(defun org-outline--goto-parent-heading ()
  "Move to the visible parent heading of the current heading.
Return point when found, else nil and restore point."
  (let ((origin (point))
        (found nil))
    (unless (org-outline--heading-at-point-p)
      (org-outline--previous-visible-heading))
    (let ((level (org-outline--heading-level-at-point)))
      (when level
        (while (and (not found)
                    (org-outline--previous-visible-heading))
          (let ((other-level (org-outline--heading-level-at-point)))
            (when (and other-level (< other-level level))
              (setq found (point)))))))
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

(defun org-outline--sort-entry-key (child with-case sorting-type getkey-func)
  "Return sort key for CHILD.
WITH-CASE controls string case folding.  SORTING-TYPE accepts the lightweight
subset `?a', `?A', `?n', and `?N'.  GETKEY-FUNC, when non-nil, is called with
point at CHILD's heading."
  (save-excursion
    (goto-char (plist-get child :heading-start))
    (let ((key (if getkey-func
                   (funcall getkey-func)
                 (org-get-heading t t t t))))
      (cond
       ((memq sorting-type '(?n ?N))
        (string-to-number (format "%s" key)))
       ((or with-case (not (stringp key)))
        (format "%s" key))
       (t
        (downcase key))))))

(defun org-outline--sort-child-plist (child with-case sorting-type getkey-func)
  "Return CHILD extended with text and sort key."
  (append child
          (list :sort-key
                (org-outline--sort-entry-key
                 child with-case sorting-type getkey-func)
                :text
                (buffer-substring-no-properties
                 (plist-get child :heading-start)
                 (plist-get child :subtree-end)))))

(defun org-outline--sort-child-less-p (left right sorting-type compare-func)
  "Return non-nil when LEFT should sort before RIGHT."
  (let ((left-key (plist-get left :sort-key))
        (right-key (plist-get right :sort-key))
        (descending (memq sorting-type '(?A ?N))))
    (if compare-func
        (if descending
            (funcall compare-func right-key left-key)
          (funcall compare-func left-key right-key))
      (let ((result
             (if (numberp left-key)
                 (< left-key right-key)
               (string< (format "%s" left-key)
                        (format "%s" right-key)))))
        (if descending
            (not (or result (equal left-key right-key)))
          result)))))

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

(defun org-outline--tag-string-p (text)
  "Return non-nil when TEXT is an Org heading tag suffix."
  (and (stringp text)
       (string-match-p "\\`:[[:alnum:]_@#%:]+:\\'" text)))

(defun org-outline--split-heading-tags ()
  "Return plist describing current heading body and trailing tags.
The plist contains `:body', `:tags', `:body-start', `:tag-start', and
`:line-end'."
  (org-outline--require-heading)
  (let* ((line-end (line-end-position))
         (body-start (save-excursion
                       (beginning-of-line)
                       (looking-at org-outline--heading-regexp)
                       (match-end 0)))
         (text (buffer-substring-no-properties body-start line-end))
         (trimmed-end (string-match "[ \t]*\\'" text))
         (content (substring text 0 trimmed-end))
         (tokens (split-string content "[ \t]+" t))
         (last-token (car (last tokens)))
         (tags nil)
         (tag-start nil)
         (body text))
    (when (org-outline--tag-string-p last-token)
      (setq tags (split-string (substring last-token 1 -1) ":" t))
      (let ((relative (string-match
                       (concat "[ \t]*" (regexp-quote last-token)
                               "[ \t]*\\'")
                       text)))
        (setq tag-start (+ body-start relative))
        (setq body (substring text 0 relative))))
    (list :body (replace-regexp-in-string "[ \t]*\\'" "" body)
          :tags tags
          :body-start body-start
          :tag-start tag-start
          :line-end line-end)))

(defun org-outline--normalize-tags (tags)
  "Return normalized list of Org TAGS."
  (cond
   ((null tags) nil)
   ((stringp tags)
    (split-string (replace-regexp-in-string
                   "\\`[ \t:]+\\|[ \t:]+\\'" "" tags)
                  "[: \t,]+" t))
   ((listp tags)
    (delq nil
          (mapcar (lambda (tag)
                    (let ((text (replace-regexp-in-string
                                 "\\`[ \t:]+\\|[ \t:]+\\'"
                                 "" (format "%s" tag))))
                      (and (> (length text) 0) text)))
                  tags)))
   (t
    (list (format "%s" tags)))))

(defun org-outline--set-heading-tags (tags)
  "Replace current heading's trailing tags with TAGS."
  (let* ((info (org-outline--split-heading-tags))
         (normalized (org-outline--normalize-tags tags))
         (body (plist-get info :body))
         (line-end (plist-get info :line-end))
         (tag-start (plist-get info :tag-start))
         (insert-at (or tag-start line-end))
         (suffix (if normalized
                     (concat " :" (mapconcat #'identity normalized ":") ":")
                   "")))
    (delete-region insert-at line-end)
    (goto-char insert-at)
    (insert suffix)
    (goto-char (plist-get info :body-start))
    (when (> (length body) 0)
      (search-forward body (line-end-position) t))
    normalized))

(defun org-outline--map-match-p (match)
  "Return non-nil when current heading satisfies lightweight MATCH.
This supports nil/t, simple tag names, +tag conjunctions, -tag exclusions,
and TODO=\"KEYWORD\" clauses."
  (cond
   ((or (null match) (eq match t))
    t)
   ((not (stringp match))
    t)
   (t
    (let* ((text (replace-regexp-in-string
                  "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" match))
           (tags (org-get-tags))
           (heading (org-get-heading t nil t))
           (ok t)
           (saw-token nil)
           (pos 0))
      (if (= (length text) 0)
          t
        (while (and ok
                    (string-match
                     "\\([+-]?\\)\\(?:TODO=\"\\([^\"]+\\)\"\\|\\([[:alnum:]_@#%:]+\\)\\)"
                     text pos))
          (let* ((sign (match-string 1 text))
                 (todo (match-string 2 text))
                 (tag (match-string 3 text))
                 (negative (equal sign "-")))
            (setq pos (match-end 0))
            (setq saw-token t)
            (cond
             (todo
              (if negative
                  (when (string-match-p
                         (concat "\\`" (regexp-quote todo) "\\(?:\\'\\| \\)")
                         heading)
                    (setq ok nil))
                (unless (string-match-p
                         (concat "\\`" (regexp-quote todo) "\\(?:\\'\\| \\)")
                         heading)
                  (setq ok nil))))
             (tag
              (if negative
                  (when (member tag tags)
                    (setq ok nil))
                (unless (member tag tags)
                  (setq ok nil)))))))
        (and ok
             (or saw-token
                 (not (string-match-p "[[:alnum:]_@#%:]" text)))))))))

(defun org-outline--map-scope-bounds (scope)
  "Return (START . END) bounds for lightweight `org-map-entries' SCOPE."
  (cond
   ((eq scope 'tree)
    (let ((bounds (org-outline--subtree-bounds)))
      (cons (plist-get bounds :heading-start)
            (plist-get bounds :subtree-end))))
   ((and (eq scope 'region)
         (use-region-p))
    (cons (region-beginning) (region-end)))
   (t
    (cons (point-min) (point-max)))))

(defun org-outline--archive-heading-position ()
  "Return the position of the archive heading, creating it when needed."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward org-outline--heading-regexp nil t))
        (beginning-of-line)
        (when (and (= (org-outline--heading-level-at-point) 1)
                   (equal (org-get-heading t t t) org-archive-heading))
          (setq found (point)))
        (unless found
          (forward-line 1)))
      (unless found
        (goto-char (point-max))
        (unless (or (bobp)
                    (eq (char-before) ?\n))
          (insert "\n"))
        (setq found (point))
        (insert "* " org-archive-heading "\n")))
    found))

(defun org-outline--archive-insertion-point ()
  "Return the insertion point at the end of the archive heading subtree."
  (save-excursion
    (goto-char (org-outline--archive-heading-position))
    (plist-get (org-outline--subtree-bounds) :subtree-end)))

(defun org-outline--clock-time (time)
  "Return TIME or the current time when TIME is nil."
  (or time (current-time)))

(defun org-outline--clock-timestamp (time)
  "Return an inactive Org CLOCK timestamp for TIME."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defun org-outline--clock-duration-minutes (start end)
  "Return whole clock minutes between START and END."
  (max 0 (floor (/ (float-time (time-subtract end start)) 60))))

(defun org-outline--clock-duration-string (start end)
  "Return HH:MM duration between START and END."
  (let* ((minutes (org-outline--clock-duration-minutes start end))
         (hours (/ minutes 60))
         (mins (% minutes 60)))
    (format "%d:%02d" hours mins)))

(defun org-outline--clock-insert-line (start)
  "Insert an open CLOCK line for START under the current heading."
  (let ((insert-at (org-outline--line-end-with-newline)))
    (goto-char insert-at)
    (insert "CLOCK: " (org-outline--clock-timestamp start) "\n")
    (forward-line -1)
    (line-beginning-position)))

(defun org-outline--find-heading-by-title (title)
  "Return position of the first heading whose normalized title is TITLE."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward org-outline--heading-regexp nil t))
        (beginning-of-line)
        (when (equal (org-get-heading t t t) title)
          (setq found (point)))
        (unless found
          (forward-line 1))))
    found))

(defun org-outline--refile-target-position (rfloc)
  "Return target heading position described by RFLOC."
  (cond
   ((and (consp rfloc)
         (integer-or-marker-p (nth 3 rfloc)))
    (nth 3 rfloc))
   ((stringp rfloc)
    (or (org-outline--find-heading-by-title rfloc)
        (user-error "No refile target: %s" rfloc)))
   ((null rfloc)
    (let* ((title (read-string "Refile to heading: "))
           (pos (org-outline--find-heading-by-title title)))
      (or pos
          (user-error "No refile target: %s" title))))
   (t
    (user-error "Unsupported refile location"))))

(defun org-outline--refile-insertion-point (target-marker)
  "Return insertion point at the end of TARGET-MARKER's subtree."
  (save-excursion
    (goto-char target-marker)
    (plist-get (org-outline--subtree-bounds) :subtree-end)))

(defun org-outline--goto-current-heading ()
  "Move to the heading that owns point.
Return point at the heading, or signal `user-error' before the first
heading."
  (unless (org-outline--heading-at-point-p)
    (unless (org-outline--previous-heading)
      (user-error "Before first Org heading")))
  (point))

;;;###autoload
(defun org-at-heading-p (&optional _ignored)
  "Return non-nil when point is on an Org heading line.
The optional argument is accepted for compatibility and ignored by this
lightweight subset."
  (org-outline--heading-at-point-p))

;;;###autoload
(defun org-back-to-heading (&optional _invisible-ok)
  "Move to the heading that owns point and return point.
`_INVISIBLE-OK' is accepted for compatibility and currently ignored."
  (interactive)
  (org-outline--goto-current-heading))

(defun org-outline--remove-leading-todo-keyword (text)
  "Return TEXT without a leading configured TODO keyword when available."
  (if (and (fboundp 'org-todo--all-keywords)
           (string-match "\\`\\([^ \t]+\\)\\(?:[ \t]+\\|\\'\\)" text)
           (member (match-string 1 text) (org-todo--all-keywords)))
      (replace-regexp-in-string
       "\\`[ \t]+" ""
       (substring text (match-end 0)))
    text))

(defun org-outline--remove-leading-priority (text)
  "Return TEXT without a leading Org priority cookie."
  (if (string-match "\\`[ \t]*\\[#.\\][ \t]*" text)
      (substring text (match-end 0))
    text))

;;;###autoload
(defun org-get-heading (&optional no-tags no-todo no-priority no-comment)
  "Return the current Org heading text.
NO-TAGS removes trailing tags.  NO-TODO removes a configured leading TODO
keyword when the TODO module is loaded.  NO-PRIORITY removes a leading
priority cookie.  NO-COMMENT removes a leading COMMENT marker."
  (let* ((info (org-outline--split-heading-tags))
         (text (if no-tags
                   (plist-get info :body)
                 (buffer-substring-no-properties
                  (plist-get info :body-start)
                  (plist-get info :line-end)))))
    (when no-todo
      (setq text (org-outline--remove-leading-todo-keyword text)))
    (when no-priority
      (setq text (org-outline--remove-leading-priority text)))
    (when (and no-comment
               (string-match "\\`COMMENT\\(?:[ \t]+\\|\\'\\)" text))
      (setq text (replace-regexp-in-string
                  "\\`[ \t]+" ""
                  (substring text (match-end 0)))))
    (replace-regexp-in-string "[ \t]*\\'" "" text)))

(defun org-outline--normalize-property-name (property)
  "Return canonical Org PROPERTY name without surrounding colons."
  (let* ((name (format "%s" property))
         (start 0)
         (end (length name)))
    (while (and (< start end) (= (aref name start) ?:))
      (setq start (1+ start)))
    (while (and (< start end) (= (aref name (1- end)) ?:))
      (setq end (1- end)))
    (upcase (substring name start end))))

(defun org-outline--property-drawer-range (&optional create)
  "Return plist for the current heading's property drawer.
When CREATE is non-nil, insert a drawer immediately after the heading if
none exists.  The plist contains `:start', `:content-start',
`:end-line-start', and `:end'."
  (let* ((bounds (org-outline--subtree-bounds))
         (heading-start (plist-get bounds :heading-start))
         (heading-end (plist-get bounds :heading-end))
         (subtree-end (copy-marker (plist-get bounds :subtree-end)))
         (found nil)
         (stopped nil))
    (save-excursion
      (goto-char heading-end)
      (while (and (not found) (not stopped) (< (point) subtree-end))
        (cond
         ((org-outline--heading-at-point-p)
          (setq stopped t))
         ((string-match-p "\\`[ \t]*\\'" (org-outline--line-string))
          (forward-line 1))
         ((string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'"
                          (org-outline--line-string))
          (let ((drawer-start (line-beginning-position)))
            (forward-line 1)
            (let ((content-start (point))
                  (end-line-start nil)
                  (drawer-end nil))
              (while (and (not drawer-end) (< (point) subtree-end))
                (cond
                 ((string-match-p "\\`[ \t]*:END:[ \t]*\\'"
                                  (org-outline--line-string))
                  (setq end-line-start (line-beginning-position))
                  (setq drawer-end (org-outline--line-end-with-newline)))
                 ((org-outline--heading-at-point-p)
                  (user-error "Malformed Org property drawer"))
                 (t
                  (forward-line 1))))
              (unless drawer-end
                (user-error "Malformed Org property drawer"))
              (setq found (list :start drawer-start
                                :content-start content-start
                                :end-line-start end-line-start
                                :end drawer-end)))))
         (t
          (setq stopped t)))))
    (when (and (not found) create)
      (goto-char heading-end)
      (insert ":PROPERTIES:\n:END:\n")
      (goto-char heading-start)
      (setq found (org-outline--property-drawer-range nil)))
    (set-marker subtree-end nil)
    found))

(defun org-outline--property-entry-range (drawer property)
  "Return plist for PROPERTY inside DRAWER, or nil."
  (let ((name (org-outline--normalize-property-name property))
        (found nil))
    (save-excursion
      (goto-char (plist-get drawer :content-start))
      (while (and (not found)
                  (< (point) (plist-get drawer :end-line-start)))
        (let ((line (org-outline--line-string)))
          (when (string-match "\\`[ \t]*:\\([^:\n]+\\):[ \t]*\\(.*\\)\\'" line)
            (when (string= (org-outline--normalize-property-name
                            (match-string 1 line))
                           name)
              (setq found (list :start (line-beginning-position)
                                :end (org-outline--line-end-with-newline)
                                :value (match-string 2 line))))))
        (unless found
          (forward-line 1))))
    found))

(defun org-outline--property-drawer-empty-p (drawer)
  "Return non-nil when DRAWER contains no property lines."
  (let ((empty t))
    (save-excursion
      (goto-char (plist-get drawer :content-start))
      (while (and empty (< (point) (plist-get drawer :end-line-start)))
        (unless (string-match-p "\\`[ \t]*\\'" (org-outline--line-string))
          (setq empty nil))
        (forward-line 1)))
    empty))

(defun org-outline--with-property-heading (pom fn)
  "Call FN at POM's owning heading.
POM may be nil, a marker, or a buffer position."
  (save-excursion
    (when pom
      (cond
       ((markerp pom) (goto-char pom))
       ((integerp pom) (goto-char pom))
       (t (user-error "Unsupported Org property location"))))
    (org-outline--goto-current-heading)
    (funcall fn)))

(defun org-outline--link-at-point ()
  "Return Org bracket link plist at point, or nil.
The plist contains `:start', `:end', `:target', and `:description'."
  (let ((pos (point))
        (limit (line-beginning-position))
        (found nil))
    (save-excursion
      (while (and (not found)
                  (search-backward "[[" limit t))
        (let ((start (point)))
          (when (search-forward "]]" (line-end-position) t)
            (let ((end (point)))
              (when (and (<= start pos) (<= pos end))
                (let* ((inner (buffer-substring-no-properties (+ start 2)
                                                              (- end 2)))
                       (split (string-match "\\]\\[" inner))
                       (target (if split
                                   (substring inner 0 split)
                                 inner))
                       (description (and split
                                         (substring inner (+ split 2)))))
                  (setq found (list :start start
                                    :end end
                                    :target target
                                    :description description)))))))))
    found))

(defun org-outline--link-open-plan (target)
  "Return a lightweight open plan plist for link TARGET."
  (cond
   ((string-match "\\`\\([[:alpha:]][[:alnum:]+.-]*\\):\\(.*\\)\\'" target)
    (let ((scheme (downcase (match-string 1 target)))
          (body (match-string 2 target)))
      (cond
       ((member scheme '("http" "https" "mailto"))
        (list :type 'url :target target))
       ((string= scheme "file")
        (list :type 'file :target body))
       ((string= scheme "id")
        (list :type 'id :target body))
       (t
        (list :type 'unknown :scheme scheme :target target)))))
   (t
    (list :type 'file :target target))))

(defun org-outline--open-link-target (target)
  "Open TARGET when an appropriate host function exists, else return a plan."
  (let ((plan (org-outline--link-open-plan target)))
    (pcase (plist-get plan :type)
      ('url
       (if (fboundp 'browse-url)
           (browse-url target)
         plan))
      ('file
       (if (fboundp 'find-file)
           (find-file (plist-get plan :target))
         plan))
      ('id
       (cond
        ((fboundp 'org-id-open)
         (org-id-open (plist-get plan :target) nil))
        ((fboundp 'org-roam-id-open)
         (org-roam-id-open (plist-get plan :target)))
        (t
         plan)))
      (_ plan))))

(defun org-outline--line-number-at-pos (&optional pos)
  "Return the 1-based line number for POS, or point when POS is nil."
  (save-excursion
    (when pos
      (goto-char pos))
    (1+ (count-lines (point-min) (line-beginning-position)))))

(defun org-outline--store-link-description ()
  "Return a best-effort description for `org-store-link'."
  (or (ignore-errors
        (save-excursion
          (org-outline--goto-current-heading)
          (org-get-heading t t t)))
      (buffer-name)))

(defun org-outline--store-link-target ()
  "Return a best-effort link target for `org-store-link'."
  (let ((existing (org-outline--link-at-point)))
    (if existing
        (plist-get existing :target)
      (let ((file (buffer-file-name)))
        (if (and (stringp file) (> (length file) 0))
            (format "file:%s::%d"
                    (expand-file-name file)
                    (org-outline--line-number-at-pos))
          (format "buffer:%s::%d"
                  (buffer-name)
                  (org-outline--line-number-at-pos)))))))

(defun org-outline--next-sibling-bounds (bounds)
  "Return the next same-level sibling subtree after BOUNDS, or nil."
  (let ((level (plist-get bounds :level))
        (next-start (plist-get bounds :subtree-end)))
    (save-excursion
      (goto-char next-start)
      (when (and (< (point) (point-max))
                 (org-outline--heading-at-point-p)
                 (= (org-outline--heading-level-at-point) level))
        (org-outline--subtree-bounds)))))

(defun org-outline--previous-sibling-bounds (bounds)
  "Return the previous same-level sibling subtree before BOUNDS, or nil."
  (let ((level (plist-get bounds :level))
        (start (plist-get bounds :heading-start))
        (found nil)
        (blocked nil))
    (save-excursion
      (goto-char start)
      (while (and (not found)
                  (not blocked)
                  (org-outline--previous-heading))
        (let ((other-level (org-outline--heading-level-at-point)))
          (cond
           ((= other-level level)
            (let ((candidate (org-outline--subtree-bounds)))
              (when (= (plist-get candidate :subtree-end) start)
                (setq found candidate))))
           ((< other-level level)
            (setq blocked t))))))
    found))

(defun org-outline--swap-adjacent-subtrees (first second follow-second)
  "Swap adjacent FIRST and SECOND subtrees.
When FOLLOW-SECOND is non-nil, leave point at the moved SECOND subtree.
Otherwise leave point at the moved FIRST subtree."
  (let* ((first-start (plist-get first :heading-start))
         (first-end (plist-get first :subtree-end))
         (second-start (plist-get second :heading-start))
         (second-end (plist-get second :subtree-end))
         (first-text (buffer-substring first-start first-end))
         (second-text (buffer-substring second-start second-end)))
    (unless (= first-end second-start)
      (user-error "Org subtrees are not adjacent"))
    (delete-region first-start second-end)
    (goto-char first-start)
    (insert second-text first-text)
    (goto-char (if follow-second
                   first-start
                 (+ first-start (length second-text))))
    (org-outline--move-to-heading-text)
    (point)))

(defun org-outline--relevel-current-subtree (delta)
  "Change every heading in the current subtree by DELTA levels.
Positive DELTA demotes; negative DELTA promotes.  The caller is
responsible for checking that promotion does not move the root above
level 1."
  (let* ((bounds (org-outline--subtree-bounds))
         (start (plist-get bounds :heading-start))
         (end (copy-marker (plist-get bounds :subtree-end)))
         (step (abs delta)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (when (org-outline--heading-at-point-p)
          (beginning-of-line)
          (if (> delta 0)
              (insert (make-string step ?*))
            (delete-char step)))
        (forward-line 1)))
    (set-marker end nil)
    (goto-char start)
    (org-outline--move-to-heading-text)
    (point)))

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
(defun org-narrow-to-subtree ()
  "Narrow the current buffer to the Org subtree at point."
  (interactive)
  (let (start end)
    (save-excursion
      (org-outline--goto-current-heading)
      (let ((bounds (org-outline--subtree-bounds)))
        (setq start (plist-get bounds :heading-start))
        (setq end (plist-get bounds :subtree-end))))
    (if (and (fboundp 'nelisp-ec-narrow-to-region)
             (fboundp 'nelisp-ec-buffer-p)
             (ignore-errors (nelisp-ec-buffer-p (current-buffer))))
        (nelisp-ec-narrow-to-region start end)
      (narrow-to-region start end))
    (point)))

;;;###autoload
(defun org-next-visible-heading (&optional arg)
  "Move to the next visible heading.
With numeric ARG, move forward ARG visible headings.  Negative ARG moves
backward.  Folded-away headings are skipped."
  (interactive "p")
  (let ((count (or arg 1)))
    (unless (or (= count 0)
                (org-outline--move-visible-heading count))
      (user-error "No next visible heading")))
  (point))

;;;###autoload
(defun outline-next-visible-heading (&optional arg)
  "Move to the next visible heading.
In Org buffers this delegates to `org-next-visible-heading'.  In plain
outline buffers it uses `outline-regexp' and treats all headings as visible."
  (interactive "p")
  (if (derived-mode-p 'org-mode)
      (org-next-visible-heading arg)
    (let ((count (or arg 1))
          (ok t))
      (while (and ok (> count 0))
        (setq ok (emacs-outline--next-heading))
        (setq count (1- count)))
      (unless ok
        (user-error "No next visible heading"))
      (point))))

;;;###autoload
(defun org-previous-visible-heading (&optional arg)
  "Move to the previous visible heading.
With numeric ARG, move backward ARG visible headings.  Negative ARG moves
forward.  Folded-away headings are skipped."
  (interactive "p")
  (let ((count (- (or arg 1))))
    (unless (or (= count 0)
                (org-outline--move-visible-heading count))
      (user-error "No previous visible heading")))
  (point))

;;;###autoload
(defun org-forward-heading-same-level (&optional arg)
  "Move to the next visible heading at the same level.
With numeric ARG, move ARG same-level headings.  Negative ARG moves
backward."
  (interactive "p")
  (let ((count (or arg 1)))
    (unless (or (= count 0)
                (org-outline--goto-heading-same-level count))
      (user-error "No same-level heading")))
  (point))

;;;###autoload
(defun org-backward-heading-same-level (&optional arg)
  "Move to the previous visible heading at the same level.
With numeric ARG, move ARG same-level headings backward.  Negative ARG
moves forward."
  (interactive "p")
  (let ((count (- (or arg 1))))
    (unless (or (= count 0)
                (org-outline--goto-heading-same-level count))
      (user-error "No same-level heading")))
  (point))

;;;###autoload
(defun outline-up-heading (&optional arg _invisible-ok)
  "Move to the visible parent heading.
With numeric ARG, move up ARG parent headings.  `_INVISIBLE-OK' is accepted
for compatibility and currently ignored because folded headings stay
hidden in this subset."
  (interactive "p")
  (let ((remaining (or arg 1)))
    (when (< remaining 0)
      (user-error "Negative parent heading movement is not supported"))
    (while (> remaining 0)
      (unless (if (derived-mode-p 'org-mode)
                  (org-outline--goto-parent-heading)
                (emacs-outline--goto-parent-heading))
        (user-error "No parent heading"))
      (setq remaining (1- remaining))))
  (point))

;;;###autoload
(defun org-entry-get (pom property &optional _inherit literal-nil)
  "Return PROPERTY value for the Org entry at POM.
POM may be nil, a marker, or a buffer position.  Inheritance is not
implemented in this lightweight subset.  When LITERAL-NIL is nil, a stored
value of \"nil\" is returned as nil for compatibility."
  (org-outline--with-property-heading
   pom
   (lambda ()
     (let* ((drawer (org-outline--property-drawer-range nil))
            (entry (and drawer
                        (org-outline--property-entry-range drawer property)))
            (value (plist-get entry :value)))
       (if (and value (not literal-nil) (string= value "nil"))
           nil
         value)))))

;;;###autoload
(defun org-entry-put (pom property value)
  "Set PROPERTY to VALUE for the Org entry at POM.
POM may be nil, a marker, or a buffer position.  Return VALUE as a string."
  (let ((text (format "%s" (or value "")))
        (name (org-outline--normalize-property-name property)))
    (org-outline--with-property-heading
     pom
     (lambda ()
       (let* ((drawer (org-outline--property-drawer-range t))
              (entry (org-outline--property-entry-range drawer name))
              (line (format ":%s: %s\n" name text)))
         (if entry
             (progn
               (delete-region (plist-get entry :start)
                              (plist-get entry :end))
               (goto-char (plist-get entry :start))
               (insert line))
           (goto-char (plist-get drawer :end-line-start))
           (insert line)))))
    text))

;;;###autoload
(defun org-entry-delete (pom property)
  "Delete PROPERTY from the Org entry at POM.
POM may be nil, a marker, or a buffer position.  Return nil."
  (org-outline--with-property-heading
   pom
   (lambda ()
     (let* ((drawer (org-outline--property-drawer-range nil))
            (entry (and drawer
                        (org-outline--property-entry-range drawer property))))
       (when entry
         (delete-region (plist-get entry :start)
                        (plist-get entry :end))
         (let ((updated-drawer (org-outline--property-drawer-range nil)))
           (when (and updated-drawer
                      (org-outline--property-drawer-empty-p updated-drawer))
             (delete-region (plist-get updated-drawer :start)
                            (plist-get updated-drawer :end))))))))
  nil)

;;;###autoload
(defun org-set-property (property value)
  "Interactively set Org PROPERTY to VALUE on the current entry."
  (interactive
   (list (read-string "Property: ")
         (read-string "Value: ")))
  (org-entry-put nil property value))

;;;###autoload
(defun org-set-effort (&optional value)
  "Set the current Org entry's EFFORT property to VALUE.
When called interactively, prompt for VALUE.  Return the stored effort string."
  (interactive (list (read-string "Effort: ")))
  (org-entry-put nil "EFFORT" value))

;;;###autoload
(defun org-get-tags (&optional _pos _local)
  "Return tags on the current Org heading.
POS and LOCAL are accepted for compatibility and currently ignored."
  (plist-get (org-outline--split-heading-tags) :tags))

;;;###autoload
(defun org-set-tags-command (&optional tags)
  "Set trailing Org TAGS on the current heading.
TAGS may be a list or a string.  A nil interactive value prompts for a
colon, comma, or whitespace separated tag list."
  (interactive
   (list (read-string "Tags: ")))
  (org-outline--set-heading-tags tags))

;;;###autoload
(defun org-toggle-archive-tag (&optional _find-done)
  "Toggle the ARCHIVE tag on the current Org heading.
FIND-DONE is accepted for compatibility and currently ignored by this
lightweight subset."
  (interactive "P")
  (org-outline--goto-current-heading)
  (let* ((origin (point-marker))
         (tags (org-get-tags))
         (updated (if (member "ARCHIVE" tags)
                      (remove "ARCHIVE" tags)
                    (append tags '("ARCHIVE")))))
    (org-outline--set-heading-tags updated)
    (goto-char origin)
    (set-marker origin nil)
    updated))

(defun org-outline--columns-row ()
  "Return a lightweight columns row for the current Org heading."
  (let ((level (or (org-outline--heading-level-at-point) 0))
        (item (org-get-heading t t t t))
        (tags (mapconcat #'identity (or (org-get-tags) nil) ":"))
        (effort (or (org-entry-get nil "EFFORT") "")))
    (list level item tags effort)))

(defun org-outline--columns-insert-row (row)
  "Insert ROW into the current columns buffer."
  (insert (number-to-string (nth 0 row)) "\t"
          (nth 1 row) "\t"
          (nth 2 row) "\t"
          (nth 3 row) "\n"))

(defun org-outline--export-plain-text ()
  "Return a lightweight plain text export of the current Org buffer."
  (let ((source (buffer-string)))
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) \\(.*\\)$" nil t)
        (replace-match "\\2" nil nil))
      (let ((case-fold-search t))
        (replace-regexp-in-string
         "\\(^\\|\n\\)[ \t]*\\(?:#[+]\\)?RESULTS:[ \t]*"
         "\\1Results:"
         (buffer-string)
         t)))))

;;;###autoload
(defun org-export-dispatch (&optional _arg)
  "Create a lightweight plain text export buffer for the current Org buffer.
This subset returns the export buffer.  Interactive calls also display it."
  (interactive "P")
  (let ((text (org-outline--export-plain-text))
        (buffer (get-buffer-create org-export-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (when (called-interactively-p 'interactive)
      (display-buffer buffer))
    buffer))

;;;###autoload
(defun org-columns (&optional _arg)
  "Create a lightweight Org columns buffer for the current file.
The generated buffer contains tab-separated Level, Item, Tags, and Effort
columns and is returned to the caller."
  (interactive "P")
  (let ((rows (org-map-entries #'org-outline--columns-row nil 'file))
        (buffer (get-buffer-create org-columns-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Level\tItem\tTags\tEffort\n")
      (dolist (row rows)
        (org-outline--columns-insert-row row))
      (goto-char (point-min)))
    (when (called-interactively-p 'interactive)
      (display-buffer buffer))
    buffer))

;;;###autoload
(defun org-map-entries (func &optional match scope &rest _skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.
This lightweight subset supports nil, `file', `tree', and active `region'
scopes.  MATCH may be nil/t, a simple tag expression such as \"+work-next\",
or a TODO=\"KEYWORD\" clause.  Return the collected values from FUNC."
  (let ((bounds (org-outline--map-scope-bounds scope))
        (results nil)
        org-map-continue-from)
    (save-excursion
      (goto-char (car bounds))
      (beginning-of-line)
      (while (and (< (point) (cdr bounds))
                  (re-search-forward org-outline--heading-regexp
                                     (cdr bounds) t))
        (beginning-of-line)
        (when (org-outline--map-match-p match)
          (push (save-excursion
                  (cond
                   ((functionp func)
                    (funcall func))
                   (t
                    (eval func t))))
                results))
        (if (and org-map-continue-from
                 (integer-or-marker-p org-map-continue-from))
            (goto-char org-map-continue-from)
          (end-of-line)
          (forward-char 1))
        (setq org-map-continue-from nil)))
    (nreverse results)))

;;;###autoload
(defun org-clock-in (&optional _select start-time)
  "Start a lightweight Org clock on the current heading.
SELECT is accepted for compatibility and ignored.  START-TIME may be an
Emacs time value; nil uses `current-time'."
  (interactive "P")
  (org-outline--require-heading)
  (when org-clock-marker
    (org-clock-out nil t start-time))
  (let* ((start (org-outline--clock-time start-time))
         (heading (org-get-heading t t t))
         (heading-start (line-beginning-position))
         (line-start (org-outline--clock-insert-line start)))
    (setq org-clock-marker (copy-marker heading-start))
    (setq org-clock-start-line-marker (copy-marker line-start))
    (setq org-clock-start-time start)
    (message "Clock starts at %s - %s"
             (format-time-string "%H:%M" start)
             heading)))

;;;###autoload
(defun org-clock-out (&optional _switch-to-state fail-quietly at-time)
  "Stop the currently running lightweight Org clock.
FAIL-QUIETLY suppresses the user error when no clock is active.  AT-TIME
may be an Emacs time value; nil uses `current-time'."
  (interactive "P")
  (if (not org-clock-marker)
      (unless fail-quietly
        (user-error "No active Org clock"))
    (let* ((end (org-outline--clock-time at-time))
           (duration (org-outline--clock-duration-string
                      org-clock-start-time end))
           (heading (save-excursion
                      (goto-char org-clock-marker)
                      (org-get-heading t t t))))
      (save-excursion
        (goto-char org-clock-start-line-marker)
        (end-of-line)
        (insert "--" (org-outline--clock-timestamp end)
                " => " duration))
      (setq org-clock-marker nil)
      (setq org-clock-start-line-marker nil)
      (setq org-clock-start-time nil)
      (message "Clock stopped after %s - %s" duration heading))))

;;;###autoload
(defun org-clock-get-clock-string ()
  "Return the lightweight mode-line string for the active Org clock."
  (if (not org-clock-marker)
      ""
    (let* ((now (current-time))
           (duration (org-outline--clock-duration-string
                      org-clock-start-time now))
           (heading (save-excursion
                      (goto-char org-clock-marker)
                      (org-get-heading t t t))))
      (format "Clocking: %s (%s)" heading duration))))

;;;###autoload
(defun org-refile (&optional _arg _default-buffer rfloc msg)
  "Move the current subtree under a target heading.
This lightweight subset supports RFLOC as an Org-style
(NAME FILE NIL POSITION) list or as a heading name string in the current
buffer.  ARG, DEFAULT-BUFFER, and cross-file targets are accepted for API
compatibility but ignored."
  (interactive "P")
  (let* ((source-bounds (org-outline--subtree-bounds))
         (source-start (plist-get source-bounds :heading-start))
         (source-end (plist-get source-bounds :subtree-end))
         (source-level (plist-get source-bounds :level))
         (source-heading (org-get-heading t t t))
         (target-position (org-outline--refile-target-position rfloc))
         (target-marker (copy-marker target-position))
         (text (buffer-substring-no-properties source-start source-end))
         target-level
         insert-start
         delta
         verb)
    (when (and (>= (marker-position target-marker) source-start)
               (< (marker-position target-marker) source-end))
      (user-error "Cannot refile a subtree under itself"))
    (save-excursion
      (goto-char target-marker)
      (setq target-level (org-outline--require-heading)))
    (setq delta (- (1+ target-level) source-level))
    (delete-region source-start source-end)
    (goto-char (org-outline--refile-insertion-point target-marker))
    (unless (or (bobp)
                (eq (char-before) ?\n))
      (insert "\n"))
    (setq insert-start (point))
    (insert text)
    (unless (or (= (length text) 0)
                (eq (aref text (1- (length text))) ?\n))
      (insert "\n"))
    (goto-char insert-start)
    (when (/= delta 0)
      (org-outline--relevel-current-subtree delta))
    (goto-char insert-start)
    (setq verb (or msg "Refiled"))
    (message "%s subtree: %s" verb source-heading)))

;;;###autoload
(defun org-archive-subtree (&optional _find-done)
  "Move the current subtree under the lightweight archive heading.
The archive destination is the top-level heading named by
`org-archive-heading' in the current buffer.  FIND-DONE is accepted for
compatibility and currently ignored."
  (interactive "P")
  (let* ((bounds (org-outline--subtree-bounds))
         (level (plist-get bounds :level))
         (heading (org-get-heading t t t))
         (start (plist-get bounds :heading-start))
         (end (plist-get bounds :subtree-end))
         (text (buffer-substring-no-properties start end))
         insert-start)
    (when (and (= level 1)
               (equal heading org-archive-heading))
      (user-error "Cannot archive the archive heading"))
    (delete-region start end)
    (goto-char (org-outline--archive-insertion-point))
    (unless (or (bobp)
                (eq (char-before) ?\n))
      (insert "\n"))
    (setq insert-start (point))
    (insert text)
    (unless (or (= (length text) 0)
                (eq (aref text (1- (length text))) ?\n))
      (insert "\n"))
    (goto-char insert-start)
    (message "Archived subtree: %s" heading)))

;;;###autoload
(defun org-insert-link (target &optional description)
  "Insert an Org bracket link to TARGET.
When DESCRIPTION is non-empty, insert `[[TARGET][DESCRIPTION]]';
otherwise insert `[[TARGET]]'."
  (interactive
   (list (read-string "Link: ")
         (let ((value (read-string "Description: ")))
           (and (> (length value) 0) value))))
  (let ((text (if (and description (> (length description) 0))
                  (format "[[%s][%s]]" target description)
                (format "[[%s]]" target))))
    (insert text)
    text))

;;;###autoload
(defun org-store-link (&optional _arg interactive?)
  "Store a lightweight link to the current location.
When INTERACTIVE? is non-nil, add (LINK DESCRIPTION) to
`org-stored-links'.  When INTERACTIVE? is nil, return the link string
without storing it."
  (interactive (list current-prefix-arg t))
  (let* ((existing (org-outline--link-at-point))
         (link (org-outline--store-link-target))
         (description (or (plist-get existing :description)
                          (org-outline--store-link-description))))
    (when interactive?
      (push (list link description) org-stored-links)
      (message "Stored: %s" link))
    link))

;;;###autoload
(defun org-open-at-point (&optional _arg)
  "Open the Org bracket link at point.
This lightweight subset recognizes `[[target]]' and
`[[target][description]]'.  It delegates to `browse-url', `find-file',
`org-id-open', or `org-roam-id-open' when available.  When no opener is
available, return a plist describing the open plan."
  (interactive "P")
  (let ((link (org-outline--link-at-point)))
    (unless link
      (user-error "No Org link at point"))
    (org-outline--open-link-target (plist-get link :target))))

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

;;;###autoload
(defun org-promote-subtree (&optional arg)
  "Promote every heading in the current subtree by ARG levels.
ARG defaults to 1.  Signals `user-error' if the subtree root would move
above level 1."
  (interactive "p")
  (let* ((count (or arg 1))
         (level (org-outline--require-heading)))
    (cond
     ((< count 0)
      (org-demote-subtree (- count)))
     ((= count 0)
      (point))
     ((<= level count)
      (user-error "Cannot promote subtree above level 1"))
     (t
      (org-outline--relevel-current-subtree (- count))))))

;;;###autoload
(defun org-demote-subtree (&optional arg)
  "Demote every heading in the current subtree by ARG levels.
ARG defaults to 1.  Negative ARG promotes the subtree."
  (interactive "p")
  (let ((count (or arg 1)))
    (cond
     ((< count 0)
      (org-promote-subtree (- count)))
     ((= count 0)
      (point))
     (t
      (org-outline--relevel-current-subtree count)))))

;;;###autoload
(defun org-sort (&optional with-case sorting-type getkey-func compare-func
                           _property _interactive?)
  "Sort direct child subtrees below the current Org heading.
This lightweight subset supports alphabetical `?a' and `?A' plus numeric
`?n' and `?N' sorting.  GETKEY-FUNC and COMPARE-FUNC are accepted for
compatibility; GETKEY-FUNC is called at each child heading, and COMPARE-FUNC
compares the computed keys."
  (interactive (list nil ?a nil nil nil t))
  (let* ((type (or sorting-type ?a))
         (parent (progn
                   (org-outline--goto-current-heading)
                   (point-marker)))
         (children
          (mapcar
           (lambda (child)
             (org-outline--sort-child-plist
              child with-case type getkey-func))
           (org-outline--collect-direct-children
            (org-outline--subtree-bounds)))))
    (when (> (length children) 1)
      (let ((start (plist-get (car children) :heading-start))
            (end (plist-get (car (last children)) :subtree-end))
            (sorted
             (sort (copy-sequence children)
                   (lambda (left right)
                     (org-outline--sort-child-less-p
                      left right type compare-func)))))
        (delete-region start end)
        (goto-char start)
        (dolist (child sorted)
          (insert (plist-get child :text)))))
    (goto-char parent)
    (set-marker parent nil)
    (point)))

;;;###autoload
(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG same-level sibling subtrees.
ARG defaults to 1.  Children and body text move with the subtree."
  (interactive "p")
  (let ((remaining (or arg 1)))
    (if (< remaining 0)
        (org-move-subtree-up (- remaining))
      (while (> remaining 0)
        (let* ((bounds (org-outline--subtree-bounds))
               (next (org-outline--next-sibling-bounds bounds)))
          (unless next
            (user-error "Cannot move subtree down"))
          (org-outline--swap-adjacent-subtrees bounds next nil))
        (setq remaining (1- remaining)))))
  (point))

;;;###autoload
(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG same-level sibling subtrees.
ARG defaults to 1.  Children and body text move with the subtree."
  (interactive "p")
  (let ((remaining (or arg 1)))
    (if (< remaining 0)
        (org-move-subtree-down (- remaining))
      (while (> remaining 0)
        (let* ((bounds (org-outline--subtree-bounds))
               (previous (org-outline--previous-sibling-bounds bounds)))
          (unless previous
            (user-error "Cannot move subtree up"))
          (org-outline--swap-adjacent-subtrees previous bounds t))
        (setq remaining (1- remaining)))))
  (point))

;;;###autoload
(defun org-metaup (&optional arg)
  "Move the current Org subtree up.
This lightweight compatibility command delegates to
`org-move-subtree-up'."
  (interactive "p")
  (org-move-subtree-up arg))

;;;###autoload
(defun org-metadown (&optional arg)
  "Move the current Org subtree down.
This lightweight compatibility command delegates to
`org-move-subtree-down'."
  (interactive "p")
  (org-move-subtree-down arg))

(defun org-outline--babel-src-bounds ()
  "Return plist describing the Org source block at point."
  (save-excursion
    (let ((origin (point))
          begin-start begin-end language body-start body-end end-start end-end)
      (beginning-of-line)
      (unless (looking-at "^[ \t]*#\\+begin_src[ \t]+\\([^ \t\n]+\\)")
        (while (and (not begin-start)
                    (re-search-backward
                     "^[ \t]*#\\+begin_src[ \t]+\\([^ \t\n]+\\)"
                     nil t))
          (let ((candidate-start (line-beginning-position))
                (candidate-end (line-end-position))
                (candidate-language (match-string 1)))
            (when (and (>= origin candidate-start)
                       (save-excursion
                         (forward-line 1)
                         (re-search-forward "^[ \t]*#\\+end_src[ \t]*$"
                                            nil t)
                         (>= (line-end-position) origin)))
              (setq begin-start candidate-start
                    begin-end candidate-end
                    language candidate-language)))))
      (when (and (not begin-start)
                 (looking-at "^[ \t]*#\\+begin_src[ \t]+\\([^ \t\n]+\\)"))
        (setq begin-start (line-beginning-position)
              begin-end (line-end-position)
              language (match-string 1)))
      (unless begin-start
        (user-error "No source block at point"))
      (goto-char begin-start)
      (forward-line 1)
      (setq body-start (point))
      (unless (re-search-forward "^[ \t]*#\\+end_src[ \t]*$" nil t)
        (user-error "Source block has no #+end_src"))
      (setq end-start (line-beginning-position)
            end-end (line-end-position)
            body-end end-start)
      (list :begin-start begin-start
            :begin-end begin-end
            :language (downcase language)
            :body-start body-start
            :body-end body-end
            :end-start end-start
            :end-end end-end))))

(defun org-outline--babel-elisp-result (body)
  "Evaluate BODY as Emacs Lisp and return the last form's value."
  (let ((pos 0)
        form result)
    (while (and (< pos (length body))
                (string-match-p "[^[:space:]]" (substring body pos)))
      (setq form (read-from-string body pos)
            pos (cdr form)
            result (eval (car form) t)))
    result))

(defun org-outline--babel-result-string (result)
  "Return a stable Org Babel result string for RESULT."
  (cond
   ((stringp result) result)
   ((null result) "")
   (t (format "%S" result))))

(defun org-outline--babel-insert-result (bounds result)
  "Insert RESULT after source block BOUNDS."
  (let ((text (org-outline--babel-result-string result)))
    (save-excursion
      (goto-char (plist-get bounds :end-end))
      (unless (eolp)
        (end-of-line))
      (forward-line 1)
      (insert "#+RESULTS:\n")
      (insert ": " text "\n"))))

;;;###autoload
(defun org-babel-execute-src-block (&optional _arg info _params)
  "Execute the Org source block at point.
This lightweight subset supports `emacs-lisp', `elisp', `shell', and `sh'.
INFO and PARAMS are accepted for compatibility and currently ignored."
  (interactive "P")
  (let* ((bounds (org-outline--babel-src-bounds))
         (language (or (car-safe info)
                       (plist-get bounds :language)))
         (body (buffer-substring-no-properties
                (plist-get bounds :body-start)
                (plist-get bounds :body-end)))
         (result
          (cond
           ((member language '("emacs-lisp" "elisp"))
            (org-outline--babel-elisp-result body))
           ((member language '("shell" "sh"))
            (replace-regexp-in-string
             "[\n\r]+\\'" "" (org-babel-eval body "")))
           (t
            (user-error "Unsupported source block language: %s" language)))))
    (org-outline--babel-insert-result bounds result)
    result))

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
      (catch 'org-element-ast-map-first
        (cl-labels
            ((walk (node)
               (when (consp node)
                 (let ((type (and (symbolp (car node)) (car node))))
                   (when (and type
                              (or (eq types-list t)
                                  (memq type types-list)))
                     (let ((value (funcall call node)))
                       (when (and first-match value)
                         (throw 'org-element-ast-map-first value))
                       (push value results)))
                   (dolist (child (if type (cddr node) node))
                     (walk child))))))
          (walk data))
        (nreverse results)))))

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
      (catch 'org-element-lineage-map-first
        (dolist (node (org-element-lineage datum types with-self))
          (let ((value (funcall call node)))
            (when first-match
              (throw 'org-element-lineage-map-first value))
            (when value
              (push value results))))
        (nreverse results)))))

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

;;;; --- void-function polyfills surfaced by the org runtime probes ------
;; These were void on the standalone image; real org navigation/parse
;; callers reach for them.  Built on the existing `org-outline--' helpers.

(unless (fboundp 'outline-next-heading)
  (defun outline-next-heading ()
    "Move to the next heading line; return point, or nil if none."
    (emacs-outline--next-heading)))

(unless (fboundp 'outline-previous-heading)
  (defun outline-previous-heading ()
    "Move to the previous heading line; return point, or nil if none."
    (emacs-outline--previous-heading)))

(unless (fboundp 'outline-on-heading-p)
  (defun outline-on-heading-p (&optional _invisible-ok)
    "Return non-nil when point is on a generic outline heading line."
    (emacs-outline--heading-at-point-p)))

(unless (fboundp 'outline-level)
  (defun outline-level ()
    "Return the current generic outline heading level."
    (or (emacs-outline--heading-level-at-point)
        (user-error "Not on an outline heading"))))

(unless (fboundp 'outline-back-to-heading)
  (defun outline-back-to-heading (&optional _invisible-ok)
    "Move to current or preceding generic outline heading."
    (or (emacs-outline--back-to-heading)
        (user-error "Before first outline heading"))))

(unless (fboundp 'outline-end-of-subtree)
  (defun outline-end-of-subtree ()
    "Move to the end of the generic outline subtree at point."
    (outline-back-to-heading)
    (let* ((level (funcall outline-level))
           (end (save-excursion
                  (or (and (emacs-outline--find-next-heading-at-or-above level)
                           (line-beginning-position))
                      (point-max)))))
      (goto-char end)
      (point))))

(unless (fboundp 'org-get-todo-state)
  (defun org-get-todo-state ()
    "Return the TODO keyword of the heading at point, or nil."
    (save-excursion
      (when (ignore-errors (org-back-to-heading t) t)
        (let ((text (org-outline--line-string)))
          (when (string-match org-outline--heading-regexp text)
            (let ((rest (substring text (match-end 0))))
              (when (string-match "\\`\\([A-Z][0-9A-Z_-]*\\)\\(?:[ \t]\\|\\'\\)" rest)
                (let ((kw (match-string 1 rest)))
                  (if (fboundp 'org-todo--all-keywords)
                      (and (member kw (org-todo--all-keywords)) kw)
                    (and (member kw '("TODO" "DONE" "NEXT" "WAIT" "HOLD"
                                      "DOING" "CANCELLED"))
                         kw)))))))))))

(unless (fboundp 'org-end-of-subtree)
  (defun org-end-of-subtree (&optional _invisible-ok _to-heading)
    "Move to the end of the subtree at point; return point."
    (org-back-to-heading t)
    (let ((end (plist-get (org-outline--subtree-bounds) :subtree-end)))
      (goto-char end)
      ;; `:subtree-end' is exclusive (next heading bol or point-max); step back
      ;; over the boundary newline so point sits on the subtree's own text.
      (when (and (not (eobp)) (bolp) (> (point) (point-min)))
        (backward-char 1))
      (point))))

(unless (fboundp 'org-metaright)
  (defun org-metaright (&optional _arg)
    "Demote the heading at point by one level (lightweight subset)."
    (interactive)
    (if (org-at-heading-p)
        (org-outline--replace-heading-stars
         (1+ (org-outline--heading-level-at-point)))
      (insert "  "))))

(defvar emacs-org-outline--vendor-org-element-load-error nil
  "Last error raised while trying to load vendored `org-element'.")

(defun emacs-org-outline--repo-root ()
  "Return the repository root inferred from this source file."
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun emacs-org-outline--vendor-org-load-paths ()
  "Return load-path entries needed by vendored Org."
  (let ((root (emacs-org-outline--repo-root)))
    (list (expand-file-name "vendor/emacs-lisp" root)
          (expand-file-name "vendor/emacs-lisp/emacs-lisp" root)
          (expand-file-name "vendor/emacs-lisp/org" root)
          (expand-file-name "vendor/emacs-lisp/gnus" root)
          (expand-file-name "vendor/emacs-lisp/mail" root))))

(defun emacs-org-outline--try-vendor-org-element ()
  "Try loading the vendored GNU Org element parser.
This module must not replace `org-element-parse-buffer' with an independent
parser.  A failure here identifies missing Emacs C-core-compatible substrate
that should be fixed in `src/' or the runtime."
  (unless (featurep 'org-element)
    (let ((load-path (append (emacs-org-outline--vendor-org-load-paths)
                             load-path)))
      (condition-case err
          (require 'org-element)
        (error
         (setq emacs-org-outline--vendor-org-element-load-error err)
         nil)))))

(emacs-org-outline--try-vendor-org-element)

(unless (fboundp 'org-element-type)
  (defun org-element-type (node &optional _anonymous)
    "Return the type symbol of org element NODE (`plain-text' for strings)."
    (cond
     ((stringp node) 'plain-text)
     ((null node) nil)
     ((and (consp node) (symbolp (car node))) (car node))
     (t nil))))

(unless (fboundp 'org-element-at-point)
  (defun org-element-at-point (&optional _pom _cached-only)
    "Return a lightweight org element node for the line at point.
A heading line yields a `headline' node, any other line a `paragraph'."
    (save-excursion
      (beginning-of-line)
      (if (org-at-heading-p)
          (org-element-create
           'headline
           (list :level (org-outline--heading-level-at-point)
                 :begin (line-beginning-position)
                 :end (nth 1 (org-outline--heading-line-range))
                 :raw-value (org-get-heading t t t)
                 :todo-keyword (org-get-todo-state)))
        (org-element-create
         'paragraph
         (list :begin (line-beginning-position)
               :end (org-outline--line-end-with-newline)))))))

(unless (fboundp 'org-element-parse-buffer)
  (defun org-element-parse-buffer
      (&optional _granularity _visible-only _keep-deferred)
    "Signal that vendored `org-element' is not yet loadable."
    (user-error "vendored org-element unavailable: %S"
                emacs-org-outline--vendor-org-element-load-error)))

(unless (fboundp 'org-element-map)
  (defun org-element-map
      (data types fun &optional info first-match no-recursion
            with-affiliated _no-recursion-types)
    "Map FUN over DATA nodes whose type is in TYPES.
This lightweight fallback delegates to `org-element-ast-map'."
    (org-element-ast-map data types fun info first-match no-recursion
                         with-affiliated)))

(provide 'emacs-org-outline)

;;; emacs-org-outline.el ends here
