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

(provide 'emacs-org-outline)

;;; emacs-org-outline.el ends here
