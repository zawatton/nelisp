;;; regi.el --- lightweight regular-expression interpreter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Pure Elisp implementation of the small `regi' engine used by vendor
;; packages.  It interprets a frame of line predicates and actions over
;; the current buffer without pulling in the full vendor file.

;;; Code:

(require 'emacs-buffer-builtins)
(require 'emacs-line-builtins)
(require 'emacs-search-builtins)

(defvar curline nil
  "Current line visible while a `regi-interpret' action is evaluated.")
(defvar curframe nil
  "Current regi frame visible while a `regi-interpret' action is evaluated.")
(defvar curentry nil
  "Current regi frame entry visible while a `regi-interpret' action is evaluated.")

(defun regi--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this facade."
  (if (not (boundp 'emacs-version))
      t
    (not (fboundp symbol))))

(when (regi--install-function-p 'current-column)
  (defun current-column ()
    "Return current zero-based column."
    (- (point) (line-beginning-position))))

(when (regi--install-function-p 'back-to-indentation)
  (defun back-to-indentation ()
    "Move to the first non-space character on the current line."
    (interactive)
    (beginning-of-line)
    (let ((end (line-end-position)))
      (while (and (< (point) end)
                  (memq (aref (buffer-substring-no-properties
                               (point) (1+ (point)))
                              0)
                        '(?\s ?\t)))
        (forward-char 1)))
    (point)))

(defun regi-pos (&optional position col-p)
  "Return point or column at a line-relative POSITION.
POSITION can be `bol', `boi', `eol', `bonl', or `bopl'.  When COL-P is
non-nil, return `current-column' instead of point."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (end-of-line)))
    (if col-p (current-column) (point))))

(defun regi-mapcar (predlist func &optional negate-p case-fold-search-p)
  "Build a regi frame from PREDLIST and FUNC.
Each predicate in PREDLIST is associated with FUNC.  NEGATE-P and
CASE-FOLD-SEARCH-P are appended to each entry when non-nil."
  (let (frame)
    (dolist (pred predlist (nreverse frame))
      (let ((entry (list pred func)))
        (when (or negate-p case-fold-search-p)
          (setq entry (append entry (list negate-p))))
        (when case-fold-search-p
          (setq entry (append entry (list case-fold-search-p))))
        (push entry frame)))))

(defun regi--line-string ()
  "Return the current line without the trailing newline."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun regi--predicate-match-p (pred negate-p case-fold-search-value)
  "Return non-nil when PRED matches the current line."
  (let* ((case-fold-search case-fold-search-value)
         (value (eval pred))
         (matched
          (cond
           ((stringp value) (looking-at value))
           (t value))))
    (if negate-p (not matched) matched)))

(defun regi--handle-result (result working-frame current-frame)
  "Return (DONE-P WORKING-FRAME CURRENT-FRAME STEP) from action RESULT."
  (let ((done-p nil)
        (step 1))
    (when (consp result)
      (let ((frame-cell (assq 'frame result))
            (step-cell (assq 'step result)))
        (when frame-cell
          (setq working-frame (cdr frame-cell)))
        (when step-cell
          (setq step (cdr step-cell)))
        (when (memq 'continue result)
          (setq current-frame (cdr current-frame)))
        (when (memq 'abort result)
          (setq done-p t))))
    (unless (and (consp result) (memq 'continue result))
      (setq current-frame working-frame))
    (list done-p working-frame current-frame step)))

(defun regi--frame-specials (frame)
  "Return (BEGIN END EVERY WORKING-FRAME) for FRAME."
  (let (begin-tag end-tag every-tag working-frame)
    (dolist (entry frame)
      (let ((pred (car entry))
            (func (cadr entry)))
        (cond
         ((eq pred 'begin) (setq begin-tag func))
         ((eq pred 'end)   (setq end-tag func))
         ((eq pred 'every) (setq every-tag func))
         (t                (push entry working-frame)))))
    (list begin-tag end-tag every-tag (nreverse working-frame))))

(defun regi-interpret (frame &optional start end)
  "Interpret regi FRAME over the current buffer.
START and END restrict processing to complete lines covering that region.
Frame entries have the form (PRED FUNC [NEGATE-P [CASE-FOLD-SEARCH]])."
  (save-excursion
    (save-restriction
      (when (and start end)
        (let ((lo (min start end))
              (hi (max start end)))
          (narrow-to-region
           (save-excursion
             (goto-char lo)
             (line-beginning-position))
           (save-excursion
             (goto-char hi)
             (forward-line 1)
             (point)))))
      (goto-char (point-min))
      (let* ((specials (regi--frame-specials frame))
             (begin-tag (nth 0 specials))
             (end-tag (nth 1 specials))
             (every-tag (nth 2 specials))
             (working-frame (nth 3 specials))
             (current-frame working-frame)
             done-p)
        (when begin-tag
          (eval begin-tag))
        (while (and (not done-p) (not (eobp)))
          (cond
           ((null current-frame)
            (setq current-frame working-frame)
            (forward-line 1))
           (t
            (let* ((entry (car current-frame))
                   (pred (nth 0 entry))
                   (func (nth 1 entry))
                   (negate-p (nth 2 entry))
                   (case-fold-search-value (nth 3 entry)))
              (cond
               ((regi--predicate-match-p pred negate-p case-fold-search-value)
                (let* ((curline (regi--line-string))
                       (curframe current-frame)
                       (curentry entry)
                       (result (eval func))
                       (state (regi--handle-result
                               result working-frame current-frame)))
                  (setq done-p (nth 0 state)
                        working-frame (nth 1 state)
                        current-frame (nth 2 state))
                  (unless (and (consp result) (memq 'continue result))
                    (forward-line (nth 3 state)))))
               (t
                (setq current-frame (cdr current-frame)))))))
          (when every-tag
            (eval every-tag)))
        (when end-tag
          (eval end-tag))))))

(provide 'regi)

;;; regi.el ends here
