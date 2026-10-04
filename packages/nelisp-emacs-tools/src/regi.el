;;; regi.el --- lightweight regular-expression interpreter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Pure Elisp implementation of the small `regi' engine used by vendor
;; packages.  It interprets a frame of line predicates and actions over
;; the current buffer without pulling in the full vendor file.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- column/indentation primitives operate on ec-buffers.
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
  "Return non-nil when SYMBOL should be installed by this facade.
Standalone NeLisp binds `emacs-version' too (for vendor compatibility),
so a bare `(not (boundp 'emacs-version))' test misfires there; detect the
standalone path by a NeLisp-only primitive instead, matching
`emacs-char-table--standalone-p' in `emacs-char-table.el'."
  (if (or (fboundp 'nl-write-file)
          (not (boundp 'emacs-version)))
      t
    (not (fboundp symbol))))

(when (regi--install-function-p 'current-column)
  (defvar ctl-arrow t
    "Non-nil means display ASCII control characters using caret notation.")
  (defvar buffer-display-table nil
    "Display table for the current buffer, or nil for the standard table.")
  (when (fboundp 'make-variable-buffer-local)
    (make-variable-buffer-local 'ctl-arrow)
    (make-variable-buffer-local 'buffer-display-table)))

(defun regi--column-character (char column tab-size control-arrow table multibyte)
  "Advance COLUMN over CHAR using the current buffer's display policy."
  (let ((glyphs (and table (aref table char))))
    (cond
     ((vectorp glyphs)
      ;; Display-table glyphs occupy one column, even for wide characters.
      ;; Tab and newline glyphs retain their column-motion meaning.
      (let ((i 0))
        (while (< i (length glyphs))
          (let ((glyph (aref glyphs i)))
            (when (integerp glyph) (setq glyph (logand glyph #x3fffff)))
            (setq column
                  (cond
                   ((eq glyph ?\t) (+ column (- tab-size (% column tab-size))))
                   ((eq glyph ?\n) 0)
                   (t (1+ column)))))
          (setq i (1+ i))))
      column)
     ((eq char ?\t) (+ column (- tab-size (% column tab-size))))
     ((eq char ?\n) 0)
     ((or (< char 32) (= char 127))
      (+ column (if control-arrow 2 4)))
     ((or (and (>= char 128) (< char 160))
          (>= char #x3fff80)
          (and (not multibyte) (>= char 128)))
      (+ column 4))
     (t (+ column (char-width char))))))

(defun regi--column-string (string tab-size control-arrow table)
  "Return STRING's display width, with fixed-width tabs in replacements."
  (let ((width 0) (i 0) (multibyte (multibyte-string-p string)))
    (while (< i (length string))
      (let* ((char (aref string i))
             (glyphs (and table (aref table char))))
        (if (vectorp glyphs)
            (let ((j 0))
              (while (< j (length glyphs))
                (let ((glyph (aref glyphs j)))
                  (setq width
                        (+ width (if (integerp glyph)
                                     (regi--column-character
                                      (logand glyph #x3fffff) 0 tab-size
                                      control-arrow nil t)
                                   1))))
                (setq j (1+ j))))
          (setq width (+ width (regi--column-character
                                char 0 tab-size control-arrow nil multibyte)))))
      (setq i (1+ i)))
    width))

(defun regi--column-hidden-p (position)
  "Return non-nil if POSITION is invisible without an ellipsis."
  (let ((property (get-char-property position 'invisible))
        (spec (and (boundp 'buffer-invisibility-spec) buffer-invisibility-spec))
        (hidden nil))
    (when property
      (if (eq spec t)
          (setq hidden t)
        (while (consp spec)
          (let* ((entry (car spec))
                 (key (if (consp entry) (car entry) entry)))
            (when (or (eq property key) (and (consp property) (memq key property)))
              (if (and (consp entry) (cdr entry))
                  (setq hidden 'ellipsis)
                (unless (eq hidden 'ellipsis) (setq hidden t)))))
          (setq spec (cdr spec)))))
    (eq hidden t)))

(when (regi--install-function-p 'current-column)
  (defun current-column ()
    "Return the zero-based display column of point on its accessible line.
Expand tabs using `tab-width' and count character display widths.
With `selective-display' equal to t, carriage return starts a new line.
Invisible text without ellipses occupies no columns."
    (let* ((start (line-beginning-position))
           (text (buffer-substring-no-properties start (point)))
           (tab-size (if (and (boundp 'tab-width) (integerp tab-width)
                              (> tab-width 0) (<= tab-width 1000))
                         tab-width 8))
           (control-arrow (if (boundp 'ctl-arrow) ctl-arrow t))
           (multibyte (if (boundp 'enable-multibyte-characters)
                          enable-multibyte-characters t))
           (table (or (and (fboundp 'window-display-table)
                           (window-display-table))
                      (and (boundp 'buffer-display-table) buffer-display-table)
                      (and (boundp 'standard-display-table) standard-display-table)))
           (column 0)
           (i 0))
      (unless (char-table-p table) (setq table nil))
      ;; Find the last carriage return before measuring, so hidden text
      ;; and display-table substitutions cannot conceal a line boundary.
      (when (and (boundp 'selective-display) (eq selective-display t))
        (let ((j 0))
          (while (< j (length text))
            (when (eq (aref text j) ?\r) (setq i (1+ j)))
            (setq j (1+ j)))))
      (while (< i (length text))
        (let* ((position (+ start i))
               (display (get-char-property position 'display)))
          (cond
           ;; GNU counts the underlying text when invisibility requests
           ;; ellipses (invisible-p returns 2), rather than hiding it here.
           ((regi--column-hidden-p position))
           ((stringp display)
            (setq column (+ column (regi--column-string
                                    display tab-size control-arrow table)))
            (while (and (< (1+ i) (length text))
                        (eq (get-char-property (+ start i 1) 'display) display))
              (setq i (1+ i))))
           ((and (consp display) (eq (car display) 'space)
                 (let ((width (plist-get (cdr display) :width))
                       (align (plist-get (cdr display) :align-to)))
                   (cond
                    ((and (integerp width) (> width 0))
                     (setq column (+ column width)))
                    ((and (integerp align) (> align column))
                     (setq column align)))))
            (while (and (< (1+ i) (length text))
                        (eq (get-char-property (+ start i 1) 'display) display))
              (setq i (1+ i))))
           (t
            (setq column (regi--column-character
                          (aref text i) column tab-size control-arrow
                          table multibyte)))))
        (setq i (1+ i)))
      column)))

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
