;;; emacs-org-table.el --- Org table edit subset for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 daily-driver §3.3.3.
;;
;; This module implements the M3.3 Org table-edit subset:
;;
;; - `org-table-p' table-line predicate
;; - `org-table-align' column-width normalization
;; - field navigation via `org-table-next-field' / `org-table-previous-field'
;; - row / column insertion and deletion
;; - context-sensitive TAB / S-TAB dispatch layered onto `org-mode-map'
;;
;; Formula evaluation (`#+TBLFM:'), hline semantics, and the wider Org
;; table feature set are intentionally out of scope for v0.1.

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-buffer-builtins)
(require 'emacs-keymap-builtins)
(require 'emacs-string)
(require 'emacs-org-outline)

;;;; Low-level parsing helpers

(defconst org-table--line-regexp "^[ \t]*|"
  "Regexp matching a table line for the v0.1 Org table subset.")

(defun org-table--line-string ()
  "Return the current line as a string without text properties."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun org-table--line-end-with-newline ()
  "Return the position just after the current line's terminating newline.
If the current line is the last line without a trailing newline, return
`line-end-position'."
  (let ((line-end (line-end-position)))
    (if (and (< line-end (point-max))
             (eq (char-after line-end) ?\n))
        (1+ line-end)
      line-end)))

(defun org-table--table-line-p-string (line)
  "Return non-nil when LINE is a table line."
  (and line
       (string-match org-table--line-regexp line)))

;;;###autoload
(defun org-table-p (&optional point-or-nil)
  "Return non-nil when POINT-OR-NIL lies on an Org table line.
When POINT-OR-NIL is nil, inspect the current line.  A table line starts
with optional whitespace followed by `|'."
  (save-excursion
    (when point-or-nil
      (goto-char point-or-nil))
    (org-table--table-line-p-string (org-table--line-string))))

(defun org-table--line-indent (line)
  "Return LINE's leading indentation."
  (if (string-match "^\\([ \t]*\\)" line)
      (match-string 1 line)
    ""))

(defun org-table--split-cells (body)
  "Split BODY into raw cell strings.
BODY should be the table text between the outermost delimiters."
  (split-string body "|" nil))

(defun org-table--parse-line (line)
  "Parse table LINE into a plist with `:indent' and `:cells'."
  (unless (org-table--table-line-p-string line)
    (user-error "Not on an Org table"))
  (let* ((indent (org-table--line-indent line))
         (trimmed (string-trim-left line))
         (body (substring trimmed 1))
         (body (if (and (> (length body) 0)
                        (eq (aref body (1- (length body))) ?|))
                   (substring body 0 -1)
                 body))
         (cells (mapcar #'string-trim (org-table--split-cells body))))
    (list :indent indent
          :cells cells)))

(defun org-table--copy-row (row)
  "Return a shallow copy of ROW."
  (list :indent (plist-get row :indent)
        :cells (copy-sequence (plist-get row :cells))))

(defun org-table--column-count (rows)
  "Return the maximum column count across ROWS."
  (let ((count 0))
    (dolist (row rows count)
      (setq count (max count (length (plist-get row :cells)))))))

(defun org-table--normalize-row (row count)
  "Return ROW padded to COUNT cells."
  (let ((cells (copy-sequence (plist-get row :cells))))
    (while (< (length cells) count)
      (setq cells (append cells (list ""))))
    (list :indent (plist-get row :indent)
          :cells cells)))

(defun org-table--normalize-rows (rows)
  "Return ROWS padded so each row has the same column count."
  (let ((count (max 1 (org-table--column-count rows)))
        (normalized nil))
    (dolist (row rows (nreverse normalized))
      (push (org-table--normalize-row row count) normalized))))

(defun org-table--compute-widths (rows)
  "Return a vector of maximum cell widths for normalized ROWS."
  (let* ((count (max 1 (org-table--column-count rows)))
         (widths (make-vector count 0)))
    (dolist (row rows widths)
      (cl-loop for cell in (plist-get row :cells)
               for idx from 0
               do (aset widths idx (max (aref widths idx) (length cell)))))))

(defun org-table--format-row (row widths)
  "Return ROW rendered with column WIDTHS."
  (let ((pieces nil))
    (cl-loop for cell in (plist-get row :cells)
             for idx from 0
             for width = (aref widths idx)
             do (push (format (format " %%-%ds |" width) cell) pieces))
    (concat (plist-get row :indent) "|" (apply #'concat (nreverse pieces)))))

(defun org-table--line-column-index (line point-offset)
  "Return the zero-based column index in LINE at POINT-OFFSET."
  (let ((bars 0)
        (start 0))
    (while (and (< start (length line))
                (string-match "|" line start)
                (< (match-beginning 0) point-offset))
      (setq bars (1+ bars))
      (setq start (1+ (match-beginning 0))))
    (max 0 (1- bars))))

(defun org-table--current-column-index (rows)
  "Return the zero-based table column index at point within ROWS."
  (let* ((count (max 1 (org-table--column-count rows)))
         (offset (- (point) (line-beginning-position)))
         (line (org-table--line-string)))
    (min (1- count)
         (org-table--line-column-index line offset))))

(defun org-table--table-info ()
  "Return a plist describing the contiguous table at point.
The result contains `:start', `:end', `:rows', `:row-index',
`:column-index', and `:trailing-newline'."
  (unless (org-table-p)
    (user-error "Not on an Org table"))
  (let ((origin (point))
        (origin-line (line-beginning-position))
        start
        end
        rows
        (row-index 0)
        trailing-newline)
    (save-excursion
      (beginning-of-line)
      (while (and (> (line-beginning-position) (point-min))
                  (save-excursion
                    (forward-line -1)
                    (org-table-p)))
        (forward-line -1))
      (setq start (line-beginning-position))
      (goto-char start)
      (setq rows nil)
      (while (org-table-p)
        (when (= (line-beginning-position) origin-line)
          (setq row-index (length rows)))
        (push (org-table--parse-line (org-table--line-string)) rows)
        (let ((line-end (org-table--line-end-with-newline)))
          (goto-char line-end)
          (when (>= (point) (point-max))
            (goto-char (point-max)))))
      (setq rows (nreverse rows))
      (if (and (> (point) start) (not (org-table-p)))
          (forward-line -1))
      (setq end (org-table--line-end-with-newline))
      (setq trailing-newline
            (and (< (line-end-position) (point-max))
                 (eq (char-after (line-end-position)) ?\n))))
    (save-excursion
      (goto-char origin)
      (list :start start
            :end end
            :rows rows
            :row-index row-index
            :column-index (org-table--current-column-index rows)
            :trailing-newline trailing-newline))))

(defun org-table--table-string (rows trailing-newline)
  "Return ROWS rendered as a full table string.
TRAILING-NEWLINE controls whether the last line ends with a newline."
  (let* ((normalized (org-table--normalize-rows rows))
         (widths (org-table--compute-widths normalized))
         (lines nil))
    (dolist (row normalized)
      (push (org-table--format-row row widths) lines))
    (setq lines (nreverse lines))
    (concat (mapconcat #'identity lines "\n")
            (if trailing-newline "\n" ""))))

(defun org-table--find-cell-bounds-on-line (column-index)
  "Return (START . END) for COLUMN-INDEX on the current line."
  (let* ((line (org-table--line-string))
         (line-begin (line-beginning-position))
         (search-start 0)
         (left-bar nil)
         (right-bar nil)
         (idx 0))
    (while (and (string-match "|" line search-start)
                (<= idx column-index))
      (setq left-bar (match-beginning 0))
      (setq search-start (1+ left-bar))
      (if (not (string-match "|" line search-start))
          (setq right-bar left-bar)
        (setq right-bar (match-beginning 0)))
      (setq idx (1+ idx)))
    (unless (and left-bar right-bar (> right-bar left-bar))
      (user-error "Invalid table cell"))
    (let ((start (+ line-begin left-bar 1))
          (end (+ line-begin right-bar)))
      (when (and (< start end)
                 (eq (char-after start) ?\s))
        (setq start (1+ start)))
      (when (and (< start end)
                 (eq (char-before end) ?\s))
        (setq end (1- end)))
      (cons start end))))

(defun org-table--goto-cell (table-start row-index column-index)
  "Move point to COLUMN-INDEX on ROW-INDEX within the table at TABLE-START."
  (goto-char table-start)
  (forward-line row-index)
  (goto-char (car (org-table--find-cell-bounds-on-line column-index))))

(defun org-table--replace-table (info rows target-row target-column)
  "Replace the current table described by INFO with ROWS.
After replacement, move point to TARGET-ROW and TARGET-COLUMN when
TARGET-ROW is non-nil.  Return non-nil."
  (let ((start (plist-get info :start))
        (end (plist-get info :end))
        (table-string (org-table--table-string
                       rows
                       (plist-get info :trailing-newline))))
    (goto-char start)
    (delete-region start end)
    (insert table-string)
    (when target-row
      (org-table--goto-cell start target-row target-column))
    t))

(defun org-table--insert-cell-at (cells index)
  "Return CELLS with an empty string inserted at INDEX."
  (let ((result nil)
        (current 0)
        inserted)
    (dolist (cell cells)
      (when (and (not inserted) (= current index))
        (push "" result)
        (setq inserted t))
      (push cell result)
      (setq current (1+ current)))
    (unless inserted
      (push "" result))
    (nreverse result)))

(defun org-table--delete-cell-at (cells index)
  "Return CELLS with the entry at INDEX removed."
  (let ((result nil)
        (current 0))
    (dolist (cell cells (nreverse result))
      (unless (= current index)
        (push cell result))
      (setq current (1+ current)))))

(defun org-table--empty-row (column-count indent)
  "Return a new empty row with COLUMN-COUNT cells and INDENT."
  (let ((cells nil))
    (dotimes (_ column-count)
      (push "" cells))
    (list :indent indent
          :cells (nreverse cells))))

(defun org-table--insert-row-after (rows row-index new-row)
  "Return ROWS with NEW-ROW inserted after ROW-INDEX."
  (let ((result nil)
        (current 0)
        (inserted nil))
    (dolist (row rows)
      (push row result)
      (when (= current row-index)
        (push new-row result)
        (setq inserted t))
      (setq current (1+ current)))
    (unless inserted
      (push new-row result))
    (nreverse result)))

(defun org-table--delete-row-at (rows row-index)
  "Return ROWS with ROW-INDEX removed."
  (let ((result nil)
        (current 0))
    (dolist (row rows (nreverse result))
      (unless (= current row-index)
        (push row result))
      (setq current (1+ current)))))

(defun org-table--replace-row-at (rows row-index new-row)
  "Return ROWS with NEW-ROW stored at ROW-INDEX."
  (let ((result nil)
        (current 0))
    (dolist (row rows (nreverse result))
      (push (if (= current row-index) new-row row) result)
      (setq current (1+ current)))))

(defun org-table--map-rows (rows fn)
  "Return ROWS after applying FN to each row."
  (let ((result nil))
    (dolist (row rows (nreverse result))
      (push (funcall fn row) result))))

;;;; Public commands

;;;###autoload
(defun org-table-align ()
  "Align the Org table at point by padding cells to column width."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (plist-get info :rows)))
    (org-table--replace-table
     info
     rows
     (plist-get info :row-index)
     (plist-get info :column-index))))

;;;###autoload
(defun org-table-next-field ()
  "Move point to the next Org table field, creating a row when needed."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (org-table--normalize-rows (plist-get info :rows)))
         (row-index (plist-get info :row-index))
         (column-index (plist-get info :column-index))
         (row-count (length rows))
         (column-count (max 1 (org-table--column-count rows))))
    (if (and (= row-index (1- row-count))
             (= column-index (1- column-count)))
        (let* ((last-row (nth row-index rows))
               (new-row (org-table--empty-row
                         column-count
                         (plist-get last-row :indent))))
          (setq rows (org-table--insert-row-after rows row-index new-row))
          (org-table--replace-table info rows (1+ row-index) 0))
      (org-table--replace-table
       info
       rows
       (if (= column-index (1- column-count))
           (1+ row-index)
         row-index)
       (if (= column-index (1- column-count))
           0
         (1+ column-index))))))

;;;###autoload
(defun org-table-previous-field ()
  "Move point to the previous Org table field."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (org-table--normalize-rows (plist-get info :rows)))
         (row-index (plist-get info :row-index))
         (column-index (plist-get info :column-index))
         (column-count (max 1 (org-table--column-count rows))))
    (org-table--replace-table
     info
     rows
     (if (and (= row-index 0) (= column-index 0))
         0
       (if (= column-index 0)
           (1- row-index)
         row-index))
     (if (and (= row-index 0) (= column-index 0))
         0
       (if (= column-index 0)
           (1- column-count)
         (1- column-index))))))

;;;###autoload
(defun org-table-insert-column ()
  "Insert an empty column at the current field across the whole table."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (org-table--normalize-rows (plist-get info :rows)))
         (column-index (plist-get info :column-index))
         (new-rows
          (org-table--map-rows
           rows
           (lambda (row)
             (let ((copy (org-table--copy-row row)))
               (plist-put copy :cells
                          (org-table--insert-cell-at
                           (plist-get copy :cells)
                           column-index))
               copy)))))
    (org-table--replace-table
     info
     new-rows
     (plist-get info :row-index)
     column-index)))

;;;###autoload
(defun org-table-delete-column ()
  "Delete the current column across the whole table."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (org-table--normalize-rows (plist-get info :rows)))
         (column-index (plist-get info :column-index))
         (column-count (max 1 (org-table--column-count rows))))
    (when (<= column-count 1)
      (user-error "Cannot delete the only column"))
    (let ((new-rows
           (org-table--map-rows
            rows
            (lambda (row)
              (let ((copy (org-table--copy-row row)))
                (plist-put copy :cells
                           (org-table--delete-cell-at
                            (plist-get copy :cells)
                            column-index))
                copy)))))
      (org-table--replace-table
       info
       new-rows
       (plist-get info :row-index)
       (min column-index (- column-count 2))))))

;;;###autoload
(defun org-table-insert-row ()
  "Insert an empty row below the current Org table row."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (org-table--normalize-rows (plist-get info :rows)))
         (row-index (plist-get info :row-index))
         (column-count (max 1 (org-table--column-count rows)))
         (current-row (nth row-index rows))
         (new-row (org-table--empty-row column-count
                                        (plist-get current-row :indent))))
    (setq rows (org-table--insert-row-after rows row-index new-row))
    (org-table--replace-table info rows (1+ row-index) 0)))

;;;###autoload
(defun org-table-delete-row ()
  "Delete the current Org table row."
  (interactive)
  (let* ((info (org-table--table-info))
         (rows (plist-get info :rows))
         (row-index (plist-get info :row-index))
         (new-rows (org-table--delete-row-at rows row-index))
         (start (plist-get info :start))
         (end (plist-get info :end))
         (trailing-newline (plist-get info :trailing-newline)))
    (if new-rows
        (org-table--replace-table
         info
         new-rows
         (min row-index (1- (length new-rows)))
         0)
      (progn
        (goto-char start)
        (delete-region start end)
        (when (and trailing-newline
                   (< (point) (point-max))
                   (eq (char-after) ?\n))
          (delete-char 1))
        (beginning-of-line)
        t))))

;;;; TAB dispatch

;;;###autoload
(defun org-tab-context ()
  "Dispatch TAB according to Org table context."
  (interactive)
  (if (org-table-p)
      (org-table-next-field)
    (org-cycle)))

;;;###autoload
(defun org-shifttab-context ()
  "Dispatch S-TAB according to Org table context."
  (interactive)
  (if (org-table-p)
      (org-table-previous-field)
    (org-shifttab)))

(define-key org-mode-map (kbd "TAB") #'org-tab-context)
(define-key org-mode-map (kbd "<tab>") #'org-tab-context)
(define-key org-mode-map (kbd "<backtab>") #'org-shifttab-context)
(define-key org-mode-map (kbd "S-TAB") #'org-shifttab-context)

(provide 'emacs-org-table)

;;; emacs-org-table.el ends here
