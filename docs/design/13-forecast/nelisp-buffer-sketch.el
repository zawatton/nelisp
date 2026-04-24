;;; nelisp-buffer-sketch.el --- Phase 5-B.1 forecast sketch -*- lexical-binding: t; -*-
;;
;; Minimal skeleton of the upcoming `src/nelisp-buffer.el'.  Walked
;; by `docs/design/13-audit-script.el' to forecast primitive gaps.
;; Not loaded at runtime — `--audit-only'.
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct nelisp-buffer
  (name "*unnamed*")
  (before-gap "")            ; text before the gap (string)
  (after-gap "")             ; text after the gap (string)
  (point 1)                  ; 1-based position
  (mark nil)
  (modified nil)
  (markers nil)              ; list of live nelisp-marker
  (overlays nil)
  (narrow-start nil)
  (narrow-end nil))

(defvar nelisp-buffer--registry (make-hash-table :test 'equal)
  "Name -> buffer registry.")

(defvar nelisp-buffer--current nil
  "Currently selected NeLisp buffer.")

(defun nelisp-buffer-generate-new (name)
  "Create fresh buffer with NAME (uniquified)."
  (let* ((base name)
         (count 0)
         (final name))
    (while (gethash final nelisp-buffer--registry)
      (setq count (1+ count))
      (setq final (concat base "<" (number-to-string count) ">")))
    (let ((buf (make-nelisp-buffer :name final)))
      (puthash final buf nelisp-buffer--registry)
      buf)))

(defun nelisp-buffer-get-create (name)
  "Return buffer named NAME, creating if absent."
  (or (gethash name nelisp-buffer--registry)
      (let ((buf (make-nelisp-buffer :name name)))
        (puthash name buf nelisp-buffer--registry)
        buf)))

(defun nelisp-buffer-current ()
  nelisp-buffer--current)

(defun nelisp-buffer-set (buf)
  (setq nelisp-buffer--current buf)
  buf)

(defmacro nelisp-with-buffer (buf &rest body)
  (declare (indent 1))
  `(let ((nelisp-buffer--current ,buf))
     ,@body))

(defun nelisp-buffer-point ()
  (nelisp-buffer-point nelisp-buffer--current))

(defun nelisp-buffer-point-min ()
  1)

(defun nelisp-buffer-point-max (buf)
  (+ 1 (length (nelisp-buffer-before-gap buf))
     (length (nelisp-buffer-after-gap buf))))

(defun nelisp-buffer-goto-char (buf pos)
  "Move gap to absolute POS, updating before/after-gap."
  (let* ((before (nelisp-buffer-before-gap buf))
         (after (nelisp-buffer-after-gap buf))
         (total (concat before after))
         (clamped (max 1 (min (+ 1 (length total)) pos)))
         (idx (1- clamped)))
    (setf (nelisp-buffer-before-gap buf) (substring total 0 idx))
    (setf (nelisp-buffer-after-gap buf) (substring total idx))
    (setf (nelisp-buffer-point buf) clamped)
    clamped))

(defun nelisp-buffer-insert (buf text)
  "Insert TEXT at current point of BUF."
  (setf (nelisp-buffer-before-gap buf)
        (concat (nelisp-buffer-before-gap buf) text))
  (setf (nelisp-buffer-point buf)
        (+ (nelisp-buffer-point buf) (length text)))
  (setf (nelisp-buffer-modified buf) t)
  ;; Adjust markers to the right of insertion point.
  (dolist (m (nelisp-buffer-markers buf))
    (when (>= (nelisp-marker-position m) (nelisp-buffer-point buf))
      (setf (nelisp-marker-position m)
            (+ (nelisp-marker-position m) (length text)))))
  nil)

(defun nelisp-buffer-delete-region (buf start end)
  "Delete bytes in [START, END) — 1-based inclusive start, exclusive end."
  (let* ((before (nelisp-buffer-before-gap buf))
         (after (nelisp-buffer-after-gap buf))
         (total (concat before after))
         (s (1- start))
         (e (1- end))
         (kept (concat (substring total 0 s) (substring total e))))
    (setf (nelisp-buffer-before-gap buf) (substring kept 0 s))
    (setf (nelisp-buffer-after-gap buf) (substring kept s))
    (setf (nelisp-buffer-point buf) start)
    (setf (nelisp-buffer-modified buf) t))
  nil)

(defun nelisp-buffer-string (buf)
  "Return the full text of BUF."
  (concat (nelisp-buffer-before-gap buf)
          (nelisp-buffer-after-gap buf)))

(defun nelisp-buffer-erase (buf)
  (setf (nelisp-buffer-before-gap buf) "")
  (setf (nelisp-buffer-after-gap buf) "")
  (setf (nelisp-buffer-point buf) 1)
  (setf (nelisp-buffer-modified buf) t)
  nil)

(defun nelisp-buffer-char-at (buf pos)
  "Return the character at POS in BUF, or nil at end."
  (let* ((total (nelisp-buffer-string buf))
         (idx (1- pos)))
    (and (>= idx 0) (< idx (length total))
         (elt total idx))))

(defun nelisp-buffer-substring (buf start end)
  "Return substring of BUF between START and END (1-based inclusive/exclusive)."
  (let ((total (nelisp-buffer-string buf)))
    (substring total (1- start) (1- end))))

(defun nelisp-buffer-search-forward (buf needle &optional bound)
  "Search for NEEDLE from point forward.  Return match position or nil."
  (let* ((text (nelisp-buffer-string buf))
         (start (1- (nelisp-buffer-point buf)))
         (found (string-search needle text start)))
    (when found
      (let ((match-end (+ 1 found (length needle))))
        (when (or (null bound) (<= match-end bound))
          (setf (nelisp-buffer-point buf) match-end)
          match-end)))))

(defun nelisp-buffer-copy-text (buf)
  "Return a copy of BUF's text — copy-sequence on the whole string."
  (copy-sequence (nelisp-buffer-string buf)))

(provide 'nelisp-buffer-sketch)
;;; nelisp-buffer-sketch.el ends here
