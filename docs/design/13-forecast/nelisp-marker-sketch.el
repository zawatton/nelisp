;;; nelisp-marker-sketch.el --- Phase 5-B.2 forecast sketch -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct nelisp-marker
  buffer
  (position 1)
  (insertion-type nil))

(cl-defstruct nelisp-overlay
  buffer
  (start 1)
  (end 1)
  (props nil))

(defun nelisp-make-marker (buf pos)
  (let ((m (make-nelisp-marker :buffer buf :position pos)))
    (push m (nelisp-buffer-markers buf))
    m))

(defun nelisp-marker-move (m pos)
  (setf (nelisp-marker-position m) pos)
  m)

(defun nelisp-marker-delete (m)
  (let ((buf (nelisp-marker-buffer m)))
    (setf (nelisp-buffer-markers buf)
          (delq m (nelisp-buffer-markers buf)))
    nil))

(defun nelisp-make-overlay (buf start end)
  (let ((o (make-nelisp-overlay :buffer buf :start start :end end)))
    (push o (nelisp-buffer-overlays buf))
    o))

(defun nelisp-overlay-put (o key val)
  (let ((cell (assq key (nelisp-overlay-props o))))
    (if cell
        (setcdr cell val)
      (push (cons key val) (nelisp-overlay-props o))))
  val)

(defun nelisp-overlay-get (o key)
  (let ((cell (assq key (nelisp-overlay-props o))))
    (and cell (cdr cell))))

(defun nelisp-overlays-at (buf pos)
  "Return overlays whose range covers POS in BUF."
  (let (result)
    (dolist (o (nelisp-buffer-overlays buf))
      (when (and (>= pos (nelisp-overlay-start o))
                 (< pos (nelisp-overlay-end o)))
        (push o result)))
    (nreverse result)))

(defun nelisp-overlay-delete (o)
  (let ((buf (nelisp-overlay-buffer o)))
    (setf (nelisp-buffer-overlays buf)
          (delq o (nelisp-buffer-overlays buf)))
    nil))

(defun nelisp-put-text-property (buf start end key val)
  "Sparse text property impl — store (START END KEY VAL) records."
  ;; Phase 5-B.2 MVP: store as overlay with key=face etc.
  (let ((ov (nelisp-make-overlay buf start end)))
    (nelisp-overlay-put ov key val)))

(defun nelisp-get-text-property (buf pos key)
  (let (found)
    (dolist (o (nelisp-overlays-at buf pos))
      (when-let* ((v (nelisp-overlay-get o key)))
        (setq found v)))
    found))

(provide 'nelisp-marker-sketch)
;;; nelisp-marker-sketch.el ends here
