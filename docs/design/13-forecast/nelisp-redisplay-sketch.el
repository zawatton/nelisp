;;; nelisp-redisplay-sketch.el --- Phase 5-B.3 forecast sketch -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct nelisp-window
  buffer
  (top-line 1)
  (height 24)
  (width 80)
  (cursor-row 0)
  (cursor-col 0)
  (dirty-rows nil))  ; list of row indices to repaint

(defvar nelisp-redisplay--escape-csi "\e["
  "CSI prefix for terminal control.")

(defun nelisp-redisplay--goto-xy (row col)
  "Emit escape to move cursor to 1-based ROW/COL."
  (send-string-to-terminal
   (format "%s%d;%dH" nelisp-redisplay--escape-csi row col)))

(defun nelisp-redisplay--clear-line ()
  (send-string-to-terminal (format "%s2K" nelisp-redisplay--escape-csi)))

(defun nelisp-redisplay--split-lines (text)
  "Split TEXT into a list of lines preserving empty trailing line."
  (split-string text "\n"))

(defun nelisp-redisplay--take-rows (lines start count)
  "Take COUNT lines starting at 1-based START, pad with empty strings."
  (let* ((total (length lines))
         (result nil)
         (i 0))
    (while (< i count)
      (let ((idx (+ (1- start) i)))
        (push (if (< idx total) (nth idx lines) "") result))
      (setq i (1+ i)))
    (nreverse result)))

(defun nelisp-redisplay-window (win)
  "Repaint dirty rows of WIN."
  (let* ((buf (nelisp-window-buffer win))
         (text (nelisp-buffer-string buf))
         (lines (nelisp-redisplay--split-lines text))
         (top (nelisp-window-top-line win))
         (h (nelisp-window-height win))
         (rows (nelisp-redisplay--take-rows lines top h)))
    (dolist (row-idx (nelisp-window-dirty-rows win))
      (when (and (>= row-idx 0) (< row-idx h))
        (nelisp-redisplay--goto-xy (1+ row-idx) 1)
        (nelisp-redisplay--clear-line)
        (let ((line (nth row-idx rows)))
          (send-string-to-terminal
           (substring line 0 (min (length line)
                                  (nelisp-window-width win)))))))
    ;; Place cursor.
    (nelisp-redisplay--goto-xy (1+ (nelisp-window-cursor-row win))
                               (1+ (nelisp-window-cursor-col win)))
    (setf (nelisp-window-dirty-rows win) nil)
    nil))

(defun nelisp-redisplay-mark-dirty (win row)
  "Add ROW to WIN's dirty-rows list."
  (unless (memq row (nelisp-window-dirty-rows win))
    (push row (nelisp-window-dirty-rows win))))

(defun nelisp-redisplay-mark-all-dirty (win)
  (let ((h (nelisp-window-height win))
        (i 0))
    (while (< i h)
      (nelisp-redisplay-mark-dirty win i)
      (setq i (1+ i)))))

(defun nelisp-redisplay-clear-screen ()
  (send-string-to-terminal (format "%s2J" nelisp-redisplay--escape-csi))
  (nelisp-redisplay--goto-xy 1 1))

(defun nelisp-redisplay-terminal-size ()
  "Return (WIDTH . HEIGHT) from host frame."
  (cons (frame-width) (frame-height)))

(provide 'nelisp-redisplay-sketch)
;;; nelisp-redisplay-sketch.el ends here
