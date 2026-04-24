;;; nelisp-redisplay.el --- NeLisp redisplay (terminal, dirty-region) -*- lexical-binding: t; -*-
;;
;; Phase 5-B.3 per Doc 13.  Renders a `nelisp-buffer' onto a VT100
;; compatible terminal via `send-string-to-terminal' using per-row
;; dirty-region incremental repaint.  Color / font-lock is out of
;; scope for this phase.
;;
;;; Code:

(require 'cl-lib)
(require 'nelisp-buffer)

(cl-defstruct (nelisp-window
               (:constructor nelisp-window--make)
               (:copier nil))
  buffer
  (top-line 1)
  (height 24)
  (width 80)
  (cursor-row 0)
  (cursor-col 0)
  (dirty-rows nil))

(defconst nelisp-redisplay--csi "\e["
  "CSI prefix for VT100/ANSI escape sequences.")

(defvar nelisp-redisplay--output-fn #'send-string-to-terminal
  "Function called with terminal-bound strings.
Tests rebind this to a capturing closure so escape / text output
can be inspected without actually driving a terminal.")

(defun nelisp-redisplay--emit (str)
  (funcall nelisp-redisplay--output-fn str))

(defun nelisp-redisplay--goto-xy (row col)
  "Move cursor to 1-based ROW / COL."
  (nelisp-redisplay--emit
   (format "%s%d;%dH" nelisp-redisplay--csi row col)))

(defun nelisp-redisplay--clear-line ()
  "Clear current line (CSI 2K)."
  (nelisp-redisplay--emit
   (format "%s2K" nelisp-redisplay--csi)))

(defun nelisp-redisplay-clear-screen ()
  "Clear entire screen + home cursor."
  (nelisp-redisplay--emit
   (format "%s2J" nelisp-redisplay--csi))
  (nelisp-redisplay--goto-xy 1 1))

(defun nelisp-redisplay--split-lines (text)
  (split-string text "\n"))

(defun nelisp-redisplay--take-rows (lines start count)
  "Take COUNT lines from LINES starting at 1-based row START.
Pads with empty strings when LINES is shorter than required."
  (let ((total (length lines))
        (result nil)
        (i 0))
    (while (< i count)
      (let ((idx (+ (1- start) i)))
        (push (if (< idx total) (nth idx lines) "") result))
      (setq i (1+ i)))
    (nreverse result)))

(defun nelisp-make-window (&optional buffer height width)
  "Create a `nelisp-window' with optional BUFFER, HEIGHT, WIDTH."
  (nelisp-window--make
   :buffer buffer
   :height (or height 24)
   :width (or width 80)))

(defun nelisp-window-mark-dirty (win row)
  "Add ROW (0-based row index within WIN) to dirty list.
Idempotent: existing rows are not duplicated."
  (unless (memq row (nelisp-window-dirty-rows win))
    (push row (nelisp-window-dirty-rows win))))

(defun nelisp-window-mark-all-dirty (win)
  "Mark every row of WIN dirty."
  (let ((h (nelisp-window-height win))
        (i 0))
    (while (< i h)
      (nelisp-window-mark-dirty win i)
      (setq i (1+ i)))))

(defun nelisp-redisplay-window (win)
  "Repaint dirty rows of WIN onto the terminal.
Emits escape + text via `nelisp-redisplay--output-fn'.  Dirty
rows are painted in ascending order to mimic tty scan order.
After painting, the cursor is placed at
\(WIN cursor-row, cursor-col\) and the dirty list is cleared."
  (let* ((buf (nelisp-window-buffer win))
         (text (if buf (nelisp-buffer-string buf) ""))
         (lines (nelisp-redisplay--split-lines text))
         (top (nelisp-window-top-line win))
         (h (nelisp-window-height win))
         (w (nelisp-window-width win))
         (rows (nelisp-redisplay--take-rows lines top h))
         (ordered (sort (copy-sequence (nelisp-window-dirty-rows win)) #'<)))
    (dolist (row-idx ordered)
      (when (and (>= row-idx 0) (< row-idx h))
        (nelisp-redisplay--goto-xy (1+ row-idx) 1)
        (nelisp-redisplay--clear-line)
        (let ((line (nth row-idx rows)))
          (nelisp-redisplay--emit
           (substring line 0 (min (length line) w))))))
    (nelisp-redisplay--goto-xy
     (1+ (nelisp-window-cursor-row win))
     (1+ (nelisp-window-cursor-col win)))
    (setf (nelisp-window-dirty-rows win) nil)
    nil))

(defun nelisp-redisplay-terminal-size ()
  "Return terminal size as (WIDTH . HEIGHT) from the host frame."
  (cons (frame-width) (frame-height)))

(provide 'nelisp-redisplay)
;;; nelisp-redisplay.el ends here
