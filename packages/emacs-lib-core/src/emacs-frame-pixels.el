;;; emacs-frame-pixels.el --- Backend-neutral realized pixel geometry -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'emacs-frame)
(require 'emacs-window)
(defvar tty-defined-color-alist nil
  "Terminal palette, initially empty before a terminal driver registers it.")

(defun emacs-frame-pixels-install (frame width height provider)
  "Attach measured cell WIDTH/HEIGHT and a pixel PROVIDER to FRAME.
PROVIDER accepts an operation followed by its arguments.  :measure returns
the logical (WIDTH . HEIGHT) of a string; :cells returns visible glyph boxes
as (WINDOW POSITION X Y WIDTH HEIGHT COLUMN ROW TEXT ACTUAL-COLUMN).  Coordinates are
frame pixels and positions are character positions, never UTF-8 byte indices.
This contract belongs to the editor; native shaping belongs to the backend."
  (unless (and (integerp width) (> width 0) (integerp height) (> height 0))
    (signal 'wrong-type-argument (list 'positive-cell-metrics width height)))
  (emacs-frame-set-frame-parameter frame 'char-width width)
  (emacs-frame-set-frame-parameter frame 'char-height height)
  (emacs-frame-set-frame-parameter frame 'pixel-provider provider)
  (setf (emacs-frame-pixel-width frame) (* (emacs-frame-width frame) width)
        (emacs-frame-pixel-height frame) (* (emacs-frame-height frame) height)))

(defun emacs-frame-pixels-resize (frame pixel-width pixel-height)
  "Resize FRAME and its shared window layout to realized physical pixels.
Keep fractional-cell remainder pixels in the frame's public dimensions.
The frame library owns menu/minibuffer space; frontends pass geometry only."
  (let* ((cols (max 2 (/ pixel-width (emacs-frame-frame-char-width frame))))
         (lines (max 2 (/ pixel-height (emacs-frame-frame-char-height frame))))
         (top (emacs-frame-menu-bar-lines frame)))
    (emacs-frame-set-frame-size frame cols lines)
    (setf (emacs-frame-pixel-width frame) pixel-width
          (emacs-frame-pixel-height frame) pixel-height)
    (emacs-window-layout-frame cols (max 2 (- lines top)) top)
    (cons cols lines)))

(defun emacs-frame-pixels-query (operation &rest args)
  "Query the selected frame's realized pixel provider."
  (let ((provider (emacs-frame-frame-parameter nil 'pixel-provider)))
    (unless provider (error "Frame has no realized pixel provider"))
    (apply provider operation args)))

(defun emacs-frame-pixels-default-font-width ()
  (emacs-frame-frame-char-width))
(defun emacs-frame-pixels-default-font-height ()
  (emacs-frame-frame-char-height))
(defun emacs-frame-pixels-line-height ()
  (emacs-frame-frame-char-height))

(defun emacs-frame-input-focus-in ()
  "Publish shared focus state after a frontend focus-in event."
  (interactive)
  (setq emacs-frame--focus (emacs-frame-selected-frame)))
(defun emacs-frame-input-focus-out ()
  "Clear shared focus state after a frontend focus-out event."
  (interactive)
  (setq emacs-frame--focus nil))
(defun emacs-frame-pixels-string-columns (string &optional tab-size)
  "Return STRING's logical grid advance using shared character semantics.
Tabs advance to a tab stop; nonspacing/enclosing marks compose with their
base. This is the same fixed cell contract as editor glyph matrices. Native
backends shape and align font runs to those cells, including fallback fonts."
  (let ((column 0) (maximum 0) (index 0) (tab-size (or tab-size 8)))
    (while (< index (length string))
      (let* ((char (aref string index))
             (category (and (fboundp 'get-char-code-property)
                            (get-char-code-property char 'general-category))))
        (cond ((= char ?\n) (setq maximum (max maximum column) column 0))
              ((= char ?\t) (setq column (+ column (- tab-size (% column tab-size)))))
              ((memq category '(Mn Me)) nil)
              (t (setq column (+ column (max 0 (char-width char)))))))
      (setq index (1+ index)))
    (max maximum column)))

(defun emacs-frame-pixels-string-width (string)
  (car (emacs-frame-pixels-query :measure string)))

(defun emacs-frame-pixels-window-text-size
    (&optional window from to x-limit y-limit mode-lines ignore-line-at-end)
  "Measure WINDOW's text using its frame's shaping provider."
  (let* ((window (or window (emacs-window-selected-window)))
         (buffer (emacs-window-buffer window))
         (text (if (nelisp-ec-buffer-p buffer)
                   (nelisp-ec-with-current-buffer buffer (nelisp-ec-buffer-string))
                 (with-current-buffer buffer (buffer-string))))
         (start (if (integerp from) (max 0 (1- from)) 0))
         (end (if (integerp to) (min (length text) (1- to)) (length text)))
         (lines (split-string (substring text start end) "\n" nil))
         (width 0) (height 0))
    (when (and ignore-line-at-end lines (equal (car (last lines)) ""))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (let ((size (emacs-frame-pixels-query :measure line)))
        (setq width (max width (car size))
              height (+ height (emacs-frame-frame-char-height)))))
    (when mode-lines (setq height (+ height (emacs-frame-frame-char-height))))
    (cons (if x-limit (min x-limit width) width)
          (if y-limit (min y-limit height) height))))

(defun emacs-frame-pixels-cell-posn (cell &optional timestamp x y)
  "Make the standard Emacs position list for a visible CELL."
  (let* ((window (nth 0 cell)) (edges (emacs-window-window-edges window))
         (cw (emacs-frame-frame-char-width)) (ch (emacs-frame-frame-char-height))
         (px (or x (nth 2 cell))) (py (or y (nth 3 cell))))
    (list window (nth 1 cell)
          (cons (- px (* (car edges) cw)) (- py (* (cadr edges) ch)))
          (or timestamp 0) nil (cons (nth 6 cell) (nth 7 cell))
          (cons (or (nth 9 cell) (nth 6 cell)) (nth 7 cell)) nil
          (cons (- px (nth 2 cell)) (- py (nth 3 cell)))
          (cons (nth 4 cell) (nth 5 cell)))))

(defun emacs-frame-pixels-posn-at-point (&optional position window)
  "Return the first visible glyph containing POSITION, or nil."
  (let ((window (or window (emacs-window-selected-window))))
    (setq position (or position (emacs-window-window-point window)))
    (catch 'found
      (dolist (cell (emacs-frame-pixels-query :cells))
        (when (and (eq window (car cell)) (>= position (nth 1 cell))
                   (< position (+ (nth 1 cell) (length (nth 8 cell)))))
          (throw 'found (emacs-frame-pixels-cell-posn cell)))))))

(defun emacs-frame-pixels-hit (x y &optional timestamp)
  "Hit a frame pixel using the exact boxes published by painting."
  (catch 'found
    (dolist (cell (emacs-frame-pixels-query :cells))
      (when (and (>= x (nth 2 cell)) (< x (+ (nth 2 cell) (nth 4 cell)))
                 (>= y (nth 3 cell)) (< y (+ (nth 3 cell) (nth 5 cell))))
        (throw 'found (emacs-frame-pixels-cell-posn cell timestamp x y))))))

(defun emacs-frame-pixels-posn-at-x-y (x y &optional frame-or-window whole)
  "Return a standard position at window-relative or frame-relative pixels."
  (let ((target (or frame-or-window (emacs-window-selected-window))))
    (when (emacs-window-windowp target)
      (let ((edges (emacs-window-window-edges target)))
        (setq x (+ x (* (car edges) (emacs-frame-frame-char-width))
                   (if whole 0 (or (emacs-window-window-parameter target 'text-pixel-inset) 0)))
              y (+ y (* (cadr edges) (emacs-frame-frame-char-height))))))
    (emacs-frame-pixels-hit x y)))

(defun emacs-frame-pixels-install-builtins ()
  "Install explicit GUI compatibility shims only in standalone NeLisp."
  (when (fboundp 'nelisp--write-stdout-bytes)
    (defalias 'frame-char-width #'emacs-frame-frame-char-width)
    (defalias 'frame-char-height #'emacs-frame-frame-char-height)
    (defalias 'frame-pixel-width #'emacs-frame-frame-pixel-width)
    (defalias 'frame-pixel-height #'emacs-frame-frame-pixel-height)
    (dolist (entry '((default-font-width . emacs-frame-pixels-default-font-width)
                     (default-font-height . emacs-frame-pixels-default-font-height)
                     (line-pixel-height . emacs-frame-pixels-line-height)
                     (string-pixel-width . emacs-frame-pixels-string-width)
                     (window-text-pixel-size . emacs-frame-pixels-window-text-size)
                     (posn-at-point . emacs-frame-pixels-posn-at-point)
                     (posn-at-x-y . emacs-frame-pixels-posn-at-x-y)))
      (defalias (car entry) (cdr entry)))))

(defun emacs-frame-display-color-cells (&optional _display)
  "Return color capacity reported by the selected frame's visual."
  (expt 2 (or (emacs-frame-frame-parameter nil 'display-depth) 1)))
(unless (fboundp 'display-color-cells)
  (defalias 'display-color-cells #'emacs-frame-display-color-cells))

(provide 'emacs-frame-pixels)
