;;; nelisp-gui-menu.el --- In-window rendering of shared menu keymaps -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-gui-pango)
(require 'emacs-keymap)
(defvar nelisp-gui-menu--boxes nil)
(defvar nelisp-gui-menu--popup nil)
(defvar nelisp-gui-menu--suppress-release nil)

(defun nelisp-gui-menu--draw (renderer label x y width sequence definition)
  "Paint a menu item and record its transport hit box."
  (let ((nelisp-gui-pango--row-inset x)
        (face '((:foreground rgb 232 232 232) (:background rgb 52 68 84))))
    (nelisp-gui-pango-run renderer (concat " " label) face 0 y width)
    (push (list x (* y nelisp-gui-pango-line-height)
                (* width nelisp-gui-pango-cell-width) nelisp-gui-pango-line-height
                sequence definition label) nelisp-gui-menu--boxes)))

(defun nelisp-gui-menu-paint (renderer)
  "Render the active shared menu-bar map and the open shared popup."
  (setq nelisp-gui-menu--boxes nil)
  (when (> (emacs-frame-menu-bar-lines (emacs-frame-selected-frame)) 0)
    (let ((map (emacs-keymap-menu-binding [menu-bar])) (col 0))
      (when (emacs-keymap-keymapp map)
        (dolist (item (emacs-keymap-menu-items map [menu-bar]))
          (let ((width (+ 2 (length (car item)))))
            (nelisp-gui-menu--draw renderer (car item) (* col nelisp-gui-pango-cell-width) 0
                                   width (nth 1 item) (nth 2 item))
            (setq col (+ col width)))))))
  (when nelisp-gui-menu--popup
    (let ((items (nth 0 nelisp-gui-menu--popup)) (x (nth 1 nelisp-gui-menu--popup))
          (row (nth 2 nelisp-gui-menu--popup)) (width 20))
      (dolist (item items)
        (nelisp-gui-menu--draw renderer (car item) x row width (nth 1 item) (nth 2 item))
        (setq row (1+ row))))))

(defun nelisp-gui-menu-pointer (raw)
  "Consume menu UI pointer input; enqueue selected ordinary key sequences."
  (let ((type (nth 0 raw)) (button (nth 1 raw)) (x (nth 2 raw)) (y (nth 3 raw)) hit consumed)
    (dolist (box nelisp-gui-menu--boxes)
      (when (and (>= x (nth 0 box)) (< x (+ (nth 0 box) (nth 2 box)))
                 (>= y (nth 1 box)) (< y (+ (nth 1 box) (nth 3 box)))) (setq hit box)))
    (cond
     ((and (= type 5) nelisp-gui-menu--suppress-release)
      (setq nelisp-gui-menu--suppress-release nil consumed t))
     ((and (= type 4) (= button 1) hit)
      (let ((sequence (nth 4 hit)) (definition (nth 5 hit)))
        (if (emacs-keymap-keymapp definition)
            (setq nelisp-gui-menu--popup
                  (list (emacs-keymap-menu-items definition sequence) (nth 0 hit) 1))
          (setq nelisp-gui-menu--popup nil)
          (apply #'emacs-command-loop-feed-events (append sequence nil))
          (princ (format "GUI-MENU|label=%S|sequence=%S|\n" (nth 6 hit) sequence))))
      (setq nelisp-gui-menu--suppress-release t consumed t))
     ((and (= type 4) (= button 3))
      (let ((map (emacs-keymap-menu-binding [context-menu])))
        (when (emacs-keymap-keymapp map)
          (setq nelisp-gui-menu--popup
                (list (emacs-keymap-menu-items map [context-menu]) x (/ y nelisp-gui-pango-line-height))
                nelisp-gui-menu--suppress-release t consumed t))))
     ((and nelisp-gui-menu--popup (= type 4)) (setq nelisp-gui-menu--popup nil)))
    (when consumed (setq nelisp-gui-frontend--paint-needed t))
    consumed))
(provide 'nelisp-gui-menu)
