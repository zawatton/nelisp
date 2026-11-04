;;; emacs-cc-census-display-b5501.el --- Terminal position hit testing  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-display-b5501--edges (window)
  "Return WINDOW's frame-relative terminal edges."
  (if (fboundp 'window-edges)
      (window-edges window)
    (emacs-window-window-edges window)))

(defun emacs-cc-census-display-b5501--contains (window x y)
  "Return non-nil if WINDOW contains frame coordinates X and Y."
  (let ((edges (emacs-cc-census-display-b5501--edges window)))
    (and (<= (nth 0 edges) x) (< x (nth 2 edges))
         (<= (nth 1 edges) y) (< y (nth 3 edges)))))

(defun emacs-cc-census-display-b5501--character (position compatibility)
  "Read the character at POSITION in the active buffer representation."
  (if compatibility
      (aref (nelisp-ec-buffer-substring position (1+ position)) 0)
    (char-after position)))

(defun emacs-cc-census-display-b5501--text (window x y compatibility)
  "Return position, column, row and offsets for a terminal text hit.
COMPATIBILITY selects the library's buffer representation.  Scan from
WINDOW's start only as far as the requested row and column.  Batch
display does not wrap lines, and its glyph dimensions are zero."
  (let* ((minimum (if compatibility (nelisp-ec-point-min) (point-min)))
         (maximum (if compatibility (nelisp-ec-point-max) (point-max)))
         (position (max minimum (min maximum (window-start window))))
         (row 0) (column 0) (last-width 0) (done nil)
         (tab (if (and (boundp 'tab-width) (integerp tab-width)
                       (> tab-width 0)) tab-width 8)))
    (while (and (< position maximum) (< row y))
      (let ((character (emacs-cc-census-display-b5501--character
                        position compatibility)))
        (setq column
              (cond ((= character ?\n) 0)
                    ((= character ?\t) (* tab (1+ (/ column tab))))
                    (t (+ column (max 0 (char-width character))))))
        (setq last-width
              (cond ((= character ?\n) 0)
                    ((or (and (< character 32) (/= character ?\t))
                         (= character 127)) 1)
                    (t (max 0 (char-width character)))))
        (when (= character ?\n) (setq row (1+ row)))
        (setq position (1+ position))))
    (while (and (< position maximum) (not done) (= row y))
      (let* ((character (emacs-cc-census-display-b5501--character
                         position compatibility))
             (width (if (= character ?\t)
                        (- (* tab (1+ (/ column tab))) column)
                      (max 0 (char-width character)))))
        (if (or (= character ?\n) (< x (+ column width)))
            (setq done t)
          (setq column (+ column width) position (1+ position)
                last-width (if (or (and (< character 32) (/= character ?\t))
                                   (= character 127)) 1 width)))))
    (let* ((newline (and (< position maximum)
                         (= (emacs-cc-census-display-b5501--character
                             position compatibility) ?\n)))
           ;; The iterator expands tabs and wide characters into terminal
           ;; cells.  At EOL its newline has width zero; at EOB the
           ;; iterator retains the last glyph's width for extra columns.
           (inside (and done (not newline)))
           (glyph-x (if inside x column))
           (hit-column (if inside x
                         (+ column (max 0 (- x column
                                             (if newline 0 last-width)))))))
      (list position hit-column row (- x glyph-x) (- y row)))))

(defun emacs-cc-census-display-b5501--buffer-hit (window x y)
  "Find terminal text at X, Y in WINDOW without changing buffer point."
  (let ((buffer (window-buffer window)))
    (if (and (fboundp 'nelisp-ec-buffer-p) (nelisp-ec-buffer-p buffer))
        (nelisp-ec-with-current-buffer buffer
          (emacs-cc-census-display-b5501--text window x y t))
      (with-current-buffer buffer
        (emacs-cc-census-display-b5501--text window x y nil)))))

(unless (fboundp 'posn-at-x-y)
  (defun posn-at-x-y (x y &optional frame-or-window whole)
    "Return position information for terminal coordinates X and Y.
Coordinates default to the selected window's text area, including its
tab and header lines.  FRAME-OR-WINDOW may specify another live window
or frame.  Non-nil WHOLE makes X relative to the window's left edge.
Return a mouse position list, or a frame position outside all windows."
    (unless (fixnump x)
      (signal 'wrong-type-argument (list 'fixnump x)))
    (when (< x -1)
      (signal 'wrong-type-argument (list 'wholenump x)))
    (unless (fixnump y)
      (signal 'wrong-type-argument (list 'fixnump y)))
    (when (< y -1)
      (signal 'wrong-type-argument (list 'wholenump y)))
    (let* ((target (or frame-or-window (selected-window)))
           (window (and (windowp target) target))
           frame)
      (when window
        (unless (window-live-p window)
          (signal 'wrong-type-argument (list 'window-live-p window)))
        (let ((edges (emacs-cc-census-display-b5501--edges window))
              (margins (and (not whole) (window-margins window))))
          (setq x (+ x (car edges) (or (car margins) 0))
                y (+ y (cadr edges))))
        (setq target (window-frame window)))
      (unless (frame-live-p target)
        (signal 'wrong-type-argument (list 'frame-live-p target)))
      (setq frame target)
      (unless (and window
                   (emacs-cc-census-display-b5501--contains window x y))
        (setq window nil)
        (let ((windows (window-list frame t)))
          (while (and windows (not window))
            (when (emacs-cc-census-display-b5501--contains (car windows) x y)
              (setq window (car windows)))
            (setq windows (cdr windows)))))
      (if (not window)
          (list frame nil (cons x y) 0)
        (let* ((edges (emacs-cc-census-display-b5501--edges window))
               (wx (- x (car edges))) (wy (- y (cadr edges)))
               (height (- (nth 3 edges) (nth 1 edges)))
               (tab (window-tab-line-height window))
               (header (window-header-line-height window))
               (mode (if (window-minibuffer-p window) 0
                       (window-mode-line-height window)))
               (margins (window-margins window))
               (left (or (car margins) 0))
               (right (or (cdr margins) 0))
               (width (- (nth 2 edges) (car edges)))
               (area (cond ((< wy tab) 'tab-line)
                           ((< wy (+ tab header)) 'header-line)
                           ((>= wy (- height mode)) 'mode-line)
                           ((and (= wx (1- width))
                                 (< (nth 2 edges) (frame-width frame)))
                            'vertical-line)
                           ((< wx left) 'left-margin)
                           ((>= wx (- width right)) 'right-margin))))
          (if (memq area '(tab-line header-line mode-line))
              (let ((row (if (eq area 'mode-line) (1- height) 0)))
                (list window area (cons wx wy) 0 nil nil
                      (cons 0 row) nil (cons 0 wy) (cons 0 0)))
            (let* ((tx (if (memq area '(left-margin vertical-line))
                           0 (- wx left)))
                   (ty (- wy tab header))
                   (hit (emacs-cc-census-display-b5501--buffer-hit window tx ty))
                   (position (car hit)))
              (list window (or area position)
                    (cons (if area wx tx) ty) 0 nil position
                    (cons (nth 1 hit) (nth 2 hit)) nil
                    (cons (if (eq area 'vertical-line) 0 (nth 3 hit))
                          (if (eq area 'vertical-line) wy (nth 4 hit)))
                    (cons (if (eq area 'vertical-line) 1 0) 0)))))))))

(provide 'emacs-cc-census-display-b5501)
;;; emacs-cc-census-display-b5501.el ends here
