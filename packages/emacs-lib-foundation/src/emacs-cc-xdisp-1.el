;;; emacs-cc-xdisp-1.el --- xdisp C-core compatibility -*- lexical-binding: t; -*-
;;; Code:

(defun emacs-cc-xdisp-1--object-string (object)
  (cond ((stringp object) object)
        ((null object) (and (fboundp 'buffer-string) (buffer-string)))
        ((and (fboundp 'bufferp) (bufferp object))
         (with-current-buffer object (buffer-string)))
        ((and (fboundp 'windowp) (windowp object))
         (let ((b (window-buffer object)))
           (when (and (fboundp 'bufferp) (bufferp b))
             (with-current-buffer b (buffer-string)))))
        (t (signal 'wrong-type-argument (list 'buffer-or-string-p object)))))

(unless (fboundp 'bidi-find-overridden-directionality)
  (defun bidi-find-overridden-directionality (from to object &optional base-dir)
    "Return position between FROM and TO where directionality was overridden."
    (unless (memq (or base-dir 'left-to-right) '(left-to-right right-to-left))
      (signal 'wrong-type-argument (list 'symbolp base-dir)))
    (let ((s (emacs-cc-xdisp-1--object-string object)) (i (1- from)) found)
      (while (and (< i (min (length s) (1- to))) (not found))
        (when (memq (aref s i) '(8234 8235 8237 8238 8294 8295))
          (setq found (1+ i)))
        (setq i (1+ i)))
      found)))

(unless (fboundp 'bidi-resolved-levels)
  (defun bidi-resolved-levels (&optional vpos)
    "Return the resolved bidirectional levels of characters at VPOS."
    (when (and vpos (not (integerp vpos)))
      (signal 'wrong-type-argument (list 'integerp vpos)))
    nil))

(unless (fboundp 'buffer-text-pixel-size)
  (defun buffer-text-pixel-size (&optional buffer-or-name window x-limit y-limit)
    "Return the dimensions of whole text of BUFFER-OR-NAME in WINDOW."
    (ignore window x-limit y-limit)
    (let* ((b (cond ((null buffer-or-name) (current-buffer))
                    ((and (fboundp 'bufferp) (bufferp buffer-or-name)) buffer-or-name)
                    ((stringp buffer-or-name) (get-buffer buffer-or-name))
                    (t nil))))
      (unless (and b (buffer-live-p b))
        (signal 'wrong-type-argument (list 'buffer-live-p buffer-or-name)))
      (let* ((s (with-current-buffer b (buffer-string)))
             (lines (split-string s "\n" nil)) (wid 0))
        (dolist (line lines) (setq wid (max wid (length line))))
        (cons 0 (if (string-empty-p s) 0 (length lines)))))))

(unless (fboundp 'current-bidi-paragraph-direction)
  (defun current-bidi-paragraph-direction (&optional buffer)
    "Return paragraph direction at point in BUFFER."
    (let ((s (emacs-cc-xdisp-1--object-string buffer)))
      (if (string-match-p "[א-תء-ي]" s)
          'right-to-left 'left-to-right))))

(unless (fboundp 'display--line-is-continued-p)
  (defun display--line-is-continued-p ()
    "Return non-nil if the current screen line is continued on display."
    (let ((text (and (fboundp 'buffer-string) (buffer-string))))
      (and text (> (length text) 1000000)))))

(unless (fboundp 'format-mode-line)
  (defun format-mode-line (format &optional face window buffer)
    "Return a string formatted according to mode-line format specification."
    (ignore format face window buffer)
    ""))

(unless (fboundp 'get-display-property)
  (defun get-display-property (position spec &optional object properties)
    "Get the value of the display specification SPEC at POSITION."
    (let* ((display (if properties properties (get-text-property position 'display object)))
           (entry (and (listp display) (plist-member display spec))))
      (and entry (plist-get display spec)))))

(unless (fboundp 'line-pixel-height)
  (defun line-pixel-height ()
    "Return height in pixels of text line in the selected window."
    1))

(unless (fboundp 'long-line-optimizations-p)
  (defun long-line-optimizations-p ()
    "Return non-nil if long-line optimizations are in effect in current buffer."
    nil))

(unless (fboundp 'lookup-image-map)
  (defun lookup-image-map (map x y)
    "Lookup in image map MAP coordinates X and Y."
    (unless (listp map) (signal 'wrong-type-argument (list 'listp map)))
    (catch 'hit
      (dolist (item map)
        (let* ((area (car item)) (kind (car-safe area)) (data (cdr-safe area)))
          (when (cond
                 ((eq kind 'rect)
                  (let ((a (car data)) (b (cdr data)))
                    (and (>= x (car a)) (>= y (cdr a)) (<= x (car b)) (<= y (cdr b)))))
                 ((eq kind 'circle)
                  (let* ((c (car data)) (r (cdr data))
                         (dx (- x (car c))) (dy (- y (cdr c))))
                    (<= (+ (* dx dx) (* dy dy)) (* r r))))
                 ((eq kind 'poly) nil)
                 (t nil))
            (throw 'hit item))))
      nil)))

(unless (fboundp 'move-point-visually)
  (defun move-point-visually (direction)
    "Move point in the visual order in the specified DIRECTION."
    (unless (memq direction '(1 -1)) (signal 'wrong-type-argument (list '(member 1 -1) direction)))
    (signal 'args-out-of-range (list (point) (point)))))

(unless (fboundp 'remember-mouse-glyph)
  (defun remember-mouse-glyph (frame x y)
    "Return the extents of glyph in FRAME for mouse event generation."
    (ignore frame x y)
    (error "Window system frame should be used")))

(provide 'emacs-cc-xdisp-1)
