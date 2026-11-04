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

(defvar long-line-threshold 50000
  "Minimum displayed line length that enables long-line optimizations.
Nil disables detection, matching GNU Emacs.")

(defun emacs-cc-xdisp-1--long-line-clip (buffer)
  "Return BUFFER's current narrowing bounds when available."
  (when (and (fboundp 'nelisp-ec-buffer-narrow-start)
             (fboundp 'nelisp-ec-buffer-narrow-end))
    (cons (nelisp-ec-buffer-narrow-start buffer)
          (nelisp-ec-buffer-narrow-end buffer))))

(defun emacs-cc-xdisp-1--long-line-found-p (text threshold)
  "Return non-nil when TEXT has a line longer than THRESHOLD."
  (let ((index 0)
        (line-start 0)
        (length (length text))
        found)
    (while (and (< index length) (not found))
      (if (= (aref text index) ?\n)
          (progn
            (when (> (1+ (- index line-start)) threshold)
              (setq found t))
            (setq line-start (1+ index)))
        (when (> (1+ (- index line-start)) threshold)
          (setq found t)))
      (setq index (1+ index)))
    (or found (> (- length line-start) threshold))))

(defun emacs-cc-xdisp-1--update-long-line-state (buffer text-tick text-function)
  "Update BUFFER's long-line state during a redisplay pass.
TEXT-FUNCTION is called only when GNU's change trigger requests a scan."
  (when (and buffer
             (fboundp 'emacs-buffer-buffer-local-variables)
             (fboundp 'emacs-buffer-set-buffer-local-value))
    (let* ((previous
            (cdr (assq 'emacs-cc-xdisp-1--long-line-state
                       (emacs-buffer-buffer-local-variables buffer))))
           (clip (emacs-cc-xdisp-1--long-line-clip buffer))
           (old-tick (car-safe previous))
           (latched (nth 2 previous))
           (scan-p (or (null previous)
                       (not (equal clip (nth 1 previous)))
                       (and text-tick old-tick
                            (> (- text-tick old-tick) 8))))
           (threshold long-line-threshold))
      (when (and scan-p (not latched) threshold (integerp threshold))
        (setq latched
              (emacs-cc-xdisp-1--long-line-found-p
               (funcall text-function) threshold)))
      (emacs-buffer-set-buffer-local-value
       'emacs-cc-xdisp-1--long-line-state buffer (list text-tick clip latched))
      latched)))

(unless (fboundp 'long-line-optimizations-p)
  (defun long-line-optimizations-p ()
    "Return non-nil if long-line optimizations are in effect in current buffer."
    (let ((buffer (cond
                   ((fboundp 'emacs-buffer--property-current-buffer)
                    (emacs-buffer--property-current-buffer))
                   ((and (boundp 'nelisp-ec--current-buffer)
                         nelisp-ec--current-buffer)
                    nelisp-ec--current-buffer)
                   ((fboundp 'emacs-buffer--current)
                    (emacs-buffer--current))
                   (t nil))))
      (and buffer
           (fboundp 'emacs-buffer-buffer-local-variables)
           (nth 2 (cdr (assq 'emacs-cc-xdisp-1--long-line-state
                             (emacs-buffer-buffer-local-variables buffer))))))))

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
