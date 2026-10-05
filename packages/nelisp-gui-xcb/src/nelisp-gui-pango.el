;;; nelisp-gui-pango.el --- Cairo/Pango consumer of shared glyph matrices -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-gui-xcb)
(require 'emacs-redisplay)
(require 'emacs-window)

(defvar nelisp-gui-pango-cell-width 12)
(defvar nelisp-gui-pango-line-height 28)
(defvar nelisp-gui-pango-font-size 20.0)
(defvar nelisp-gui-pango-font "DejaVu Sans Mono")
(defvar nelisp-gui-pango-foreground '(232 232 232))
(defvar nelisp-gui-pango-background '(24 32 40))
(defvar nelisp-gui-pango-cursor-color '(128 255 128))

(defun nelisp-gui-pango-open (xcb cols lines)
  "Create process-local surface/context/layout/font owners, after cold-load."
  (dolist (lib '("libcairo.so.2" "libpangocairo-1.0.so.0" "libgobject-2.0.so.0"))
    (ffi:library lib))
  (let ((r (vector 0 0 0 0 nil cols lines xcb)) (complete nil))
    (unwind-protect
        (progn
          (aset r 0 (nl-ffi-libffi-call "libcairo.so.2" "cairo_xcb_surface_create" :pointer
                                      '(:pointer :uint32 :pointer :sint32 :sint32)
                                      (aref xcb 0) (aref xcb 1) (aref xcb 2)
                                      (* cols nelisp-gui-pango-cell-width)
                                      (* lines nelisp-gui-pango-line-height)))
          (unless (= 0 (nelisp-gui-xcb-call "cairo_surface_status" [:sint32 :pointer] (aref r 0)))
            (error "Cairo XCB surface rejected visual"))
          (aset r 1 (nelisp-gui-xcb-call "cairo_create" [:pointer :pointer] (aref r 0)))
          (aset r 2 (nelisp-gui-xcb-call "pango_cairo_create_layout" [:pointer :pointer] (aref r 1)))
          (let ((o (nl-ffi-memory-cstring nelisp-gui-pango-font)))
            (unwind-protect
                (aset r 3 (nelisp-gui-xcb-call "pango_font_description_from_string" [:pointer :pointer]
                                             (nl-ffi-memory-address o)))
              (nl-ffi-memory-release o)))
          (nelisp-gui-xcb-call "pango_font_description_set_absolute_size" [:void :pointer :double]
                               (aref r 3) (* 1024.0 nelisp-gui-pango-font-size))
          (nelisp-gui-xcb-call "pango_layout_set_font_description" [:void :pointer :pointer]
                               (aref r 2) (aref r 3))
          ;; Editor layout owns wrapping; Pango shapes only the supplied run.
          (nelisp-gui-xcb-call "pango_layout_set_single_paragraph_mode" [:void :pointer :sint32]
                               (aref r 2) 1)
          (setq complete t) r)
      (unless complete (nelisp-gui-pango-close r)))))

(defun nelisp-gui-pango--color (value default)
  "Translate the shared realized-face color vocabulary into Cairo channels."
  (cond
   ((and (consp value) (eq (car value) 'rgb)) (cdr value))
   ((or (null value) (eq value 'default)) default)
   ((consp value)
    (let ((n (cadr value)))
      (cond ((< n 16) (nth n '((0 0 0) (205 0 0) (0 205 0) (205 205 0)
                               (0 0 238) (205 0 205) (0 205 205) (229 229 229)
                               (127 127 127) (255 0 0) (0 255 0) (255 255 0)
                               (92 92 255) (255 0 255) (0 255 255) (255 255 255))))
            ((>= n 232) (let ((v (+ 8 (* 10 (- n 232))))) (list v v v)))
            (t (let ((v (- n 16)) (levels [0 95 135 175 215 255]))
                 (list (aref levels (/ v 36)) (aref levels (% (/ v 6) 6)) (aref levels (% v 6))))))))
   (t (or (cdr (assq value '((black 0 0 0) (red 205 0 0) (green 0 205 0)
                             (yellow 205 205 0) (blue 0 0 238) (magenta 205 0 205)
                             (cyan 0 205 205) (white 229 229 229)
                             (bright-black 127 127 127) (bright-red 255 0 0)
                             (bright-green 0 255 0) (bright-yellow 255 255 0)
                             (bright-blue 92 92 255) (bright-magenta 255 0 255)
                             (bright-cyan 0 255 255) (bright-white 255 255 255)))) default))))

(defun nelisp-gui-pango--source (r color)
  (nelisp-gui-xcb-call "cairo_set_source_rgb" [:void :pointer :double :double :double]
                       (aref r 1) (/ (nth 0 color) 255.0) (/ (nth 1 color) 255.0) (/ (nth 2 color) 255.0)))

(defun nelisp-gui-pango--rect (r x y width height)
  (nl-ffi-libffi-call "libcairo.so.2" "cairo_rectangle" :void
                     '(:pointer :double :double :double :double)
                     (aref r 1) (float x) (float y) (float width) (float height)))

(defun nelisp-gui-pango--families (r)
  "Record actual Pango font runs, including fallback, for diagnostics."
  (let ((it (nelisp-gui-xcb-call "pango_layout_get_iter" [:pointer :pointer] (aref r 2))) (go t))
    (unwind-protect
        (while go
          (let ((run (nelisp-gui-xcb-call "pango_layout_iter_get_run_readonly" [:pointer :pointer] it)))
            (when (> run 0)
              ;; Public x86-64 PangoGlyphItem.item -> PangoItem.analysis.font.
              (let* ((item (ptr-read-u64 run 0)) (font (ptr-read-u64 item 32))
                     (desc (nelisp-gui-xcb-call "pango_font_describe" [:pointer :pointer] font)))
                (unwind-protect
                    (let ((family (nl-ffi-get-string
                                   (nelisp-gui-xcb-call "pango_font_description_get_family" [:pointer :pointer] desc))))
                      (unless (member family (aref r 4)) (aset r 4 (cons family (aref r 4)))))
                  (nelisp-gui-xcb-call "pango_font_description_free" [:void :pointer] desc)))))
          (setq go (= 1 (nelisp-gui-xcb-call "pango_layout_iter_next_run" [:sint32 :pointer] it))))
      (nelisp-gui-xcb-call "pango_layout_iter_free" [:void :pointer] it))))

(defun nelisp-gui-pango-run (r text face col row cells)
  "Paint TEXT at the shared grid origin, clipped to the shared advance CELLS."
  (let* ((cr (aref r 1)) (layout (aref r 2)) (desc (aref r 3))
         (x (* col nelisp-gui-pango-cell-width)) (y (* row nelisp-gui-pango-line-height))
         (width (* cells nelisp-gui-pango-cell-width))
         (fg (nelisp-gui-pango--color (cdr (assq :foreground face)) nelisp-gui-pango-foreground))
         (bg (nelisp-gui-pango--color (cdr (assq :background face)) nelisp-gui-pango-background))
         (bytes (encode-coding-string text 'utf-8)) (owner (nl-ffi-memory-cstring bytes)))
    (unwind-protect
        (progn
          (when (cdr (assq :reverse face)) (let ((temp fg)) (setq fg bg bg temp)))
          (nelisp-gui-xcb-call "cairo_save" [:void :pointer] cr)
          (nelisp-gui-pango--rect r x y width nelisp-gui-pango-line-height)
          (nelisp-gui-xcb-call "cairo_clip" [:void :pointer] cr)
          (nelisp-gui-pango--source r bg)
          (nelisp-gui-xcb-call "cairo_paint" [:void :pointer] cr)
          (nelisp-gui-xcb-call "pango_font_description_set_weight" [:void :pointer :sint32]
                               desc (if (cdr (assq :bold face)) 700 400))
          (nelisp-gui-xcb-call "pango_font_description_set_style" [:void :pointer :sint32]
                               desc (if (cdr (assq :italic face)) 2 0))
          (nelisp-gui-xcb-call "pango_layout_set_font_description" [:void :pointer :pointer] layout desc)
          (nelisp-gui-xcb-call "pango_layout_set_text" [:void :pointer :pointer :sint32]
                               layout (nl-ffi-memory-address owner) (length bytes))
          (unless (= 0 (nelisp-gui-xcb-call "pango_layout_get_unknown_glyphs_count" [:sint32 :pointer] layout))
            (error "Pango: missing glyphs"))
          (nelisp-gui-pango--families r)
          (nelisp-gui-pango--source r fg)
          ;; Baseline is aligned across fonts, rather than each fallback top edge.
          (let ((baseline (/ (nelisp-gui-xcb-call "pango_layout_get_baseline" [:sint32 :pointer] layout) 1024.0)))
            (nelisp-gui-xcb-call "cairo_move_to" [:void :pointer :double :double]
                                 cr (float x) (+ y (- 22.0 baseline))))
          (nelisp-gui-xcb-call "pango_cairo_show_layout" [:void :pointer :pointer] cr layout)
          (when (cdr (assq :underline face))
            (nelisp-gui-pango--rect r x (+ y 24) width 1)
            (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] cr))
          (nelisp-gui-xcb-call "cairo_restore" [:void :pointer] cr))
      (nl-ffi-memory-release owner))))

(defun nelisp-gui-pango-row (r glyph-row left top width)
  "Consume the full shared matrix's cell/face/width data without editor layout."
  (let ((glyphs (emacs-redisplay-glyph-row-glyphs glyph-row)) (i 0))
    (while (< i width)
      (let* ((g (aref glyphs i))
             (face (and g (emacs-redisplay-glyph-realized-face g)))
             (advance (if g (emacs-redisplay-glyph-width g) 1))
             (start i) (chars nil))
        (if (> advance 1)
            (progn
              (nelisp-gui-pango-run r (char-to-string (emacs-redisplay-glyph-char g))
                                   face (+ left i) top advance)
              (setq i (+ i advance)))
          (while (and (< i width)
                      (let ((next (aref glyphs i)))
                        (and (or (null next) (= 1 (emacs-redisplay-glyph-width next)))
                             (equal face (and next (emacs-redisplay-glyph-realized-face next))))))
            (let ((next (aref glyphs i)))
              (push (if next (emacs-redisplay-glyph-char next) ?\s) chars))
            (setq i (1+ i)))
          (when (> i start)
            (nelisp-gui-pango-run r (apply #'string (nreverse chars)) face (+ left start) top (- i start))))))))

(defun nelisp-gui-pango-paint (r handle)
  "Draw the shared frame/windows/matrices and shared selected-window cursor."
  (nelisp-gui-xcb-check (aref r 7))
  (nelisp-gui-pango--source r nelisp-gui-pango-background)
  (nelisp-gui-xcb-call "cairo_paint" [:void :pointer] (aref r 1))
  (dolist (w (emacs-window-window-list))
    (when (emacs-window-leaf-p w)
      (let* ((m (emacs-redisplay-redisplay-window handle w))
             (edges (emacs-window-window-edges w))
             (rows (emacs-redisplay-glyph-matrix-rows m)))
        (dotimes (i (emacs-redisplay-glyph-matrix-height m))
          (nelisp-gui-pango-row r (aref rows i) (nth 0 edges) (+ (nth 1 edges) i)
                               (emacs-redisplay-glyph-matrix-width m))))))
  (let* ((w (emacs-window-selected-window)) (m (emacs-redisplay-glyph-matrix handle w))
         (cursor (emacs-redisplay-glyph-matrix-cursor m)) (edges (emacs-window-window-edges w)))
    (when cursor
      (nelisp-gui-pango--source r nelisp-gui-pango-cursor-color)
      (nelisp-gui-pango--rect r (* (+ (nth 0 edges) (cdr cursor)) nelisp-gui-pango-cell-width)
                             (* (+ (nth 1 edges) (car cursor)) nelisp-gui-pango-line-height)
                             2 nelisp-gui-pango-line-height)
      (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] (aref r 1))))
  (nelisp-gui-xcb-call "cairo_surface_flush" [:void :pointer] (aref r 0))
  (unless (= 0 (nelisp-gui-xcb-call "cairo_status" [:sint32 :pointer] (aref r 1))) (error "Cairo paint error"))
  (nelisp-gui-xcb-call "xcb_flush" [:sint32 :pointer] (aref (aref r 7) 0))
  (nelisp-gui-xcb-check (aref r 7))
  (aref r 4))

(defun nelisp-gui-pango-close (r)
  "Free dependents before their native font/surface/connection owners."
  (dolist (entry '((2 . "g_object_unref") (3 . "pango_font_description_free")
                   (1 . "cairo_destroy") (0 . "cairo_surface_destroy")))
    (when (> (aref r (car entry)) 0)
      (nelisp-gui-xcb-call (cdr entry) [:void :pointer] (aref r (car entry)))
      (aset r (car entry) 0))))

(provide 'nelisp-gui-pango)
