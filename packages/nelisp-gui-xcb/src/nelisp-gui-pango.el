;;; nelisp-gui-pango.el --- Cairo/Pango consumer of shared glyph matrices -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-gui-xcb)
(require 'emacs-redisplay)
(require 'emacs-window)
(require 'emacs-frame-pixels)

(defvar nelisp-gui-pango-cell-width 12)
(defvar nelisp-gui-pango-line-height 28)
(defvar nelisp-gui-pango-font-size 20.0)
(defvar nelisp-gui-pango-font "DejaVu Sans Mono")
(defvar nelisp-gui-pango-foreground '(232 232 232))
(defvar nelisp-gui-pango-background '(24 32 40))
(defvar nelisp-gui-pango-cursor-color '(128 255 128))
(defvar nelisp-gui-pango-dpi 96)
(defvar nelisp-gui-pango-baseline 22)
(defvar nelisp-gui-pango-fringe 0)
(defvar nelisp-gui-pango-margin 0)
(defvar nelisp-gui-pango--cells nil)
(defvar nelisp-gui-pango--cells-dirty t)
(defvar nelisp-gui-pango--row-inset 0)
(defvar nelisp-gui-pango--region nil)
(defvar nelisp-gui-pango--row-pos-delta 0)
(defvar nelisp-gui-pango--row-input nil)
(defvar nelisp-gui-pango--row-cleared nil)
(defvar nelisp-gui-pango-force-paint t)
(defvar nelisp-gui-pango-maximum-layouts 256
  "Maximum retained native run layouts for one renderer.
Run origins can disappear as faces, regions and popup contents change.
Clear the owned cache on capacity pressure; plain typing reuses its layouts.")

(defun nelisp-gui-pango--text (layout text &optional renderer)
  "Set LAYOUT's copied UTF-8 text, reusing RENDERER's owned scratch mapping."
  (let* ((bytes (encode-coding-string text 'utf-8)) (n (length bytes))
         (cache (and renderer (aref renderer 12)))
         (owner (and cache (aref cache 0))))
    (if (not renderer)
        ;; Configuration precedes renderer creation; retain transient ownership.
        (let ((o (nl-ffi-memory-cstring bytes)))
          (unwind-protect
              (nelisp-gui-xcb-call "pango_layout_set_text" [:void :pointer :pointer :sint32]
                                   layout (nl-ffi-memory-address o) n)
            (nl-ffi-memory-release o)))
      (when (or (null owner) (< (aref cache 1) (1+ n)))
        (let* ((capacity (max 256 (1+ n) (if cache (* 2 (aref cache 1)) 0)))
               (new (nl-ffi-memory-allocate capacity)) (complete nil))
          (unwind-protect
              (progn
                (when owner (nl-ffi-memory-release owner))
                (setq owner new)
                (aset renderer 12 (vector owner capacity))
                (setq complete t))
            (unless complete (nl-ffi-memory-release new)))))
      (let ((p (nl-ffi-memory-address owner)) (i 0))
        ;; Validate and copy in one pass, retaining the byte-string contract.
        (while (< i n)
          (let ((byte (aref bytes i)))
            (unless (and (integerp byte) (<= 0 byte) (< byte 256))
              (error "Pango: UTF-8 encoder returned a non-byte at %d" i))
            (ptr-write-u8 p i byte))
          (setq i (1+ i)))
        (ptr-write-u8 p n 0)
        (nelisp-gui-xcb-call "pango_layout_set_text" [:void :pointer :pointer :sint32] layout p n)))))

(defun nelisp-gui-pango--size (layout &optional renderer)
  "Read LAYOUT size into reusable renderer storage, or a transient owner."
  (let ((o (or (and renderer (aref renderer 13)) (nl-ffi-memory-allocate 8))))
    (when renderer (aset renderer 13 o))
    (unwind-protect
        (let ((p (nl-ffi-memory-address o)))
          (nelisp-gui-xcb-call "pango_layout_get_pixel_size" [:void :pointer :pointer :pointer]
                               layout p (+ p 4))
          (cons (nl-ffi-libffi-u32 p 0) (nl-ffi-libffi-u32 p 4)))
      (unless renderer (nl-ffi-memory-release o)))))

(defun nelisp-gui-pango-configure (dpi)
  "Realize a fresh Pango context at resource DPI before creating the window."
  (dolist (lib '("libpangocairo-1.0.so.0" "libgobject-2.0.so.0")) (ffi:library lib))
  (let* ((map (nelisp-gui-xcb-call "pango_cairo_font_map_get_default" [:pointer]))
         (context (nelisp-gui-xcb-call "pango_font_map_create_context" [:pointer :pointer] map))
         (layout (nelisp-gui-xcb-call "pango_layout_new" [:pointer :pointer] context))
         (o (nl-ffi-memory-cstring nelisp-gui-pango-font))
         (desc (nelisp-gui-xcb-call "pango_font_description_from_string" [:pointer :pointer]
                                    (nl-ffi-memory-address o))))
    (unwind-protect
        (progn
          (setq nelisp-gui-pango-dpi dpi nelisp-gui-pango-font-size (* 20.0 (/ dpi 96.0)))
          (nelisp-gui-xcb-call "pango_cairo_context_set_resolution" [:void :pointer :double]
                               context (float dpi))
          (nelisp-gui-xcb-call "pango_font_description_set_absolute_size" [:void :pointer :double]
                               desc (* 1024.0 nelisp-gui-pango-font-size))
          (nelisp-gui-xcb-call "pango_layout_set_font_description" [:void :pointer :pointer] layout desc)
          (nelisp-gui-pango--text layout "M")
          (let ((size (nelisp-gui-pango--size layout)))
            (setq nelisp-gui-pango-cell-width (car size)
                  nelisp-gui-pango-line-height (+ (cdr size) (round (* 4 (/ dpi 96.0))))
                  nelisp-gui-pango-baseline
                  (+ (/ (nelisp-gui-xcb-call "pango_layout_get_baseline" [:sint32 :pointer] layout) 1024.0)
                     (round (* 2 (/ dpi 96.0)))))))
      (nl-ffi-memory-release o)
      (nelisp-gui-xcb-call "pango_font_description_free" [:void :pointer] desc)
      (nelisp-gui-xcb-call "g_object_unref" [:void :pointer] layout)
      (nelisp-gui-xcb-call "g_object_unref" [:void :pointer] context))))

(defun nelisp-gui-pango-natural-size (r text)
  "Measure a string with the same font, tabs and DPI as painting."
  (let ((layout (aref r 2)) (desc (aref r 3)))
    (nelisp-gui-xcb-call "pango_font_description_set_weight" [:void :pointer :sint32] desc 400)
    (nelisp-gui-xcb-call "pango_font_description_set_style" [:void :pointer :sint32] desc 0)
    (nelisp-gui-xcb-call "pango_layout_set_font_description" [:void :pointer :pointer] layout desc)
    (nelisp-gui-pango--text layout text r)
    (nelisp-gui-pango--size layout r)))

(defun nelisp-gui-pango-measure (r text)
  "Measure the realized grid-aligned Pango run, including fallback and tabs."
  (let ((natural (nelisp-gui-pango-natural-size r text)))
    (cons (* (emacs-frame-pixels-string-columns text emacs-redisplay-default-tab-width)
             nelisp-gui-pango-cell-width)
          nelisp-gui-pango-line-height)))

(defun nelisp-gui-pango-provider (operation &rest args)
  (cond ((eq operation :measure) (nelisp-gui-pango-measure nelisp-gui-frontend--renderer (car args)))
        ((eq operation :cells) (nelisp-gui-pango--ensure-cells nelisp-gui-frontend--renderer))
        (t (error "Unknown pixel query %S" operation))))

(defun nelisp-gui-pango-open (xcb cols lines)
  "Create process-local surface/context/layout/font owners, after cold-load."
  (dolist (lib '("libcairo.so.2" "libpangocairo-1.0.so.0" "libgobject-2.0.so.0"))
    (ffi:library lib))
  (let ((r (vector 0 0 0 0 nil cols lines xcb
                   nil nil nil (make-vector 4 nil) nil nil)) (complete nil))
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
  "Append Cairo's equivalent closed rectangle using scalar double calls.
The scalar ABI carries these two-double calls without allocating libffi
argument mappings and encoding four doubles for every row/cursor rectangle."
  (let ((cr (aref r 1)) (left (float x)) (top (float y))
        (right (float (+ x width))) (bottom (float (+ y height))))
    (nelisp-gui-xcb-call "cairo_move_to" [:void :pointer :double :double] cr left top)
    (nelisp-gui-xcb-call "cairo_line_to" [:void :pointer :double :double] cr right top)
    (nelisp-gui-xcb-call "cairo_line_to" [:void :pointer :double :double] cr right bottom)
    (nelisp-gui-xcb-call "cairo_line_to" [:void :pointer :double :double] cr left bottom)
    (nelisp-gui-xcb-call "cairo_close_path" [:void :pointer] cr)))

(defun nelisp-gui-pango--families (r &optional layout)
  "Record actual Pango font runs, including fallback, for diagnostics."
  (let ((it (nelisp-gui-xcb-call "pango_layout_get_iter" [:pointer :pointer] (or layout (aref r 2)))) (go t))
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

(defun nelisp-gui-pango--font (r face)
  "Reuse one of four process-local font descriptions for this renderer."
  (let* ((bold (cdr (assq :bold face))) (italic (cdr (assq :italic face)))
         (index (+ (if bold 1 0) (if italic 2 0)))
         (cache (aref r 11)) (desc (aref cache index)))
    (or desc
        (let ((desc (nelisp-gui-xcb-call "pango_font_description_copy" [:pointer :pointer] (aref r 3))))
          (nelisp-gui-xcb-call "pango_font_description_set_weight" [:void :pointer :sint32]
                               desc (if bold 700 400))
          (nelisp-gui-xcb-call "pango_font_description_set_style" [:void :pointer :sint32]
                               desc (if italic 2 0))
          (aset cache index desc) desc))))

(defun nelisp-gui-pango--layout (r text face x row)
  "Keep one owned layout per run origin; reset text without recreating its context."
  (let* ((origin (cons row x))
         (key (list text (and (cdr (assq :bold face)) t) (and (cdr (assq :italic face)) t)
                    nelisp-gui-pango-font nelisp-gui-pango-font-size nelisp-gui-pango-dpi))
         (cell (assoc origin (aref r 10))) (old (cdr cell)))
    (when (and (not cell) (>= (length (aref r 10)) nelisp-gui-pango-maximum-layouts))
      (unless (> nelisp-gui-pango-maximum-layouts 0)
        (error "Pango layout capacity must be positive"))
      (nelisp-gui-pango--clear-layouts r))
    (if (and old (equal key (aref old 0))) old
      (let ((layout (if old (aref old 1)
                      (nelisp-gui-xcb-call "pango_layout_copy" [:pointer :pointer] (aref r 2))))
            (complete nil))
        (unwind-protect
            (progn
              (unless (and old (equal (cdr key) (cdr (aref old 0))))
                (nelisp-gui-xcb-call "pango_layout_set_font_description" [:void :pointer :pointer]
                                     layout (nelisp-gui-pango--font r face)))
              (nelisp-gui-pango--text layout text r)
              (unless (= 0 (nelisp-gui-xcb-call "pango_layout_get_unknown_glyphs_count" [:sint32 :pointer] layout))
                (error "Pango: missing glyphs"))
              ;; DejaVu Sans Mono covers printable ASCII. Custom fonts may
              ;; fall back even within ASCII, so inspect their changed runs.
              (unless (and old (equal nelisp-gui-pango-font "DejaVu Sans Mono")
                           (equal (cdr key) (cdr (aref old 0)))
                           (not (string-match "[^ -~]" text))
                           (not (string-match "[^ -~]" (car (aref old 0)))))
                (nelisp-gui-pango--families r layout))
              (let ((entry (vector key layout
                                   (/ (nelisp-gui-xcb-call "pango_layout_get_baseline" [:sint32 :pointer] layout) 1024.0)
                                   (car (nelisp-gui-pango--size layout r))
                                   (* nelisp-gui-pango-cell-width
                                      (if (string-match "[^ -~]" text)
                                          (emacs-frame-pixels-string-columns text emacs-redisplay-default-tab-width)
                                        (length text))))))
                (if cell (setcdr cell entry)
                  (aset r 10 (cons (cons origin entry) (aref r 10))))
                (setq complete t) entry))
          ;; An old layout remains owned by its cache cell even on failure.
          (unless (or complete old)
            (nelisp-gui-xcb-call "g_object_unref" [:void :pointer] layout)))))))

(defun nelisp-gui-pango--clear-layouts (r)
  "Release row layouts before clearing their process-local cache."
  (dolist (cell (aref r 10))
    (nelisp-gui-xcb-call "g_object_unref" [:void :pointer] (aref (cdr cell) 1)))
  (aset r 10 nil))

(defun nelisp-gui-pango-run (r text face col row cells)
  "Paint TEXT at the shared grid origin, clipped to the shared advance CELLS."
  (let* ((cr (aref r 1))
         (x (+ nelisp-gui-pango--row-inset (* col nelisp-gui-pango-cell-width)))
         (y (* row nelisp-gui-pango-line-height))
         (width (* cells nelisp-gui-pango-cell-width))
         (fg (nelisp-gui-pango--color (cdr (assq :foreground face)) nelisp-gui-pango-foreground))
         (bg (nelisp-gui-pango--color (cdr (assq :background face)) nelisp-gui-pango-background))
         (entry (nelisp-gui-pango--layout r text face x row))
         (layout (aref entry 1)))
    (progn
      (when (cdr (assq :reverse face)) (let ((temp fg)) (setq fg bg bg temp)))
      (nelisp-gui-xcb-call "cairo_save" [:void :pointer] cr)
      (nelisp-gui-pango--rect r x y width nelisp-gui-pango-line-height)
      (nelisp-gui-xcb-call "cairo_clip" [:void :pointer] cr)
      ;; The row painter already cleared this background.
      (unless (and nelisp-gui-pango--row-cleared (equal bg nelisp-gui-pango-background))
        (nelisp-gui-pango--source r bg)
        (nelisp-gui-xcb-call "cairo_paint" [:void :pointer] cr))
      (nelisp-gui-pango--source r fg)
      ;; Baseline is aligned across fonts, rather than each fallback top edge.
      (let* ((baseline (aref entry 2))
             (natural (aref entry 3))
             (advance (aref entry 4)))
        ;; Fallback fonts can have another natural advance. Align the
        ;; shaped run to the shared logical cells instead of clipping it.
        (nelisp-gui-xcb-call "cairo_translate" [:void :pointer :double :double]
                             cr (float x) (float y))
        (when (and (> natural 0) (> advance 0) (/= natural advance))
          (nelisp-gui-xcb-call "cairo_scale" [:void :pointer :double :double]
                               cr (/ (float advance) natural) 1.0))
        (nelisp-gui-xcb-call "cairo_move_to" [:void :pointer :double :double]
                             cr 0.0 (- nelisp-gui-pango-baseline baseline)))
      (nelisp-gui-xcb-call "pango_cairo_show_layout" [:void :pointer :pointer] cr layout)
      (when (cdr (assq :underline face))
        (nelisp-gui-pango--rect r 0 (+ nelisp-gui-pango-baseline 2) width 1)
        (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] cr))
      (nelisp-gui-xcb-call "cairo_restore" [:void :pointer] cr))))

(defun nelisp-gui-pango-glyph-text (glyph)
  (apply #'string (cons (emacs-redisplay-glyph-char glyph)
                        (emacs-redisplay-glyph-composition glyph))))

(defun nelisp-gui-pango-glyph-face (glyph)
  "Choose painting colors from shared face and shared active region state."
  (let* ((face (and glyph (emacs-redisplay-glyph-realized-face glyph)))
         (pos (and glyph (emacs-redisplay-glyph-buf-pos glyph)
                   (+ nelisp-gui-pango--row-pos-delta (emacs-redisplay-glyph-buf-pos glyph))))
         (region nelisp-gui-pango--region))
    (if (and pos region (>= pos (car region)) (< pos (cdr region)))
        (cons '(:background rgb 64 80 100) (assq-delete-all :background (copy-sequence face)))
      face)))

(defun nelisp-gui-pango-row (r glyph-row left top width)
  "Consume the full shared matrix's cell/face/width data without editor layout."
  (let* ((input nelisp-gui-pango--row-input)
         (source (and (eq (car-safe input) :source) (= (nth 2 input) 0) (nth 1 input)))
         (text (and source (null (nth 6 source)) (null (nth 7 source))
                    (null nelisp-gui-pango--region) (car source))))
    (if (and text (= (length text) width) (not (string-match "[^ -~]" text)))
        (when (> width 0) (nelisp-gui-pango-run r text nil left top width))
  (let ((glyphs (emacs-redisplay-glyph-row-glyphs glyph-row)) (i 0))
    (while (< i width)
      (let* ((g (aref glyphs i))
             (face (nelisp-gui-pango-glyph-face g))
             (advance (if g (emacs-redisplay-glyph-width g) 1))
             (start i) (chars nil))
        (if (or (> advance 1) (and g (emacs-redisplay-glyph-composition g)))
            (progn
              (nelisp-gui-pango-run r (nelisp-gui-pango-glyph-text g)
                                    face (+ left i) top advance)
              (setq i (+ i advance)))
          (while (and (< i width)
                      (let ((next (aref glyphs i)))
                        (and (or (null next) (and (= 1 (emacs-redisplay-glyph-width next))
                                                  (null (emacs-redisplay-glyph-composition next))))
                             (equal face (nelisp-gui-pango-glyph-face next)))))
            (let ((next (aref glyphs i)))
              (push (if next (emacs-redisplay-glyph-char next) ?\s) chars))
            (setq i (1+ i)))
          (when (> i start)
            (nelisp-gui-pango-run r (apply #'string (nreverse chars)) face (+ left start) top (- i start))))))))))

(defun nelisp-gui-pango--ensure-cells (r)
  "Materialize pending hit-map rows only when a pixel consumer requests them."
  (when nelisp-gui-pango--cells-dirty
    (let (cell-rows)
      (dolist (w (emacs-window-window-list))
        (let ((cache (cdr (assq (emacs-window-id w) (aref r 8)))))
          (dotimes (i (length cache))
            (let* ((entry (aref cache i)) (pending (and entry (aref entry 3))))
              (when pending
                (let* ((row (nth 0 pending)) (edges (nth 1 pending)) (inset (nth 2 pending))
                       (glyphs (emacs-redisplay-glyph-row-glyphs row))
                       (used (emacs-redisplay-glyph-row-used row))
                       (delta (emacs-redisplay-glyph-row-pos-delta row))
                       (start (emacs-redisplay-glyph-row-start-pos row))
                       (y (* (+ (nth 1 edges) i) nelisp-gui-pango-line-height))
                       (col 0) cells)
                  (while (< col used)
                    (let* ((g (aref glyphs col))
                           (advance (if g (max 1 (emacs-redisplay-glyph-width g)) 1))
                           (pos (and g (emacs-redisplay-glyph-buf-pos g)))
                           (x (+ inset (* (+ (nth 0 edges) col) nelisp-gui-pango-cell-width))))
                      (when (and pos (< (+ x (* advance nelisp-gui-pango-cell-width))
                                        (* (nth 2 edges) nelisp-gui-pango-cell-width)))
                        (push (list w (+ pos delta) x y (* advance nelisp-gui-pango-cell-width)
                                    nelisp-gui-pango-line-height col i (nelisp-gui-pango-glyph-text g)
                                    (- (+ pos delta) start)) cells))
                      (setq col (+ col advance))))
                  (aset entry 1 (nreverse cells)) (aset entry 3 nil)))
              (when (and entry (aref entry 1)) (push (aref entry 1) cell-rows))))))
      (setq nelisp-gui-pango--cells (apply #'append (nreverse cell-rows))
            nelisp-gui-pango--cells-dirty nil)))
  nelisp-gui-pango--cells)

(defun nelisp-gui-pango-paint (r handle)
  "Paint only damaged matrix rows, restoring old and new cursor rows.
The per-row cell maps survive unchanged paints and their positions shift
lazily with shared glyph rows.  Expose/resize and menu changes force repaint."
  (nelisp-gui-xcb-check (aref r 7))
  (let* ((windows (emacs-window-window-list))
         (scene (list (mapcar (lambda (w) (list w (emacs-window-window-edges w))) windows)
                      (and (boundp 'nelisp-gui-menu--popup) nelisp-gui-menu--popup)
                      nelisp-gui-pango-fringe nelisp-gui-pango-margin
                      (when (> (emacs-frame-menu-bar-lines (emacs-frame-selected-frame)) 0)
                        (mapcar (lambda (item) (copy-sequence (car item)))
                                (emacs-keymap-menu-items
                                 (emacs-keymap-menu-binding [menu-bar]) [menu-bar])))))
         (previous (aref r 9))
         (full (or nelisp-gui-pango-force-paint (not (equal scene (car previous)))))
         (old-cursor (cadr previous))
         (selected (emacs-window-selected-window))
         (painted 0) (total 0) (new-cursor nil))
    (when full
      (setq nelisp-gui-pango--cells-dirty t)
      (aset r 8 nil)
      (nelisp-gui-pango--clear-layouts r)
      (nelisp-gui-pango--source r nelisp-gui-pango-background)
      (nelisp-gui-xcb-call "cairo_paint" [:void :pointer] (aref r 1)))
    (dolist (w windows)
      (when (emacs-window-leaf-p w)
        (let* ((m (let ((start (and (boundp 'nelisp-gui-frontend--timing)
                                    nelisp-gui-frontend--timing (float-time))))
                    (prog1 (emacs-redisplay-redisplay-window handle w)
                      (when start
                        (setq nelisp-gui-frontend--timing-redisplay
                              (+ nelisp-gui-frontend--timing-redisplay (- (float-time) start)))))))
               (edges (emacs-window-window-edges w))
               (rows (emacs-redisplay-glyph-matrix-rows m))
               (height (emacs-redisplay-glyph-matrix-height m))
               (width (emacs-redisplay-glyph-matrix-width m))
               (inputs (emacs-redisplay-glyph-matrix-line-cache m))
               (dirty (emacs-redisplay-glyph-matrix-dirty-set m))
               (cursor (and (eq w selected) (emacs-redisplay-glyph-matrix-cursor m)))
               (cached-window (assq (emacs-window-id w) (aref r 8)))
               (cache (or (cdr cached-window)
                          (make-vector (emacs-redisplay-glyph-matrix-height m) nil)))
               (inset (+ nelisp-gui-pango-fringe (* nelisp-gui-pango-margin nelisp-gui-pango-cell-width)))
               (region (when (and (eq w selected) (boundp 'mark-active) mark-active)
                         (let ((mark (emacs-mouse-mark)) (point (nelisp-gui-frontend--point)))
                           (and mark (cons (min point mark) (max point mark)))))))
          (unless cached-window
            (aset r 8 (cons (cons (emacs-window-id w) cache) (aref r 8))))
          (when cursor (setq new-cursor (cons w cursor)))
          (emacs-window-set-window-parameter w 'text-pixel-inset inset)
          (setq total (+ total height))
          (dotimes (i height)
            (let ((row (aref rows i)) (old (aref cache i)))
              ;; A matrix damage bit plus the semantic input/region/position
              ;; proves that neither pixels nor the hit map changed. Avoid
              ;; reconstructing run keys and cell maps for every unchanged row.
              (if (and (not full) old (not (aref dirty i))
                       (equal (aref inputs i) (car (aref old 0)))
                       (or (null (aref inputs i))
                           (and (equal region (nth 2 (aref old 0)))
                                (= (emacs-redisplay-glyph-row-pos-delta row) (aref old 2))))
                       (not (and old-cursor (eq w (car old-cursor)) (= i (cadr old-cursor))))
                       (not (and cursor (= i (car cursor)))))
                  nil
		(let* ((row (aref rows i)) (glyphs (emacs-redisplay-glyph-row-glyphs row))
                       (text-row (or (emacs-redisplay-glyph-row-start-pos row)
                                     (= (emacs-redisplay-glyph-row-used row) 0)))
                       (nelisp-gui-pango--row-inset (if text-row inset 0))
                       (nelisp-gui-pango--row-pos-delta (emacs-redisplay-glyph-row-pos-delta row))
                       (nelisp-gui-pango--region region)
                       (nelisp-gui-pango--row-input (aref inputs i))
                       (nelisp-gui-pango--row-cleared t)
                       (y (* (+ (nth 1 edges) i) nelisp-gui-pango-line-height))
                       (key (list (aref (emacs-redisplay-glyph-matrix-line-cache m) i)
				  (emacs-redisplay-glyph-row-hash row) region))
                       (old (aref cache i))
                       (changed (not (and old (equal key (aref old 0)))))
                       (damage (or full changed
				   (and old-cursor (eq w (car old-cursor)) (= i (cadr old-cursor)))
				   (and cursor (= i (car cursor)))))
                       )
		  (when damage
                    (setq painted (1+ painted))
                    ;; Clear the complete row to erase removed text and cursor.
                    (nelisp-gui-pango--source r nelisp-gui-pango-background)
                    (nelisp-gui-pango--rect r (* (nth 0 edges) nelisp-gui-pango-cell-width) y
                                            (* width nelisp-gui-pango-cell-width) nelisp-gui-pango-line-height)
                    (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] (aref r 1))
                    (nelisp-gui-pango-row r row (nth 0 edges) (+ (nth 1 edges) i)
					  (min width (emacs-redisplay-glyph-row-used row)))
                    (when text-row
                      (when (> inset 0)
			(let ((left (* (nth 0 edges) nelisp-gui-pango-cell-width))
                              (right (* (nth 2 edges) nelisp-gui-pango-cell-width)))
			  (nelisp-gui-pango--source r '(40 52 64))
			  (nelisp-gui-pango--rect r left y inset nelisp-gui-pango-line-height)
			  (nelisp-gui-pango--rect r (- right inset) y inset nelisp-gui-pango-line-height)
			  (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] (aref r 1))
			  (nelisp-gui-pango--source r '(70 88 105))
			  (nelisp-gui-pango--rect r left y nelisp-gui-pango-fringe nelisp-gui-pango-line-height)
			  (nelisp-gui-pango--rect r (- right nelisp-gui-pango-fringe) y nelisp-gui-pango-fringe nelisp-gui-pango-line-height)
			  (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] (aref r 1))))))
		  (when (or changed (not old)
                            (/= (aref old 2) nelisp-gui-pango--row-pos-delta))
                    ;; Hit testing needs current glyph positions, but typing does
                    ;; not query pixels. Keep the latest row until :cells requests
                    ;; it, rather than building every character's hit box per key.
                    (aset cache i (vector key nil nelisp-gui-pango--row-pos-delta
					  (and text-row (list row edges inset))))
                    (setq nelisp-gui-pango--cells-dirty t)))))))))
    (when new-cursor
      (let* ((w (car new-cursor)) (cursor (cdr new-cursor)) (edges (emacs-window-window-edges w)))
        (nelisp-gui-pango--source r nelisp-gui-pango-cursor-color)
        (nelisp-gui-pango--rect r (+ (or (emacs-window-window-parameter w 'text-pixel-inset) 0)
                                     (* (+ (nth 0 edges) (cdr cursor)) nelisp-gui-pango-cell-width))
                                (* (+ (nth 1 edges) (car cursor)) nelisp-gui-pango-line-height)
                                2 nelisp-gui-pango-line-height)
        (nelisp-gui-xcb-call "cairo_fill" [:void :pointer] (aref r 1))))
    (when (fboundp 'nelisp-gui-menu-paint) (nelisp-gui-menu-paint r))
    (nelisp-gui-xcb-call "cairo_surface_flush" [:void :pointer] (aref r 0))
    (unless (= 0 (nelisp-gui-xcb-call "cairo_status" [:sint32 :pointer] (aref r 1))) (error "Cairo paint error"))
    (nelisp-gui-xcb-call "xcb_flush" [:sint32 :pointer] (aref (aref r 7) 0))
    (nelisp-gui-xcb-check (aref r 7))
    (aset r 9 (list scene new-cursor))
    (setq nelisp-gui-pango-force-paint nil)
    (when (and (boundp 'nelisp-gui-frontend--timing) nelisp-gui-frontend--timing)
      (princ (format "GUI-DAMAGE|rows=%d|total=%d|layouts=%d|fonts=%d|\n"
                     painted total (length (aref r 10)) (length (delq nil (append (aref r 11) nil))))))
    (aref r 4)))

(defun nelisp-gui-pango-close (r)
  "Free dependents before their native font/surface/connection owners."
  (nelisp-gui-pango--clear-layouts r)
  (dolist (slot '(12 13))
    (when (and (< slot (length r)) (aref r slot))
      (nl-ffi-memory-release (if (= slot 12) (aref (aref r slot) 0) (aref r slot)))
      (aset r slot nil)))
  (dotimes (i 4)
    (when (aref (aref r 11) i)
      (nelisp-gui-xcb-call "pango_font_description_free" [:void :pointer] (aref (aref r 11) i))
      (aset (aref r 11) i nil)))
  (dolist (entry '((2 . "g_object_unref") (3 . "pango_font_description_free")
                   (1 . "cairo_destroy") (0 . "cairo_surface_destroy")))
    (when (> (aref r (car entry)) 0)
      (nelisp-gui-xcb-call (cdr entry) [:void :pointer] (aref r (car entry)))
      (aset r (car entry) 0))))

(provide 'nelisp-gui-pango)
