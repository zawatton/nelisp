;; Windows-native GTK4 freehand-paint demo driven by NeLisp AOT (no added Rust).
;;
;; Drag to draw: the pointer trajectory is accumulated into a malloc'd point
;; array and redrawn as a stroked cairo path, so strokes persist.  Lifting the
;; button and dragging elsewhere starts a new stroke (each point carries a
;; "stroke start" flag, so on_draw move_to's there and line_to's within).
;;
;; Demonstrates variable-length data walked with a `while' loop whose counter
;; lives in memory (no mutable locals needed), plus a helper defun `add_point'
;; called from the GTK callbacks (defun-calling-defun across the C boundary).
;;
;; Controllers: GtkEventControllerMotion (absolute coords) + GtkGestureDrag
;; (press/release bracket via drag-begin / drag-end set a "pressed" flag; points
;; are appended in on_motion while pressed).
;;
;; ctx: 8=area 16=loop 24=points 32=count 40=pressed 48=hover_x 56=hover_y 64=i
;; each point = 24 bytes: [0]=x [8]=y [16]=stroke-start flag.  capacity 4000.
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-paint-native.el \
;;        /tmp/sumi-paint $(pkg-config --libs gtk4)
(seq
 (data-blob title       "sumi - GTK4 freehand paint (NeLisp AOT)\0" rodata)
 (data-blob sig_motion  "motion\0" rodata)
 (data-blob sig_dbegin  "drag-begin\0" rodata)
 (data-blob sig_dend    "drag-end\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_hint   "drag to draw\0" rodata)

 ;; Append (x, y, flag) to the point array unless it is full.
 (defun add_point (ctx x y flag)
   (let ((cnt (ptr-read-u64 ctx 32)))
     (if (< cnt 4000)
         (let ((p (+ (ptr-read-u64 ctx 24) (* cnt 24))))
           (seq
            (ptr-write-u64 p 0 x)
            (ptr-write-u64 p 8 y)
            (ptr-write-u64 p 16 flag)
            (ptr-write-u64 ctx 32 (+ cnt 1))
            0))
       0)))

 ;; Motion: track hover; while pressed, append the point.
 (defun on_motion (self (x :type f64) (y :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 48 (f64-to-i64-trunc x))
    (ptr-write-u64 ctx 56 (f64-to-i64-trunc y))
    (if (= (ptr-read-u64 ctx 40) 0)
        0
      (add_point ctx (f64-to-i64-trunc x) (f64-to-i64-trunc y) 0))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 ;; Drag begin: pen down — start a new stroke at the press point.
 (defun on_drag_begin (self (sx :type f64) (sy :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 40 1)
    (add_point ctx (f64-to-i64-trunc sx) (f64-to-i64-trunc sy) 1)
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 ;; Drag end: pen up.
 (defun on_drag_end (self (ox :type f64) (oy :type f64) ctx)
   (seq (ptr-write-u64 ctx 40 0) 0))

 (defun on_draw (area cr width height ctx)
   (seq
    (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
    (extern-call cairo_paint cr)
    ;; build + stroke the freehand path
    (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
    (extern-call cairo_set_line_width cr (:f64 2.5))
    (ptr-write-u64 ctx 64 0)
    (while (< (ptr-read-u64 ctx 64) (ptr-read-u64 ctx 32))
      (seq
       (let ((p (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 64) 24))))
         (if (= (ptr-read-u64 p 16) 1)
             (extern-call cairo_move_to cr
                          (:f64 (i64-to-f64 (ptr-read-u64 p 0)))
                          (:f64 (i64-to-f64 (ptr-read-u64 p 8))))
           (extern-call cairo_line_to cr
                        (:f64 (i64-to-f64 (ptr-read-u64 p 0)))
                        (:f64 (i64-to-f64 (ptr-read-u64 p 8))))))
       (ptr-write-u64 ctx 64 (+ (ptr-read-u64 ctx 64) 1))))
    (extern-call cairo_stroke cr)
    ;; hover marker
    (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.90) (:f64 0.85))
    (extern-call cairo_rectangle cr
                 (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 48) 3)))
                 (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 56) 3)))
                 (:f64 6.0) (:f64 6.0))
    (extern-call cairo_fill cr)
    ;; hint
    (extern-call cairo_set_source_rgb cr (:f64 0.7) (:f64 0.7) (:f64 0.7))
    (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
    (extern-call cairo_set_font_size cr (:f64 14.0))
    (extern-call cairo_move_to cr (:f64 8.0) (:f64 16.0))
    (extern-call cairo_show_text cr (data-addr text_hint))
    0))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 96))
          (pts (extern-call malloc 96000)))
      (seq
       (ptr-write-u64 ctx 24 pts)
       (ptr-write-u64 ctx 32 0)
       (ptr-write-u64 ctx 40 0)
       (ptr-write-u64 ctx 48 160)
       (ptr-write-u64 ctx 56 100)
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 320 220)
          (let ((area    (extern-call gtk_drawing_area_new))
                (motion  (extern-call gtk_event_controller_motion_new))
                (drag    (extern-call gtk_gesture_drag_new))
                (loop    (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 8 area)
             (ptr-write-u64 ctx 16 loop)
             (extern-call gtk_widget_set_size_request area 320 220)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call g_signal_connect_data
                          motion (data-addr sig_motion) (addr-of on_motion) ctx 0 0)
             (extern-call gtk_widget_add_controller area motion)
             (extern-call g_signal_connect_data
                          drag (data-addr sig_dbegin) (addr-of on_drag_begin) ctx 0 0)
             (extern-call g_signal_connect_data
                          drag (data-addr sig_dend) (addr-of on_drag_end) ctx 0 0)
             (extern-call gtk_widget_add_controller area drag)
             (extern-call gtk_window_set_child window area)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 60000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
