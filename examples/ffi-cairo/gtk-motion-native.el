;; Windows-native GTK4 motion + drag demo driven by NeLisp AOT (no added Rust).
;;
;; Two controllers on the drawing area feed AOT defuns:
;;   on_motion      GtkEventControllerMotion::motion
;;                  void(self, gdouble x, gdouble y, gpointer)        -> marker follows cursor
;;   on_drag_begin  GtkGestureDrag::drag-begin
;;                  void(self, gdouble start_x, gdouble start_y, gpointer)
;;   on_drag_update GtkGestureDrag::drag-update
;;                  void(self, gdouble off_x, gdouble off_y, gpointer)  -> rubber-band box
;; All three are 4-arg mixed gp/f64 callbacks: under Win64 self->rcx,
;; x->xmm1, y->xmm2, user_data->r9 (all in registers, no stack arg).
;;
;; drag-update reports an OFFSET from the press point, so current = start +
;; offset.  on_draw strokes a rectangle from the drag start to the current
;; point and fills a small marker square at the hover position.
;;
;; ctx: 8=area 16=loop 32=hover_x 40=hover_y 48=start_x 56=start_y
;;      64=cur_x 72=cur_y 80=has_box
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-motion-native.el \
;;        /tmp/sumi-motion $(pkg-config --libs gtk4)
(seq
 (data-blob title       "sumi - GTK4 motion/drag (NeLisp AOT)\0" rodata)
 (data-blob sig_motion  "motion\0" rodata)
 (data-blob sig_dbegin  "drag-begin\0" rodata)
 (data-blob sig_dupdate "drag-update\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_hint   "move = marker; drag = box\0" rodata)

 ;; Motion: marker follows the cursor.
 (defun on_motion (self (x :type f64) (y :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 32 (f64-to-i64-trunc x))
    (ptr-write-u64 ctx 40 (f64-to-i64-trunc y))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 ;; Drag begin: record the press point; current starts there.
 (defun on_drag_begin (self (sx :type f64) (sy :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 48 (f64-to-i64-trunc sx))
    (ptr-write-u64 ctx 56 (f64-to-i64-trunc sy))
    (ptr-write-u64 ctx 64 (f64-to-i64-trunc sx))
    (ptr-write-u64 ctx 72 (f64-to-i64-trunc sy))
    (ptr-write-u64 ctx 80 1)
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 ;; Drag update: current = start + offset.
 (defun on_drag_update (self (ox :type f64) (oy :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 64 (+ (ptr-read-u64 ctx 48) (f64-to-i64-trunc ox)))
    (ptr-write-u64 ctx 72 (+ (ptr-read-u64 ctx 56) (f64-to-i64-trunc oy)))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 (defun on_draw (area cr width height ctx)
   (seq
    (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
    (extern-call cairo_paint cr)
    ;; rubber-band box (yellow outline) if a drag has happened
    (if (= (ptr-read-u64 ctx 80) 0)
        0
      (seq
       (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
       (extern-call cairo_set_line_width cr (:f64 2.0))
       (extern-call cairo_rectangle cr
                    (:f64 (i64-to-f64 (ptr-read-u64 ctx 48)))
                    (:f64 (i64-to-f64 (ptr-read-u64 ctx 56)))
                    (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 64) (ptr-read-u64 ctx 48))))
                    (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 72) (ptr-read-u64 ctx 56)))))
       (extern-call cairo_stroke cr)))
    ;; cyan marker square at the hover position
    (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.90) (:f64 0.85))
    (extern-call cairo_rectangle cr
                 (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 32) 4)))
                 (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 40) 4)))
                 (:f64 8.0) (:f64 8.0))
    (extern-call cairo_fill cr)
    ;; hint
    (extern-call cairo_set_source_rgb cr (:f64 0.7) (:f64 0.7) (:f64 0.7))
    (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
    (extern-call cairo_set_font_size cr (:f64 15.0))
    (extern-call cairo_move_to cr (:f64 10.0) (:f64 20.0))
    (extern-call cairo_show_text cr (data-addr text_hint))
    0))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 96)))
      (seq
       (ptr-write-u64 ctx 80 0)
       (ptr-write-u64 ctx 32 150)
       (ptr-write-u64 ctx 40 80)
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 320 200)
          (let ((area    (extern-call gtk_drawing_area_new))
                (motion  (extern-call gtk_event_controller_motion_new))
                (drag    (extern-call gtk_gesture_drag_new))
                (loop    (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 8 area)
             (ptr-write-u64 ctx 16 loop)
             (extern-call gtk_widget_set_size_request area 320 200)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call g_signal_connect_data
                          motion (data-addr sig_motion) (addr-of on_motion) ctx 0 0)
             (extern-call gtk_widget_add_controller area motion)
             (extern-call g_signal_connect_data
                          drag (data-addr sig_dbegin) (addr-of on_drag_begin) ctx 0 0)
             (extern-call g_signal_connect_data
                          drag (data-addr sig_dupdate) (addr-of on_drag_update) ctx 0 0)
             (extern-call gtk_widget_add_controller area drag)
             (extern-call gtk_window_set_child window area)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 60000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
