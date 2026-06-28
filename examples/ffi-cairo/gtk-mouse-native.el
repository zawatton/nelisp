;; Windows-native GTK4 mouse-event demo driven by NeLisp AOT (no added Rust).
;;
;; A GtkGestureClick on the drawing area delivers pointer presses to an AOT
;; defun whose signature mixes gp and f64 params:
;;   on_press  GtkGestureClick::pressed
;;             void(GtkGestureClick*, gint n_press, gdouble x, gdouble y, gpointer)
;; Under Win64 that is rcx, rdx, xmm2, xmm3, [stack] — a mixed-class defun with
;; the two coordinate doubles in xmm2/xmm3 and the 5th gp arg on the stack.
;;
;; The handler truncates the click coords to ints (f64-to-i64-trunc) and stores
;; them; on_draw moves a marker square to the last click (i64-to-f64 back into
;; cairo's doubles via cairo_rectangle) and shows a click counter (int->decimal
;; text).  Click anywhere in the window to move the marker.
;;
;; ctx[0]=clicks  ctx[8]=area  ctx[16]=loop  ctx[24]=text buf  ctx[32]=x  ctx[40]=y
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-mouse-native.el \
;;        /tmp/sumi-mouse $(pkg-config --libs gtk4)
(seq
 (data-blob title       "sumi - GTK4 mouse (NeLisp AOT)\0" rodata)
 (data-blob sig_pressed "pressed\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_hint   "click anywhere\0" rodata)
 (data-blob lbl_clicks  "clicks \0" rodata)

 ;; GtkGestureClick "pressed": x,y are gdouble -> mixed gp/f64 params.
 (defun on_press (gesture npress (x :type f64) (y :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 32 (f64-to-i64-trunc x))
    (ptr-write-u64 ctx 40 (f64-to-i64-trunc y))
    (ptr-write-u64 ctx 0 (+ (ptr-read-u64 ctx 0) 1))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 (defun on_draw (area cr width height ctx)
   (let ((c (ptr-read-u64 ctx 0)))
     (seq
      (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
      (extern-call cairo_paint cr)
      ;; cyan marker square centred on the last click
      (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.90) (:f64 0.85))
      (extern-call cairo_rectangle cr
                   (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 32) 6)))
                   (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 40) 6)))
                   (:f64 12.0) (:f64 12.0))
      (extern-call cairo_fill cr)
      ;; clicks counter -> 5-digit decimal text at ctx[24]
      (ptr-write-u8 ctx 24 (+ 48 (mod (/ c 10000) 10)))
      (ptr-write-u8 ctx 25 (+ 48 (mod (/ c 1000) 10)))
      (ptr-write-u8 ctx 26 (+ 48 (mod (/ c 100) 10)))
      (ptr-write-u8 ctx 27 (+ 48 (mod (/ c 10) 10)))
      (ptr-write-u8 ctx 28 (+ 48 (mod c 10)))
      (ptr-write-u8 ctx 29 0)
      (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
      (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
      (extern-call cairo_set_font_size cr (:f64 20.0))
      (extern-call cairo_move_to cr (:f64 12.0) (:f64 26.0))
      (extern-call cairo_show_text cr (data-addr text_hint))
      (extern-call cairo_move_to cr (:f64 12.0) (:f64 120.0))
      (extern-call cairo_show_text cr (data-addr lbl_clicks))
      (extern-call cairo_show_text cr (+ ctx 24))
      0)))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 64)))
      (seq
       (ptr-write-u64 ctx 0 0)
       (ptr-write-u64 ctx 32 150)
       (ptr-write-u64 ctx 40 70)
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 300 150)
          (let ((area    (extern-call gtk_drawing_area_new))
                (gesture (extern-call gtk_gesture_click_new))
                (loop    (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 8 area)
             (ptr-write-u64 ctx 16 loop)
             (extern-call gtk_widget_set_size_request area 300 150)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call g_signal_connect_data
                          gesture (data-addr sig_pressed) (addr-of on_press) ctx 0 0)
             (extern-call gtk_widget_add_controller area gesture)
             (extern-call gtk_window_set_child window area)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 60000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
