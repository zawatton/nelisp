;; Windows-native GTK4 animation demo driven by NeLisp AOT (no added Rust).
;;
;; gtk_widget_add_tick_callback registers a per-frame callback synced to the
;; display's frame clock (~60 fps):
;;   on_tick  GtkTickCallback  gboolean(GtkWidget*, GdkFrameClock*, gpointer)
;; It returns G_SOURCE_CONTINUE (1) to keep animating.  Each frame it advances a
;; ball's integer position by its velocity, bounces it off the edges (negating
;; the velocity), bumps a frame counter, and queues a redraw.  on_draw paints
;; the ball with cairo_arc (its colour cycles every ~0.5 s) and shows the frame
;; count.  Pure integer physics — no sin/cos / libm needed.
;;
;; ctx: 0=bx 8=by 16=vx 24=vy 32=area 40=loop 48=frame 56=text-buf
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-anim-native.el \
;;        /tmp/sumi-anim $(pkg-config --libs gtk4)
(seq
 (data-blob title     "sumi - GTK4 animation (NeLisp AOT)\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans "sans\0" rodata)
 (data-blob lbl_frame "frame \0" rodata)

 (defun set_color (cr idx)
   (if (= idx 0)
       (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
     (if (= idx 1)
         (extern-call cairo_set_source_rgb cr (:f64 0.92) (:f64 0.30) (:f64 0.30))
       (if (= idx 2)
           (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.85) (:f64 0.85))
         (extern-call cairo_set_source_rgb cr (:f64 0.40) (:f64 0.85) (:f64 0.45))))))

 ;; Per-frame tick: integrate position, bounce, redraw.  Returns G_SOURCE_CONTINUE.
 (defun on_tick (widget frame_clock ctx)
   (seq
    ;; x
    (ptr-write-u64 ctx 0 (+ (ptr-read-u64 ctx 0) (ptr-read-u64 ctx 16)))
    (if (< (ptr-read-u64 ctx 0) 16)
        (seq (ptr-write-u64 ctx 0 16) (ptr-write-u64 ctx 16 (- 0 (ptr-read-u64 ctx 16)))) 0)
    (if (> (ptr-read-u64 ctx 0) 344)
        (seq (ptr-write-u64 ctx 0 344) (ptr-write-u64 ctx 16 (- 0 (ptr-read-u64 ctx 16)))) 0)
    ;; y
    (ptr-write-u64 ctx 8 (+ (ptr-read-u64 ctx 8) (ptr-read-u64 ctx 24)))
    (if (< (ptr-read-u64 ctx 8) 16)
        (seq (ptr-write-u64 ctx 8 16) (ptr-write-u64 ctx 24 (- 0 (ptr-read-u64 ctx 24)))) 0)
    (if (> (ptr-read-u64 ctx 8) 184)
        (seq (ptr-write-u64 ctx 8 184) (ptr-write-u64 ctx 24 (- 0 (ptr-read-u64 ctx 24)))) 0)
    (ptr-write-u64 ctx 48 (+ (ptr-read-u64 ctx 48) 1))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 32))
    1))

 (defun on_draw (area cr width height ctx)
   (let ((f (ptr-read-u64 ctx 48)))
     (seq
      (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
      (extern-call cairo_paint cr)
      ;; the ball — colour cycles every 30 frames
      (set_color cr (mod (/ f 30) 4))
      (extern-call cairo_arc cr
                   (:f64 (i64-to-f64 (ptr-read-u64 ctx 0)))
                   (:f64 (i64-to-f64 (ptr-read-u64 ctx 8)))
                   (:f64 12.0) (:f64 0.0) (:f64 6.2832))
      (extern-call cairo_fill cr)
      ;; frame counter -> 5-digit text at ctx[56]
      (ptr-write-u8 ctx 56 (+ 48 (mod (/ f 10000) 10)))
      (ptr-write-u8 ctx 57 (+ 48 (mod (/ f 1000) 10)))
      (ptr-write-u8 ctx 58 (+ 48 (mod (/ f 100) 10)))
      (ptr-write-u8 ctx 59 (+ 48 (mod (/ f 10) 10)))
      (ptr-write-u8 ctx 60 (+ 48 (mod f 10)))
      (ptr-write-u8 ctx 61 0)
      (extern-call cairo_set_source_rgb cr (:f64 0.75) (:f64 0.75) (:f64 0.75))
      (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
      (extern-call cairo_set_font_size cr (:f64 15.0))
      (extern-call cairo_move_to cr (:f64 10.0) (:f64 20.0))
      (extern-call cairo_show_text cr (data-addr lbl_frame))
      (extern-call cairo_show_text cr (+ ctx 56))
      0)))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 40)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 40)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 96)))
      (seq
       (ptr-write-u64 ctx 0 60)
       (ptr-write-u64 ctx 8 50)
       (ptr-write-u64 ctx 16 4)
       (ptr-write-u64 ctx 24 3)
       (ptr-write-u64 ctx 48 0)
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 360 200)
          (let ((area (extern-call gtk_drawing_area_new))
                (loop (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 32 area)
             (ptr-write-u64 ctx 40 loop)
             (extern-call gtk_widget_set_size_request area 360 200)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call gtk_widget_add_tick_callback area (addr-of on_tick) ctx 0)
             (extern-call gtk_window_set_child window area)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 60000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
