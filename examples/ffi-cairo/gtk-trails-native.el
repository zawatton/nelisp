;; Windows-native GTK4 particle trails (fading) — NeLisp AOT (no added Rust).
;;
;; Like the particle demo, but particles leave fading trails.  A GtkDrawingArea
;; starts each frame on a blank surface, so trails need a PERSISTENT off-screen
;; cairo image surface that accumulates across frames:
;;   on_tick : integrate particles (time-based), then on the off-screen context
;;             paint a translucent dark rect over everything (cairo_set_source_rgba
;;             alpha ~0.12 → fades old content) and stamp the particles.
;;   on_draw : blit the off-screen surface to the widget
;;             (cairo_set_source_surface + cairo_paint).
;;
;; ctx: 0=last-time 8=area 16=loop 24=particles 32=count 48=dt 56=i
;;      64=surface 72=offscreen-cr.  particle = 32 bytes: x_mp y_mp vx vy (mp/s).
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-trails-native.el \
;;        /tmp/sumi-trails $(pkg-config --libs gtk4)
(seq
 (data-blob title     "sumi - GTK4 particle trails (NeLisp AOT)\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)

 (defun set_color (cr idx)
   (if (= idx 0)
       (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
     (if (= idx 1)
         (extern-call cairo_set_source_rgb cr (:f64 0.92) (:f64 0.30) (:f64 0.30))
       (if (= idx 2)
           (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.85) (:f64 0.85))
         (extern-call cairo_set_source_rgb cr (:f64 0.40) (:f64 0.85) (:f64 0.45))))))

 (defun on_tick (widget clock ctx)
   (seq
    (let ((now (extern-call gdk_frame_clock_get_frame_time clock)))
      (seq
       (ptr-write-u64 ctx 48 (- now (ptr-read-u64 ctx 0)))
       (ptr-write-u64 ctx 0 now)))
    (if (> (ptr-read-u64 ctx 48) 50000) (ptr-write-u64 ctx 48 50000) 0)
    ;; integrate + bounce
    (ptr-write-u64 ctx 56 0)
    (while (< (ptr-read-u64 ctx 56) (ptr-read-u64 ctx 32))
      (seq
       (let ((p (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 56) 32)))
             (dt (ptr-read-u64 ctx 48)))
         (seq
          (ptr-write-u64 p 0 (+ (ptr-read-u64 p 0) (/ (* (ptr-read-u64 p 16) dt) 1000000)))
          (if (< (ptr-read-u64 p 0) 8000)
              (seq (ptr-write-u64 p 0 8000) (ptr-write-u64 p 16 (- 0 (ptr-read-u64 p 16)))) 0)
          (if (> (ptr-read-u64 p 0) 472000)
              (seq (ptr-write-u64 p 0 472000) (ptr-write-u64 p 16 (- 0 (ptr-read-u64 p 16)))) 0)
          (ptr-write-u64 p 8 (+ (ptr-read-u64 p 8) (/ (* (ptr-read-u64 p 24) dt) 1000000)))
          (if (< (ptr-read-u64 p 8) 8000)
              (seq (ptr-write-u64 p 8 8000) (ptr-write-u64 p 24 (- 0 (ptr-read-u64 p 24)))) 0)
          (if (> (ptr-read-u64 p 8) 232000)
              (seq (ptr-write-u64 p 8 232000) (ptr-write-u64 p 24 (- 0 (ptr-read-u64 p 24)))) 0)))
       (ptr-write-u64 ctx 56 (+ (ptr-read-u64 ctx 56) 1))))
    ;; accumulate onto the off-screen: fade, then stamp particles
    (let ((ocr (ptr-read-u64 ctx 72)))
      (seq
       (extern-call cairo_set_source_rgba ocr (:f64 0.05) (:f64 0.05) (:f64 0.12) (:f64 0.12))
       (extern-call cairo_paint ocr)
       (ptr-write-u64 ctx 56 0)
       (while (< (ptr-read-u64 ctx 56) (ptr-read-u64 ctx 32))
         (seq
          (let ((p (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 56) 32))))
            (seq
             (set_color ocr (mod (ptr-read-u64 ctx 56) 4))
             (extern-call cairo_arc ocr
                          (:f64 (i64-to-f64 (/ (ptr-read-u64 p 0) 1000)))
                          (:f64 (i64-to-f64 (/ (ptr-read-u64 p 8) 1000)))
                          (:f64 6.0) (:f64 0.0) (:f64 6.2832))
             (extern-call cairo_fill ocr)))
          (ptr-write-u64 ctx 56 (+ (ptr-read-u64 ctx 56) 1))))))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    1))

 ;; Blit the accumulated off-screen surface to the widget.
 (defun on_draw (area cr width height ctx)
   (seq
    (extern-call cairo_set_source_surface cr (ptr-read-u64 ctx 64) (:f64 0.0) (:f64 0.0))
    (extern-call cairo_paint cr)
    0))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 96))
          (pts (extern-call malloc 1024))
          (surf (extern-call cairo_image_surface_create 0 480 240)))
      (seq
       (ptr-write-u64 ctx 0 0)
       (ptr-write-u64 ctx 24 pts)
       (ptr-write-u64 ctx 32 16)
       (ptr-write-u64 ctx 64 surf)
       (ptr-write-u64 ctx 72 (extern-call cairo_create surf))
       ;; clear the off-screen to the background colour
       (extern-call cairo_set_source_rgb (ptr-read-u64 ctx 72) (:f64 0.05) (:f64 0.05) (:f64 0.12))
       (extern-call cairo_paint (ptr-read-u64 ctx 72))
       ;; seed particles
       (ptr-write-u64 ctx 56 0)
       (while (< (ptr-read-u64 ctx 56) 16)
         (seq
          (let ((i (ptr-read-u64 ctx 56)))
            (let ((p (+ pts (* i 32))))
              (seq
               (ptr-write-u64 p 0 (* (+ 30 (mod (* i 53) 420)) 1000))
               (ptr-write-u64 p 8 (* (+ 30 (mod (* i 37) 180)) 1000))
               (ptr-write-u64 p 16 (if (= (mod i 2) 0)
                                       (* (+ 60 (mod (* i 29) 90)) 1000)
                                     (- 0 (* (+ 60 (mod (* i 29) 90)) 1000))))
               (ptr-write-u64 p 24 (if (= (mod i 3) 0)
                                       (* (+ 50 (mod (* i 41) 80)) 1000)
                                     (- 0 (* (+ 50 (mod (* i 41) 80)) 1000)))))))
          (ptr-write-u64 ctx 56 (+ (ptr-read-u64 ctx 56) 1))))
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 480 240)
          (let ((area (extern-call gtk_drawing_area_new))
                (loop (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 8 area)
             (ptr-write-u64 ctx 16 loop)
             (extern-call gtk_widget_set_size_request area 480 240)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call gtk_widget_add_tick_callback area (addr-of on_tick) ctx 0)
             (extern-call gtk_window_set_child window area)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 60000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
