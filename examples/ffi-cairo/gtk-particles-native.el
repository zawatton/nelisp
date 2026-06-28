;; Windows-native GTK4 particle system, time-based — NeLisp AOT (no added Rust).
;;
;; N coloured particles bounce around the canvas.  Motion is TIME-based, not
;; frame-based: the per-frame tick callback reads gdk_frame_clock_get_frame_time
;; (microseconds, an i64 return) and integrates each particle by
;; velocity * dt / 1e6, so speed is constant regardless of frame rate.
;;
;; Positions are kept in "millipixels" (pixel * 1000) so the integer integrator
;; keeps sub-pixel precision; velocities are millipixels per second.  The first
;; frame's huge dt (last=0) is clamped.
;;
;; ctx: 0=last-frame-time 8=area 16=loop 24=particles 32=count 48=dt 56=i
;; each particle = 32 bytes: [0]x_mp [8]y_mp [16]vx [24]vy.  canvas 480x240.
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-particles-native.el \
;;        /tmp/sumi-particles $(pkg-config --libs gtk4)
(seq
 (data-blob title     "sumi - GTK4 particles, time-based (NeLisp AOT)\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)

 (defun set_color (cr idx)
   (if (= idx 0)
       (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
     (if (= idx 1)
         (extern-call cairo_set_source_rgb cr (:f64 0.92) (:f64 0.30) (:f64 0.30))
       (if (= idx 2)
           (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.85) (:f64 0.85))
         (extern-call cairo_set_source_rgb cr (:f64 0.40) (:f64 0.85) (:f64 0.45))))))

 ;; Per-frame tick: dt from the frame clock, integrate + bounce every particle.
 (defun on_tick (widget clock ctx)
   (seq
    (let ((now (extern-call gdk_frame_clock_get_frame_time clock)))
      (seq
       (ptr-write-u64 ctx 48 (- now (ptr-read-u64 ctx 0)))
       (ptr-write-u64 ctx 0 now)))
    ;; clamp dt (first frame / stalls) to <= 50 ms
    (if (> (ptr-read-u64 ctx 48) 50000) (ptr-write-u64 ctx 48 50000) 0)
    (ptr-write-u64 ctx 56 0)
    (while (< (ptr-read-u64 ctx 56) (ptr-read-u64 ctx 32))
      (seq
       (let ((p (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 56) 32)))
             (dt (ptr-read-u64 ctx 48)))
         (seq
          ;; x
          (ptr-write-u64 p 0 (+ (ptr-read-u64 p 0) (/ (* (ptr-read-u64 p 16) dt) 1000000)))
          (if (< (ptr-read-u64 p 0) 8000)
              (seq (ptr-write-u64 p 0 8000) (ptr-write-u64 p 16 (- 0 (ptr-read-u64 p 16)))) 0)
          (if (> (ptr-read-u64 p 0) 472000)
              (seq (ptr-write-u64 p 0 472000) (ptr-write-u64 p 16 (- 0 (ptr-read-u64 p 16)))) 0)
          ;; y
          (ptr-write-u64 p 8 (+ (ptr-read-u64 p 8) (/ (* (ptr-read-u64 p 24) dt) 1000000)))
          (if (< (ptr-read-u64 p 8) 8000)
              (seq (ptr-write-u64 p 8 8000) (ptr-write-u64 p 24 (- 0 (ptr-read-u64 p 24)))) 0)
          (if (> (ptr-read-u64 p 8) 232000)
              (seq (ptr-write-u64 p 8 232000) (ptr-write-u64 p 24 (- 0 (ptr-read-u64 p 24)))) 0)))
       (ptr-write-u64 ctx 56 (+ (ptr-read-u64 ctx 56) 1))))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    1))

 (defun on_draw (area cr width height ctx)
   (seq
    (extern-call cairo_set_source_rgb cr (:f64 0.05) (:f64 0.05) (:f64 0.12))
    (extern-call cairo_paint cr)
    (ptr-write-u64 ctx 56 0)
    (while (< (ptr-read-u64 ctx 56) (ptr-read-u64 ctx 32))
      (seq
       (let ((p (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 56) 32))))
         (seq
          (set_color cr (mod (ptr-read-u64 ctx 56) 4))
          (extern-call cairo_arc cr
                       (:f64 (i64-to-f64 (/ (ptr-read-u64 p 0) 1000)))
                       (:f64 (i64-to-f64 (/ (ptr-read-u64 p 8) 1000)))
                       (:f64 7.0) (:f64 0.0) (:f64 6.2832))
          (extern-call cairo_fill cr)))
       (ptr-write-u64 ctx 56 (+ (ptr-read-u64 ctx 56) 1))))
    0))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 96))
          (pts (extern-call malloc 1024)))
      (seq
       (ptr-write-u64 ctx 0 0)
       (ptr-write-u64 ctx 24 pts)
       (ptr-write-u64 ctx 32 16)
       ;; init 16 particles with index-derived positions + velocities
       (ptr-write-u64 ctx 56 0)
       (while (< (ptr-read-u64 ctx 56) 16)
         (seq
          (let ((i (ptr-read-u64 ctx 56)))
            (let ((p (+ pts (* i 32))))
              (seq
               (ptr-write-u64 p 0 (* (+ 30 (mod (* i 53) 420)) 1000))
               (ptr-write-u64 p 8 (* (+ 30 (mod (* i 37) 180)) 1000))
               (ptr-write-u64 p 16 (if (= (mod i 2) 0)
                                       (* (+ 50 (mod (* i 29) 80)) 1000)
                                     (- 0 (* (+ 50 (mod (* i 29) 80)) 1000))))
               (ptr-write-u64 p 24 (if (= (mod i 3) 0)
                                       (* (+ 40 (mod (* i 41) 70)) 1000)
                                     (- 0 (* (+ 40 (mod (* i 41) 70)) 1000)))))))
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
