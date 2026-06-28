;; Windows-native GTK4 paint app — NeLisp AOT (no added Rust).
;; Toolbar (GtkEntry caption + undo + save + line-width GtkScale + 4 colour
;; buttons) above a drawing canvas.  Features:
;;   * Colour palette  — each colour button sets the current colour index; every
;;                        stroke stores its colour, so on_draw strokes the path
;;                        one stroke at a time with the right source.
;;   * Line-width slider — a GtkScale; its "value-changed" handler calls
;;                        gtk_range_get_value, which RETURNS a gdouble, captured
;;                        with (f64-bits (extern-call-f64 ...)) as i64 bits into
;;                        ctx[80]; paint reads it back with bits-to-f64 for
;;                        cairo_set_line_width.  (The one f64 *return* here.)
;;   * Undo            — scans the point array backward (a `while' loop) to the
;;                        last stroke-start and truncates the count (array trunc).
;;   * Caption entry   — the GtkEntry's "changed" redraws; on_draw reads its live
;;                        text via gtk_editable_get_text (font "Meiryo" for CJK).
;;   * PNG save        — `paint_canvas' renders the identical scene to the live
;;                        widget AND an off-screen image surface written with
;;                        cairo_surface_write_to_png.
;;
;; ctx: 0=colour 8=area 16=loop 24=points 32=count 40=pressed 48=hover_x
;;      56=hover_y 64=i 72=entry 80=line-width(f64 bits).  point = 32 bytes:
;;      [0]x [8]y [16]flag [24]colour.  Change png_path / font_jp to taste.
;;
;; Static link:
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-paint-app-native.el \
;;        /tmp/sumi-paint-app $(pkg-config --libs gtk4)
(seq
 (data-blob title       "sumi - GTK4 paint (NeLisp AOT)\0" rodata)
 (data-blob sig_motion  "motion\0" rodata)
 (data-blob sig_dbegin  "drag-begin\0" rodata)
 (data-blob sig_dend    "drag-end\0" rodata)
 (data-blob sig_clicked "clicked\0" rodata)
 (data-blob sig_changed "changed\0" rodata)
 (data-blob sig_vchg    "value-changed\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_jp     "Meiryo\0" rodata)
 (data-blob lbl_undo    "undo\0" rodata)
 (data-blob lbl_save    "save\0" rodata)
 (data-blob lbl_y       "Y\0" rodata)
 (data-blob lbl_r       "R\0" rodata)
 (data-blob lbl_c       "C\0" rodata)
 (data-blob lbl_g       "G\0" rodata)
 (data-blob png_path    "C:/Users/kuroz/AppData/Local/Temp/sumi-canvas.png\0" rodata)

 (defun set_color (cr idx)
   (if (= idx 0)
       (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
     (if (= idx 1)
         (extern-call cairo_set_source_rgb cr (:f64 0.92) (:f64 0.30) (:f64 0.30))
       (if (= idx 2)
           (extern-call cairo_set_source_rgb cr (:f64 0.30) (:f64 0.85) (:f64 0.85))
         (extern-call cairo_set_source_rgb cr (:f64 0.40) (:f64 0.85) (:f64 0.45))))))

 (defun add_point (ctx x y flag color)
   (let ((cnt (ptr-read-u64 ctx 32)))
     (if (< cnt 4000)
         (let ((p (+ (ptr-read-u64 ctx 24) (* cnt 32))))
           (seq
            (ptr-write-u64 p 0 x)
            (ptr-write-u64 p 8 y)
            (ptr-write-u64 p 16 flag)
            (ptr-write-u64 p 24 color)
            (ptr-write-u64 ctx 32 (+ cnt 1))
            0))
       0)))

 (defun paint_canvas (cr ctx)
   (seq
    (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
    (extern-call cairo_paint cr)
    ;; line width comes from the slider (stored as f64 bits in ctx[80])
    (extern-call cairo_set_line_width cr (:f64 (bits-to-f64 (ptr-read-u64 ctx 80))))
    (ptr-write-u64 ctx 64 0)
    (while (< (ptr-read-u64 ctx 64) (ptr-read-u64 ctx 32))
      (seq
       (let ((p (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 64) 32))))
         (if (= (ptr-read-u64 p 16) 1)
             (seq
              (if (> (ptr-read-u64 ctx 64) 0) (extern-call cairo_stroke cr) 0)
              (set_color cr (ptr-read-u64 p 24))
              (extern-call cairo_move_to cr
                           (:f64 (i64-to-f64 (ptr-read-u64 p 0)))
                           (:f64 (i64-to-f64 (ptr-read-u64 p 8)))))
           (extern-call cairo_line_to cr
                        (:f64 (i64-to-f64 (ptr-read-u64 p 0)))
                        (:f64 (i64-to-f64 (ptr-read-u64 p 8))))))
       (ptr-write-u64 ctx 64 (+ (ptr-read-u64 ctx 64) 1))))
    (extern-call cairo_stroke cr)
    (extern-call cairo_set_source_rgb cr (:f64 0.80) (:f64 0.80) (:f64 0.80))
    (extern-call cairo_select_font_face cr (data-addr font_jp) 0 0)
    (extern-call cairo_set_font_size cr (:f64 16.0))
    (extern-call cairo_move_to cr (:f64 8.0) (:f64 18.0))
    (extern-call cairo_show_text cr (extern-call gtk_editable_get_text (ptr-read-u64 ctx 72)))
    0))

 (defun on_motion (self (x :type f64) (y :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 48 (f64-to-i64-trunc x))
    (ptr-write-u64 ctx 56 (f64-to-i64-trunc y))
    (if (= (ptr-read-u64 ctx 40) 0)
        0
      (add_point ctx (f64-to-i64-trunc x) (f64-to-i64-trunc y) 0 (ptr-read-u64 ctx 0)))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 (defun on_drag_begin (self (sx :type f64) (sy :type f64) ctx)
   (seq
    (ptr-write-u64 ctx 40 1)
    (add_point ctx (f64-to-i64-trunc sx) (f64-to-i64-trunc sy) 1 (ptr-read-u64 ctx 0))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 (defun on_drag_end (self (ox :type f64) (oy :type f64) ctx)
   (seq (ptr-write-u64 ctx 40 0) 0))

 (defun on_pick0 (button ctx) (seq (ptr-write-u64 ctx 0 0) 0))
 (defun on_pick1 (button ctx) (seq (ptr-write-u64 ctx 0 1) 0))
 (defun on_pick2 (button ctx) (seq (ptr-write-u64 ctx 0 2) 0))
 (defun on_pick3 (button ctx) (seq (ptr-write-u64 ctx 0 3) 0))

 ;; Slider moved: read the gdouble value (f64 return) and store its bits.
 (defun on_scale (range ctx)
   (seq
    (ptr-write-u64 ctx 80 (f64-bits (extern-call-f64 gtk_range_get_value range)))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 (defun on_undo (button ctx)
   (seq
    (if (= (ptr-read-u64 ctx 32) 0)
        0
      (seq
       (ptr-write-u64 ctx 64 (- (ptr-read-u64 ctx 32) 1))
       (while (and (> (ptr-read-u64 ctx 64) 0)
                   (= (ptr-read-u64 (+ (ptr-read-u64 ctx 24) (* (ptr-read-u64 ctx 64) 32)) 16) 0))
         (ptr-write-u64 ctx 64 (- (ptr-read-u64 ctx 64) 1)))
       (ptr-write-u64 ctx 32 (ptr-read-u64 ctx 64))))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 (defun on_save (button ctx)
   (let ((surface (extern-call cairo_image_surface_create 0 480 200)))
     (let ((scr (extern-call cairo_create surface)))
       (seq
        (paint_canvas scr ctx)
        (extern-call cairo_surface_write_to_png surface (data-addr png_path))
        (extern-call cairo_destroy scr)
        (extern-call cairo_surface_destroy surface)
        0))))

 (defun on_changed (editable ctx)
   (seq (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8)) 0))

 (defun on_draw (area cr width height ctx)
   (seq
    (paint_canvas cr ctx)
    ;; hover marker in the current colour (live only, not saved)
    (set_color cr (ptr-read-u64 ctx 0))
    (extern-call cairo_rectangle cr
                 (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 48) 3)))
                 (:f64 (i64-to-f64 (- (ptr-read-u64 ctx 56) 3)))
                 (:f64 6.0) (:f64 6.0))
    (extern-call cairo_fill cr)
    0))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))
 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun add_button (bar label handler ctx)
   (let ((b (extern-call gtk_button_new_with_label label)))
     (seq
      (extern-call g_signal_connect_data b (data-addr sig_clicked) handler ctx 0 0)
      (extern-call gtk_box_append bar b)
      0)))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 96))
          (pts (extern-call malloc 128000)))
      (seq
       (ptr-write-u64 ctx 0 0)
       (ptr-write-u64 ctx 24 pts)
       (ptr-write-u64 ctx 32 0)
       (ptr-write-u64 ctx 40 0)
       (ptr-write-u64 ctx 48 160)
       (ptr-write-u64 ctx 56 120)
       (ptr-write-u64 ctx 80 (f64-bits (i64-to-f64 3)))
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 480 260)
          (let ((vbox  (extern-call gtk_box_new 1 4))
                (bar   (extern-call gtk_box_new 0 4))
                (entry (extern-call gtk_entry_new))
                (scale (extern-call gtk_scale_new_with_range 0 (:f64 1.0) (:f64 16.0) (:f64 1.0)))
                (area  (extern-call gtk_drawing_area_new))
                (motion (extern-call gtk_event_controller_motion_new))
                (drag  (extern-call gtk_gesture_drag_new))
                (loop  (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 8 area)
             (ptr-write-u64 ctx 16 loop)
             (ptr-write-u64 ctx 72 entry)
             (extern-call gtk_box_append bar entry)
             (add_button bar (data-addr lbl_undo) (addr-of on_undo) ctx)
             (add_button bar (data-addr lbl_save) (addr-of on_save) ctx)
             ;; line-width slider (its value-changed handler reads an f64 back)
             (extern-call gtk_widget_set_size_request scale 120 16)
             (extern-call gtk_range_set_value scale (:f64 3.0))
             (extern-call g_signal_connect_data
                          scale (data-addr sig_vchg) (addr-of on_scale) ctx 0 0)
             (extern-call gtk_box_append bar scale)
             (add_button bar (data-addr lbl_y) (addr-of on_pick0) ctx)
             (add_button bar (data-addr lbl_r) (addr-of on_pick1) ctx)
             (add_button bar (data-addr lbl_c) (addr-of on_pick2) ctx)
             (add_button bar (data-addr lbl_g) (addr-of on_pick3) ctx)
             (extern-call g_signal_connect_data
                          entry (data-addr sig_changed) (addr-of on_changed) ctx 0 0)
             (extern-call gtk_widget_set_size_request area 480 200)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call g_signal_connect_data
                          motion (data-addr sig_motion) (addr-of on_motion) ctx 0 0)
             (extern-call gtk_widget_add_controller area motion)
             (extern-call g_signal_connect_data
                          drag (data-addr sig_dbegin) (addr-of on_drag_begin) ctx 0 0)
             (extern-call g_signal_connect_data
                          drag (data-addr sig_dend) (addr-of on_drag_end) ctx 0 0)
             (extern-call gtk_widget_add_controller area drag)
             (extern-call gtk_box_append vbox bar)
             (extern-call gtk_box_append vbox area)
             (extern-call gtk_window_set_child window vbox)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 90000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
