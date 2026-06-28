;; Windows-native GTK4 widgets/events demo driven by NeLisp AOT (no added Rust).
;;
;; Builds on gtk-window-native.el (a plain drawing area) by adding real widgets
;; and input, exercising several distinct g_signal callback shapes — all
;; landing in AOT-compiled defuns:
;;   on_draw    GtkDrawingAreaDrawFunc  void(GtkDrawingArea*, cairo_t*, int, int, gpointer)
;;   on_clicked GtkButton::clicked      void(GtkButton*, gpointer)
;;   on_key     GtkEventControllerKey::key-pressed
;;                                      gboolean(self, guint keyval, guint keycode,
;;                                               GdkModifierType state, gpointer)   <- 5 args, bool return
;;   on_tick    GSourceFunc             gboolean(gpointer)   (auto-advance every 800ms)
;;   on_quit    GSourceFunc             gboolean(gpointer)   (one-shot, quits at 60s)
;;   on_destroy GtkWidget::destroy      void(GtkWidget*, gpointer)
;;
;; The 5-arg key callback is the interesting one: under Win64 the 5th arg
;; (user_data) arrives on the stack, and the handler returns a gboolean in rax.
;;
;; Shared mutable state lives in a malloc'd context struct, read/written from
;; the callbacks via ptr-read/write-u64:
;;   ctx[0] = counter   ctx[8] = drawing area   ctx[16] = main loop
;; The background colour toggles with the counter's parity, so every click,
;; key press, or auto-tick visibly flips navy <-> maroon.  Esc or 'q' quits; a
;; 60s one-shot timeout also quits so an unattended run still exits 0.
;;
;; Static link (gtk/cairo/glib symbols resolved at link time):
;;   bash examples/ffi-cairo/gtkwin.sh examples/ffi-cairo/gtk-widgets-native.el \
;;        /tmp/sumi-widgets $(pkg-config --libs gtk4)
(seq
 (data-blob title       "sumi - GTK4 widgets/events (NeLisp AOT)\0" rodata)
 (data-blob btn_label   "click me  (or press any key; Esc/q = quit)\0" rodata)
 (data-blob sig_clicked "clicked\0" rodata)
 (data-blob sig_keyprs  "key-pressed\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_sumi   "sumi\0" rodata)
 (data-blob lbl_count   "count \0" rodata)

 ;; Draw: parity of ctx[0] picks the background colour, and the counter is
 ;; rendered as text.  int -> decimal string is an unrolled digit extraction
 ;; (no loop / no mutable locals needed): each digit is `(mod (/ n 10^k) 10)',
 ;; written as an ASCII byte into the scratch buffer at ctx[24] with
 ;; ptr-write-u8, NUL-terminated, then drawn with cairo_show_text.
 (defun on_draw (area cr width height ctx)
   (let ((n (ptr-read-u64 ctx 0)))
     (seq
      (if (= (logand n 1) 0)
          (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
        (extern-call cairo_set_source_rgb cr (:f64 0.20) (:f64 0.05) (:f64 0.09)))
      (extern-call cairo_paint cr)
      ;; counter -> 5-digit zero-padded decimal at ctx[24..28], NUL at ctx[29]
      (ptr-write-u8 ctx 24 (+ 48 (mod (/ n 10000) 10)))
      (ptr-write-u8 ctx 25 (+ 48 (mod (/ n 1000) 10)))
      (ptr-write-u8 ctx 26 (+ 48 (mod (/ n 100) 10)))
      (ptr-write-u8 ctx 27 (+ 48 (mod (/ n 10) 10)))
      (ptr-write-u8 ctx 28 (+ 48 (mod n 10)))
      (ptr-write-u8 ctx 29 0)
      ;; "sumi" title
      (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
      (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
      (extern-call cairo_set_font_size cr (:f64 44.0))
      (extern-call cairo_move_to cr (:f64 20.0) (:f64 58.0))
      (extern-call cairo_show_text cr (data-addr text_sumi))
      ;; "count NNNNN" below (cairo advances the point, so the number follows
      ;; the label).  The number pointer is ctx + 24.
      (extern-call cairo_set_font_size cr (:f64 26.0))
      (extern-call cairo_move_to cr (:f64 20.0) (:f64 102.0))
      (extern-call cairo_show_text cr (data-addr lbl_count))
      (extern-call cairo_show_text cr (+ ctx 24))
      0)))

 ;; Button "clicked": advance the counter and request a redraw.
 (defun on_clicked (button ctx)
   (seq
    (ptr-write-u64 ctx 0 (+ (ptr-read-u64 ctx 0) 1))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    0))

 ;; Key controller "key-pressed" (5 args, returns gboolean handled).
 ;; Esc (0xff1b) or 'q' (0x71) quits; any other key advances + redraws.
 (defun on_key (controller keyval keycode state ctx)
   (seq
    (if (or (= keyval 65307) (= keyval 113))
        (extern-call g_main_loop_quit (ptr-read-u64 ctx 16))
      (seq
       (ptr-write-u64 ctx 0 (+ (ptr-read-u64 ctx 0) 1))
       (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))))
    1))

 ;; Auto-advance timer (G_SOURCE_CONTINUE = 1 keeps it firing).
 (defun on_tick (ctx)
   (seq
    (ptr-write-u64 ctx 0 (+ (ptr-read-u64 ctx 0) 1))
    (extern-call gtk_widget_queue_draw (ptr-read-u64 ctx 8))
    1))

 (defun on_quit (ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun on_destroy (widget ctx)
   (seq (extern-call g_main_loop_quit (ptr-read-u64 ctx 16)) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((ctx (extern-call malloc 48)))
      (seq
       (ptr-write-u64 ctx 0 0)
       (let ((window (extern-call gtk_window_new)))
         (seq
          (extern-call gtk_window_set_title window (data-addr title))
          (extern-call gtk_window_set_default_size window 300 220)
          (let ((box    (extern-call gtk_box_new 1 6))
                (area   (extern-call gtk_drawing_area_new))
                (button (extern-call gtk_button_new_with_label (data-addr btn_label)))
                (keyctl (extern-call gtk_event_controller_key_new))
                (loop   (extern-call g_main_loop_new 0 0)))
            (seq
             (ptr-write-u64 ctx 8 area)
             (ptr-write-u64 ctx 16 loop)
             (extern-call gtk_widget_set_size_request area 240 120)
             (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) ctx 0)
             (extern-call g_signal_connect_data
                          button (data-addr sig_clicked) (addr-of on_clicked) ctx 0 0)
             (extern-call g_signal_connect_data
                          keyctl (data-addr sig_keyprs) (addr-of on_key) ctx 0 0)
             (extern-call gtk_widget_add_controller window keyctl)
             (extern-call gtk_box_append box area)
             (extern-call gtk_box_append box button)
             (extern-call gtk_window_set_child window box)
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) ctx 0 0)
             (extern-call g_timeout_add 800 (addr-of on_tick) ctx)
             (extern-call g_timeout_add 60000 (addr-of on_quit) ctx)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
