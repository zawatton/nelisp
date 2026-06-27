;; Phase 3 (static path): draw a sumi panel to PNG by calling libcairo
;; through static `extern-call' (link-time, -lcairo).  f64 args use the
;; `(:f64 EXPR)' arg shape (xmm0-7 per SysV).  C-strings come from
;; `data-blob' + `data-addr'.  `main' returns the PNG write status (0 = ok).
(seq
 (data-blob font_sans "sans\0" rodata)
 (data-blob text_sumi "sumi\0" rodata)
 (data-blob png_path "/mnt/c/Users/kuroz/Cowork/Notes/dev/nelisp/examples/ffi-cairo/sumi-static.png\0" rodata)
 (defun main ()
   ;; cairo_image_surface_create(CAIRO_FORMAT_ARGB32=0, 240, 120)
   (let ((surface (extern-call cairo_image_surface_create 0 240 120)))
     (let ((cr (extern-call cairo_create surface)))
       (seq
        ;; dark navy background
        (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
        (extern-call cairo_rectangle cr (:f64 0.0) (:f64 0.0) (:f64 240.0) (:f64 120.0))
        (extern-call cairo_fill cr)
        ;; yellow title text
        (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
        (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
        (extern-call cairo_set_font_size cr (:f64 48.0))
        (extern-call cairo_move_to cr (:f64 24.0) (:f64 78.0))
        (extern-call cairo_show_text cr (data-addr text_sumi))
        ;; flush to PNG, then tear down
        (let ((status (extern-call cairo_surface_write_to_png surface (data-addr png_path))))
          (extern-call cairo_destroy cr)
          (extern-call cairo_surface_destroy surface)
          status))))))
