;; Phase 3 (dynamic path): draw the sumi panel to PNG with NO link-time
;; cairo dependency.  libcairo is dlopen'd at runtime, every cairo entry is
;; resolved with dlsym, and each is invoked through `extern-call-ptr' (an
;; indirect `call r11' with the SysV gp/f64 arg placement).  f64 args use the
;; `(:f64 EXPR)' shape.  Only -ldl is linked; cairo is found at runtime.
;; main returns the PNG write status (0 = ok); 200 = dlopen failed.
(seq
 (data-blob lib_cairo "libcairo.so.2\0" rodata)
 (data-blob s_isc       "cairo_image_surface_create\0" rodata)
 (data-blob s_create    "cairo_create\0" rodata)
 (data-blob s_ssr       "cairo_set_source_rgb\0" rodata)
 (data-blob s_rect      "cairo_rectangle\0" rodata)
 (data-blob s_fill      "cairo_fill\0" rodata)
 (data-blob s_sff       "cairo_select_font_face\0" rodata)
 (data-blob s_sfs       "cairo_set_font_size\0" rodata)
 (data-blob s_moveto    "cairo_move_to\0" rodata)
 (data-blob s_showtext  "cairo_show_text\0" rodata)
 (data-blob s_wtp       "cairo_surface_write_to_png\0" rodata)
 (data-blob s_destroy   "cairo_destroy\0" rodata)
 (data-blob s_sdestroy  "cairo_surface_destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_sumi   "sumi\0" rodata)
 (data-blob png_path    "/mnt/c/Users/kuroz/Cowork/Notes/dev/nelisp/examples/ffi-cairo/sumi-dynamic.png\0" rodata)
 (defun main ()
   (let ((h (extern-call dlopen (data-addr lib_cairo) 2)))
     (if (= h 0)
         200
       (let ((f_isc      (extern-call dlsym h (data-addr s_isc)))
             (f_create   (extern-call dlsym h (data-addr s_create)))
             (f_ssr      (extern-call dlsym h (data-addr s_ssr)))
             (f_rect     (extern-call dlsym h (data-addr s_rect)))
             (f_fill     (extern-call dlsym h (data-addr s_fill)))
             (f_sff      (extern-call dlsym h (data-addr s_sff)))
             (f_sfs      (extern-call dlsym h (data-addr s_sfs)))
             (f_moveto   (extern-call dlsym h (data-addr s_moveto)))
             (f_showtext (extern-call dlsym h (data-addr s_showtext)))
             (f_wtp      (extern-call dlsym h (data-addr s_wtp)))
             (f_destroy  (extern-call dlsym h (data-addr s_destroy)))
             (f_sdestroy (extern-call dlsym h (data-addr s_sdestroy))))
         (let ((surface (extern-call-ptr f_isc 0 240 120)))
           (let ((cr (extern-call-ptr f_create surface)))
             (seq
              ;; dark navy background
              (extern-call-ptr f_ssr cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
              (extern-call-ptr f_rect cr (:f64 0.0) (:f64 0.0) (:f64 240.0) (:f64 120.0))
              (extern-call-ptr f_fill cr)
              ;; yellow title text
              (extern-call-ptr f_ssr cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
              (extern-call-ptr f_sff cr (data-addr font_sans) 0 0)
              (extern-call-ptr f_sfs cr (:f64 48.0))
              (extern-call-ptr f_moveto cr (:f64 24.0) (:f64 78.0))
              (extern-call-ptr f_showtext cr (data-addr text_sumi))
              (let ((status (extern-call-ptr f_wtp surface (data-addr png_path))))
                (extern-call-ptr f_destroy cr)
                (extern-call-ptr f_sdestroy surface)
                status)))))))))
