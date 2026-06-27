;; Phase 4: a generic runtime FFI marshaller over `extern-call-ptr'.
;;
;; The AOT model needs each call's arg classes (gp vs f64) fixed at compile
;; time, so a "generic" dynamic call is a DISPATCH over signature SHAPES:
;;   * args are passed uniformly in an i64 buffer (8 bytes per slot); an f64
;;     argument travels as its IEEE-754 bit pattern and is re-materialised in
;;     the thunk via `(:f64 (bits-to-f64 ...))';
;;   * one shape-thunk exists per (arity, f64-bitmask) the FFI needs;
;;   * `ffi-apply' routes (fn, argbuf, sig) to the matching thunk, where
;;     sig = (arity << 8) | f64-mask.
;; This renders the sumi panel entirely through `ffi-apply', proving the
;; generic path.  New shapes are one thunk + one cond arm.

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
 (data-blob png_path    "/mnt/c/Users/kuroz/Cowork/Notes/dev/nelisp/examples/ffi-cairo/sumi-marshalled.png\0" rodata)

 ;; --- shape thunks: read args from buffer AB, materialise, indirect-call ---
 (defun sh_1_0 (fn ab)
   (extern-call-ptr fn (ptr-read-u64 ab 0)))
 (defun sh_2_0 (fn ab)
   (extern-call-ptr fn (ptr-read-u64 ab 0) (ptr-read-u64 ab 8)))
 (defun sh_3_0 (fn ab)
   (extern-call-ptr fn (ptr-read-u64 ab 0) (ptr-read-u64 ab 8) (ptr-read-u64 ab 16)))
 (defun sh_4_0 (fn ab)
   (extern-call-ptr fn (ptr-read-u64 ab 0) (ptr-read-u64 ab 8)
                    (ptr-read-u64 ab 16) (ptr-read-u64 ab 24)))
 (defun sh_2_2 (fn ab)            ; arg1 f64
   (extern-call-ptr fn (ptr-read-u64 ab 0)
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 8)))))
 (defun sh_3_6 (fn ab)            ; args1,2 f64
   (extern-call-ptr fn (ptr-read-u64 ab 0)
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 8)))
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 16)))))
 (defun sh_4_14 (fn ab)           ; args1,2,3 f64
   (extern-call-ptr fn (ptr-read-u64 ab 0)
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 8)))
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 16)))
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 24)))))
 (defun sh_5_30 (fn ab)           ; args1,2,3,4 f64
   (extern-call-ptr fn (ptr-read-u64 ab 0)
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 8)))
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 16)))
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 24)))
                    (:f64 (bits-to-f64 (ptr-read-u64 ab 32)))))

 ;; --- generic dispatcher: sig = (arity << 8) | f64-bitmask ---
 (defun ffi-apply (fn ab sig)
   (cond
    ((= sig 256) (sh_1_0 fn ab))    ; (1<<8)|0
    ((= sig 512) (sh_2_0 fn ab))    ; (2<<8)|0
    ((= sig 768) (sh_3_0 fn ab))    ; (3<<8)|0
    ((= sig 1024) (sh_4_0 fn ab))   ; (4<<8)|0
    ((= sig 514) (sh_2_2 fn ab))    ; (2<<8)|2
    ((= sig 774) (sh_3_6 fn ab))    ; (3<<8)|6
    ((= sig 1038) (sh_4_14 fn ab))  ; (4<<8)|14
    ((= sig 1310) (sh_5_30 fn ab))  ; (5<<8)|30
    (t -1)))

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
             (f_sdestroy (extern-call dlsym h (data-addr s_sdestroy)))
             (ab (frame-alloc 64)))
         ;; surface = cairo_image_surface_create(ARGB32=0, 240, 120)
         (ptr-write-u64 ab 0 0) (ptr-write-u64 ab 8 240) (ptr-write-u64 ab 16 120)
         (let ((surface (ffi-apply f_isc ab 768)))
           (ptr-write-u64 ab 0 surface)
           (let ((cr (ffi-apply f_create ab 256)))
             (seq
              ;; set_source_rgb(cr, 0.07, 0.07, 0.17)   bits packed as f64 args
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 4589708452245819884)   ; 0.07
              (ptr-write-u64 ab 16 4589708452245819884)  ; 0.07
              (ptr-write-u64 ab 24 4595292915783759299)  ; 0.17
              (ffi-apply f_ssr ab 1038)
              ;; rectangle(cr, 0.0, 0.0, 240.0, 120.0)
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 0)                      ; 0.0
              (ptr-write-u64 ab 16 0)                     ; 0.0
              (ptr-write-u64 ab 24 4642648265865560064)   ; 240.0
              (ptr-write-u64 ab 32 4638144666238189568)   ; 120.0
              (ffi-apply f_rect ab 1310)
              ;; fill(cr)
              (ptr-write-u64 ab 0 cr)
              (ffi-apply f_fill ab 256)
              ;; set_source_rgb(cr, 0.94, 0.86, 0.24)
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 4606641986844732948)   ; 0.94
              (ptr-write-u64 ab 16 4605921410904353669)  ; 0.86
              (ptr-write-u64 ab 24 4597814931575086776)  ; 0.24
              (ffi-apply f_ssr ab 1038)
              ;; select_font_face(cr, "sans", 0, 0)
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 (data-addr font_sans))
              (ptr-write-u64 ab 16 0) (ptr-write-u64 ab 24 0)
              (ffi-apply f_sff ab 1024)
              ;; set_font_size(cr, 48.0)
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 4631952216750555136)   ; 48.0
              (ffi-apply f_sfs ab 514)
              ;; move_to(cr, 24.0, 78.0)
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 4627448617123184640)   ; 24.0
              (ptr-write-u64 ab 16 4635189178982727680)  ; 78.0
              (ffi-apply f_moveto ab 774)
              ;; show_text(cr, "sumi")
              (ptr-write-u64 ab 0 cr)
              (ptr-write-u64 ab 8 (data-addr text_sumi))
              (ffi-apply f_showtext ab 512)
              ;; surface_write_to_png(surface, path)
              (ptr-write-u64 ab 0 surface)
              (ptr-write-u64 ab 8 (data-addr png_path))
              (let ((status (ffi-apply f_wtp ab 512)))
                (ptr-write-u64 ab 0 cr)      (ffi-apply f_destroy ab 256)
                (ptr-write-u64 ab 0 surface) (ffi-apply f_sdestroy ab 256)
                status))))))))))
