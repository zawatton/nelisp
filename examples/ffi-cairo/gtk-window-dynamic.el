;; Windows-native GTK4 window via the DYNAMIC FFI path — the Windows sibling of
;; cairo-dynamic.el (which used dlopen/dlsym on Linux).  Here every gtk / glib /
;; gobject / cairo entry point is resolved at runtime with kernel32's
;; LoadLibraryA + GetProcAddress and invoked through `extern-call-ptr'.
;;
;; The payoff vs gtk-window-native.el (static): the linked binary has NO import
;; dependency on gtk/cairo/glib at all — only kernel32 (+ the CRT).  Verify:
;;   objdump -p OUT.exe | grep 'DLL Name'   # => KERNEL32.dll, msvcrt.dll only
;; All of GTK is pulled in by name at runtime, exactly like the Linux dlopen
;; example.  Link with `gcc OUT.o -o OUT.exe' (no `pkg-config --libs gtk4').
;;
;; The cairo library handle is handed to on_draw via GTK's draw-func user_data,
;; so the draw callback resolves cairo entries on demand.  on_destroy /
;; on_timeout re-resolve g_main_loop_quit from glib (called rarely).  Win64
;; defun prologues save rbx, so these cairo-calling callbacks invoked from deep
;; inside GTK do not corrupt its callee-saved registers.
(seq
 (data-blob lib_gtk     "libgtk-4-1.dll\0" rodata)
 (data-blob lib_cairo   "libcairo-2.dll\0" rodata)
 (data-blob lib_glib    "libglib-2.0-0.dll\0" rodata)
 (data-blob lib_gobject "libgobject-2.0-0.dll\0" rodata)
 (data-blob s_init      "gtk_init\0" rodata)
 (data-blob s_winnew    "gtk_window_new\0" rodata)
 (data-blob s_title     "gtk_window_set_title\0" rodata)
 (data-blob s_size      "gtk_window_set_default_size\0" rodata)
 (data-blob s_areanew   "gtk_drawing_area_new\0" rodata)
 (data-blob s_drawfn    "gtk_drawing_area_set_draw_func\0" rodata)
 (data-blob s_setchild  "gtk_window_set_child\0" rodata)
 (data-blob s_present   "gtk_window_present\0" rodata)
 (data-blob s_loopnew   "g_main_loop_new\0" rodata)
 (data-blob s_looprun   "g_main_loop_run\0" rodata)
 (data-blob s_loopquit  "g_main_loop_quit\0" rodata)
 (data-blob s_timeout   "g_timeout_add\0" rodata)
 (data-blob s_connect   "g_signal_connect_data\0" rodata)
 (data-blob s_ssr       "cairo_set_source_rgb\0" rodata)
 (data-blob s_paint     "cairo_paint\0" rodata)
 (data-blob s_sff       "cairo_select_font_face\0" rodata)
 (data-blob s_sfs       "cairo_set_font_size\0" rodata)
 (data-blob s_moveto    "cairo_move_to\0" rodata)
 (data-blob s_showtext  "cairo_show_text\0" rodata)
 (data-blob title       "sumi (NeLisp -> GTK4, dynamic, Windows native)\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_sumi   "sumi\0" rodata)

 ;; GtkDrawingAreaDrawFunc — user_data (5th arg) is the cairo lib handle.
 (defun on_draw (area cr width height hc)
   (let ((f_ssr      (extern-call GetProcAddress hc (data-addr s_ssr)))
         (f_paint    (extern-call GetProcAddress hc (data-addr s_paint)))
         (f_sff      (extern-call GetProcAddress hc (data-addr s_sff)))
         (f_sfs      (extern-call GetProcAddress hc (data-addr s_sfs)))
         (f_moveto   (extern-call GetProcAddress hc (data-addr s_moveto)))
         (f_showtext (extern-call GetProcAddress hc (data-addr s_showtext))))
     (seq
      (extern-call-ptr f_ssr cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
      (extern-call-ptr f_paint cr)
      (extern-call-ptr f_ssr cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
      (extern-call-ptr f_sff cr (data-addr font_sans) 0 0)
      (extern-call-ptr f_sfs cr (:f64 48.0))
      (extern-call-ptr f_moveto cr (:f64 24.0) (:f64 78.0))
      (extern-call-ptr f_showtext cr (data-addr text_sumi))
      0)))

 ;; Quit the loop on window close — re-resolve g_main_loop_quit from glib.
 (defun on_destroy (widget loop)
   (let ((hl (extern-call LoadLibraryA (data-addr lib_glib))))
     (let ((f_quit (extern-call GetProcAddress hl (data-addr s_loopquit))))
       (seq (extern-call-ptr f_quit loop) 0))))

 ;; GSourceFunc auto-quit safety net; return FALSE (0) to remove the source.
 (defun on_timeout (loop)
   (let ((hl (extern-call LoadLibraryA (data-addr lib_glib))))
     (let ((f_quit (extern-call GetProcAddress hl (data-addr s_loopquit))))
       (seq (extern-call-ptr f_quit loop) 0))))

 (defun main ()
   (let ((hg (extern-call LoadLibraryA (data-addr lib_gtk)))
         (hc (extern-call LoadLibraryA (data-addr lib_cairo)))
         (hl (extern-call LoadLibraryA (data-addr lib_glib)))
         (ho (extern-call LoadLibraryA (data-addr lib_gobject))))
     (if (= hg 0)
         200
       (let ((f_init    (extern-call GetProcAddress hg (data-addr s_init)))
             (f_winnew  (extern-call GetProcAddress hg (data-addr s_winnew)))
             (f_title   (extern-call GetProcAddress hg (data-addr s_title)))
             (f_size    (extern-call GetProcAddress hg (data-addr s_size)))
             (f_areanew (extern-call GetProcAddress hg (data-addr s_areanew)))
             (f_drawfn  (extern-call GetProcAddress hg (data-addr s_drawfn)))
             (f_setchild(extern-call GetProcAddress hg (data-addr s_setchild)))
             (f_present (extern-call GetProcAddress hg (data-addr s_present)))
             (f_loopnew (extern-call GetProcAddress hl (data-addr s_loopnew)))
             (f_looprun (extern-call GetProcAddress hl (data-addr s_looprun)))
             (f_timeout (extern-call GetProcAddress hl (data-addr s_timeout)))
             (f_connect (extern-call GetProcAddress ho (data-addr s_connect))))
         (seq
          (extern-call-ptr f_init)
          (let ((window (extern-call-ptr f_winnew)))
            (seq
             (extern-call-ptr f_title window (data-addr title))
             (extern-call-ptr f_size window 240 120)
             (let ((area (extern-call-ptr f_areanew)))
               (seq
                (extern-call-ptr f_drawfn area (addr-of on_draw) hc 0)
                (extern-call-ptr f_setchild window area)
                (let ((loop (extern-call-ptr f_loopnew 0 0)))
                  (seq
                   (extern-call-ptr f_connect
                                    window (data-addr sig_destroy) (addr-of on_destroy) loop 0 0)
                   (extern-call-ptr f_timeout 30000 (addr-of on_timeout) loop)
                   (extern-call-ptr f_present window)
                   (extern-call-ptr f_looprun loop)
                   0))))))))))))
