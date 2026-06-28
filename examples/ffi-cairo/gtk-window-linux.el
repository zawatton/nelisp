;; Linux/WSL GTK4 window via the dynamic FFI path (dlopen/dlsym + extern-call-ptr).
;;
;; The Linux sibling of gtk-window-dynamic.el (which used Windows
;; LoadLibraryA/GetProcAddress).  gtk/cairo/glib are dlopen'd by name; only
;; -ldl is linked.  GTK calls back into AOT-compiled SysV defuns (on_draw /
;; on_destroy / on_timeout) to paint the "sumi" panel.
;;
;; This example only became safe once the SysV defun prologue learned to save
;; rbx: on_draw paints via cairo `extern-call's whose dynamic-alignment idiom
;; (`mov rbx, rsp') clobbers rbx, and GTK invokes on_draw from deep inside its
;; closure dispatch with live callee-saved state.  rbx is callee-saved in SysV,
;; so without the prologue save/restore the callback would corrupt GTK's
;; registers on return — exactly the win64 fix, now mirrored for SysV.
;;
;; Build + run (compile on a mingw host, run the ELF under WSL with WSLg):
;;   emacs -Q --batch -L lisp -L src -l nelisp-aot-compiler \
;;     --eval "(nelisp-aot-compile-to-object SEXP \"/path/gtk.o\" :format 'elf)"
;;   wsl -e bash -lc "cc gtk.o -ldl -o gtk && GSK_RENDERER=cairo ./gtk"
;; (Native Linux: the same cc/run, without the wsl wrapper.)
(seq
 (data-blob lib_gtk     "libgtk-4.so.1\0" rodata)
 (data-blob lib_cairo   "libcairo.so.2\0" rodata)
 (data-blob lib_glib    "libglib-2.0.so.0\0" rodata)
 (data-blob lib_gobject "libgobject-2.0.so.0\0" rodata)
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
 (data-blob title       "sumi (NeLisp -> GTK4, Linux/WSL)\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_sumi   "sumi\0" rodata)

 ;; GtkDrawingAreaDrawFunc — user_data (5th arg) is the cairo lib handle.
 ;; SysV defun: the cairo extern-call's below clobber rbx, which the prologue
 ;; now saves/restores so GTK's callee-saved rbx survives the callback.
 (defun on_draw (area cr width height hc)
   (let ((f_ssr      (extern-call dlsym hc (data-addr s_ssr)))
         (f_paint    (extern-call dlsym hc (data-addr s_paint)))
         (f_sff      (extern-call dlsym hc (data-addr s_sff)))
         (f_sfs      (extern-call dlsym hc (data-addr s_sfs)))
         (f_moveto   (extern-call dlsym hc (data-addr s_moveto)))
         (f_showtext (extern-call dlsym hc (data-addr s_showtext))))
     (seq
      (extern-call-ptr f_ssr cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
      (extern-call-ptr f_paint cr)
      (extern-call-ptr f_ssr cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
      (extern-call-ptr f_sff cr (data-addr font_sans) 0 0)
      (extern-call-ptr f_sfs cr (:f64 48.0))
      (extern-call-ptr f_moveto cr (:f64 24.0) (:f64 78.0))
      (extern-call-ptr f_showtext cr (data-addr text_sumi))
      0)))

 (defun on_destroy (widget loop)
   (let ((hl (extern-call dlopen (data-addr lib_glib) 2)))
     (let ((f_quit (extern-call dlsym hl (data-addr s_loopquit))))
       (seq (extern-call-ptr f_quit loop) 0))))

 (defun on_timeout (loop)
   (let ((hl (extern-call dlopen (data-addr lib_glib) 2)))
     (let ((f_quit (extern-call dlsym hl (data-addr s_loopquit))))
       (seq (extern-call-ptr f_quit loop) 0))))

 (defun main ()
   (let ((hg (extern-call dlopen (data-addr lib_gtk) 2))
         (hc (extern-call dlopen (data-addr lib_cairo) 2))
         (hl (extern-call dlopen (data-addr lib_glib) 2))
         (ho (extern-call dlopen (data-addr lib_gobject) 2)))
     (if (= hg 0)
         200
       (let ((f_init    (extern-call dlsym hg (data-addr s_init)))
             (f_winnew  (extern-call dlsym hg (data-addr s_winnew)))
             (f_title   (extern-call dlsym hg (data-addr s_title)))
             (f_size    (extern-call dlsym hg (data-addr s_size)))
             (f_areanew (extern-call dlsym hg (data-addr s_areanew)))
             (f_drawfn  (extern-call dlsym hg (data-addr s_drawfn)))
             (f_setchild(extern-call dlsym hg (data-addr s_setchild)))
             (f_present (extern-call dlsym hg (data-addr s_present)))
             (f_loopnew (extern-call dlsym hl (data-addr s_loopnew)))
             (f_looprun (extern-call dlsym hl (data-addr s_looprun)))
             (f_timeout (extern-call dlsym hl (data-addr s_timeout)))
             (f_connect (extern-call dlsym ho (data-addr s_connect))))
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
