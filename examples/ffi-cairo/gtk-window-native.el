;; Windows-native GTK4 window driven entirely by NeLisp AOT (no added Rust).
;;
;; This is the windowed counterpart of the PNG examples in this directory.
;; Instead of rendering an image surface to a file, it opens a real native
;; Win64 window and lets GTK call back into AOT-compiled elisp to paint it.
;;
;; Pipeline (see gtkwin.sh):
;;   elisp --(nelisp-aot-compile-to-object :format 'coff)--> Win64 COFF .o
;;        --(mingw gcc + `pkg-config --libs gtk4`)--> native PE .exe
;;
;; Three C->elisp callbacks prove signal/draw dispatch into AOT-compiled
;; defuns over the SysV-vs-Win64 boundary:
;;   on_draw     GtkDrawingAreaDrawFunc  void(GtkDrawingArea*, cairo_t*, int, int, gpointer)
;;   on_destroy  GtkWidget::destroy      void(GtkWidget*, gpointer)
;;   on_timeout  GSourceFunc             gboolean(gpointer)
;;
;; Why this is safe under Win64: a callback invoked from deep inside GTK must
;; preserve the C callee-saved registers.  Our extern-call dynamic-alignment
;; idiom clobbers rbx, but the Win64 defun prologue already saves/restores rbx
;; (along with rdi/rsi/xmm6-15), so painting via cairo from on_draw does not
;; corrupt GTK's register state.  (The SysV/ELF defun prologue does NOT yet
;; save rbx, which is why the Linux/WSL windowed path would need a compiler
;; fix first; the Windows-native path works as-is.)
;;
;; The window closes when you dismiss it (on_destroy -> g_main_loop_quit); a
;; 30-second g_timeout also auto-quits so an unattended run still terminates
;; with a checkable exit code.  Adjust or drop the g_timeout_add line to taste.
(seq
 (data-blob title       "sumi (NeLisp -> GTK4, Windows native)\0" rodata)
 (data-blob sig_destroy "destroy\0" rodata)
 (data-blob font_sans   "sans\0" rodata)
 (data-blob text_sumi   "sumi\0" rodata)

 ;; GtkDrawingAreaDrawFunc — paint the same "sumi" panel as the PNG examples,
 ;; but into the live window surface GTK hands us in `cr'.
 (defun on_draw (area cr width height data)
   (seq
    (extern-call cairo_set_source_rgb cr (:f64 0.07) (:f64 0.07) (:f64 0.17))
    (extern-call cairo_paint cr)
    (extern-call cairo_set_source_rgb cr (:f64 0.94) (:f64 0.86) (:f64 0.24))
    (extern-call cairo_select_font_face cr (data-addr font_sans) 0 0)
    (extern-call cairo_set_font_size cr (:f64 48.0))
    (extern-call cairo_move_to cr (:f64 24.0) (:f64 78.0))
    (extern-call cairo_show_text cr (data-addr text_sumi))
    0))

 ;; GtkWidget::destroy — user dismissed the window; user_data carries the loop.
 (defun on_destroy (widget loop)
   (seq (extern-call g_main_loop_quit loop) 0))

 ;; GSourceFunc — auto-quit safety net; return FALSE (0) to remove the source.
 (defun on_timeout (loop)
   (seq (extern-call g_main_loop_quit loop) 0))

 (defun main ()
   (seq
    (extern-call gtk_init)
    (let ((window (extern-call gtk_window_new)))
      (seq
       (extern-call gtk_window_set_title window (data-addr title))
       (extern-call gtk_window_set_default_size window 240 120)
       (let ((area (extern-call gtk_drawing_area_new)))
         (seq
          (extern-call gtk_drawing_area_set_draw_func area (addr-of on_draw) 0 0)
          (extern-call gtk_window_set_child window area)
          (let ((loop (extern-call g_main_loop_new 0 0)))
            (seq
             (extern-call g_signal_connect_data
                          window (data-addr sig_destroy) (addr-of on_destroy) loop 0 0)
             (extern-call g_timeout_add 30000 (addr-of on_timeout) loop)
             (extern-call gtk_window_present window)
             (extern-call g_main_loop_run loop)
             0)))))))))
