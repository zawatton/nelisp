;;; nelisp-x11-dired.el --- directory browser in an X window -*- lexical-binding: t; -*-
;;
;; Runs the nelisp-dired browser inside the pure-elisp X11 window.  Reuses the
;; X11 client (window/font/text) + the dired navigation model; RET on a file
;; opens it in the X editor (nelisp-x11-editor).
;;
;; Keys: Down/C-n/n, Up/C-p/p move; RET / Right enter dir (or open file);
;;       Left / u parent; C-q quit.
;;
;; Deps the caller loads first: prelude, src/nelisp-buffer.el,
;; examples/nelisp-async-editor.el, packages/nelisp-x11/src/nelisp-x11.el,
;; examples/nelisp-x11-editor.el, packages/nelisp-dired/src/nelisp-dired.el.

(defun nxd--render (dpy wid gc s)
  "Repaint dired state S into window WID."
  (nelisp-x11-clear-area dpy wid 0 0 0 0)
  (let ((row 0))
    (dolist (line (nelisp-dired-render-lines s))
      (when (> (length line) 0)
        (nelisp-x11-image-text8 dpy wid gc 2 (+ 12 (* row 15)) line))
      (setq row (1+ row)))))

(defun nxd--key->action (k)
  "Map a decoded key K to a dired action symbol, or nil."
  (cond
   ((eq k 'down) 'next) ((eq k 'up) 'prev)
   ((eq k 'left) 'parent) ((eq k 'right) 'enter)
   ((integerp k)
    (cond ((= k 14) 'next) ((= k 16) 'prev) ((= k 110) 'next) ((= k 112) 'prev)
          ((= k 13) 'enter) ((= k 117) 'parent)
          ((= k 17) 'quit) ((= k 113) 'quit)
          (t nil)))
   (t nil)))

(defun nelisp-x11-dired (&optional path)
  "Browse PATH (default \".\") in an X window.  RET opens dirs/files; C-q quits."
  (let* ((s (nelisp-dired-open (or path ".")))
         (dpy (nelisp-x11-connect 0)))
    (if (eq (car-safe dpy) 'error) dpy
      (let* ((kbd (nelisp-x11-get-keyboard-mapping dpy))
             (wid (nelisp-x11-create-window dpy 60 60 760 560))
             (font (nelisp-x11-open-font dpy "9x15"))
             (gc (nelisp-x11-create-gc dpy wid
                                       (aref dpy nelisp-x11--d-black)
                                       (aref dpy nelisp-x11--d-white) font))
             (run t) (visit nil))
        (nelisp-x11-map-window dpy wid)
        (while run
          (let ((ev (nelisp-x11-poll-event dpy 120000)))
            (when ev
              (let ((code (logand (ptr-read-u8 ev 0) 127)))
                (cond
                 ((= code 12) (nxd--render dpy wid gc s))   ; Expose
                 ((= code 2)                                ; KeyPress
                  (let* ((kc (ptr-read-u8 ev 1))
                         (state (nelisp-x11--u16 ev 28))
                         (act (nxd--key->action (nelisp-x11-decode-key kbd kc state))))
                    (cond
                     ((eq act 'next) (nelisp-dired-move s 1))
                     ((eq act 'prev) (nelisp-dired-move s -1))
                     ((eq act 'parent) (setq s (nelisp-dired-parent s)))
                     ((eq act 'enter)
                      (let ((res (nelisp-dired-enter s)))
                        (if (vectorp res) (setq s res)       ; entered a dir
                          (setq visit (cdr res) run nil))))  ; file -> visit
                     ((eq act 'quit) (setq run nil))
                     (t nil))
                    (when run (nxd--render dpy wid gc s))))
                 (t nil))))))
        (nelisp-x11-close dpy)
        ;; If a file was chosen, open it in the X editor.
        (if (and visit (fboundp 'nelisp-x11-editor))
            (nelisp-x11-editor visit)
          'done)))))

;;; nelisp-x11-dired.el ends here
