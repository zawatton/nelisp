;;; nelisp-x11-editor.el --- the editor in a real X window -*- lexical-binding: t; -*-
;;
;; Runs the gap-buffer editor inside an X11 window drawn by the pure-elisp X11
;; client (packages/nelisp-x11).  Reuses nelisp-async-editor's buffer + command
;; functions (nae--buf, nae--key, motion, undo, kill/yank); supplies an X11
;; renderer and an X11 keyboard input loop instead of the TTY ones.
;;
;; Keys: printable -> insert; Return; Backspace; arrows / C-f C-b C-p C-n /
;;   C-a C-e; C-d; C-k C-y; C-/ undo; C-q quit.
;;
;; Run: tools/nelisp-x11-editor.sh  (on the host display)
;;
;; Deps the caller must load first: prelude, src/nelisp-buffer.el,
;; examples/nelisp-async-editor.el, packages/nelisp-x11/src/nelisp-x11.el.

(defconst nxe--fw 9   "Cell width of the 9x15 font.")
(defconst nxe--fh 15  "Cell height of the 9x15 font.")
(defconst nxe--asc 12 "Font ascent (text baseline within a cell).")
(defconst nxe--x0 2   "Left margin in pixels.")

(defun nxe--render (dpy wid gc)
  "Repaint the buffer + a block cursor into window WID."
  (nelisp-x11-clear-area dpy wid 0 0 0 0)
  (let ((lines (split-string (nae--text) "\n")) (row 0))
    (while lines
      (let ((line (car lines)))
        (when (> (length line) 0)
          (nelisp-x11-image-text8 dpy wid gc nxe--x0
                                  (+ nxe--asc (* row nxe--fh)) line)))
      (setq lines (cdr lines) row (1+ row))))
  ;; Cursor: a thin block at the bottom of the current cell.
  (let* ((rc (nae--point->rc)) (cr (car rc)) (cc (cdr rc)))
    (nelisp-x11-fill-rect dpy wid gc
                          (+ nxe--x0 (* cc nxe--fw))
                          (+ (* cr nxe--fh) (- nxe--fh 2))
                          nxe--fw 2)))

(defun nxe--dispatch (k)
  "Apply a decoded key K (byte or motion symbol) to the buffer."
  (cond
   ((eq k 'left)  (nae--back))
   ((eq k 'right) (nae--forward))
   ((eq k 'up)    (nae--up))
   ((eq k 'down)  (nae--down))
   ((eq k 'home)  (nae--line-start))
   ((eq k 'end)   (nae--line-end))
   ((integerp k)  (nae--key k))
   (t nil)))

(defun nxe--setup (file)
  (setq nae--buf (nelisp-buffer--make :name "*x11-editor*"))
  (setq nae--undo nil nae--redo nil nae--typing nil nae--kill nil
        nae--msg "" nae--file file)
  (when file
    (let ((c (nae--read-file file)))
      (when (and c (> (length c) 0))
        (nelisp-insert c nae--buf)
        (nelisp-goto-char (nelisp-point-min nae--buf) nae--buf)))))

(defun nelisp-x11-editor (&optional file)
  "Open an X window and edit FILE (or a scratch buffer) in it.  C-q quits."
  (nxe--setup file)
  (let ((dpy (nelisp-x11-connect 0)))
    (if (eq (car-safe dpy) 'error) dpy
      (let* ((kbd (nelisp-x11-get-keyboard-mapping dpy))
             (wid (nelisp-x11-create-window dpy 80 80 640 400))
             (font (nelisp-x11-open-font dpy "9x15"))
             (gc (nelisp-x11-create-gc dpy wid
                                       (aref dpy nelisp-x11--d-black)
                                       (aref dpy nelisp-x11--d-white) font)))
        (nelisp-x11-map-window dpy wid)
        (let ((run t))
          (while run
            (let ((ev (nelisp-x11-poll-event dpy 120000)))
              (when ev
                (let ((code (logand (ptr-read-u8 ev 0) 127)))
                  (cond
                   ((= code 12) (nxe--render dpy wid gc))   ; Expose
                   ((= code 2)                              ; KeyPress
                    (let* ((kc (ptr-read-u8 ev 1))
                           (state (nelisp-x11--u16 ev 28))
                           (k (nelisp-x11-decode-key kbd kc state)))
                      (if (eq k 17) (setq run nil)          ; C-q
                        (nxe--dispatch k)
                        (nxe--render dpy wid gc))))
                   (t nil)))))))
        (nelisp-x11-close dpy)
        'done))))

;;; nelisp-x11-editor.el ends here
