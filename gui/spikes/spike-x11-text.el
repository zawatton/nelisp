;;; spike-x11-text.el --- pure-elisp X11 editor view with real text -*- lexical-binding: t; -*-
;;
;; Extends spike-x11.el: a core X11 font (OpenFont) + ImageText8 to draw actual
;; elisp source -- a graphical *scratch* buffer.  Still socket + wire protocol
;; only: no library, no dynamic linker, no FFI, no ptr-call.

(defun x-mmap (len) (syscall-direct 9 0 len 3 34 -1 0))
(defun x-rdu (ptr off size)
  (let* ((base (- off (mod off 8))) (sh (* (mod off 8) 8)) (lo (ptr-read-u64 ptr base))
         (need (+ (mod off 8) size)) (mask (if (>= size 8) -1 (1- (ash 1 (* size 8))))) (v (ash lo (- sh))))
    (when (> need 8) (setq v (logior v (ash (ptr-read-u64 ptr (+ base 8)) (- 64 sh))))) (logand v mask)))
(defun x-wbytes (page bytes)
  (let ((i 0) (n (length bytes)))
    (while (< i n) (let ((v 0) (k 0))
      (while (and (< k 8) (< (+ i k) n)) (setq v (logior v (ash (logand (nth (+ i k) bytes) 255) (* k 8)))) (setq k (1+ k)))
      (ptr-write-u64 page i v) (setq i (+ i 8))))))
(defun x-le (n nbytes) (let ((r nil) (i 0)) (while (< i nbytes) (setq r (append r (list (logand (ash n (* -8 i)) 255)))) (setq i (1+ i))) r))
(defun x-pad4 (n) (* 4 (/ (+ n 3) 4)))
(defun x-send (fd buf bytes) (x-wbytes buf bytes) (syscall-direct 1 fd buf (length bytes) 0 0 0))

(defun x-fill (fd buf gc win color x y w h)
  (x-send fd buf (append (list 56 0) (x-le 4 2) (x-le gc 4) (x-le 4 4) (x-le color 4)))
  (x-send fd buf (append (list 70 0) (x-le 5 2) (x-le win 4) (x-le gc 4)
                         (x-le x 2) (x-le y 2) (x-le w 2) (x-le h 2))))

;; ImageText8 with GC foreground=FG, background=BG, at baseline (X,Y).
(defun x-text (fd buf gc win fg bg x y str)
  (x-send fd buf (append (list 56 0) (x-le 5 2) (x-le gc 4) (x-le 12 4) (x-le fg 4) (x-le bg 4)))  ; GCForeground|GCBackground
  (let* ((sb (string-to-list str)) (n (length sb)) (pad (- (x-pad4 n) n)) (reqlen (+ 4 (/ (x-pad4 n) 4))))
    (x-send fd buf (append (list 76 n) (x-le reqlen 2) (x-le win 4) (x-le gc 4) (x-le x 2) (x-le y 2)
                           sb (x-le 0 pad)))))

(defun x-draw-lines (fd buf gc win fg bg x y0 dy lines)
  (let ((y y0)) (dolist (ln lines) (when (> (length ln) 0) (x-text fd buf gc win fg bg x y ln)) (setq y (+ y dy)))))

(defun x-paint (fd buf gc win)
  (x-fill fd buf gc win #x21252b 0 0 600 380)        ; body
  (x-fill fd buf gc win #x2e3440 0 0 600 30)         ; header
  (x-fill fd buf gc win #x4c566a 0 350 600 30)       ; mode-line
  (x-text fd buf gc win #x88c0d0 #x2e3440 12 20 "*scratch*  --  nemacs  (pure-elisp X11, no FFI)")
  (x-draw-lines fd buf gc win #xd8dee9 #x21252b 14 56 18
    (list ";; This window speaks the X11 wire protocol over a unix socket."
          ";; No library, no dynamic linker, no FFI, no ptr-call -- just"
          ";; socket syscalls and bytes, all in pure elisp on NeLisp."
          ""
          "(defun fib (n)"
          "  (if (< n 2) n"
          "    (+ (fib (- n 1)) (fib (- n 2)))))"
          ""
          "(fib 10)   ; => 55"
          ""
          "(message \"hello from a pure-elisp GUI\")"))
  (x-text fd buf gc win #xeceff4 #x4c566a 12 369 "-:**-  *scratch*   (Lisp)   L9   pure-elisp/X11   press a key to close"))

(let* ((fd (syscall-direct 41 1 1 0 0 0 0))
       (addr (x-mmap 4096)) (buf (x-mmap 65536)) (reply (x-mmap 65536))
       (sa (append (list 1 0) (string-to-list "/tmp/.X11-unix/X0") (list 0))))
  (x-wbytes addr sa)
  (syscall-direct 42 fd addr (length sa) 0 0 0)
  (x-send fd buf (list 108 0 11 0 0 0 0 0 0 0 0 0))
  (syscall-direct 0 fd reply 65536 0 0 0)
  (let* ((vlen (x-rdu reply 24 2)) (nfmt (x-rdu reply 29 1)) (rid (x-rdu reply 12 4))
         (scr (+ 40 (x-pad4 vlen) (* nfmt 8)))
         (root (x-rdu reply scr 4)) (visual (x-rdu reply (+ scr 32) 4)) (depth (x-rdu reply (+ scr 38) 1))
         (win (+ rid 1)) (gc (+ rid 2)) (fid (+ rid 3))
         (fname (string-to-list "fixed")) (fn (length fname)) (fpad (- (x-pad4 fn) fn)))
    (x-send fd buf (append (list 45 0) (x-le (+ 3 (/ (x-pad4 fn) 4)) 2) (x-le fid 4) (x-le fn 2) (x-le 0 2)
                           fname (x-le 0 fpad)))                                       ; OpenFont "fixed"
    (x-send fd buf (append (list 1 depth) (x-le 10 2) (x-le win 4) (x-le root 4)
                           (x-le 80 2) (x-le 80 2) (x-le 600 2) (x-le 380 2) (x-le 0 2) (x-le 1 2)
                           (x-le visual 4) (x-le #x802 4) (x-le #x21252b 4) (x-le #x8001 4)))   ; CreateWindow
    (x-send fd buf (append (list 55 0) (x-le 5 2) (x-le gc 4) (x-le win 4) (x-le #x4000 4) (x-le fid 4)))  ; CreateGC + GCFont
    (x-send fd buf (append (list 8 0) (x-le 2 2) (x-le win 4)))                        ; MapWindow
    (x-paint fd buf gc win)
    (let ((go t) (frames 0) (err 0))
      (while go
        (let ((rrc (syscall-direct 0 fd reply 4096 0 0 0)))
          (if (< rrc 1) (setq go nil)
            (let ((etype (logand (x-rdu reply 0 1) 127)))
              (cond ((= etype 0) (setq err (x-rdu reply 1 1)) (setq go nil))   ; X error
                    ((= etype 12) (x-paint fd buf gc win) (setq frames (1+ frames)))
                    ((= etype 2) (setq go nil)))))))
      (syscall-direct 3 fd 0 0 0 0 0)
      (list :win (format "#x%x" win) :depth depth :frames frames :x-error err))))
;;; spike-x11-text.el ends here
