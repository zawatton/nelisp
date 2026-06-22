;;; spike-x11.el --- pure-elisp X11 GUI via wire protocol over unix socket -*- lexical-binding: t; -*-
;;
;; A REAL GUI window, drawn entirely in pure elisp -- NO library, NO dynamic
;; linker, NO FFI, NO ptr-call.  Just socket syscalls + the X11 wire protocol.
;; Speaks X11 over /tmp/.X11-unix/X0 the way spike-tty speaks ANSI over stdout.
;;
;; Run (window appears on the user's display; press a key in it to close):
;;   grep -vE '^[[:space:]]*;;' spike-x11.el | DISPLAY=:0 feed-as-progn
;;   (or: ./target/nelisp --eval "(progn $(grep -vE '^[[:space:]]*;;' spike-x11.el))")

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

;; fill one rectangle (x y w h) in COLOR: ChangeGC(foreground) + PolyFillRectangle
(defun x-fill (fd buf gc win color x y w h)
  (x-send fd buf (append (list 56 0) (x-le 4 2) (x-le gc 4) (x-le 4 4) (x-le color 4)))         ; ChangeGC GCForeground
  (x-send fd buf (append (list 70 0) (x-le 5 2) (x-le win 4) (x-le gc 4)
                         (x-le x 2) (x-le y 2) (x-le w 2) (x-le h 2))))                          ; PolyFillRectangle

;; paint an Obsidian-ish dark panel with header + note cards
(defun x-paint (fd buf gc win)
  (x-fill fd buf gc win #x21252b 0 0 480 320)        ; background (dark)
  (x-fill fd buf gc win #x2e3440 0 0 480 36)         ; header bar
  (x-fill fd buf gc win #x88c0d0 12 12 120 12)       ; title accent (cyan)
  (x-fill fd buf gc win #x3b4252 16 56 448 56)       ; card 1 bg
  (x-fill fd buf gc win #xa3be8c 16 56 6 56)         ;   green stripe
  (x-fill fd buf gc win #x3b4252 16 124 448 56)      ; card 2 bg
  (x-fill fd buf gc win #xebcb8b 16 124 6 56)        ;   yellow stripe
  (x-fill fd buf gc win #x3b4252 16 192 448 56)      ; card 3 bg
  (x-fill fd buf gc win #xbf616a 16 192 6 56)        ;   red stripe
  (x-fill fd buf gc win #x4c566a 16 264 448 36))     ; footer / mode-line

(let* ((fd (syscall-direct 41 1 1 0 0 0 0))
       (addr (x-mmap 4096)) (buf (x-mmap 65536)) (reply (x-mmap 65536))
       (sa (append (list 1 0) (string-to-list "/tmp/.X11-unix/X0") (list 0))))
  (x-wbytes addr sa)
  (syscall-direct 42 fd addr (length sa) 0 0 0)                  ; connect
  (x-send fd buf (list 108 0 11 0 0 0 0 0 0 0 0 0))              ; setup request
  (syscall-direct 0 fd reply 65536 0 0 0)                        ; setup reply
  (let* ((vlen (x-rdu reply 24 2)) (nfmt (x-rdu reply 29 1)) (rid (x-rdu reply 12 4))
         (scr (+ 40 (x-pad4 vlen) (* nfmt 8)))
         (root (x-rdu reply scr 4)) (visual (x-rdu reply (+ scr 32) 4)) (depth (x-rdu reply (+ scr 38) 1))
         (win (+ rid 1)) (gc (+ rid 2)))
    ;; CreateWindow: CWBackPixel(0x2)+CWEventMask(0x800); events = Exposure|KeyPress = 0x8001
    (x-send fd buf (append (list 1 depth) (x-le 10 2) (x-le win 4) (x-le root 4)
                           (x-le 80 2) (x-le 80 2) (x-le 480 2) (x-le 320 2) (x-le 0 2) (x-le 1 2)
                           (x-le visual 4) (x-le #x802 4) (x-le #x21252b 4) (x-le #x8001 4)))
    (x-send fd buf (append (list 55 0) (x-le 4 2) (x-le gc 4) (x-le win 4) (x-le 0 4)))   ; CreateGC
    (x-send fd buf (append (list 8 0) (x-le 2 2) (x-le win 4)))                            ; MapWindow
    (x-paint fd buf gc win)                                                                 ; initial draw
    ;; event loop: draw on Expose (type 12), exit on KeyPress (type 2)
    (let ((go t) (frames 0))
      (while go
        (let ((rrc (syscall-direct 0 fd reply 4096 0 0 0)))
          (if (< rrc 1) (setq go nil)
            (let ((etype (logand (x-rdu reply 0 1) 127)))
              (cond ((= etype 12) (x-paint fd buf gc win) (setq frames (1+ frames)))   ; Expose
                    ((= etype 2) (setq go nil)))))))                                     ; KeyPress -> quit
      (syscall-direct 3 fd 0 0 0 0 0)
      (list :root (format "#x%x" root) :win (format "#x%x" win) :depth depth :frames-painted frames))))
;;; spike-x11.el ends here
