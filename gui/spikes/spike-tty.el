;;; spike-tty.el --- pure-elisp TTY renderer spike (NeLisp standalone) -*- lexical-binding: t; -*-
;;
;; nelisp-emacs UI pure-elisp (TTY-first, Doc 07 substrate-ui).
;; No FFI: interpreter syscall-direct (write=1) + ptr-write-u64 only.
;; dev loop: grep -vE '^[[:space:]]*;;' spike-tty.el | run-as-progn
;;   (NeLisp standalone --eval feed breaks on some ;; comment lines; strip them.)

(defun tty-mmap (len) (syscall-direct 9 0 len 3 34 -1 0))

(defun tty-poke (page str)
  (let* ((slen (length str)) (i 0))
    (while (< i slen)
      (let ((v 0) (k 0))
        (while (and (< k 8) (< (+ i k) slen))
          (setq v (logior v (ash (logand (aref str (+ i k)) 255) (* k 8))))
          (setq k (1+ k)))
        (ptr-write-u64 page i v)
        (setq i (+ i 8))))
    slen))

(defun tty-write (fd str)
  (let* ((page (tty-mmap 262144)) (n (tty-poke page str)))
    (syscall-direct 1 fd page n 0 0 0)))

(defun tty-esc (s) (concat (char-to-string 27) s))
(defun tty-clear () (tty-esc "[2J"))
(defun tty-goto (row col) (tty-esc (format "[%d;%dH" row col)))
(defun tty-sgr (n) (tty-esc (format "[%dm" n)))
(defun tty-reset () (tty-esc "[0m"))

(defun tty-repeat (ch n)
  (let ((s "") (i 0)) (while (< i n) (setq s (concat s ch)) (setq i (1+ i))) s))

(defun tty-pad (str w)
  (let ((l (length str)))
    (if (>= l w) (substring str 0 w) (concat str (tty-repeat " " (- w l))))))

(defun tty-box (row col w h title)
  (let ((out "") (i 1))
    (setq out (concat out (tty-goto row col) "+" (tty-repeat "-" (- w 2)) "+"))
    (when (> (length title) 0)
      (setq out (concat out (tty-goto row (+ col 2)) " " title " ")))
    (while (< i (1- h))
      (setq out (concat out (tty-goto (+ row i) col) "|"
                        (tty-goto (+ row i) (+ col w -1)) "|"))
      (setq i (1+ i)))
    (concat out (tty-goto (+ row (1- h)) col) "+" (tty-repeat "-" (- w 2)) "+")))

(defun tty-text-lines (row col lines)
  (let ((out "") (r row))
    (dolist (ln lines)
      (setq out (concat out (tty-goto r col) ln))
      (setq r (1+ r)))
    out))

(defun tty-render-frame (cols rows title buffer-lines mode-line cur-row cur-col)
  "Render a single editor window: bordered title, BUFFER-LINES, reverse MODE-LINE,
then place the terminal cursor at (CUR-ROW,CUR-COL)."
  (let ((out (concat (tty-clear) (tty-sgr 36) (tty-box 1 1 cols (- rows 1) title))))
    (setq out (concat out (tty-reset) (tty-text-lines 3 3 buffer-lines)))
    (setq out (concat out (tty-goto rows 1) (tty-sgr 7)
                      (tty-pad mode-line cols) (tty-reset)))
    (setq out (concat out (tty-goto cur-row cur-col)))
    (tty-write 1 out)))

(defun tty-demo ()
  (tty-render-frame
   60 16 "*scratch* -- nemacs pure-elisp"
   (list ";; pure-elisp nemacs running on NeLisp standalone."
         ";; This frame was drawn with syscall write(1) only:"
         ";;   no FFI, no Rust, no libc -- ANSI escapes from elisp."
         ""
         "(defun fib (n)"
         "  (if (< n 2) n"
         "    (+ (fib (- n 1)) (fib (- n 2)))))"
         ""
         "(fib 10)   ; => 55"
         ""
         "-!- cursor below, mode-line at the bottom -!-")
   " -:--  *scratch*   (Lisp)   L9   Top   pure-elisp/TTY "
   13 6))

(tty-demo)
;;; spike-tty.el ends here
