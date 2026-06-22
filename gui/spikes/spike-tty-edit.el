;;; spike-tty-edit.el --- pure-elisp TTY editor: key -> edit -> redisplay -*- lexical-binding: t; -*-
;; Doc 07 substrate-ui: buffer (§3.1) + command loop (§3.6) + redisplay (§3.5), TTY.
;; No FFI: syscall-direct (write=1) + ptr-write-u64 only.  Feed: strip ;; lines.

(defun tty-mmap (len) (syscall-direct 9 0 len 3 34 -1 0))
(defun tty-poke (page str)
  (let* ((slen (length str)) (i 0))
    (while (< i slen)
      (let ((v 0) (k 0))
        (while (and (< k 8) (< (+ i k) slen))
          (setq v (logior v (ash (logand (aref str (+ i k)) 255) (* k 8)))) (setq k (1+ k)))
        (ptr-write-u64 page i v) (setq i (+ i 8)))) slen))
(defun tty-write (fd str)
  (let* ((page (tty-mmap 262144)) (n (tty-poke page str))) (syscall-direct 1 fd page n 0 0 0)))
(defun tty-esc (s) (concat (char-to-string 27) s))
(defun tty-clear () (tty-esc "[2J"))
(defun tty-goto (row col) (tty-esc (format "[%d;%dH" row col)))
(defun tty-sgr (n) (tty-esc (format "[%dm" n)))
(defun tty-reset () (tty-esc "[0m"))
(defun tty-repeat (ch n) (let ((s "") (i 0)) (while (< i n) (setq s (concat s ch)) (setq i (1+ i))) s))
(defun tty-pad (str w) (let ((l (length str))) (if (>= l w) (substring str 0 w) (concat str (tty-repeat " " (- w l))))))
(defun tty-box (row col w h title)
  (let ((out "") (i 1))
    (setq out (concat out (tty-goto row col) "+" (tty-repeat "-" (- w 2)) "+"))
    (when (> (length title) 0) (setq out (concat out (tty-goto row (+ col 2)) " " title " ")))
    (while (< i (1- h))
      (setq out (concat out (tty-goto (+ row i) col) "|" (tty-goto (+ row i) (+ col w -1)) "|")) (setq i (1+ i)))
    (concat out (tty-goto (+ row (1- h)) col) "+" (tty-repeat "-" (- w 2)) "+")))

;; ---- edit model: single line + cursor column ----
(defun ed-init () (setq ed-line "") (setq ed-col 0))
(defun ed-key (k)
  (cond
   ((eq k 'left)  (when (> ed-col 0) (setq ed-col (1- ed-col))))
   ((eq k 'right) (when (< ed-col (length ed-line)) (setq ed-col (1+ ed-col))))
   ((eq k 'home)  (setq ed-col 0))
   ((eq k 'end)   (setq ed-col (length ed-line)))
   ((eq k 'bs)    (when (> ed-col 0)
                    (setq ed-line (concat (substring ed-line 0 (1- ed-col)) (substring ed-line ed-col)))
                    (setq ed-col (1- ed-col))))
   ((integerp k)  (setq ed-line (concat (substring ed-line 0 ed-col) (char-to-string k) (substring ed-line ed-col)))
                  (setq ed-col (1+ ed-col)))))
(defun ed-run (keys) (dolist (k keys) (ed-key k)))

;; ---- redisplay: draw current edit state ----
(defun ed-render (cols rows)
  (let ((out (concat (tty-clear) (tty-sgr 36) (tty-box 1 1 cols (- rows 1) "*scratch*  nemacs pure-elisp TTY editor"))))
    (setq out (concat out (tty-reset)
                      (tty-goto 3 3) "type some elisp, then edit it -- all in pure elisp:"
                      (tty-goto 5 3) ed-line))
    (setq out (concat out (tty-goto rows 1) (tty-sgr 7)
                      (tty-pad (format " -:**-  *scratch*   col %d   pure-elisp/TTY " ed-col) cols)
                      (tty-reset)))
    (setq out (concat out (tty-goto 5 (+ 3 ed-col))))   ; place terminal cursor at point
    (tty-write 1 out)))

;; ---- scripted session: type a defun, then move cursor into it ----
(defun ed-demo ()
  (ed-init)
  (ed-run (string-to-list "(defun sq (x) (* x x))"))
  (ed-run (list 'home 'right 'right 'right 'right 'right 'right 'right))  ; cursor after "defun "
  (ed-run (list 'bs 'bs 'bs))                                            ; delete "fun" -> "(de sq..."  demo edit
  (ed-render 62 14)
  (format "ed-line=%S ed-col=%d" ed-line ed-col))

(ed-demo)
;;; spike-tty-edit.el ends here
