;;; editor.el --- A terminal editor in pure Elisp on the standalone binary  -*- lexical-binding: nil; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A self-contained text editor running on the freestanding `target/nelisp'
;; binary with NO Emacs C core involved.  Every piece is pure Elisp evaluated by
;; the standalone reader's native evaluator:
;;
;;   * the buffer is a gap buffer (ED-PRE = chars before point, reversed; ED-POST
;;     = chars after point) built from plain cons cells;
;;   * editing commands (self-insert, delete-backward, forward/backward-char)
;;     are ordinary `defun's using `cond'/`when'/`while'/`setq' — all of which
;;     the reader now evaluates natively (no prelude macro layer required);
;;   * input is read from real STDIN via the raw `read(2)' syscall;
;;   * the screen is drawn with ANSI escapes and written to real STDOUT via the
;;     raw `write(2)' syscall.
;;
;; The only OS contact is `syscall-direct' (read=0, write=1, exit_group=231) --
;; the same pure-elisp FFI substrate used elsewhere for sockets/TLS/fonts.  There
;; is no Emacs: no C redisplay, no C buffer, no C event loop.
;;
;; Run loop (batch form, demonstrable without a TTY): read every byte available
;; on STDIN as the keystroke stream, apply it, render the final screen once.
;; Bindings: printable/newline -> self-insert, DEL/BS (127/8) -> delete-backward,
;; C-f (6) -> forward-char, C-b (2) -> backward-char.
;;
;; Usage (from the dev/nelisp repo root, after `make standalone-reader'):
;;   printf 'Hello\nworld\002\002!' | ./target/nelisp --load examples/nelisp-tty-editor/editor.el
;;
;; A raw-mode INTERACTIVE loop (per-keystroke redisplay, C-q to quit) is the
;; natural next step: put the tty in raw mode with `ioctl(TCGETS/TCSETS)' (also
;; reachable through `syscall-direct'), then read one byte at a time and
;; re-render after each.  That path needs a real terminal, so this file ships the
;; batch form that exercises the identical buffer/edit/render/IO core.

;;; Code:

(load "scripts/nelisp-stdlib-prelude.el")

(defvar ed-pre nil "Chars before point, nearest-to-point first (reversed).")
(defvar ed-post nil "Chars at/after point, in order.")

(defun ed-key (c)
  "Dispatch one input byte C against the editor command set."
  (cond
   ((= c 127) (when ed-pre (setq ed-pre (cdr ed-pre))))   ; DEL  -> delete-backward
   ((= c 8)   (when ed-pre (setq ed-pre (cdr ed-pre))))   ; BS   -> delete-backward
   ((= c 6)   (when ed-post                                ; C-f  -> forward-char
                (setq ed-pre (cons (car ed-post) ed-pre))
                (setq ed-post (cdr ed-post))))
   ((= c 2)   (when ed-pre                                 ; C-b  -> backward-char
                (setq ed-post (cons (car ed-pre) ed-post))
                (setq ed-pre (cdr ed-pre))))
   (t         (setq ed-pre (cons c ed-pre)))))            ; self-insert

(defun ed-read-all ()
  "Consume STDIN via read(2) and feed every byte to `ed-key'."
  (let ((b (alloc-bytes 4096 1)) (go t))
    (while go
      (let ((n (syscall-direct 0 0 b 4096 0 0 0)))   ; read(fd=0, b, 4096)
        (if (<= n 0)
            (setq go nil)
          (let ((i 0))
            (while (< i n) (ed-key (ptr-read-u8 b i)) (setq i (1+ i)))))))))

(defun ed-esc (s) (concat (char-to-string 27) s))

(defun ed-chars-crlf (lst)
  "Render char list LST to a string, expanding newline to CR LF for the tty."
  (let ((s ""))
    (while lst
      (let ((c (car lst)))
        (setq s (concat s (if (= c 10)
                              (concat (char-to-string 13) (char-to-string 10))
                            (char-to-string c)))))
      (setq lst (cdr lst)))
    s))

(defun ed-cursor-rc ()
  "Return (ROW . COL), 1-based, of point computed from `ed-pre'."
  (let ((col 1) (row 1) (l ed-pre) (seen nil))
    (while l
      (let ((c (car l)))
        (if (= c 10)
            (progn (setq row (1+ row)) (setq seen t))
          (unless seen (setq col (1+ col)))))
      (setq l (cdr l)))
    (cons row col)))

(defun ed-render ()
  "Build the full screen (ANSI clear + text + cursor reposition)."
  (let ((rc (ed-cursor-rc)) (crlf (concat (char-to-string 13) (char-to-string 10))))
    (concat
     (ed-esc "[2J") (ed-esc "[H")
     "nelisp editor  -  pure Elisp on the standalone binary (NO Emacs C core)" crlf
     "----------------------------------------------------------------------" crlf
     (ed-chars-crlf (reverse ed-pre)) (ed-chars-crlf ed-post)
     ;; +2 for the two header rows.
     (ed-esc (concat "[" (number-to-string (+ 2 (car rc))) ";"
                     (number-to-string (cdr rc)) "H")))))

(defun ed-write1 (s)
  "Write string S to STDOUT via write(2)."
  (let* ((n (length s)) (b (alloc-bytes (+ n 16) 1)) (i 0))
    (while (< i n) (ptr-write-u8 b i (aref s i)) (setq i (1+ i)))
    (syscall-direct 1 1 b n 0 0 0)))   ; write(fd=1, b, n)

(ed-read-all)
(ed-write1 (ed-render))
(syscall-direct 231 0 0 0 0 0 0)       ; exit_group(0)

;;; editor.el ends here
