;;; nelisp-async-editor.el --- a real interactive editor on the async loop -*- lexical-binding: t; -*-
;;
;; A multi-line editor that runs entirely on the standalone async event loop
;; (packages/nelisp-eventloop/src/nelisp-async.el):
;;
;;   - text lives in a `nelisp-buffer' (gap buffer + markers);
;;   - keystrokes arrive as `key' events from `nelisp-async-run-tty', which
;;     blocks in poll(2) until a key OR the next timer is due;
;;   - a repeating 0.5s timer ticks a counter in the status line, proving timers
;;     and input are multiplexed on one loop with zero idle CPU;
;;   - the actor decodes ESC[ arrow/home/end sequences and dispatches editing
;;     commands.
;;
;; Keys: printable -> self-insert; Enter -> newline; BS/DEL -> delete back;
;;       C-d -> delete forward; C-f/C-b/Left/Right -> char move;
;;       C-p/C-n/Up/Down -> line move; C-a/Home -> line start; C-e/End -> line
;;       end; C-l -> redraw; C-q -> quit.
;;
;; Interactive (real terminal): tools/nelisp-async-editor-run.sh
;; Headless / scripted: `nae-run-batch' reads piped stdin, prints final state.

;;; State -------------------------------------------------------------

(defvar nae--buf nil  "The editing `nelisp-buffer'.")
(defvar nae--tick 0   "Timer-driven tick counter (status-line liveness).")
(defvar nae--draw t   "When non-nil, render to the terminal on each change.")

;;; Position helpers (point is 1-based into the buffer string) ---------

(defun nae--text () (nelisp-buffer-string nae--buf))
(defun nae--pmax () (nelisp-point-max nae--buf))
(defun nae--pmin () (nelisp-point-min nae--buf))
(defun nae--pt () (nelisp-point nae--buf))
(defun nae--goto (p) (nelisp-goto-char (min (max p (nae--pmin)) (nae--pmax)) nae--buf))

(defun nae--point->rc ()
  "Return (ROW . COL), both 0-based, for the current point."
  (let* ((s (nae--text)) (lim (1- (nae--pt))) (i 0) (row 0) (col 0))
    (while (< i lim)
      (if (= (aref s i) 10) (setq row (1+ row) col 0) (setq col (1+ col)))
      (setq i (1+ i)))
    (cons row col)))

(defun nae--rc->point (row col)
  "Return the 1-based point at 0-based ROW, COL (clamped to the line)."
  (let* ((s (nae--text)) (n (length s)) (i 0) (r 0))
    (while (and (< i n) (< r row))
      (when (= (aref s i) 10) (setq r (1+ r)))
      (setq i (1+ i)))
    (let ((c 0))
      (while (and (< i n) (< c col) (not (= (aref s i) 10)))
        (setq i (1+ i) c (1+ c))))
    (1+ i)))

;;; Editing commands --------------------------------------------------

(defun nae--self-insert (b) (nelisp-insert (char-to-string b) nae--buf))
(defun nae--newline () (nelisp-insert "\n" nae--buf))

(defun nae--del-back ()
  (let ((pt (nae--pt)))
    (when (> pt (nae--pmin)) (nelisp-delete-region (1- pt) pt nae--buf))))

(defun nae--del-forward ()
  (let ((pt (nae--pt)))
    (when (< pt (nae--pmax)) (nelisp-delete-region pt (1+ pt) nae--buf))))

(defun nae--forward () (nae--goto (1+ (nae--pt))))
(defun nae--back ()    (nae--goto (1- (nae--pt))))

(defun nae--line-start () (let ((rc (nae--point->rc))) (nae--goto (nae--rc->point (car rc) 0))))
(defun nae--line-end ()   (let ((rc (nae--point->rc))) (nae--goto (nae--rc->point (car rc) 1000000))))

(defun nae--up ()
  (let ((rc (nae--point->rc)))
    (when (> (car rc) 0) (nae--goto (nae--rc->point (1- (car rc)) (cdr rc))))))

(defun nae--down ()
  (let ((rc (nae--point->rc))) (nae--goto (nae--rc->point (1+ (car rc)) (cdr rc)))))

(defun nae--key (b)
  "Dispatch a single decoded byte B (not part of an escape sequence)."
  (cond
   ((or (= b 13) (= b 10)) (nae--newline))         ; Enter
   ((or (= b 127) (= b 8)) (nae--del-back))        ; DEL / BS
   ((= b 4)  (nae--del-forward))                   ; C-d
   ((= b 6)  (nae--forward))                       ; C-f
   ((= b 2)  (nae--back))                          ; C-b
   ((= b 1)  (nae--line-start))                    ; C-a
   ((= b 5)  (nae--line-end))                      ; C-e
   ((= b 16) (nae--up))                            ; C-p
   ((= b 14) (nae--down))                          ; C-n
   ((= b 12) nil)                                  ; C-l (redraw happens after)
   ((>= b 32) (nae--self-insert b))                ; printable
   (t nil)))

(defun nae--arrow (final)
  "Dispatch the FINAL byte of an `ESC [' sequence."
  (cond
   ((= final 65) (nae--up))      ; A
   ((= final 66) (nae--down))    ; B
   ((= final 67) (nae--forward)) ; C
   ((= final 68) (nae--back))    ; D
   ((= final 72) (nae--line-start)) ; H Home
   ((= final 70) (nae--line-end))   ; F End
   (t nil)))

;;; Terminal output ---------------------------------------------------

(defun nae--write (s)
  (let* ((n (length s)) (buf (alloc-bytes (+ n 1) 1)) (i 0))
    (while (< i n) (ptr-write-u8 buf i (aref s i)) (setq i (1+ i)))
    (syscall-direct 1 1 buf n 0 0 0)))

(defun nae--render ()
  "Redraw the buffer (one terminal row per line) + a status line."
  (when nae--draw
    (let* ((s (nae--text)) (n (length s)) (out "\e[H\e[2J") (i 0) (line ""))
      ;; Body: split on \n, each line on its own terminal row.
      (while (< i n)
        (let ((ch (aref s i)))
          (if (= ch 10) (setq out (concat out line "\r\n") line "")
            (setq line (concat line (char-to-string ch)))))
        (setq i (1+ i)))
      (setq out (concat out line "\r\n"))
      ;; Status line (reverse video).
      (let ((rc (nae--point->rc)))
        (setq out (concat out "\e[7m -- nelisp-async-editor  "
                          "L" (number-to-string (1+ (car rc)))
                          " C" (number-to-string (1+ (cdr rc)))
                          "  ticks=" (number-to-string nae--tick)
                          "  (C-q quit) \e[0m"))
        ;; Place the cursor at its row/col (1-based ANSI).
        (setq out (concat out "\e[" (number-to-string (1+ (car rc)))
                          ";" (number-to-string (1+ (cdr rc))) "H")))
      (nae--write out))))

;;; Actor + loop ------------------------------------------------------

(defun nae--make-actor ()
  "Spawn the editor actor with an ESC-sequence decoder."
  (nelisp-spawn
   (nelisp-actor-lambda
     (let ((run t) (esc 0))                ; esc: 0 normal, 1 saw ESC, 2 saw ESC[
       (while run
         (let ((ev (nelisp-receive)))
           (when (and (nelisp-event-p ev) (eq (nelisp-event-kind ev) 'key))
             (let ((b (nelisp-event-data ev)))
               (cond
                ((= esc 2) (setq esc 0) (nae--arrow b))
                ((= esc 1) (if (= b 91) (setq esc 2) (setq esc 0)))  ; expect '['
                ((= b 27) (setq esc 1))                              ; ESC
                ((= b 17) (setq run nil))                            ; C-q
                (t (nae--key b)))
               (nae--render)))
           (nelisp-yield)))))))

(defun nae--setup (&optional initial)
  (setq nae--buf (nelisp-buffer--make :name "*async-editor*"))
  (when (and initial (> (length initial) 0))
    (nelisp-insert initial nae--buf)
    (nelisp-goto-char (nae--pmin) nae--buf))
  (setq nae--tick 0)
  (nelisp-actor--reset)
  (nelisp-async-reset-timers))

(defun nae-run-interactive (&optional initial)
  "Run the editor on a real terminal (raw mode + async loop)."
  (nae--setup initial)
  (setq nae--draw t)
  (let ((main (nae--make-actor)))
    (run-at-time 0.5 0.5 (lambda () (setq nae--tick (1+ nae--tick)) (nae--render)))
    (nae--render)
    (prog1 (nelisp-async-run-tty-raw main)
      (nae--write "\e[2J\e[H"))))

(defun nae-run-batch (&optional initial)
  "Run headless on piped stdin; print the final buffer + point on quit."
  (nae--setup initial)
  (setq nae--draw nil)
  (let ((main (nae--make-actor)))
    (run-at-time 0.5 0.5 (lambda () (setq nae--tick (1+ nae--tick))))
    (let ((res (nelisp-async-run-tty main)))
      (format "RESULT[%s text=%S point=%d ticks=%d]"
              res (nae--text) (nae--pt) nae--tick))))

;;; nelisp-async-editor.el ends here
