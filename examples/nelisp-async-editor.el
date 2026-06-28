;;; nelisp-async-editor.el --- a real interactive editor on the async loop -*- lexical-binding: t; -*-
;;
;; A small but genuine editor that runs entirely on the standalone async event
;; loop (packages/nelisp-eventloop/src/nelisp-async.el):
;;
;;   - text lives in a `nelisp-buffer' (gap buffer + markers);
;;   - keystrokes arrive as `key' events from `nelisp-async-run-tty', which
;;     blocks in poll(2) until a key OR the next timer is due;
;;   - a repeating 0.5s timer ticks a counter shown in the status line, proving
;;     timers and input are multiplexed on one loop with zero idle CPU;
;;   - the actor dispatches: printable -> self-insert, C-f/C-b move, BS/DEL
;;     delete, C-l redraw, C-q quit.
;;
;; Interactive use (real terminal, raw mode):
;;   ./target/nelisp --load examples/nelisp-async-editor.el \
;;     --eval '(nae-run-interactive)'
;;
;; Headless / scripted (deterministic, for the smoke test): bytes are fed on
;; stdin and the final buffer + tick count are printed on quit:
;;   printf 'hi\006\177X\021' | ./target/nelisp --load ... --eval '(nae-run-batch)'
;;
;; Dependencies are loaded by the caller (see tools/nelisp-async-editor-smoke.sh).

;;; State -------------------------------------------------------------

(defvar nae--buf nil  "The editing `nelisp-buffer'.")
(defvar nae--tick 0   "Timer-driven tick counter (status-line liveness).")
(defvar nae--draw t   "When non-nil, render to the terminal on each change.")

;;; Terminal output ---------------------------------------------------

(defun nae--write (s)
  "Write string S to stdout via the write(2) syscall."
  (let* ((n (length s))
         (buf (alloc-bytes (+ n 1) 1))
         (i 0))
    (while (< i n) (ptr-write-u8 buf i (aref s i)) (setq i (1+ i)))
    (syscall-direct 1 1 buf n 0 0 0)))

(defun nae--render ()
  "Redraw the single-line buffer view + status (ANSI, home + clear)."
  (when nae--draw
    (let* ((text (nelisp-buffer-string nae--buf))
           (pt (nelisp-point nae--buf)))
      (nae--write
       (concat "\e[H\e[2J"                       ; home + clear screen
               text "\r\n"
               "\e[7m -- nelisp-async-editor  point=" (number-to-string pt)
               "  ticks=" (number-to-string nae--tick)
               "  (C-q quit) \e[0m"
               ;; place the cursor at the buffer column (row 1).
               "\e[1;" (number-to-string pt) "H")))))

;;; Editing commands --------------------------------------------------

(defun nae--self-insert (b)
  (nelisp-insert (char-to-string b) nae--buf))

(defun nae--forward ()
  (let ((pt (nelisp-point nae--buf)))
    (when (< pt (nelisp-point-max nae--buf))
      (nelisp-goto-char (1+ pt) nae--buf))))

(defun nae--back ()
  (let ((pt (nelisp-point nae--buf)))
    (when (> pt (nelisp-point-min nae--buf))
      (nelisp-goto-char (1- pt) nae--buf))))

(defun nae--del-back ()
  (let ((pt (nelisp-point nae--buf)))
    (when (> pt (nelisp-point-min nae--buf))
      (nelisp-delete-region (1- pt) pt nae--buf))))

;;; Actor + loop ------------------------------------------------------

(defun nae--make-actor ()
  "Spawn the editor actor: receive key events, dispatch, redraw."
  (nelisp-spawn
   (nelisp-actor-lambda
     (let ((run t))
       (while run
         (let ((ev (nelisp-receive)))
           (when (and (nelisp-event-p ev) (eq (nelisp-event-kind ev) 'key))
             (let ((b (nelisp-event-data ev)))
               (cond
                ((= b 17) (setq run nil))               ; C-q
                ((= b 6)  (nae--forward))               ; C-f
                ((= b 2)  (nae--back))                  ; C-b
                ((or (= b 127) (= b 8)) (nae--del-back)) ; DEL / BS
                ((= b 12) nil)                          ; C-l (redraw below)
                ((>= b 32) (nae--self-insert b))        ; printable
                (t nil))
               (nae--render)))
           (nelisp-yield)))))))

(defun nae--setup ()
  (setq nae--buf (nelisp-buffer--make :name "*async-editor*"))
  (setq nae--tick 0)
  (nelisp-actor--reset)
  (nelisp-async-reset-timers))

(defun nae-run-interactive ()
  "Run the editor on a real terminal (raw mode + async loop)."
  (nae--setup)
  (setq nae--draw t)
  (let ((main (nae--make-actor)))
    ;; A repeating timer demonstrates timer/input multiplexing on one loop.
    (run-at-time 0.5 0.5 (lambda () (setq nae--tick (1+ nae--tick)) (nae--render)))
    (nae--render)
    (prog1 (nelisp-async-run-tty-raw main)
      (nae--write "\e[2J\e[H"))))   ; clear on exit

(defun nae-run-batch ()
  "Run headless on piped stdin; print the final buffer + tick count on quit.
The timer still runs, so a slow feed will accumulate ticks."
  (nae--setup)
  (setq nae--draw nil)
  (let ((main (nae--make-actor)))
    (run-at-time 0.5 0.5 (lambda () (setq nae--tick (1+ nae--tick))))
    (let ((res (nelisp-async-run-tty main)))
      (format "RESULT[%s text=%S point=%d ticks=%d]"
              res (nelisp-buffer-string nae--buf)
              (nelisp-point nae--buf) nae--tick))))

;;; nelisp-async-editor.el ends here
