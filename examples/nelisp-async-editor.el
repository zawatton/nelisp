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
(defvar nae--win nil  "The `nelisp-window' used for diff redisplay.")
(defvar nae--tick 0   "Timer-driven tick counter (status-line liveness).")
(defvar nae--draw t   "When non-nil, render to the terminal on each change.")
(defvar nae--kill nil "Kill-ring head: the last killed string.")
(defvar nae--file nil "Path backing the buffer (for C-x C-s save), or nil.")
(defvar nae--msg "" "Transient status-line message (e.g. save result).")
(defvar nae--undo nil "Undo stack: list of (TEXT . POINT) snapshots, newest first.")
(defvar nae--redo nil "Redo stack: list of (TEXT . POINT) snapshots, newest first.")
(defvar nae--typing nil "Non-nil while a run of self-inserts is being coalesced.")
(defconst nae--undo-limit 1000 "Maximum retained undo snapshots.")

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

(defun nae--kill-line ()
  "Kill from point to end of line into the kill ring; at line end, kill the \\n."
  (let* ((pt (nae--pt)) (rc (nae--point->rc)) (le (nae--rc->point (car rc) 1000000)))
    (cond
     ((< pt le)
      (setq nae--kill (substring (nae--text) (1- pt) (1- le)))
      (nelisp-delete-region pt le nae--buf))
     ((< pt (nae--pmax))
      (setq nae--kill "\n")
      (nelisp-delete-region pt (1+ pt) nae--buf)))))

(defun nae--yank ()
  "Insert the kill-ring head at point."
  (when (and nae--kill (> (length nae--kill) 0))
    (nelisp-insert nae--kill nae--buf)))

;;; File I/O (open/read/write/close via syscall) ----------------------

(defun nae--cpath (path)
  "Return a NUL-terminated C-string buffer holding PATH (raw bytes)."
  (let* ((n (string-bytes path)) (pb (alloc-bytes (+ n 1) 1)) (i 0))
    (while (< i n) (ptr-write-u8 pb i (string-byte path i)) (setq i (1+ i)))
    (ptr-write-u8 pb n 0)
    pb))

(defun nae--read-file (path)
  "Return the contents of PATH as a string, or nil if it cannot be opened."
  (let ((fd (syscall-direct 2 (nae--cpath path) 0 0 0 0 0)))   ; open O_RDONLY
    (if (< fd 0) nil
      (let ((chunk (alloc-bytes 65536 1)) (acc "") (go t))
        (while go
          (let ((n (syscall-direct 0 fd chunk 65536 0 0 0)))   ; read
            (if (<= n 0) (setq go nil)
              (let ((s (make-string n 0)) (i 0))
                (while (< i n) (aset s i (ptr-read-u8 chunk i)) (setq i (1+ i)))
                (setq acc (concat acc s))))))
        (syscall-direct 3 fd 0 0 0 0 0)                          ; close
        acc))))

(defun nae--write-file (path text)
  "Write TEXT to PATH (O_WRONLY|O_CREAT|O_TRUNC, 0644).  Return bytes written, or nil."
  (let ((fd (syscall-direct 2 (nae--cpath path) 577 420 0 0 0)))  ; 577=WRONLY|CREAT|TRUNC
    (if (< fd 0) nil
      (let* ((n (string-bytes text)) (buf (alloc-bytes (+ n 1) 1)) (i 0))
        (while (< i n) (ptr-write-u8 buf i (string-byte text i)) (setq i (1+ i)))
        (let ((w (syscall-direct 1 fd buf n 0 0 0)))             ; write
          (syscall-direct 3 fd 0 0 0 0 0)                         ; close
          w)))))

(defun nae--save ()
  "Save the buffer to `nae--file' (C-x C-s)."
  (if (null nae--file)
      (setq nae--msg "(no file)")
    (let ((w (nae--write-file nae--file (nae--text))))
      (setq nae--msg (if w (concat "wrote " (number-to-string w) "B -> " nae--file)
                       (concat "save FAILED: " nae--file))))))

;;; Undo / redo (snapshot based) --------------------------------------

(defun nae--snapshot ()
  "Push the current (TEXT . POINT) onto the undo stack; invalidate redo."
  (setq nae--undo (cons (cons (nae--text) (nae--pt)) nae--undo))
  (when (> (length nae--undo) nae--undo-limit)
    (setq nae--undo (nae--take nae--undo nae--undo-limit)))
  (setq nae--redo nil))

(defun nae--take (lst n)
  (let (out (i 0))
    (while (and lst (< i n)) (setq out (cons (car lst) out) lst (cdr lst) i (1+ i)))
    (nreverse out)))

(defun nae--restore (snap)
  "Replace the buffer contents/point from SNAP = (TEXT . POINT)."
  (nelisp-delete-region (nae--pmin) (nae--pmax) nae--buf)
  (when (> (length (car snap)) 0) (nelisp-insert (car snap) nae--buf))
  (nelisp-goto-char (min (cdr snap) (nae--pmax)) nae--buf))

(defun nae--do-undo ()
  (setq nae--typing nil)
  (if (null nae--undo) (setq nae--msg "(nothing to undo)")
    (setq nae--redo (cons (cons (nae--text) (nae--pt)) nae--redo))
    (let ((snap (car nae--undo)))
      (setq nae--undo (cdr nae--undo))
      (nae--restore snap)
      (setq nae--msg "undo"))))

(defun nae--do-redo ()
  (setq nae--typing nil)
  (if (null nae--redo) (setq nae--msg "(nothing to redo)")
    (setq nae--undo (cons (cons (nae--text) (nae--pt)) nae--undo))
    (let ((snap (car nae--redo)))
      (setq nae--redo (cdr nae--redo))
      (nae--restore snap)
      (setq nae--msg "redo"))))

(defun nae--edit (modifies fn)
  "Run FN; if MODIFIES, take an undo snapshot first (coalescing self-insert)."
  (cond
   ((eq modifies 'insert)
    (unless nae--typing (nae--snapshot))
    (setq nae--typing t)
    (funcall fn))
   (modifies (nae--snapshot) (setq nae--typing nil) (funcall fn))
   (t (setq nae--typing nil) (funcall fn))))

(defun nae--key (b)
  "Dispatch a single decoded byte B (not part of an escape sequence)."
  (cond
   ((or (= b 13) (= b 10)) (nae--edit t #'nae--newline))       ; Enter
   ((or (= b 127) (= b 8)) (nae--edit t #'nae--del-back))      ; DEL / BS
   ((= b 4)  (nae--edit t #'nae--del-forward))                 ; C-d
   ((= b 11) (nae--edit t #'nae--kill-line))                   ; C-k
   ((= b 25) (nae--edit t #'nae--yank))                        ; C-y
   ((= b 31) (nae--do-undo))                                   ; C-/ (= C-_) undo
   ((= b 6)  (nae--edit nil #'nae--forward))                   ; C-f
   ((= b 2)  (nae--edit nil #'nae--back))                      ; C-b
   ((= b 1)  (nae--edit nil #'nae--line-start))                ; C-a
   ((= b 5)  (nae--edit nil #'nae--line-end))                  ; C-e
   ((= b 16) (nae--edit nil #'nae--up))                        ; C-p
   ((= b 14) (nae--edit nil #'nae--down))                      ; C-n
   ((= b 12) (setq nae--typing nil))                           ; C-l (redraw after)
   ((>= b 32) (nae--edit 'insert (lambda () (nae--self-insert b)))) ; printable
   (t nil)))

(defun nae--arrow (final)
  "Dispatch the FINAL byte of an `ESC [' sequence."
  (setq nae--typing nil)                ; motion ends a self-insert run
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
  ;; Byte-faithful terminal output: emit the raw UTF-8 bytes (string-byte),
  ;; not codepoints, so non-ASCII renders correctly.  `length' is chars now.
  (let* ((n (string-bytes s)) (buf (alloc-bytes (+ n 1) 1)) (i 0))
    (while (< i n) (ptr-write-u8 buf i (string-byte s i)) (setq i (1+ i)))
    (syscall-direct 1 1 buf n 0 0 0)))

(defun nae--render ()
  "Repaint via `nelisp-redisplay' (per-line diff) + a status line.
The window's dirty rows are painted by `nelisp-redisplay-window', which also
positions the hardware cursor; the status line is drawn just below it.  All
output flows through `nelisp-redisplay--output-fn' so it is capturable."
  (when (and nae--draw nae--win)
    (let* ((rc (nae--point->rc))
           (h (nelisp-window-height nae--win)))
      (setf (nelisp-window-cursor-row nae--win) (min (car rc) (1- h)))
      (setf (nelisp-window-cursor-col nae--win) (cdr rc))
      (nelisp-window-mark-all-dirty nae--win)
      ;; Status line on the row just below the text window (reverse video).
      (nelisp-redisplay--emit
       (concat "\e[" (number-to-string (1+ h)) ";1H\e[2K"
               "\e[7m -- nelisp-async-editor  "
               "L" (number-to-string (1+ (car rc)))
               " C" (number-to-string (1+ (cdr rc)))
               "  ticks=" (number-to-string nae--tick)
               "  " nae--msg
               "  (C-x C-s save, C-q quit) \e[0m"))
      ;; Paint dirty buffer rows last so the cursor ends in the text area.
      (nelisp-redisplay-window nae--win))))

;;; Actor + loop ------------------------------------------------------

(defun nae--make-actor ()
  "Spawn the editor actor with an ESC-sequence decoder."
  (nelisp-spawn
   (nelisp-actor-lambda
     (let ((run t) (esc 0) (cx 0) (mb 0) (macc nil))  ; mb/macc: UTF-8 input assembler
       (while run
         (let ((ev (nelisp-receive)))
           (when (and (nelisp-event-p ev) (eq (nelisp-event-kind ev) 'key))
             (let ((b (nelisp-event-data ev)))
               (cond
                ((= esc 2) (setq esc 0) (nae--arrow b))
                ((= esc 1) (if (= b 91) (setq esc 2) (setq esc 0)))  ; expect '['
                ((= cx 1) (setq cx 0)                                ; C-x <key>
                 (cond ((= b 19) (nae--save))                        ; C-x C-s save
                       ((= b 117) (nae--do-undo))                    ; C-x u  undo
                       ((= b 18) (nae--do-redo))))                   ; C-x C-r redo
                ;; --- UTF-8 input: assemble a multibyte keystroke, then insert
                ;; the raw bytes (terminals deliver CJK as 2-4 UTF-8 bytes).
                ((> mb 0)
                 (setq macc (cons b macc) mb (1- mb))
                 (when (= mb 0)
                   (nae--edit 'insert
                     (lambda () (nelisp-insert (apply #'unibyte-string (reverse macc)) nae--buf)))
                   (setq macc nil)))
                ((>= b 240) (setq mb 3 macc (list b)))               ; 4-byte lead
                ((>= b 224) (setq mb 2 macc (list b)))               ; 3-byte lead
                ((>= b 192) (setq mb 1 macc (list b)))               ; 2-byte lead
                ((>= b 128)                                          ; stray byte
                 (nae--edit 'insert (lambda () (nelisp-insert (unibyte-string b) nae--buf))))
                ((= b 27) (setq esc 1))                              ; ESC
                ((= b 24) (setq cx 1))                               ; C-x prefix
                ((= b 17) (setq run nil))                            ; C-q
                (t (nae--key b)))
               (nae--render)))
           (nelisp-yield)))))))

(defun nae--setup (&optional file)
  (setq nae--buf (nelisp-buffer--make :name "*async-editor*"))
  ;; Diff-redisplay window: reserve the bottom row for the status line.
  (let* ((sz (nelisp-redisplay-terminal-size))
         (w (if (consp sz) (car sz) 80))
         (ht (if (consp sz) (cdr sz) 24)))
    (setq nae--win (nelisp-make-window nae--buf (max 1 (1- ht)) w)))
  (setq nelisp-redisplay--output-fn #'nae--write)
  (setq nae--file file)
  (when file
    (let ((contents (nae--read-file file)))
      (when (and contents (> (length contents) 0))
        (nelisp-insert contents nae--buf)
        (nelisp-goto-char (nae--pmin) nae--buf))))
  (setq nae--tick 0)
  (setq nae--kill nil)
  (setq nae--undo nil nae--redo nil nae--typing nil)
  (setq nae--msg (if file (concat "[" file "]") ""))
  (nelisp-actor--reset)
  (nelisp-async-reset-timers))

(defun nae-run-interactive (&optional file)
  "Run the editor on a real terminal (raw mode + async loop).
With FILE, load it on start; C-x C-s saves back to it."
  (nae--setup file)
  (setq nae--draw t)
  (let ((main (nae--make-actor)))
    (run-at-time 0.5 0.5 (lambda () (setq nae--tick (1+ nae--tick)) (nae--render)))
    (nae--render)
    (prog1 (nelisp-async-run-tty-raw main)
      (nae--write "\e[2J\e[H"))))

(defun nae-run-batch (&optional file)
  "Run headless on piped stdin; print the final buffer + point on quit.
With FILE, load it on start; C-x C-s saves back to it."
  (nae--setup file)
  (setq nae--draw nil)
  (let ((main (nae--make-actor)))
    (run-at-time 0.5 0.5 (lambda () (setq nae--tick (1+ nae--tick))))
    (let ((res (nelisp-async-run-tty main)))
      (format "RESULT[%s text=%S point=%d ticks=%d msg=%S]"
              res (nae--text) (nae--pt) nae--tick nae--msg))))

;;; nelisp-async-editor.el ends here
