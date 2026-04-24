;;; main.el --- NeLisp editor minimum (Phase 5-B.5 demo) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-B.5 per Doc 13 §3.5.  Three-core integration demo:
;; `nelisp-buffer' + `nelisp-redisplay' + `nelisp-eventloop' driving
;; a minimal "editor" that reacts to scripted key events.
;;
;; Public entry points:
;;
;;   `nelisp-editor-init'                 — fresh buffer + window
;;   `nelisp-editor-install-bindings'     — printable + movement keys
;;   `nelisp-editor-run-scripted'         — sync loop over key list
;;   `nelisp-editor-run-actor-scripted'   — actor main-loop variant
;;   `nelisp-editor-repaint'              — force window redisplay
;;
;; Bindings (single-char MVP):
;;   32..126, \n      -> self-insert
;;   C-f / C-b        -> forward/backward-char
;;   C-n / C-p        -> next/previous-line
;;   C-d              -> delete-char (forward)
;;   DEL (0x7f)       -> delete-backward-char
;;   C-q              -> quit (C-x C-c is multi-char — deferred to
;;                              a later prefix-handling phase)
;;
;; Real tty input / raw-mode reader are out of scope; this file is
;; exercised by `test/nelisp-editor-test.el' via `run-scripted'.

;;; Code:

(require 'cl-lib)
(require 'nelisp-buffer)
(require 'nelisp-redisplay)
(require 'nelisp-eventloop)

(defvar nelisp-editor--current-buffer nil)
(defvar nelisp-editor--current-window nil)
(defvar nelisp-editor--main-actor nil)

;;; Init ---------------------------------------------------------------

(defun nelisp-editor-init (&optional name height width)
  "Initialise a fresh editor session.  Return (BUFFER . WINDOW)."
  (let* ((buf (nelisp-generate-new-buffer (or name "*nelisp-editor*")))
         (win (nelisp-make-window buf (or height 24) (or width 80))))
    (setq nelisp-editor--current-buffer buf
          nelisp-editor--current-window win
          nelisp-editor--main-actor nil)
    (nelisp-window-mark-all-dirty win)
    (nelisp-editor-install-bindings)
    (cons buf win)))

(defun nelisp-editor-current-buffer () nelisp-editor--current-buffer)
(defun nelisp-editor-current-window () nelisp-editor--current-window)

;;; Cursor / line bookkeeping -----------------------------------------

(defun nelisp-editor--recompute-cursor ()
  "Set window cursor-row/col from buffer point (top-line=1)."
  (let* ((buf nelisp-editor--current-buffer)
         (win nelisp-editor--current-window)
         (text (nelisp-buffer-string buf))
         (pt (nelisp-point buf))
         (target (1- pt))
         (row 0) (col 0) (i 0))
    (while (< i target)
      (if (eq ?\n (aref text i))
          (setq row (1+ row) col 0)
        (setq col (1+ col)))
      (setq i (1+ i)))
    (setf (nelisp-window-cursor-row win) row)
    (setf (nelisp-window-cursor-col win) col)
    (nelisp-window-mark-dirty win row)))

(defun nelisp-editor--line-edges ()
  "Return ascending list of (START . END) 1-based line edges.
END is the position of the terminating newline, or point-max when
the line has no trailing newline."
  (let* ((buf nelisp-editor--current-buffer)
         (text (nelisp-buffer-string buf))
         (n (length text))
         (max (1+ n))
         (edges nil)
         (start 1) (i 0))
    (while (< i n)
      (when (eq ?\n (aref text i))
        (push (cons start (+ i 1)) edges)
        (setq start (+ i 2)))
      (setq i (1+ i)))
    (push (cons start max) edges)
    (nreverse edges)))

(defun nelisp-editor--find-line-index (edges pt)
  "Return 0-based index of the line containing PT in EDGES."
  (let ((idx 0) (found nil))
    (dolist (e edges)
      (when (and (null found) (<= (car e) pt) (<= pt (cdr e)))
        (setq found idx))
      (setq idx (1+ idx)))
    found))

;;; Commands ----------------------------------------------------------

(defun nelisp-editor-insert-char (ev)
  "Insert the character carried by EV at point, advance point."
  (let* ((data (nelisp-event-data ev))
         (buf nelisp-editor--current-buffer)
         (s (cond ((stringp data) data)
                  ((integerp data) (char-to-string data))
                  (t (format "%s" data)))))
    (nelisp-insert s buf)
    (when (string-match-p "\n" s)
      (nelisp-window-mark-all-dirty nelisp-editor--current-window))
    (nelisp-editor--recompute-cursor)))

(defun nelisp-editor-forward-char (_ev)
  (let* ((buf nelisp-editor--current-buffer)
         (pt (nelisp-point buf))
         (pmax (nelisp-point-max buf)))
    (when (< pt pmax)
      (nelisp-goto-char (1+ pt) buf)
      (nelisp-editor--recompute-cursor))))

(defun nelisp-editor-backward-char (_ev)
  (let* ((buf nelisp-editor--current-buffer)
         (pt (nelisp-point buf))
         (pmin (nelisp-point-min buf)))
    (when (> pt pmin)
      (nelisp-goto-char (1- pt) buf)
      (nelisp-editor--recompute-cursor))))

(defun nelisp-editor-next-line (_ev)
  "Move point to the same column on the next line."
  (let* ((buf nelisp-editor--current-buffer)
         (pt (nelisp-point buf))
         (edges (nelisp-editor--line-edges))
         (idx (nelisp-editor--find-line-index edges pt)))
    (when (and idx (< (1+ idx) (length edges)))
      (let* ((cur (nth idx edges))
             (next (nth (1+ idx) edges))
             (col (- pt (car cur)))
             (next-len (- (cdr next) (car next)))
             (target (+ (car next) (min col next-len))))
        (nelisp-goto-char target buf)
        (nelisp-editor--recompute-cursor)))))

(defun nelisp-editor-previous-line (_ev)
  "Move point to the same column on the previous line."
  (let* ((buf nelisp-editor--current-buffer)
         (pt (nelisp-point buf))
         (edges (nelisp-editor--line-edges))
         (idx (nelisp-editor--find-line-index edges pt)))
    (when (and idx (> idx 0))
      (let* ((cur (nth idx edges))
             (prev (nth (1- idx) edges))
             (col (- pt (car cur)))
             (prev-len (- (cdr prev) (car prev)))
             (target (+ (car prev) (min col prev-len))))
        (nelisp-goto-char target buf)
        (nelisp-editor--recompute-cursor)))))

(defun nelisp-editor-delete-char (_ev)
  "Delete the character at point (forward)."
  (let* ((buf nelisp-editor--current-buffer)
         (pt (nelisp-point buf))
         (pmax (nelisp-point-max buf)))
    (when (< pt pmax)
      (nelisp-delete-region pt (1+ pt) buf)
      (nelisp-window-mark-all-dirty nelisp-editor--current-window)
      (nelisp-editor--recompute-cursor))))

(defun nelisp-editor-delete-backward-char (_ev)
  "Delete the character before point."
  (let* ((buf nelisp-editor--current-buffer)
         (pt (nelisp-point buf))
         (pmin (nelisp-point-min buf)))
    (when (> pt pmin)
      (nelisp-delete-region (1- pt) pt buf)
      (nelisp-goto-char (1- pt) buf)
      (nelisp-window-mark-all-dirty nelisp-editor--current-window)
      (nelisp-editor--recompute-cursor))))

(defun nelisp-editor-quit (_ev)
  "Stop the event loop.
In the sync loop this flips `nelisp-eventloop--running' via a quit
event; in the actor loop the caller sends a quit event directly."
  (setq nelisp-eventloop--running nil)
  (when nelisp-editor--main-actor
    (nelisp-send nelisp-editor--main-actor (nelisp-make-event 'quit nil))))

;;; Bindings ----------------------------------------------------------

(defun nelisp-editor-install-bindings ()
  (nelisp-eventloop-reset-bindings)
  (cl-loop for c from 32 to 126 do
           (nelisp-eventloop-bind (char-to-string c) #'nelisp-editor-insert-char))
  (nelisp-eventloop-bind "\n" #'nelisp-editor-insert-char)
  (nelisp-eventloop-bind "\C-f" #'nelisp-editor-forward-char)
  (nelisp-eventloop-bind "\C-b" #'nelisp-editor-backward-char)
  (nelisp-eventloop-bind "\C-n" #'nelisp-editor-next-line)
  (nelisp-eventloop-bind "\C-p" #'nelisp-editor-previous-line)
  (nelisp-eventloop-bind "\C-d" #'nelisp-editor-delete-char)
  (nelisp-eventloop-bind "\177" #'nelisp-editor-delete-backward-char)
  (nelisp-eventloop-bind "\C-q" #'nelisp-editor-quit))

;;; Run helpers -------------------------------------------------------

(defun nelisp-editor--as-events (keys)
  (mapcar (lambda (k) (nelisp-make-event 'key k)) keys))

(defun nelisp-editor-run-scripted (keys)
  "Dispatch scripted KEY events through the sync event loop.
Each element of KEYS may be a char (int) or string.  Returns the
final buffer text."
  (nelisp-eventloop-run-scripted (nelisp-editor--as-events keys))
  (nelisp-buffer-string nelisp-editor--current-buffer))

(defun nelisp-editor-run-actor-scripted (keys)
  "Dispatch scripted KEY events through the actor main loop.
Returns the final buffer text after the loop terminates on quit."
  (let ((main (nelisp-eventloop-spawn-main-actor)))
    (setq nelisp-editor--main-actor main)
    (dolist (ev (nelisp-editor--as-events keys))
      (nelisp-send main ev))
    (nelisp-send main (nelisp-make-event 'quit nil))
    (nelisp-actor-run-until-idle)
    (nelisp-buffer-string nelisp-editor--current-buffer)))

;;; Redisplay glue ----------------------------------------------------

(defun nelisp-editor-repaint ()
  "Force-paint the editor window onto the terminal."
  (nelisp-redisplay-window nelisp-editor--current-window))

(provide 'nelisp-editor-main)
;;; main.el ends here
