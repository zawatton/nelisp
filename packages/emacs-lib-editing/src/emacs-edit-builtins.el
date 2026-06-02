;;; emacs-edit-builtins.el --- Editing commands + kill-ring (Track E MVP)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track E (2026-05-03) — Layer 2.
;;
;; The β-stage editing-command surface that the user-facing edit cycle
;; needs once the command loop dispatches keys to commands:
;;
;;   - `self-insert-command' / `newline' / `delete-backward-char'
;;   - `ensure-empty-lines'
;;   - kill-ring infra: `kill-ring' / `kill-new' / `copy-region-as-kill'
;;     / `kill-region' / `kill-line' / `yank'
;;   - word motion: `forward-word' / `backward-word' (= ASCII alnum
;;     boundary — syntax-table integration deferred to a future phase
;;     once `nelisp-ec' grows a syntax-class accessor).
;;
;; Out of scope (= deferred):
;;
;;   - `buffer-undo-list' / `undo' / `primitive-undo': will be a
;;     dedicated Phase once we agree on the record-list shape.
;;   - `transpose-chars' / `open-line' / `indent-*': depend on mode
;;     hooks or selection state we don't yet model.
;;
;; Each definition is gated on `unless (fboundp ...)' / `unless
;; (boundp ...)' so loading inside a host Emacs is a cheap no-op.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'emacs-buffer-builtins)
(require 'emacs-line-builtins)

(defun emacs-edit-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  (if (not (boundp 'emacs-version))
      ;; Standalone NeLisp loads `emacs-stub-bulk' first, so many of
      ;; these names are already `fboundp' as nil-returning shims.  The
      ;; edit bridge owns these command names in that environment.
      t
    (not (fboundp symbol))))

;;;; --- last-command-event placeholder ---------------------------------

;; Real Emacs sets `last-command-event' inside the command loop;
;; for now we only care that the variable exists so callers (=
;; `self-insert-command' default arg) don't void-variable.

(unless (boundp 'last-command-event)
  (defvar last-command-event nil
    "Phase E placeholder for the command-loop-set last input event."))

;;;; --- character insertion --------------------------------------------

(unless (boundp 'overwrite-mode)
  (defvar overwrite-mode nil
    "Phase 2.AI placeholder: non-nil = `self-insert-command' replaces
the char at point instead of inserting (= mirrors `overwrite-mode'
minor-mode in real Emacs).  Set to t / nil; richer values like
`overwrite-mode-binary' are deferred."))

(defun emacs-edit--self-insert-command (n char)
  "Pure-Elisp body for `self-insert-command'."
  (let* ((c (or char last-command-event))
         (count (or n 1))
         (s (cond
             ((null c)
              (signal 'error '("self-insert-command: no char to insert")))
             ((stringp c) c)
             ((integerp c) (string c))
             (t (signal 'wrong-type-argument
                        (list 'character-or-string c))))))
    (let ((i 0))
      (while (< i count)
        (let ((beg (nelisp-ec-point)))
          ;; Phase 2.AI overwrite: delete the char at point first
          ;; (unless at EOB or just before a `\n', so we don't eat
          ;; the line terminator).
          (when (and overwrite-mode
                     (< beg (nelisp-ec-point-max))
                     (not (eq (let ((sub (nelisp-ec-buffer-substring
                                          beg (1+ beg))))
                                (and (> (length sub) 0) (aref sub 0)))
                              ?\n)))
            (let ((deleted (nelisp-ec-buffer-substring beg (1+ beg))))
              (nelisp-ec-delete-region beg (1+ beg))
              (when (fboundp 'emacs-undo-record-delete)
                (emacs-undo-record-delete deleted beg))))
          (nelisp-ec-insert s)
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert beg (nelisp-ec-point)))
          ;; Doc 51 Track S — mark dirty for next jit-lock flush.
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region beg (nelisp-ec-point))))
        (setq i (+ i 1))))
    nil))

(when (emacs-edit-builtins--install-function-p 'self-insert-command)
  (defun self-insert-command (&optional n char)
    "Phase E polyfill: insert CHAR (or `last-command-event') N times.
N defaults to 1.  CHAR may be an integer or a single-char string;
when nil, falls back to `last-command-event'.

Track E.2: when the undo subsystem is loaded, records the
inserted span on `buffer-undo-list'.

Phase 2.AI: when `overwrite-mode' is non-nil and point is not at
EOB or end-of-line, the char at point is deleted before each insert
(= one-char-out, one-char-in, point advances by 1 not by 2).

Bound to printable chars in `nemacs-main-keymap'.  The `(interactive
\"p\")' form supplies N from the prefix-arg so `call-interactively'
gets a fully-formed arg list."
    (interactive "p")
    (emacs-edit--self-insert-command n char)))

(when (emacs-edit-builtins--install-function-p 'newline)
  (defun newline (&optional n interactive)
    "Phase E polyfill: insert N newlines (default 1).
Track E.2: records the inserted span on `buffer-undo-list'.
Bound to RET (= byte 13) in `nemacs-main-keymap'."
    (interactive "p")
    (ignore interactive)
    (let ((c (or n 1)) (i 0))
      (while (< i c)
        (let ((beg (nelisp-ec-point)))
          (nelisp-ec-insert "\n")
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert beg (nelisp-ec-point)))
          ;; Doc 51 Track S — mark dirty for jit-lock.
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region beg (nelisp-ec-point))))
        (setq i (+ i 1))))
    nil))

(when (emacs-edit-builtins--install-function-p 'ensure-empty-lines)
  (defun ensure-empty-lines (&optional lines)
    "Ensure that LINES empty lines appear immediately before point.
LINES defaults to 1.  If point is not at beginning of line, insert a
newline first, matching Emacs `subr.el' behavior."
    (interactive "p")
    (when (condition-case _ (current-buffer) (error nil))
      (let ((target (or lines 1)))
        (unless (bolp)
          (nelisp-ec-insert "\n"))
        (let* ((p (nelisp-ec-point))
               (lo (nelisp-ec-point-min))
               (before (nelisp-ec-buffer-substring lo p))
               (idx (1- (length before)))
               (count 0))
          (while (and (>= idx 0)
                      (eq (aref before idx) ?\n))
            (setq count (1+ count)
                  idx (1- idx)))
          (cond
           ((> count target)
            (nelisp-ec-delete-region (- p (- count target)) p))
           ((< count target)
            (let ((n (- target count)))
              (while (> n 0)
                (nelisp-ec-insert "\n")
                (setq n (1- n)))))))))
    nil))

(when (emacs-edit-builtins--install-function-p 'delete-backward-char)
  (defun delete-backward-char (&optional n killflag)
    "Phase E polyfill: delete N characters backward (default 1).
KILLFLAG (= prefix-arg-driven `kill-region' route) is accepted for
API parity but ignored in MVP.

Track E.2: captures the deleted text and records it on
`buffer-undo-list' so `undo' can re-insert it."
    (interactive "p")
    (ignore killflag)
    (let* ((n (or n 1))
           (p (nelisp-ec-point))
           (start (max (nelisp-ec-point-min) (- p n)))
           (end p)
           (text (when (and (fboundp 'emacs-undo-record-delete)
                            (> end start))
                   (nelisp-ec-buffer-substring start end))))
      (nelisp-ec-delete-char (- n))
      (when text
        (emacs-undo-record-delete text start))
      ;; Doc 51 Track S — mark dirty for jit-lock.  After a delete,
      ;; the surviving region around START needs re-syntax-state-walk;
      ;; we mark a single-char interval at START to keep it minimal.
      (when (fboundp 'emacs-font-lock-mark-dirty-region)
        (emacs-font-lock-mark-dirty-region start (1+ start))))))

;;;; --- kill ring -----------------------------------------------------

(unless (boundp 'kill-ring)
  (defvar kill-ring nil
    "Phase E placeholder: list of killed strings, most recent first."))

(unless (boundp 'kill-ring-max)
  (defvar kill-ring-max 60
    "Phase E placeholder: maximum length of `kill-ring'."))

(unless (boundp 'kill-ring-yank-pointer)
  (defvar kill-ring-yank-pointer nil
    "Phase E placeholder: cdr-pointer into `kill-ring' for `yank-pop'.
Set to `kill-ring' on each fresh kill."))

(defvar emacs-edit--last-yank-bounds nil
  "Track D Phase 2.AA: cons cell (START . END) of the most recent
`yank' / `yank-pop' insertion in the current buffer.  `yank-pop'
uses this to know what range to delete + replace.  Set to nil when
no recent yank, or when the buffer changed underneath.")

(unless (boundp 'interprogram-cut-function)
  (defvar interprogram-cut-function nil
    "Function called by `kill-new' to mirror a kill onto an external
clipboard.  The function is called with one argument STRING — the
text just pushed onto `kill-ring'.  Set by display backends
(= GTK / X11 / etc.) at boot; nil under TUI / batch."))

(unless (boundp 'interprogram-paste-function)
  (defvar interprogram-paste-function nil
    "Function called to fetch text from an external clipboard for
`yank'.  The function is called with no arguments and should return
either a string (= clipboard text) or nil (= nothing newer than the
head of `kill-ring').  Set by display backends at boot."))

(defun emacs-edit--trim-kill-ring ()
  "Truncate `kill-ring' to `kill-ring-max' entries."
  (let ((c kill-ring) (i 1))
    (while (and c (< i kill-ring-max))
      (setq c (cdr c))
      (setq i (+ i 1)))
    (when c (setcdr c nil))))

(defun emacs-edit--kill-new (string &optional replace)
  "Pure-Elisp body for `kill-new'."
  (when (and (stringp string) (> (length string) 0))
    (cond
     ((and replace kill-ring)
      (setcar kill-ring string))
     (t
      (setq kill-ring (cons string kill-ring))
      (emacs-edit--trim-kill-ring)))
    (setq kill-ring-yank-pointer kill-ring)
    (when (and interprogram-cut-function
               (functionp interprogram-cut-function))
      (funcall interprogram-cut-function string)))
  string)

(when (emacs-edit-builtins--install-function-p 'kill-new)
  (defun kill-new (string &optional replace)
    "Phase E polyfill: prepend STRING to `kill-ring'.
With REPLACE non-nil, mutate the head entry instead of pushing.
When `interprogram-cut-function' is set, also mirror STRING onto the
external clipboard (= GUI display backends bridge to GtkClipboard /
X selection here)."
    (emacs-edit--kill-new string replace)))

(when (emacs-edit-builtins--install-function-p 'copy-region-as-kill)
  (defun copy-region-as-kill (start end &optional region)
    "Phase E polyfill: push the START..END region onto `kill-ring'."
    (ignore region)
    (let ((s (min start end))
          (e (max start end)))
      (kill-new (nelisp-ec-buffer-substring s e)))
    nil))

(when (emacs-edit-builtins--install-function-p 'kill-region)
  (defun kill-region (start end &optional region)
    "Phase E polyfill: push the START..END region to `kill-ring' AND delete it.
Track E.2: records the deleted text on `buffer-undo-list'."
    (ignore region)
    (let* ((s (min start end))
           (e (max start end))
           (text (nelisp-ec-buffer-substring s e)))
      (kill-new text)
      (nelisp-ec-delete-region s e)
      (when (fboundp 'emacs-undo-record-delete)
        (emacs-undo-record-delete text s)))
    nil))

(when (emacs-edit-builtins--install-function-p 'kill-line)
  (defun kill-line (&optional arg)
    "Phase E polyfill: kill from point to end of line.
At EOL (= no chars to kill on the current line) and not at EOB,
kills the trailing `\\n' so successive `kill-line' calls collapse
the cursor toward EOB.  ARG (= multi-line variant) deferred.
Bound to C-k in `nemacs-main-keymap'."
    (interactive "P")
    (ignore arg)
    (let* ((start (nelisp-ec-point))
           (em (nelisp-ec-point-max))
           (eol (emacs-line--eol-pos)))
      (cond
       ((= start eol)
        ;; At EOL — kill the \n itself if any.
        (when (< eol em)
          (kill-region start (+ eol 1))))
       (t
        (kill-region start eol))))))

(defun emacs-edit--yank (arg)
  "Pure-Elisp body for `yank'."
  (when (and (null arg)
             interprogram-paste-function
             (functionp interprogram-paste-function))
    (let ((external (funcall interprogram-paste-function)))
      (when (and (stringp external) (> (length external) 0)
                 ;; Avoid duplicates when our own cut just pushed
                 ;; this same text onto the clipboard.
                 (not (equal external (car-safe kill-ring))))
        (setq kill-ring (cons external kill-ring))
        (emacs-edit--trim-kill-ring)
        (setq kill-ring-yank-pointer kill-ring))))
  (let ((idx (cond
              ((null arg) 0)
              ((integerp arg) (max 0 (- arg 1)))
              (t 0)))
        (entry nil))
    (let ((c kill-ring))
      (while (and c (> idx 0))
        (setq c (cdr c))
        (setq idx (- idx 1)))
      (setq entry (and c (car c)))
      (setq kill-ring-yank-pointer (or c kill-ring)))
    (when entry
      (let ((beg (nelisp-ec-point)))
        (nelisp-ec-insert entry)
        (setq emacs-edit--last-yank-bounds (cons beg (nelisp-ec-point)))
        (when (fboundp 'emacs-undo-record-insert)
          (emacs-undo-record-insert beg (nelisp-ec-point)))))
    nil))

(when (emacs-edit-builtins--install-function-p 'yank)
  (defun yank (&optional arg)
    "Phase E polyfill: insert the most recent kill at point.
ARG selects which kill-ring entry: 1 (default) = head; N>1 = N-th
older entry; `-' = `yank-pop'-style (deferred).  Negative / `-'
arg currently coerced to head-yank.

When ARG is nil (= head yank) and `interprogram-paste-function'
returns a non-nil string, that string is pushed onto `kill-ring'
first so the GUI clipboard wins over the local kill-ring head
(= matches Emacs' `current-kill' behaviour).

Track E.2: records the inserted span on `buffer-undo-list'."
    (emacs-edit--yank arg)))

(defun emacs-edit--yank-pop (arg)
  "Pure-Elisp body for `yank-pop'."
  (let* ((bounds emacs-edit--last-yank-bounds)
         (n (or arg 1)))
    (cond
     ((null bounds)
      (signal 'error '("Previous command was not a yank")))
     ((not (= (nelisp-ec-point) (cdr bounds)))
      (signal 'error '("Previous command was not a yank")))
     ((null kill-ring)
      (signal 'error '("Kill ring is empty")))
     (t
      (let* ((ring kill-ring)
             (len (length ring))
             (cur (or kill-ring-yank-pointer ring))
             (cur-idx (let ((i 0) (c ring))
                        (while (and c (not (eq c cur)))
                          (setq c (cdr c) i (1+ i)))
                        (if c i 0)))
             (new-idx (mod (+ cur-idx n) len))
             (new-cell (nthcdr new-idx ring))
             (entry (and new-cell (car new-cell)))
             (start (car bounds))
             (end (cdr bounds)))
        (when entry
          (let ((deleted (nelisp-ec-buffer-substring start end)))
            (nelisp-ec-delete-region start end)
            (when (fboundp 'emacs-undo-record-delete)
              (emacs-undo-record-delete deleted start)))
          (nelisp-ec-goto-char start)
          (nelisp-ec-insert entry)
          (setq emacs-edit--last-yank-bounds
                (cons start (nelisp-ec-point)))
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert start (nelisp-ec-point)))
          (setq kill-ring-yank-pointer new-cell)))))
    nil))

(when (emacs-edit-builtins--install-function-p 'yank-pop)
  (defun yank-pop (&optional arg)
    "Phase 2.AA polyfill: replace the just-yanked text with an older
kill-ring entry.  Bound to `M-y' in the GUI; only meaningful right
after `yank' / `yank-pop' (= the previous command must have left
the inserted bounds in `emacs-edit--last-yank-bounds' AND point at
the end of that insertion).

ARG (= step count, default 1) advances the yank pointer that many
entries.  Negative ARG steps backward.  Wraps around at the end.

Records the replacement on `buffer-undo-list' (= one delete + one
insert) so a subsequent `undo' restores the buffer."
    (interactive "p")
    (emacs-edit--yank-pop arg)))

;;;; --- word motion (ASCII alnum) -------------------------------------

(defun emacs-edit--word-char-p (ch)
  "Phase E MVP: ASCII alnum-or-underscore = word constituent."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun emacs-edit--char-at (pos)
  "Return the char at POS, or nil when out of buffer range."
  (let* ((bm (nelisp-ec-point-min))
         (em (nelisp-ec-point-max)))
    (when (and (>= pos bm) (< pos em))
      (let ((s (nelisp-ec-buffer-substring pos (+ pos 1))))
        (and (> (length s) 0) (aref s 0))))))

(when (emacs-edit-builtins--install-function-p 'forward-word)
  (defun forward-word (&optional arg)
    "Phase E polyfill: ASCII alnum word motion.
ARG > 0: move forward ARG words.  ARG < 0: move backward.
Returns t when at least one boundary was crossed, nil otherwise."
    (let* ((count (or arg 1))
           (sign (if (>= count 0) 1 -1))
           (n (abs count))
           (moved 0))
      (cond
       ((= sign 1)
        (while (> n 0)
          (let ((p (nelisp-ec-point))
                (em (nelisp-ec-point-max))
                (start-p nil))
            (while (and (< p em)
                        (not (emacs-edit--word-char-p
                              (emacs-edit--char-at p))))
              (setq p (+ p 1)))
            (setq start-p p)
            (while (and (< p em)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at p)))
              (setq p (+ p 1)))
            (when (> p start-p) (setq moved (+ moved 1)))
            (nelisp-ec-goto-char p)
            (setq n (- n 1)))))
       (t
        (while (> n 0)
          (let ((p (nelisp-ec-point))
                (bm (nelisp-ec-point-min))
                (start-p nil))
            (while (and (> p bm)
                        (not (emacs-edit--word-char-p
                              (emacs-edit--char-at (- p 1)))))
              (setq p (- p 1)))
            (setq start-p p)
            (while (and (> p bm)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at (- p 1))))
              (setq p (- p 1)))
            (when (< p start-p) (setq moved (+ moved 1)))
            (nelisp-ec-goto-char p)
            (setq n (- n 1))))))
      (> moved 0))))

(when (emacs-edit-builtins--install-function-p 'backward-word)
  (defun backward-word (&optional arg)
    "Phase E polyfill: equivalent to `(forward-word (- ARG))'."
    (forward-word (- (or arg 1)))))

(provide 'emacs-edit-builtins)

;;; emacs-edit-builtins.el ends here
