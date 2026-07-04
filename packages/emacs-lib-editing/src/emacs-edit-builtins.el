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
;;     / `kill-region' / `kill-line' / `kill-word' / `kill-sentence'
;;     / `kill-sexp' / `yank'
;;   - word motion: `forward-word' / `backward-word' (= ASCII alnum
;;     boundary — syntax-table integration deferred to a future phase
;;     once `nelisp-ec' grows a syntax-class accessor).
;;
;; Out of scope (= deferred):
;;
;;   - `buffer-undo-list' / `undo' / `primitive-undo': will be a
;;     dedicated Phase once we agree on the record-list shape.
;;   - `open-line' / `indent-*': depend on mode hooks or selection
;;     state we don't yet model.
;;
;; Each definition is gated on `unless (fboundp ...)' / `unless
;; (boundp ...)' so loading inside a host Emacs is a cheap no-op.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'emacs-buffer-builtins)
(require 'emacs-line-builtins)

(declare-function emacs-undo-record-delete "emacs-undo")
(declare-function emacs-undo-record-insert "emacs-undo")

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

(defun emacs-edit-overwrite-mode-active-p ()
  "Return non-nil when overwrite-mode should affect insertion."
  (and (boundp 'overwrite-mode)
       overwrite-mode
       (not (eq overwrite-mode 'nelisp--unbound-marker))))

(defun emacs-edit-self-insert-direct (char &optional prefer-fast)
  "Insert CHAR directly and return a plist describing the edit.
The result contains `:beg', `:end', `:text', and `:overwrote'.
When PREFER-FAST is non-nil, a standalone-safe fast primitive may be
used for non-overwrite integer insertion."
  (let* ((text (cond
                ((stringp char) char)
                ((integerp char) (string char))
                (t (signal 'wrong-type-argument
                           (list 'character-or-string char)))))
         (single-integer (and (integerp char) char)))
    (cond
     ((and prefer-fast
           single-integer
           (not (emacs-edit-overwrite-mode-active-p))
           ;; The standalone primitive currently returns without updating
           ;; `nelisp-ec-buffer-string'.  Keep the host/test fast path, but
           ;; use the general insert path in the real NeLisp runtime.
           (not (fboundp 'nl-write-file))
           (fboundp 'nelisp-ec-insert-char-code-fast))
      (let* ((end (nelisp-ec-insert-char-code-fast single-integer))
             (beg (- end (length text))))
        (when (fboundp 'emacs-undo-record-insert)
          (emacs-undo-record-insert beg end))
        (when (fboundp 'emacs-font-lock-mark-dirty-region)
          (emacs-font-lock-mark-dirty-region beg end))
        (list :beg beg :end end :text text :overwrote nil)))
     ((and (fboundp 'nelisp-ec-point)
           (fboundp 'nelisp-ec-insert))
      (let* ((beg (nelisp-ec-point))
             (overwrote nil))
        (when (and (emacs-edit-overwrite-mode-active-p)
                   (< beg (nelisp-ec-point-max))
                   (not (eq (emacs-edit-char-at beg) ?\n)))
          (let ((deleted (nelisp-ec-buffer-substring beg (1+ beg))))
            (nelisp-ec-delete-region beg (1+ beg))
            (setq overwrote t)
            (when (fboundp 'emacs-undo-record-delete)
              (emacs-undo-record-delete deleted beg))))
        (nelisp-ec-insert text)
        (let ((end (nelisp-ec-point)))
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert beg end))
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region beg end))
          (list :beg beg :end end :text text :overwrote overwrote))))
     (t
      (self-insert-command 1 char)
      (list :beg nil :end nil :text text :overwrote nil)))))

(defun emacs-edit-delete-backward-direct (&optional n)
  "Delete up to N characters before point and return an edit plist.
N defaults to 1.  The result contains `:beg', `:end', `:text',
`:delete-len', `:delete-text', and `:deleted-newline'.  `:beg' and
`:end' describe the deleted range in the pre-edit buffer.  When point
is at `point-min', the range and text are nil."
  (let* ((count (or n 1))
         (end (nelisp-ec-point))
         (start (max (nelisp-ec-point-min) (- end count))))
    (cond
     ((or (<= count 0) (<= end start))
      (list :beg nil :end nil :text nil :deleted-newline nil))
     (t
      (let ((text (nelisp-ec-buffer-substring start end)))
        (nelisp-ec-delete-region start end)
        (when (fboundp 'emacs-undo-record-delete)
          (emacs-undo-record-delete text start))
        ;; Doc 51 Track S — mark dirty for jit-lock.  After a delete,
        ;; the surviving region around START needs re-syntax-state-walk;
        ;; we mark a single-char interval at START to keep it minimal.
        (when (fboundp 'emacs-font-lock-mark-dirty-region)
          (emacs-font-lock-mark-dirty-region start (1+ start)))
        (list :beg start
              :end end
              :text text
              :delete-len (length text)
              :delete-text text
              :deleted-newline (and (stringp text)
                                    (string-match-p "\n" text)
                                    t)))))))

(defun emacs-edit-run-quoted-insert-command (char)
  "Insert literal CHAR and return a command-result plist.
The result contains `:status', `:message', and `:edit'."
  (let ((edit (emacs-edit-self-insert-direct char)))
    (list :status 'inserted
          :message (format "quoted-insert: %c (#%d)" char char)
          :edit edit)))

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
    (let ((n (or n 1)))
      (cond
       ((> n 0)
        (emacs-edit-delete-backward-direct n)
        nil)
       (t
        (nelisp-ec-delete-char (- n)))))))

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

(defun emacs-edit-copy-region-direct (start end &optional replace)
  "Copy START..END to `kill-ring' and return an edit plist.
The result contains `:beg', `:end', `:text', and `:deleted-newline'.
REPLACE is passed through to `kill-new'."
  (let* ((s (min start end))
         (e (max start end))
         (text (nelisp-ec-buffer-substring s e)))
    (kill-new text replace)
    (list :beg s
          :end e
          :text text
          :deleted-newline (and (stringp text)
                                (string-match-p "\n" text)
                                t))))

(defun emacs-edit-kill-region-direct (start end &optional replace)
  "Kill START..END and return an edit plist.
The result contains `:beg', `:end', `:text', `:delete-len',
`:delete-text', and `:deleted-newline'.  REPLACE is passed through to
`kill-new'.  `:beg' and `:end' describe the deleted range in the
pre-edit buffer."
  (let* ((edit (emacs-edit-copy-region-direct start end replace))
         (s (plist-get edit :beg))
         (e (plist-get edit :end))
         (text (plist-get edit :text)))
    (when (< s e)
      (setq edit (append edit (list :delete-len (length text)
                                    :delete-text text)))
      (nelisp-ec-delete-region s e)
      (when (fboundp 'emacs-undo-record-delete)
        (emacs-undo-record-delete text s))
      (when (fboundp 'emacs-font-lock-mark-dirty-region)
        (emacs-font-lock-mark-dirty-region s (1+ s))))
    edit))

(defun emacs-edit-kill-line-direct (&optional arg)
  "Kill from point to end of line and return an edit plist.
ARG is accepted for API parity; multi-line behavior is deferred like
the `kill-line' polyfill.  The result contains `:beg', `:end', `:text',
`:delete-len', `:delete-text', and `:deleted-newline'.  At end of
buffer, returns a no-op plist with nil range."
  (ignore arg)
  (let* ((start (nelisp-ec-point))
         (em (nelisp-ec-point-max))
         (eol (emacs-line--eol-pos))
         (end (cond
               ((and (= start eol) (< eol em)) (1+ eol))
               ((< start eol) eol)
               (t nil))))
    (cond
     (end
      (emacs-edit-kill-region-direct start end))
     (t
      (list :beg nil :end nil :text nil :deleted-newline nil)))))

(defun emacs-edit-kill-whole-line-direct ()
  "Kill the current line and return an edit plist.
The final line is killed without a trailing newline; other lines include
the trailing newline.  An empty buffer returns a no-op plist with
`:status' set to `empty-buffer'."
  (let* ((start (emacs-line--bol-pos))
         (eol (emacs-line--eol-pos))
         (pmax (nelisp-ec-point-max)))
    (cond
     ((= start eol pmax)
      (list :beg nil :end nil :text nil :status 'empty-buffer
            :deleted-newline nil))
     ((>= eol pmax)
      (append (emacs-edit-kill-region-direct start eol)
              (list :status 'last-line)))
     (t
      (append (emacs-edit-kill-region-direct start (1+ eol))
              (list :status 'whole-line))))))

(defun emacs-edit-transform-region-direct (start end transform)
  "Replace START..END with TRANSFORM applied to its text.
Return an edit plist compatible with frontend edit-result adapters.
TRANSFORM is called with the original string and must return a string."
  (let* ((s (min start end))
         (e (max start end))
         (old (nelisp-ec-buffer-substring s e))
         (new (funcall transform old)))
    (unless (stringp new)
      (signal 'wrong-type-argument (list 'stringp new)))
    (nelisp-ec-delete-region s e)
    (when (fboundp 'emacs-undo-record-delete)
      (emacs-undo-record-delete old s))
    (nelisp-ec-goto-char s)
    (nelisp-ec-insert new)
    (let ((new-end (nelisp-ec-point)))
      (when (fboundp 'emacs-undo-record-insert)
        (emacs-undo-record-insert s new-end))
      (when (fboundp 'emacs-font-lock-mark-dirty-region)
        (emacs-font-lock-mark-dirty-region s new-end))
      (list :beg s
            :end new-end
            :text new
            :replacement t
            :delete-len (length old)
            :delete-text old
            :deleted-newline (and (stringp old)
                                  (string-match-p "\n" old)
                                  t)))))

(defun emacs-edit-run-transform-region-command
    (mark-pos mark-buffer active-buffer-name transform label)
  "Run a region TRANSFORM command and return a command-result plist.
MARK-POS, MARK-BUFFER, and ACTIVE-BUFFER-NAME describe frontend region
state.  TRANSFORM and LABEL are passed to
`emacs-edit-transform-region-direct' and the resulting status message.
The result contains `:status', `:message', and, on success, `:edit'."
  (cond
   ((null mark-pos)
    (list :status 'no-mark
          :message "no mark set"))
   (t
    (let ((bounds (emacs-edit-region-bounds-direct
                   mark-pos mark-buffer active-buffer-name)))
      (cond
       ((null bounds)
        (list :status 'empty-region
              :message "empty region"))
       (t
        (let* ((edit (emacs-edit-transform-region-direct
                      (car bounds) (cdr bounds) transform))
               (chars (or (plist-get edit :delete-len)
                          (length (or (plist-get edit :text) "")))))
          (list :status 'transformed
                :message (format "%s: %d chars" label chars)
                :edit edit))))))))

(defun emacs-edit-delete-region-direct (start end &optional status)
  "Delete START..END without touching `kill-ring' and return an edit plist.
When STATUS is non-nil, include it as `:status'."
  (let ((s (min start end))
        (e (max start end)))
    (cond
     ((<= e s)
      (append (list :beg nil :end nil :text nil :deleted-newline nil)
              (when status (list :status status))))
     (t
      (let ((old (nelisp-ec-buffer-substring s e)))
        (nelisp-ec-delete-region s e)
        (when (fboundp 'emacs-undo-record-delete)
          (emacs-undo-record-delete old s))
        (when (fboundp 'emacs-font-lock-mark-dirty-region)
          (emacs-font-lock-mark-dirty-region s (1+ s)))
        (append (list :beg s
                      :end s
                      :text old
                      :delete-len (length old)
                      :delete-text old
                      :deleted-newline (and (stringp old)
                                            (string-match-p "\n" old)
                                            t))
                (when status (list :status status))))))))

(defun emacs-edit-line-bounds-at (pos)
  "Return `(BOL . EOL)' for the logical line containing POS."
  (let ((pmin (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max))
        (bol pos)
        (eol pos))
    (while (and (> bol pmin)
                (not (eq (emacs-edit-char-at (1- bol)) ?\n)))
      (setq bol (1- bol)))
    (while (and (< eol pmax)
                (not (eq (emacs-edit-char-at eol) ?\n)))
      (setq eol (1+ eol)))
    (cons bol eol)))

(defun emacs-edit-blank-line-at-p (pos)
  "Return non-nil when POS is on a line containing only spaces or tabs."
  (let* ((bounds (emacs-edit-line-bounds-at pos))
         (p (car bounds))
         (eol (cdr bounds))
         (blank t))
    (while (and blank (< p eol))
      (unless (memq (emacs-edit-char-at p) '(?\s ?\t))
        (setq blank nil))
      (setq p (1+ p)))
    blank))

(defun emacs-edit-empty-line-at-p (pos)
  "Return non-nil when POS is on a zero-width line."
  (let ((bounds (emacs-edit-line-bounds-at pos)))
    (= (car bounds) (cdr bounds))))

(defun emacs-edit-next-line-bol (pos)
  "Return the beginning position of the line after POS."
  (let* ((pmax (nelisp-ec-point-max))
         (eol (cdr (emacs-edit-line-bounds-at pos))))
    (if (and (< eol pmax)
             (eq (emacs-edit-char-at eol) ?\n))
        (1+ eol)
      pmax)))

(defun emacs-edit-previous-line-bol (pos)
  "Return the beginning position of the line before POS."
  (let* ((pmin (nelisp-ec-point-min))
         (bol (car (emacs-edit-line-bounds-at pos))))
    (if (<= bol pmin)
        pmin
      (car (emacs-edit-line-bounds-at (1- bol))))))

(defun emacs-edit-line-commented-p (bol eol)
  "Return non-nil when line BOL..EOL starts with `;;' after whitespace."
  (let ((text (nelisp-ec-buffer-substring bol eol))
        (idx 0))
    (while (and (< idx (length text))
                (memq (aref text idx) '(?\s ?\t)))
      (setq idx (1+ idx)))
    (and (<= (+ idx 2) (length text))
         (eq (aref text idx) ?\;)
         (eq (aref text (1+ idx)) ?\;))))

(defun emacs-edit-line-bols-in-range (beg end)
  "Return line beginning positions touched by BEG..END.
The end position is exclusive: when END is exactly at the beginning of a
line, that following line is not included."
  (let ((p (min beg end))
        (limit (max beg end))
        (bols '()))
    (while (< p limit)
      (let* ((bounds (emacs-edit-line-bounds-at p))
             (bol (car bounds))
             (eol (cdr bounds)))
        (push bol bols)
        (setq p (if (and (< eol limit)
                         (< eol (nelisp-ec-point-max)))
                    (1+ eol)
                  limit))))
    (nreverse (delete-dups bols))))

(defun emacs-edit-toggle-line-comment-direct (bol eol)
  "Toggle `;; ' line comment for BOL..EOL and return an edit plist.
This preserves the legacy GTK behavior: commenting inserts `;; ' at BOL;
uncommenting removes the first `;;' after leading whitespace and one
following space when present."
  (let ((text (nelisp-ec-buffer-substring bol eol)))
    (cond
     ((emacs-edit-line-commented-p bol eol)
      (let ((idx 0))
        (while (and (< idx (length text))
                    (memq (aref text idx) '(?\s ?\t)))
          (setq idx (1+ idx)))
        (let* ((remove-len (if (and (< (+ idx 2) (length text))
                                    (eq (aref text (+ idx 2)) ?\s))
                               3
                             2))
               (start (+ bol idx))
               (end (+ start remove-len))
               (deleted (nelisp-ec-buffer-substring start end)))
          (nelisp-ec-delete-region start end)
          (when (fboundp 'emacs-undo-record-delete)
            (emacs-undo-record-delete deleted start))
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region start (1+ start)))
          (list :beg start
                :end end
                :text deleted
                :delete-len (length deleted)
                :delete-text deleted
                :deleted-newline nil))))
     (t
      (nelisp-ec-goto-char bol)
      (nelisp-ec-insert ";; ")
      (when (fboundp 'emacs-undo-record-insert)
        (emacs-undo-record-insert bol (+ bol 3)))
      (when (fboundp 'emacs-font-lock-mark-dirty-region)
        (emacs-font-lock-mark-dirty-region bol (+ bol 3)))
      (list :beg bol
            :end (+ bol 3)
             :text ";; "
             :deleted-newline nil)))))

(defun emacs-edit-comment-dwim-direct (&optional bounds)
  "Toggle comment on the current line or every line in BOUNDS.
BOUNDS is a cons `(BEG . END)' using an exclusive END.  Return a plist
with `:status' (`line' or `region') and `:edits', the list of edit-result
plists produced by `emacs-edit-toggle-line-comment-direct'."
  (let ((edits '()))
    (cond
     (bounds
      (dolist (bol (reverse (emacs-edit-line-bols-in-range
                             (car bounds) (cdr bounds))))
        (let ((line (emacs-edit-line-bounds-at bol)))
          (push (emacs-edit-toggle-line-comment-direct
                 (car line) (cdr line))
                edits)))
      (list :status 'region
            :line-count (length edits)
            :edits (nreverse edits)))
     (t
      (let ((line (emacs-edit-line-bounds-at (nelisp-ec-point))))
        (push (emacs-edit-toggle-line-comment-direct
               (car line) (cdr line))
              edits))
      (list :status 'line
            :line-count 1
            :edits (nreverse edits))))))

(defun emacs-edit-transpose-chars-direct ()
  "Transpose adjacent chars around point and return an edit plist.
This preserves the legacy GTK behavior: at EOB, transpose the final two
characters; before the second character, do nothing."
  (let ((p (nelisp-ec-point))
        (pmin (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max)))
    (cond
     ((< p (+ pmin 2))
      (list :beg nil :end nil :text nil :deleted-newline nil))
     (t
      (when (= p pmax)
        (setq p (1- p)))
      (let* ((start (1- p))
             (end (1+ p))
             (old (nelisp-ec-buffer-substring start end))
             (new (concat (substring old 1 2)
                          (substring old 0 1))))
        (nelisp-ec-delete-region start end)
        (when (fboundp 'emacs-undo-record-delete)
          (emacs-undo-record-delete old start))
        (nelisp-ec-insert new)
        (let ((new-end (nelisp-ec-point)))
          (nelisp-ec-goto-char new-end)
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert start new-end))
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region start new-end))
          (list :beg start
                :end new-end
                :text new
                :replacement t
                :delete-len (length old)
                :delete-text old
                :deleted-newline nil)))))))

(defun emacs-edit-horizontal-whitespace-bounds-around (p)
  "Return the horizontal whitespace run touching P, or nil.
The returned cons is `(BEG . END)' using buffer positions.  Horizontal
whitespace is space or tab."
  (let* ((s (nelisp-ec-buffer-string))
         (pmin (nelisp-ec-point-min))
         (idx (- p pmin))
         (len (length s))
         (ws-p (lambda (c) (or (eq c ?\s) (eq c ?\t)))))
    (let ((b idx)
          (e idx))
      (while (and (> b 0) (funcall ws-p (aref s (1- b))))
        (setq b (1- b)))
      (while (and (< e len) (funcall ws-p (aref s e)))
        (setq e (1+ e)))
      (unless (= b e)
        (cons (+ pmin b) (+ pmin e))))))

(defun emacs-edit-delete-horizontal-space-direct (&optional point)
  "Delete horizontal whitespace touching POINT and return an edit plist.
POINT defaults to the current point.  When no whitespace touches POINT,
return a no-op plist."
  (let ((bounds (emacs-edit-horizontal-whitespace-bounds-around
                 (or point (nelisp-ec-point)))))
    (cond
     ((null bounds)
      (list :beg nil :end nil :text nil :deleted-newline nil))
     (t
      (let* ((start (car bounds))
             (end (cdr bounds))
             (old (nelisp-ec-buffer-substring start end)))
        (nelisp-ec-delete-region start end)
        (when (fboundp 'emacs-undo-record-delete)
          (emacs-undo-record-delete old start))
        (when (fboundp 'emacs-font-lock-mark-dirty-region)
          (emacs-font-lock-mark-dirty-region start (1+ start)))
        (list :beg start
              :end start
              :text old
              :delete-len (length old)
              :delete-text old
              :deleted-newline nil))))))

(defun emacs-edit-just-one-space-direct (&optional point)
  "Collapse horizontal whitespace touching POINT to one space.
POINT defaults to the current point.  Return an edit plist compatible
with frontend edit-result adapters."
  (let ((bounds (emacs-edit-horizontal-whitespace-bounds-around
                 (or point (nelisp-ec-point)))))
    (cond
     ((null bounds)
      (list :beg nil :end nil :text nil :deleted-newline nil))
     (t
      (let* ((start (car bounds))
             (end (cdr bounds))
             (old (nelisp-ec-buffer-substring start end))
             (new " "))
        (nelisp-ec-delete-region start end)
        (when (fboundp 'emacs-undo-record-delete)
          (emacs-undo-record-delete old start))
        (nelisp-ec-insert new)
        (let ((new-end (nelisp-ec-point)))
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert start new-end))
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region start new-end))
          (list :beg start
                :end new-end
                :text new
                :replacement t
                :delete-len (length old)
                :delete-text old
                :deleted-newline nil)))))))

(defun emacs-edit-delete-indentation-direct ()
  "Join the current line with the previous line and return an edit plist.
This removes the preceding newline plus leading spaces/tabs on the
current line.  It inserts one space at the join point unless the
preceding character is already a space or tab.  At BOB, return a no-op
plist with `:status' set to `bob'."
  (let* ((bol (emacs-line--bol-pos))
         (pmin (nelisp-ec-point-min)))
    (cond
     ((<= bol pmin)
      (list :beg nil :end nil :text nil :status 'bob
            :deleted-newline nil))
     (t
      (let* ((start (1- bol))
             (skip bol)
             (pmax (nelisp-ec-point-max)))
        (while (and (< skip pmax)
                    (memq (emacs-edit--char-at skip) '(?\s ?\t)))
          (setq skip (1+ skip)))
        (let* ((old (nelisp-ec-buffer-substring start skip))
               (prev (and (> start pmin)
                          (emacs-edit--char-at (1- start))))
               (insert-text (if (and prev (not (memq prev '(?\s ?\t))))
                                " "
                              "")))
          (nelisp-ec-delete-region start skip)
          (when (fboundp 'emacs-undo-record-delete)
            (emacs-undo-record-delete old start))
          (nelisp-ec-goto-char start)
          (when (> (length insert-text) 0)
            (nelisp-ec-insert insert-text))
          (let ((new-end (nelisp-ec-point)))
            (when (and (> (length insert-text) 0)
                       (fboundp 'emacs-undo-record-insert))
              (emacs-undo-record-insert start new-end))
            (when (fboundp 'emacs-font-lock-mark-dirty-region)
              (emacs-font-lock-mark-dirty-region start (max (1+ start) new-end)))
            (list :beg start
                  :end new-end
                  :text insert-text
                  :replacement t
                  :delete-len (length old)
                  :delete-text old
                  :deleted-newline t
                  :status 'joined))))))))

(defun emacs-edit-scan-forward-to-char (ch &optional limit)
  "Scan forward from point for CH up to LIMIT.
Return the position one past CH, or nil when CH is not found.  LIMIT
defaults to `point-max'.  This does not move point."
  (let ((p (nelisp-ec-point))
        (end (or limit (nelisp-ec-point-max)))
        found)
    (while (and (< p end) (null found))
      (when (eq (emacs-edit--char-at p) ch)
        (setq found (1+ p)))
      (setq p (1+ p)))
    found))

(defun emacs-edit-zap-to-char-direct (ch &optional limit)
  "Kill from point through the next CH and return an edit plist.
LIMIT defaults to `point-max'.  When CH is not found, return a no-op
plist with `:status' set to `not-found'."
  (let* ((start (nelisp-ec-point))
         (end (emacs-edit-scan-forward-to-char
               ch (or limit (nelisp-ec-point-max)))))
    (cond
     ((null end)
      (list :beg nil :end nil :text nil :target ch :status 'not-found
            :deleted-newline nil))
     (t
      (append (emacs-edit-kill-region-direct start end)
              (list :target ch :status 'killed))))))

(defun emacs-edit-sort-lines-direct (start end &optional reverse)
  "Sort lines in START..END and return an edit plist.
With REVERSE non-nil, sort in descending order.  The result preserves a
trailing newline when the input region has one and includes
`:line-count' for frontend status messages."
  (let* ((s (min start end))
         (e (max start end))
         (text (nelisp-ec-buffer-substring s e))
         (trailing-nl (and (> (length text) 0)
                           (eq (aref text (1- (length text))) ?\n)))
         (chunk (if trailing-nl
                    (substring text 0 (1- (length text)))
                  text))
         (lines (split-string chunk "\n"))
         (sorted (sort (copy-sequence lines) #'string<))
         (ordered (if reverse (nreverse sorted) sorted))
         (rejoined (concat (mapconcat #'identity ordered "\n")
                           (if trailing-nl "\n" ""))))
    (append (emacs-edit-transform-region-direct
             s e (lambda (_old) rejoined))
            (list :line-count (length lines)
                  :status 'sorted))))

(defun emacs-edit-delete-blank-lines-direct ()
  "Delete blank lines around point and return an edit plist.
On a blank line, collapse the surrounding blank run to one blank line.
On a non-blank line, delete the blank lines immediately following it."
  (let* ((pmin (nelisp-ec-point-min))
         (pmax (nelisp-ec-point-max))
         (p (nelisp-ec-point))
         (bol (car (emacs-edit-line-bounds-at p))))
    (cond
     ((emacs-edit-blank-line-at-p p)
      (let ((run-start bol)
            (run-end-bol bol))
        (catch 'done-up
          (while (> run-start pmin)
            (let ((prev-bol (1- run-start)))
              (while (and (> prev-bol pmin)
                          (not (eq (emacs-edit-char-at (1- prev-bol)) ?\n)))
                (setq prev-bol (1- prev-bol)))
              (cond
               ((emacs-edit-blank-line-at-p prev-bol)
                (setq run-start prev-bol))
               (t
                (throw 'done-up nil))))))
        (let ((next-bol bol))
          (catch 'done-down
            (while (< next-bol pmax)
              (let ((eol (cdr (emacs-edit-line-bounds-at next-bol))))
                (when (and (< eol pmax)
                           (eq (emacs-edit-char-at eol) ?\n))
                  (setq next-bol (1+ eol))
                  (cond
                   ((and (< next-bol pmax)
                         (emacs-edit-blank-line-at-p next-bol))
                    (setq run-end-bol next-bol))
                   (t
                    (throw 'done-down nil))))
                (when (= eol pmax)
                  (throw 'done-down nil))))))
        (let* ((run-end-eol (cdr (emacs-edit-line-bounds-at run-end-bol)))
               (run-end (if (and (< run-end-eol pmax)
                                  (eq (emacs-edit-char-at run-end-eol) ?\n))
                             (1+ run-end-eol)
                           run-end-eol))
               (keep-end (min run-end (1+ run-start))))
          (cond
           ((>= keep-end run-end)
            (list :beg nil :end nil :text nil :deleted-newline nil
                  :status 'nothing-to-remove))
           (t
            (let ((edit (emacs-edit-delete-region-direct
                         keep-end run-end 'deleted)))
              (nelisp-ec-goto-char run-start)
              edit))))))
     (t
      (let* ((line-end (cdr (emacs-edit-line-bounds-at bol)))
             (next-bol (if (and (< line-end pmax)
                                (eq (emacs-edit-char-at line-end) ?\n))
                           (1+ line-end)
                         line-end))
             (scan next-bol))
        (while (and (< scan pmax)
                    (emacs-edit-blank-line-at-p scan))
          (let ((eol (cdr (emacs-edit-line-bounds-at scan))))
            (setq scan (if (and (< eol pmax)
                                (eq (emacs-edit-char-at eol) ?\n))
                           (1+ eol)
                         pmax))))
        (cond
         ((> scan next-bol)
          (emacs-edit-delete-region-direct next-bol scan 'deleted))
         (t
          (list :beg nil :end nil :text nil :deleted-newline nil
                :status 'none-to-delete))))))))

(defun emacs-edit-trailing-whitespace-ranges ()
  "Return trailing space/tab deletion ranges for the current buffer.
Ranges are cons cells `(START . END)' using buffer positions.  They are
returned in descending order so callers can delete them without shifting
later ranges."
  (let* ((text (nelisp-ec-buffer-string))
         (tlen (length text))
         (i 0)
         (line-start 0)
         (ranges '()))
    (while (< i tlen)
      (let ((c (aref text i)))
        (when (or (eq c ?\n) (= i (1- tlen)))
          (let ((eol (if (eq c ?\n) i (1+ i))))
            (let ((j eol))
              (while (and (> j line-start)
                          (memq (aref text (1- j)) '(?\s ?\t)))
                (setq j (1- j)))
              (when (< j eol)
                (push (cons (1+ j) (1+ eol)) ranges)))
            (setq line-start (1+ i)))))
      (setq i (1+ i)))
    ranges))

(defun emacs-edit-delete-trailing-whitespace-direct ()
  "Delete trailing spaces/tabs in the current buffer and return a plist.
The result includes `:delete-ranges' in descending pre-edit buffer
positions, plus `:char-count' and `:line-count' for status messages.
When nothing is deleted, `:status' is `none'."
  (let ((ranges (emacs-edit-trailing-whitespace-ranges))
        (char-count 0)
        (line-count 0)
        (edits '()))
    (cond
     ((null ranges)
      (list :beg nil :end nil :text nil :deleted-newline nil
            :delete-ranges nil :char-count 0 :line-count 0
            :status 'none))
     (t
      (dolist (range ranges)
        (let* ((start (car range))
               (end (cdr range))
               (edit (emacs-edit-delete-region-direct
                      start end 'deleted-trailing-whitespace)))
          (setq char-count (+ char-count (or (plist-get edit :delete-len) 0)))
          (setq line-count (1+ line-count))
          (push edit edits)))
      (list :beg (plist-get (car (last edits)) :beg)
            :end (plist-get (car edits) :end)
            :text nil
            :deleted-newline nil
            :delete-ranges ranges
            :edits (nreverse edits)
            :char-count char-count
            :line-count line-count
            :status 'deleted)))))

(defun emacs-edit-fill-paragraph-bounds ()
  "Return `(START . END)' bounds for the paragraph around point.
Paragraphs are delimited by zero-width empty lines, preserving the
legacy GTK fill behavior."
  (let* ((pmin (nelisp-ec-point-min))
         (pmax (nelisp-ec-point-max))
         (start (nelisp-ec-point)))
    (when (emacs-edit-empty-line-at-p start)
      (setq start (emacs-edit-next-line-bol start)))
    (while (and (> start pmin)
                (not (emacs-edit-empty-line-at-p start)))
      (setq start (emacs-edit-previous-line-bol start)))
    (when (emacs-edit-empty-line-at-p start)
      (setq start (emacs-edit-next-line-bol start)))
    (let ((end start))
      (while (and (< end pmax)
                  (not (emacs-edit-empty-line-at-p end)))
        (setq end (emacs-edit-next-line-bol end)))
      (cons start end))))

(defun emacs-edit-canonicalize-fill-text (text)
  "Collapse space, tab, and newline runs in TEXT to single spaces."
  (let ((i 0)
        (n (length text))
        (canon "")
        (last-ws nil))
    (while (< i n)
      (let ((ch (aref text i)))
        (cond
         ((or (eq ch ?\s) (eq ch ?\t) (eq ch ?\n))
          (unless last-ws
            (setq canon (concat canon " "))
            (setq last-ws t)))
         (t
          (setq canon (concat canon (string ch)))
          (setq last-ws nil))))
      (setq i (1+ i)))
    (when (and (> (length canon) 0)
               (eq (aref canon (1- (length canon))) ?\s))
      (setq canon (substring canon 0 (1- (length canon)))))
    canon))

(defun emacs-edit-wrap-fill-text (text column)
  "Greedily wrap canonical TEXT at COLUMN."
  (let ((parts '())
        (col 0)
        (j 0)
        (m (length text)))
    (while (< j m)
      (while (and (< j m) (eq (aref text j) ?\s))
        (setq j (1+ j)))
      (let ((wstart j))
        (while (and (< j m) (not (eq (aref text j) ?\s)))
          (setq j (1+ j)))
        (let* ((word (substring text wstart j))
               (wlen (length word))
               (sep (if (= col 0) "" " ")))
          (cond
           ((= wlen 0))
           ((= col 0)
            (push word parts)
            (setq col wlen))
           ((<= (+ col 1 wlen) column)
            (push sep parts)
            (push word parts)
            (setq col (+ col 1 wlen)))
           (t
            (push "\n" parts)
            (push word parts)
            (setq col wlen))))))
    (apply #'concat (nreverse parts))))

(defun emacs-edit-fill-paragraph-direct (&optional column)
  "Fill the paragraph around point and return an edit plist.
COLUMN defaults to 70.  The result includes `:old-length',
`:new-length', and `:status' for frontend status messages."
  (let* ((column (or column 70))
         (bounds (emacs-edit-fill-paragraph-bounds))
         (start (car bounds))
         (end (cdr bounds)))
    (cond
     ((>= start end)
      (list :beg nil :end nil :text nil :deleted-newline nil
            :old-length 0 :new-length 0 :status 'empty))
     (t
      (let* ((text (nelisp-ec-buffer-substring start end))
             (canon (emacs-edit-canonicalize-fill-text text))
             (rebuilt (emacs-edit-wrap-fill-text canon column)))
        (append (emacs-edit-transform-region-direct
                 start end (lambda (_old) rebuilt))
                (list :old-length (length text)
                      :new-length (length rebuilt)
                      :status 'filled)))))))

(defun emacs-edit-run-fill-paragraph-command (&optional column)
  "Run `fill-paragraph' semantics and return a command-result plist.
The result contains `:status', `:message', and `:edit'."
  (let ((edit (emacs-edit-fill-paragraph-direct column)))
    (list :status (plist-get edit :status)
          :message (if (eq (plist-get edit :status) 'empty)
                       "fill-paragraph: empty"
                     (format "fill-paragraph: %d→%d chars"
                             (plist-get edit :old-length)
                             (plist-get edit :new-length)))
          :edit edit)))

(defun emacs-edit-forward-paragraph-position (&optional pos)
  "Return the forward paragraph target from POS.
This preserves the legacy GTK behavior: skip zero-width empty lines,
then advance through non-empty lines until the next empty line or EOB."
  (let ((p (or pos (nelisp-ec-point)))
        (pmax (nelisp-ec-point-max)))
    (while (and (< p pmax)
                (emacs-edit-empty-line-at-p p))
      (setq p (emacs-edit-next-line-bol p)))
    (while (and (< p pmax)
                (not (emacs-edit-empty-line-at-p p)))
      (setq p (emacs-edit-next-line-bol p)))
    p))

(defun emacs-edit-backward-paragraph-position (&optional pos)
  "Return the backward paragraph target from POS.
This preserves the legacy GTK behavior: move one line up, skip empty
lines upward, then continue upward through non-empty lines."
  (let ((p (emacs-edit-previous-line-bol (or pos (nelisp-ec-point))))
        (pmin (nelisp-ec-point-min)))
    (while (and (> p pmin)
                (emacs-edit-empty-line-at-p p))
      (setq p (emacs-edit-previous-line-bol p)))
    (while (and (> p pmin)
                (not (emacs-edit-empty-line-at-p p)))
      (setq p (emacs-edit-previous-line-bol p)))
    p))

(defun emacs-edit-set-mark-direct (&optional buffer-name)
  "Return a frontend-neutral mark plist for the current point.
BUFFER-NAME, when non-nil, is copied to `:buffer'.  The current point is
reported as `:mark' and `:point'."
  (let ((point (nelisp-ec-point)))
    (list :status 'marked
          :mark point
          :point point
          :buffer buffer-name
          :shift-region nil
          :message (format "Mark set @ %d" point))))

(defun emacs-edit-region-bounds-direct
    (mark-pos mark-buffer active-buffer-name)
  "Return active region bounds for MARK-POS in ACTIVE-BUFFER-NAME.
MARK-BUFFER must equal ACTIVE-BUFFER-NAME.  The current buffer's point is
used as the other endpoint.  Returns nil when no active, non-empty region
exists."
  (when (and mark-pos
             (equal mark-buffer active-buffer-name))
    (let ((point (nelisp-ec-point)))
      (cond
       ((= point mark-pos) nil)
       ((< mark-pos point) (cons mark-pos point))
       (t (cons point mark-pos))))))

(defun emacs-edit-shift-selection-plan (event mods &rest plist)
  "Return a frontend-neutral shift-selection state plan.
PLIST accepts `:shift-mask', `:motion-events', `:point', `:mark-pos',
`:mark-buffer', `:active-buffer-name', and `:shift-region'.  The result
contains `:action', one of `activate', `deactivate', or `none'."
  (let* ((shift-mask (plist-get plist :shift-mask))
         (motion-events (plist-get plist :motion-events))
         (point (plist-get plist :point))
         (mark-pos (plist-get plist :mark-pos))
         (mark-buffer (plist-get plist :mark-buffer))
         (active-buffer-name (plist-get plist :active-buffer-name))
         (shift-region (plist-get plist :shift-region))
         (motion-p (memq event motion-events))
         (shift-p (and shift-mask
                       (= (logand mods shift-mask) shift-mask)))
         (active-here (and mark-pos
                           (equal mark-buffer active-buffer-name))))
    (cond
     ((not motion-p)
      (list :action 'none))
     ((and shift-p (not active-here))
      (list :action 'activate
            :mark point
            :buffer active-buffer-name
            :shift-region t
            :message "Mark activated"))
     ((and (not shift-p) shift-region)
      (list :action 'deactivate))
     (t
      (list :action 'none)))))

(defun emacs-edit-mouse-drag-region-plan
    (press-point point mark-pos mark-buffer active-buffer-name)
  "Return frontend-neutral mark state for a mouse drag.
PRESS-POINT is the remembered mouse-1 press position.  POINT is the drag
target.  MARK-POS and MARK-BUFFER describe the current frontend mark.
ACTIVE-BUFFER-NAME is the current frontend buffer name."
  (cond
   ((null press-point)
    (list :status 'no-press))
   (t
    (let* ((active-here (and mark-pos
                             (equal mark-buffer active-buffer-name)))
           (mark (if active-here mark-pos press-point)))
      (list :status (if active-here 'extended 'anchored)
            :mark mark
            :buffer active-buffer-name
            :shift-region nil
            :point point
            :message (format "drag → %d..%d" mark point))))))

(defun emacs-edit-page-scroll-direct (direction viewport-height)
  "Move point by one page and return a scroll result plist.
DIRECTION is `up' or `down'.  VIEWPORT-HEIGHT is the number of text rows
available in the frontend.  The page delta matches GTK's existing rule:
`max(1, VIEWPORT-HEIGHT - 2)'."
  (let* ((delta (max 1 (- viewport-height 2)))
         (signed-delta (pcase direction
                         ('up (- delta))
                         ('down delta)
                         (_ (signal 'wrong-type-argument
                                    (list '(member up down) direction)))))
         (old-point (nelisp-ec-point))
         (new-point old-point))
    (dotimes (_ (abs signed-delta))
      (setq new-point
            (if (< signed-delta 0)
                (emacs-edit-previous-line-bol new-point)
              (emacs-edit-next-line-bol new-point))))
    (nelisp-ec-goto-char new-point)
    (list :status 'moved
          :direction direction
          :delta signed-delta
          :old-point old-point
          :point (nelisp-ec-point))))

(defun emacs-edit-mark-paragraph-bounds (&optional pos)
  "Return `(START . END)' bounds for `mark-paragraph' from POS.
When POS is on an empty line, select the preceding paragraph, matching
the existing GTK command behavior."
  (let ((p (or pos (nelisp-ec-point)))
        (pmin (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max)))
    (while (and (> p pmin)
                (emacs-edit-empty-line-at-p p))
      (setq p (emacs-edit-previous-line-bol p)))
    (while (and (> p pmin)
                (not (emacs-edit-empty-line-at-p p)))
      (setq p (emacs-edit-previous-line-bol p)))
    (when (emacs-edit-empty-line-at-p p)
      (setq p (emacs-edit-next-line-bol p)))
    (let ((start p)
          (end p))
      (while (and (< end pmax)
                  (not (emacs-edit-empty-line-at-p end)))
        (setq end (emacs-edit-next-line-bol end)))
      (cons start end))))

(defun emacs-edit-forward-paragraph-direct ()
  "Move point forward by a paragraph and return a movement plist."
  (let* ((old (nelisp-ec-point))
         (new (emacs-edit-forward-paragraph-position old)))
    (nelisp-ec-goto-char new)
    (list :old-point old :point new :status 'moved)))

(defun emacs-edit-backward-paragraph-direct ()
  "Move point backward by a paragraph and return a movement plist."
  (let* ((old (nelisp-ec-point))
         (new (emacs-edit-backward-paragraph-position old)))
    (nelisp-ec-goto-char new)
    (list :old-point old :point new :status 'moved)))

(defun emacs-edit-mark-paragraph-direct ()
  "Move point to paragraph end and return mark bounds.
The returned plist contains `:beg' for mark position and `:end' / `:point'
for the new point."
  (let* ((bounds (emacs-edit-mark-paragraph-bounds))
         (start (car bounds))
         (end (cdr bounds)))
    (nelisp-ec-goto-char end)
    (list :beg start :end end :point end :status 'marked)))

(defun emacs-edit-run-mark-paragraph-command (&optional buffer-name)
  "Run `mark-paragraph' semantics and return frontend-neutral mark state.
BUFFER-NAME, when non-nil, is copied to `:buffer'.  The result contains
`:message', `:mark', `:point', `:buffer', and `:shift-region'."
  (let ((mark (emacs-edit-mark-paragraph-direct)))
    (append mark
            (list :mark (plist-get mark :beg)
                  :buffer buffer-name
                  :shift-region nil
                  :message "Mark paragraph"))))

(defun emacs-edit-goto-buffer-boundary-direct (boundary)
  "Move point to buffer BOUNDARY and return a movement plist.
BOUNDARY must be `beginning' or `end'."
  (let* ((old (nelisp-ec-point))
         (target (pcase boundary
                   ('beginning (nelisp-ec-point-min))
                   ('end (nelisp-ec-point-max))
                   (_ (signal 'wrong-type-argument
                              (list '(member beginning end) boundary))))))
    (nelisp-ec-goto-char target)
    (list :status 'moved
          :boundary boundary
          :old-point old
          :point target)))

(defun emacs-edit-mark-whole-buffer-direct (&optional buffer-name)
  "Mark the whole buffer and return frontend-neutral mark state.
BUFFER-NAME, when non-nil, is copied to `:buffer'."
  (let ((beg (nelisp-ec-point-min))
        (end (nelisp-ec-point-max)))
    (nelisp-ec-goto-char end)
    (list :status 'marked
          :beg beg
          :end end
          :point end
          :mark beg
          :buffer buffer-name
          :shift-region nil
          :char-count (- end beg)
          :message (format "Selected whole buffer (%d chars)"
                           (- end beg)))))

(defun emacs-edit-exchange-point-and-mark-direct
    (mark-pos mark-buffer active-buffer-name)
  "Exchange point and mark using frontend mark state.
MARK-POS and MARK-BUFFER describe the current frontend mark.
ACTIVE-BUFFER-NAME is the current frontend buffer name."
  (cond
   ((or (null mark-pos)
        (not (equal mark-buffer active-buffer-name)))
    (list :status 'no-mark
          :message "exchange-point-and-mark: no mark"))
   (t
    (let ((point (nelisp-ec-point)))
      (nelisp-ec-goto-char mark-pos)
      (list :status 'exchanged
            :point mark-pos
            :mark point
            :buffer active-buffer-name
            :shift-region nil
            :message "Exchange point and mark")))))

(defun emacs-edit-beginning-of-defun-position (&optional pos)
  "Return the nearest preceding line-starting `(' from POS.
This is the substrate-MVP definition of `beginning-of-defun': any `(' at
column zero is treated as a top-level form opener.  Return nil when no
such opener exists before POS."
  (let ((pmin (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max))
        (p (or pos (nelisp-ec-point)))
        found)
    (setq p (min pmax (max pmin p)))
    (catch 'done
      (while (>= p pmin)
        (let ((bol p))
          (while (and (> bol pmin)
                      (not (eq (emacs-edit-char-at (1- bol)) ?\n)))
            (setq bol (1- bol)))
          (when (and (< bol pmax)
                     (eq (emacs-edit-char-at bol) ?\())
            (setq found bol)
            (throw 'done nil))
          (if (<= bol pmin)
              (throw 'done nil)
            (setq p (1- bol))))))
    found))

(defun emacs-edit-defun-bounds (&optional pos)
  "Return bounds plist for the top-level form before POS.
The result contains `:beg', `:end', and `:status'.  Status is `ok',
`no-top-level', or `scan-error'.  This helper restores point before
returning."
  (let ((old (nelisp-ec-point))
        (start (emacs-edit-beginning-of-defun-position pos))
        end)
    (cond
     ((null start)
      (list :beg nil :end nil :status 'no-top-level))
     (t
      (nelisp-ec-goto-char start)
      (setq end (emacs-edit-scan-sexp-forward (nelisp-ec-point-max)))
      (nelisp-ec-goto-char old)
      (cond
       ((null end)
        (list :beg start :end nil :status 'scan-error))
       (t
        (list :beg start :end end :status 'ok)))))))

(defun emacs-edit-mark-defun-direct ()
  "Move point to defun start and return mark-defun bounds plist.
The caller owns frontend mark activation.  Status is `marked',
`no-top-level', or `scan-error'."
  (let ((bounds (emacs-edit-defun-bounds)))
    (cond
     ((eq (plist-get bounds :status) 'ok)
      (nelisp-ec-goto-char (plist-get bounds :beg))
      (list :beg (plist-get bounds :beg)
            :end (plist-get bounds :end)
            :point (plist-get bounds :beg)
            :status 'marked))
     ((eq (plist-get bounds :status) 'scan-error)
      (nelisp-ec-goto-char (plist-get bounds :beg))
      (append bounds (list :point (plist-get bounds :beg))))
     (t
      bounds))))

(defun emacs-edit-narrow-to-defun-direct ()
  "Narrow to the current top-level form and return a bounds plist.
Status is `narrowed', `no-top-level', or `scan-error'.  On success,
point is left at the end of the narrowed defun, matching the former GTK
adapter behavior."
  (let ((bounds (emacs-edit-defun-bounds)))
    (cond
     ((eq (plist-get bounds :status) 'ok)
      (if (fboundp 'nelisp-ec-narrow-to-region)
          (nelisp-ec-narrow-to-region (plist-get bounds :beg)
                                      (plist-get bounds :end))
        (narrow-to-region (plist-get bounds :beg)
                          (plist-get bounds :end)))
      (nelisp-ec-goto-char (plist-get bounds :end))
      (list :beg (plist-get bounds :beg)
            :end (plist-get bounds :end)
            :point (plist-get bounds :end)
            :status 'narrowed))
     ((eq (plist-get bounds :status) 'scan-error)
      (nelisp-ec-goto-char (plist-get bounds :beg))
      (append bounds (list :point (plist-get bounds :beg))))
     (t
      bounds))))

(defun emacs-edit-count-words-in-range (beg end)
  "Return the number of ASCII word runs in BEG..END.
Words use `emacs-edit-word-char-p', matching the current edit builtins
word-motion semantics."
  (let ((p (min beg end))
        (limit (max beg end))
        (in-word nil)
        (count 0))
    (while (< p limit)
      (let ((ch (emacs-edit-char-at p)))
        (cond
         ((emacs-edit-word-char-p ch)
          (unless in-word
            (setq count (1+ count)
                  in-word t)))
         (t
          (setq in-word nil))))
      (setq p (1+ p)))
    count))

(defun emacs-edit-count-lines-in-range (beg end)
  "Return the logical line count in BEG..END.
This counts newlines, plus one for a non-empty range that does not end
with a newline."
  (let ((s (min beg end))
        (e (max beg end))
        (count 0))
    (let ((p s))
      (while (< p e)
        (when (eq (emacs-edit-char-at p) ?\n)
          (setq count (1+ count)))
        (setq p (1+ p))))
    (cond
     ((= s e) 0)
     ((eq (emacs-edit-char-at (1- e)) ?\n) count)
     (t (1+ count)))))

(defun emacs-edit-count-range (beg end)
  "Return a plist of line, word, and character counts for BEG..END."
  (let ((s (min beg end))
        (e (max beg end)))
    (list :beg s
          :end e
          :lines (emacs-edit-count-lines-in-range s e)
          :words (emacs-edit-count-words-in-range s e)
          :chars (- e s)
          :status 'counted)))

(defun emacs-edit-dabbrev-word-at-point-prefix ()
  "Return `(BEG . PREFIX)' for the word fragment before point.
Return nil when point is not adjacent to a word character."
  (let ((p (nelisp-ec-point))
        (pmin (nelisp-ec-point-min)))
    (let ((q p))
      (while (and (> q pmin)
                  (emacs-edit-word-char-p
                   (emacs-edit-char-at (1- q))))
        (setq q (1- q)))
      (unless (= q p)
        (cons q (nelisp-ec-buffer-substring q p))))))

(defun emacs-edit-dabbrev-find-completion (prefix scan-from cycled)
  "Search backward from SCAN-FROM for a dabbrev completion of PREFIX.
CYCLED is a list of words to skip.  Return `(BEG END WORD NEW-SCAN-FROM)'
on hit, or nil on miss."
  (let ((p scan-from)
        (pmin (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max))
        (plen (length prefix))
        hit)
    (while (and (> p pmin) (null hit))
      (setq p (1- p))
      (when (emacs-edit-word-char-p (emacs-edit-char-at p))
        (let ((bow p))
          (while (and (> bow pmin)
                      (emacs-edit-word-char-p
                       (emacs-edit-char-at (1- bow))))
            (setq bow (1- bow)))
          (let ((eow p))
            (while (and (< eow pmax)
                        (emacs-edit-word-char-p
                         (emacs-edit-char-at eow)))
              (setq eow (1+ eow)))
            (let ((word (nelisp-ec-buffer-substring bow eow)))
              (when (and (> (length word) plen)
                         (string= prefix (substring word 0 plen))
                         (not (member word cycled)))
                (setq hit (list bow eow word bow)))))
          (setq p bow))))
    hit))

(defun emacs-edit-dabbrev-expand-direct (start end word)
  "Replace START..END with dabbrev WORD and return an edit plist."
  (append (emacs-edit-transform-region-direct
           start end (lambda (_old) word))
          (list :word word
                :status 'expanded)))

(defun emacs-edit-current-column-in-line ()
  "Return point's zero-based column within the current line."
  (let ((p (nelisp-ec-point))
        (q (nelisp-ec-point))
        (pmin (nelisp-ec-point-min)))
    (while (and (> q pmin)
                (not (eq (emacs-edit-char-at (1- q)) ?\n)))
      (setq q (1- q)))
    (- p q)))

(defun emacs-edit-tab-to-tab-stop-direct (&optional stop-width)
  "Insert spaces to the next tab stop and return an edit plist.
STOP-WIDTH defaults to 4.  At least one space is inserted."
  (let* ((stop (or stop-width 4))
         (col (emacs-edit-current-column-in-line))
         (delta (- stop (mod col stop)))
         (n (if (= delta 0) stop delta))
         (text (make-string n ?\s)))
    (append (emacs-edit-self-insert-direct text)
            (list :columns-added n
	          :old-column col
	          :new-column (+ col n)
	          :status 'inserted))))

(defconst emacs-edit-electric-pair-default-open-pairs
  '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}) (?\" . ?\"))
  "Default opener-to-closer pairs for `emacs-edit-electric-pair-direct'.")

(defconst emacs-edit-electric-pair-default-close-set
  '(?\) ?\] ?\} ?\")
  "Default closer chars for `emacs-edit-electric-pair-direct'.")

(defun emacs-edit-electric-pair-direct (char &optional open-pairs close-set)
  "Apply electric-pair insertion for CHAR and return an edit plist.
OPEN-PAIRS is an alist mapping opener chars to closer chars.  CLOSE-SET
is the list of closer chars that should step past a matching next char.

The result has `:status' set to `paired', `skipped', or `inserted'.
Paired and inserted results include the edit range from
`emacs-edit-self-insert-direct'.  Skipped results move point only and do
not include an edit range."
  (let* ((pairs (or open-pairs emacs-edit-electric-pair-default-open-pairs))
         (closers (or close-set emacs-edit-electric-pair-default-close-set))
         (p (nelisp-ec-point))
         (pmax (nelisp-ec-point-max))
         (next (and (< p pmax) (emacs-edit-char-at p)))
         (open-pair (assq char pairs)))
    (cond
     ((and (memq char closers)
           (eq next char))
      (nelisp-ec-goto-char (1+ p))
      (list :old-point p
            :point (nelisp-ec-point)
            :char char
            :status 'skipped))
     (open-pair
      (let ((close (cdr open-pair)))
        (let ((edit (emacs-edit-self-insert-direct (string char close))))
          (nelisp-ec-goto-char (1+ p))
          (append edit
                  (list :point (nelisp-ec-point)
                        :char char
                        :close close
                        :status 'paired)))))
     (t
      (append (emacs-edit-self-insert-direct char)
              (list :point (nelisp-ec-point)
                    :char char
                    :status 'inserted))))))

(defun emacs-edit-run-electric-pair-command
    (char &optional open-pairs close-set)
  "Run electric-pair command semantics for CHAR.
OPEN-PAIRS and CLOSE-SET have the same meaning as in
`emacs-edit-electric-pair-direct'.  Return a command-result plist with
`:status', `:message', and `:edit'."
  (let* ((edit (emacs-edit-electric-pair-direct char open-pairs close-set))
         (status (plist-get edit :status))
         (message
          (cond
           ((eq status 'skipped)
            (format "electric-pair: skip %c" char))
           ((eq status 'paired)
            (format "electric-pair: %c%c" char (plist-get edit :close)))
           (t
            (format "electric-pair: %c (no match)" char)))))
    (list :status status
          :message message
          :edit edit)))

(defun emacs-edit-register-put (registers char value)
  "Return REGISTERS with CHAR set to VALUE.
The register store is an alist keyed by character."
  (cons (cons char value)
        (assq-delete-all char registers)))

(defun emacs-edit-register-value (registers char)
  "Return CHAR's value in REGISTERS, or nil when absent."
  (cdr (assq char registers)))

(defun emacs-edit-register-point-value-p (value)
  "Return non-nil when VALUE is a point-register record."
  (and (consp value)
       (eq (car value) :point)
       (stringp (nth 1 value))
       (integerp (nth 2 value))))

(defun emacs-edit-copy-to-register-direct (registers char start end)
  "Copy START..END into CHAR in REGISTERS and return a result plist.
The result includes the updated register store as `:registers'."
  (let* ((s (min start end))
         (e (max start end))
         (text (nelisp-ec-buffer-substring s e))
         (new-registers (emacs-edit-register-put registers char text)))
    (list :registers new-registers
          :char char
          :value text
          :beg s
          :end e
          :text text
          :status 'stored)))

(defun emacs-edit-insert-register-direct (registers char)
  "Insert CHAR's string register value and return an edit plist.
When CHAR is empty or stores a point record, no buffer mutation happens
and `:status' is `empty' or `position'."
  (let* ((cell (assq char registers))
         (value (cdr cell)))
    (cond
     ((null cell)
      (list :char char :status 'empty))
     ((stringp value)
      (append (emacs-edit-self-insert-direct value)
              (list :char char
                    :value value
                    :status 'inserted)))
     ((emacs-edit-register-point-value-p value)
      (list :char char
            :value value
            :status 'position))
     (t
      (list :char char
            :value value
            :status 'unsupported)))))

(defun emacs-edit-point-to-register-direct (registers char buffer-name
                                                      &optional point)
  "Store BUFFER-NAME and POINT in CHAR and return a result plist.
POINT defaults to the current point.  The updated register store is
returned as `:registers'."
  (let* ((pos (or point (nelisp-ec-point)))
         (value (list :point buffer-name pos))
         (new-registers (emacs-edit-register-put registers char value)))
    (list :registers new-registers
          :char char
          :value value
          :buffer buffer-name
          :point pos
          :status 'stored)))

(defun emacs-edit-jump-to-register-target (registers char)
  "Return CHAR's point-register target from REGISTERS as a plist.
`:status' is `point', `empty', `string', or `unsupported'."
  (let* ((cell (assq char registers))
         (value (cdr cell)))
    (cond
     ((null cell)
      (list :char char :status 'empty))
     ((emacs-edit-register-point-value-p value)
      (list :char char
            :value value
            :buffer (nth 1 value)
            :point (nth 2 value)
            :status 'point))
     ((stringp value)
      (list :char char
            :value value
            :status 'string))
     (t
      (list :char char
            :value value
            :status 'unsupported)))))

(defun emacs-edit-goto-position-direct (pos)
  "Clamp POS to the current buffer and move point there.
Return a plist with the requested and actual point."
  (let ((clamped (max (nelisp-ec-point-min)
                      (min pos (nelisp-ec-point-max)))))
    (nelisp-ec-goto-char clamped)
    (list :requested-point pos
          :point clamped
          :status 'moved)))

(defun emacs-edit-goto-register-position-direct (pos)
  "Clamp register POS to the current buffer and move point there.
Return a plist with the requested and actual point."
  (emacs-edit-goto-position-direct pos))

(when (emacs-edit-builtins--install-function-p 'transpose-chars)
  (defun transpose-chars (&optional arg)
    "MVP polyfill for `transpose-chars'."
    (interactive "p")
    (ignore arg)
    (emacs-edit-transpose-chars-direct)
    nil))

(when (emacs-edit-builtins--install-function-p 'delete-horizontal-space)
  (defun delete-horizontal-space (&optional backward-only)
    "MVP polyfill for `delete-horizontal-space'."
    (interactive "*P")
    (ignore backward-only)
    (emacs-edit-delete-horizontal-space-direct)
    nil))

(when (emacs-edit-builtins--install-function-p 'tab-to-tab-stop)
  (defun tab-to-tab-stop ()
    "MVP polyfill for `tab-to-tab-stop'."
    (interactive)
    (emacs-edit-tab-to-tab-stop-direct
     (if (and (boundp 'tab-width) (integerp tab-width) (> tab-width 0))
         tab-width
       4))
    nil))

(when (emacs-edit-builtins--install-function-p 'just-one-space)
  (defun just-one-space (&optional n)
    "MVP polyfill for `just-one-space'."
    (interactive "*p")
    (ignore n)
    (emacs-edit-just-one-space-direct)
    nil))

(when (emacs-edit-builtins--install-function-p 'delete-indentation)
  (defun delete-indentation (&optional arg)
    "MVP polyfill for `delete-indentation'."
    (interactive "*P")
    (ignore arg)
    (emacs-edit-delete-indentation-direct)
    nil))

(when (emacs-edit-builtins--install-function-p 'zap-to-char)
  (defun zap-to-char (&optional arg char)
    "MVP polyfill for `zap-to-char'."
    (interactive "p\ncZap to char: ")
    (ignore arg)
    (unless char
      (signal 'wrong-type-argument (list 'characterp char)))
    (emacs-edit-zap-to-char-direct char)
    nil))

(when (emacs-edit-builtins--install-function-p 'sort-lines)
  (defun sort-lines (&optional reverse beg end)
    "MVP polyfill for `sort-lines'."
    (interactive "P\nr")
    (emacs-edit-sort-lines-direct beg end reverse)
    nil))

(when (emacs-edit-builtins--install-function-p 'delete-blank-lines)
  (defun delete-blank-lines ()
    "MVP polyfill for `delete-blank-lines'."
    (interactive)
    (emacs-edit-delete-blank-lines-direct)
    nil))

(when (emacs-edit-builtins--install-function-p 'delete-trailing-whitespace)
  (defun delete-trailing-whitespace (&optional start end)
    "MVP polyfill for `delete-trailing-whitespace'."
    (interactive)
    (ignore start end)
    (emacs-edit-delete-trailing-whitespace-direct)
    nil))

(when (emacs-edit-builtins--install-function-p 'fill-paragraph)
  (defun fill-paragraph (&optional justify region)
    "MVP polyfill for `fill-paragraph'."
    (interactive "P")
    (ignore justify region)
    (emacs-edit-fill-paragraph-direct
     (if (boundp 'fill-column) fill-column 70))
    nil))

(when (emacs-edit-builtins--install-function-p 'forward-paragraph)
  (defun forward-paragraph (&optional arg)
    "MVP polyfill for `forward-paragraph'."
    (interactive "p")
    (let ((count (or arg 1)))
      (cond
       ((>= count 0)
        (dotimes (_ count)
          (emacs-edit-forward-paragraph-direct)))
       (t
        (dotimes (_ (- count))
          (emacs-edit-backward-paragraph-direct)))))
    nil))

(when (emacs-edit-builtins--install-function-p 'backward-paragraph)
  (defun backward-paragraph (&optional arg)
    "MVP polyfill for `backward-paragraph'."
    (interactive "p")
    (forward-paragraph (- (or arg 1)))))

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
    (emacs-edit-copy-region-direct start end)
    nil))

(when (emacs-edit-builtins--install-function-p 'kill-region)
  (defun kill-region (start end &optional region)
    "Phase E polyfill: push the START..END region to `kill-ring' AND delete it.
Track E.2: records the deleted text on `buffer-undo-list'."
    (ignore region)
    (emacs-edit-kill-region-direct start end)
    nil))

(when (emacs-edit-builtins--install-function-p 'kill-line)
  (defun kill-line (&optional arg)
    "Phase E polyfill: kill from point to end of line.
At EOL (= no chars to kill on the current line) and not at EOB,
kills the trailing `\\n' so successive `kill-line' calls collapse
the cursor toward EOB.  ARG (= multi-line variant) deferred.
Bound to C-k in `nemacs-main-keymap'."
    (interactive "P")
    (emacs-edit-kill-line-direct arg)
    nil))

(defun emacs-edit-yank-direct (&optional arg)
  "Insert a kill-ring entry at point and return an edit plist.
The result contains `:beg', `:end', `:text', and `:deleted-newline'.
ARG selects the kill-ring entry using the same MVP rules as `yank'."
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
  (let* ((idx (cond
               ((null arg) 0)
               ((integerp arg) (max 0 (- arg 1)))
               (t 0)))
         (cell kill-ring)
         entry)
    (while (and cell (> idx 0))
      (setq cell (cdr cell)
            idx (1- idx)))
    (setq entry (and cell (car cell)))
    (setq kill-ring-yank-pointer (or cell kill-ring))
    (cond
     (entry
      (let ((beg (nelisp-ec-point)))
        (nelisp-ec-insert entry)
        (let ((end (nelisp-ec-point)))
          (setq emacs-edit--last-yank-bounds (cons beg end))
          (when (fboundp 'emacs-undo-record-insert)
            (emacs-undo-record-insert beg end))
          (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region beg end))
          (list :beg beg :end end :text entry :deleted-newline nil))))
     (t
      (list :beg nil :end nil :text nil :deleted-newline nil)))))

(defun emacs-edit-mouse-yank-primary-direct (point &optional arg)
  "Move to POINT, yank, and return an edit plist.
ARG is forwarded to `emacs-edit-yank-direct'.  The result also contains
`:point', the pre-yank target point selected by the frontend."
  (unless (integerp point)
    (signal 'wrong-type-argument (list 'integerp point)))
  (nelisp-ec-goto-char point)
  (append (emacs-edit-yank-direct arg)
          (list :point point)))

(defun emacs-edit-run-mouse-yank-primary-command (&rest plist)
  "Run a frontend mouse primary paste command.
PLIST accepts `:event', `:point-function', `:current-buffer',
`:apply-function', `:status-function', and `:arg'.  EVENT is expected to
carry row/column values at indexes 2 and 3, matching the GUI mouse event
shape used by frontends."
  (let* ((event (plist-get plist :event))
         (point-function (plist-get plist :point-function))
         (current-buffer-function (plist-get plist :current-buffer))
         (apply-function (plist-get plist :apply-function))
         (status-function (plist-get plist :status-function))
         (arg (plist-get plist :arg)))
    (when event
      (let* ((row (nth 2 event))
             (col (nth 3 event))
             (point (and point-function
                         (funcall point-function row col)))
             (buffer (if current-buffer-function
                         (funcall current-buffer-function)
                       (current-buffer)))
             (result (with-current-buffer buffer
                       (emacs-edit-mouse-yank-primary-direct point arg))))
        (when apply-function
          (funcall apply-function result))
        (when status-function
          (funcall status-function
                   (format "mouse-2 yank @ point %d" point)))
        result))))

(defun emacs-edit--yank (arg)
  "Pure-Elisp body for `yank'."
  (emacs-edit-yank-direct arg)
  nil)

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

(defun emacs-edit-yank-pop-direct (&optional arg)
  "Replace the most recent yank with another kill-ring entry.
Return an edit plist containing `:beg', `:end', `:text', `:delete-len',
`:delete-text', `:replacement', and `:deleted-newline'.  `:beg' and
`:end' describe the new inserted range after replacement."
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
             (cur-idx (let ((i 0)
                            (cell ring))
                        (while (and cell (not (eq cell cur)))
                          (setq cell (cdr cell)
                                i (1+ i)))
                        (if cell i 0)))
             (new-idx (mod (+ cur-idx n) len))
             (new-cell (nthcdr new-idx ring))
             (entry (and new-cell (car new-cell)))
             (start (car bounds))
             (end (cdr bounds)))
        (when entry
          (let ((deleted (nelisp-ec-buffer-substring start end)))
            (nelisp-ec-delete-region start end)
            (when (fboundp 'emacs-undo-record-delete)
              (emacs-undo-record-delete deleted start))
            (when (fboundp 'emacs-font-lock-mark-dirty-region)
              (emacs-font-lock-mark-dirty-region start (1+ start)))
            (nelisp-ec-goto-char start)
            (nelisp-ec-insert entry)
            (let ((new-end (nelisp-ec-point)))
              (setq emacs-edit--last-yank-bounds (cons start new-end))
              (when (fboundp 'emacs-undo-record-insert)
                (emacs-undo-record-insert start new-end))
              (when (fboundp 'emacs-font-lock-mark-dirty-region)
                (emacs-font-lock-mark-dirty-region start new-end))
              (setq kill-ring-yank-pointer new-cell)
              (list :beg start
                    :end new-end
                    :text entry
                    :replacement t
                    :delete-len (length deleted)
                    :delete-text deleted
                    :deleted-newline (and (stringp deleted)
                                          (string-match-p "\n" deleted)
                                          t))))))))))

(defun emacs-edit-yank-pop-result-direct (&optional arg)
  "Run `emacs-edit-yank-pop-direct' and return a frontend result plist.
On success, the edit plist is returned with `:status' set to `ok' and
`:message' set to \"yank-pop\".  On failure, no edit is performed and
the result contains `:status' `error', `:condition', `:data', and
`:message'."
  (condition-case err
      (append (emacs-edit-yank-pop-direct arg)
              (list :status 'ok
                    :message "yank-pop"))
    (error
     (let ((reason (or (cadr err) (car err))))
       (list :status 'error
	     :condition (car err)
	     :data (cdr err)
	     :message (format "yank-pop: %s" reason))))))

(defun emacs-edit-run-yank-pop-command (&rest plist)
  "Run a frontend yank-pop command through the shared edit result API.
PLIST accepts `:current-buffer', `:arg', `:apply-function', and
`:status-function'.  The result plist from
`emacs-edit-yank-pop-result-direct' is returned."
  (let* ((current-buffer-function (plist-get plist :current-buffer))
         (arg (or (plist-get plist :arg) 1))
         (apply-function (plist-get plist :apply-function))
         (status-function (plist-get plist :status-function))
         (buffer (if current-buffer-function
                     (funcall current-buffer-function)
                   (current-buffer)))
         (result (with-current-buffer buffer
                   (emacs-edit-yank-pop-result-direct arg))))
    (when (and apply-function (eq 'ok (plist-get result :status)))
      (funcall apply-function result))
    (when status-function
      (funcall status-function (plist-get result :message)))
    result))

(defun emacs-edit--yank-pop (arg)
  "Pure-Elisp body for `yank-pop'."
  (emacs-edit-yank-pop-direct arg)
  nil)

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

(defun emacs-edit-word-char-p (ch)
  "Return non-nil when CH is an ASCII word constituent.
This is the public wrapper for `emacs-edit--word-char-p', intended for
frontends and feature modules that need the editor-core word predicate
without depending on a private helper."
  (emacs-edit--word-char-p ch))

(defun emacs-edit--char-at (pos)
  "Return the char at POS, or nil when out of buffer range."
  (let* ((bm (nelisp-ec-point-min))
         (em (nelisp-ec-point-max)))
    (when (and (>= pos bm) (< pos em))
      (let ((s (nelisp-ec-buffer-substring pos (+ pos 1))))
        (and (> (length s) 0) (aref s 0))))))

(defun emacs-edit-char-at (pos)
  "Return the character at POS, or nil when POS is outside the buffer.
This is the public wrapper for `emacs-edit--char-at', intended for
frontends and feature modules that need point-relative inspection without
depending on a private helper."
  (emacs-edit--char-at pos))

(defun emacs-edit-buffer-line-count ()
  "Return the logical line count for the current buffer.
This matches the GTK cache convention: one plus the number of newline
characters, so an empty buffer still has one logical line."
  (let ((p (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max))
        (count 1))
    (while (< p pmax)
      (when (eq (emacs-edit-char-at p) ?\n)
        (setq count (1+ count)))
      (setq p (1+ p)))
    count))

(defun emacs-edit-line-start-position-for-number (line)
  "Return the beginning position for 1-based LINE, clamped to the buffer."
  (let* ((total (emacs-edit-buffer-line-count))
         (target (max 1 (min line total)))
         (p (nelisp-ec-point-min))
         (pmax (nelisp-ec-point-max))
         (cur 1))
    (while (and (< p pmax) (< cur target))
      (when (eq (emacs-edit-char-at p) ?\n)
        (setq cur (1+ cur)))
      (setq p (1+ p)))
    (list :line target
          :total-lines total
          :point p
          :status 'found)))

(defun emacs-edit-goto-line-direct (line)
  "Move point to 1-based LINE's beginning and return a movement plist.
LINE is clamped to the buffer's logical line range.  Non-positive LINE
does not move point and returns `:status' `bad-number'."
  (cond
   ((<= line 0)
    (list :line line
          :total-lines (emacs-edit-buffer-line-count)
          :point (nelisp-ec-point)
          :status 'bad-number))
   (t
    (let ((target (emacs-edit-line-start-position-for-number line)))
      (nelisp-ec-goto-char (plist-get target :point))
      (plist-put target :status 'moved)))))

(defun emacs-edit-buffer-row-at-point (&optional pos)
  "Return the zero-based buffer row containing POS.
POS defaults to the current point.  Rows count newline characters before
POS, matching GTK's scroll-offset coordinate system."
  (let* ((target (max (nelisp-ec-point-min)
                      (min (or pos (nelisp-ec-point))
                           (nelisp-ec-point-max))))
         (p (nelisp-ec-point-min))
         (row 0))
    (while (< p target)
      (when (eq (emacs-edit-char-at p) ?\n)
        (setq row (1+ row)))
      (setq p (1+ p)))
    row))

(defun emacs-edit-buffer-row-start-position (row)
  "Return the point at the beginning of zero-based buffer ROW.
ROW is clamped at the top; rows past the end return `point-max'."
  (let ((target (max 0 row))
        (p (nelisp-ec-point-min))
        (pmax (nelisp-ec-point-max))
        (cur 0))
    (while (and (< p pmax) (< cur target))
      (when (eq (emacs-edit-char-at p) ?\n)
        (setq cur (1+ cur)))
      (setq p (1+ p)))
    p))

(defun emacs-edit-move-to-buffer-row-direct (row)
  "Move point to the beginning of zero-based buffer ROW."
  (let ((point (emacs-edit-buffer-row-start-position row)))
    (nelisp-ec-goto-char point)
    (list :row (max 0 row)
          :point point
          :status 'moved)))

(defun emacs-edit-window-line-target-row (scroll rows state)
  "Return target-row metadata for `move-to-window-line-top-bottom'.
SCROLL is the zero-based first visible buffer row, ROWS is viewport
height, and STATE is the 0/1/2 top-middle-bottom cycle state."
  (let* ((label (cond
                 ((= state 0) "top")
                 ((= state 1) "middle")
                 (t "bottom")))
         (row (cond
               ((= state 0) scroll)
               ((= state 1) (+ scroll (/ rows 2)))
               (t (+ scroll (max 0 (- rows 1)))))))
    (list :row row
          :label label
          :next-state (mod (1+ state) 3)
          :status 'target)))

(defun emacs-edit-recenter-scroll-offset (row height)
  "Return a non-negative scroll offset centering ROW in HEIGHT."
  (max 0 (- row (/ height 2))))

(defun emacs-edit-word-bounds-at (pos)
  "Return `(BEG . END)' for the word containing POS, or nil.
Word constituents use `emacs-edit-word-char-p'."
  (cond
   ((not (emacs-edit-word-char-p (emacs-edit-char-at pos))) nil)
   (t
    (let ((beg pos)
          (end pos)
          (pmin (nelisp-ec-point-min))
          (pmax (nelisp-ec-point-max)))
      (while (and (> beg pmin)
                  (emacs-edit-word-char-p (emacs-edit-char-at (1- beg))))
        (setq beg (1- beg)))
      (while (and (< end pmax)
                  (emacs-edit-word-char-p (emacs-edit-char-at end)))
        (setq end (1+ end)))
      (cons beg end)))))

(defun emacs-edit-select-word-at-direct (pos)
  "Select the word at POS by moving point to word end.
Return a mark plist with `:beg', `:end', `:point', and `:status'.  When
POS is not on a word constituent, do not move point and return
`:status' `not-word'."
  (let ((bounds (emacs-edit-word-bounds-at pos)))
    (cond
     ((null bounds)
      (list :beg nil
            :end nil
            :point (nelisp-ec-point)
            :status 'not-word))
     (t
      (nelisp-ec-goto-char (cdr bounds))
      (list :beg (car bounds)
            :end (cdr bounds)
            :point (cdr bounds)
            :status 'selected)))))

(defun emacs-edit-run-select-word-at-command (pos &optional buffer-name)
  "Run mouse word selection at POS and return frontend mark state.
BUFFER-NAME, when non-nil, is copied to `:buffer'."
  (let ((mark (emacs-edit-select-word-at-direct pos)))
    (cond
     ((eq (plist-get mark :status) 'selected)
      (append mark
              (list :mark (plist-get mark :beg)
                    :buffer buffer-name
                    :shift-region nil
                    :message (format "Selected word (%d chars)"
                                     (- (plist-get mark :end)
                                        (plist-get mark :beg))))))
     (t
      (append mark
              (list :message "double-click: no word at point"))))))

(defun emacs-edit-select-line-at-direct (pos)
  "Select the line containing POS by moving point to line end.
Return a mark plist with `:beg', `:end', `:point', and `:status'."
  (let ((bounds (emacs-edit-line-bounds-at pos)))
    (nelisp-ec-goto-char (cdr bounds))
    (list :beg (car bounds)
          :end (cdr bounds)
          :point (cdr bounds)
          :status 'selected)))

(defun emacs-edit-run-select-line-at-command (pos &optional buffer-name)
  "Run mouse line selection at POS and return frontend mark state.
BUFFER-NAME, when non-nil, is copied to `:buffer'."
  (let ((mark (emacs-edit-select-line-at-direct pos)))
    (append mark
            (list :mark (plist-get mark :beg)
                  :buffer buffer-name
                  :shift-region nil
                  :message (format "Selected line (%d chars)"
                                   (- (plist-get mark :end)
                                      (plist-get mark :beg)))))))

(defun emacs-edit-forward-word-position (pos &optional arg)
  "Return the point reached by moving from POS across ARG ASCII words.
ARG defaults to 1.  This does not move point and uses the same ASCII
word predicate as the `forward-word' polyfill."
  (let* ((count (or arg 1))
         (sign (if (>= count 0) 1 -1))
         (n (abs count))
         (p pos))
    (cond
     ((= sign 1)
      (while (> n 0)
        (let ((em (nelisp-ec-point-max)))
          (while (and (< p em)
                      (not (emacs-edit--word-char-p
                            (emacs-edit--char-at p))))
            (setq p (1+ p)))
          (while (and (< p em)
                      (emacs-edit--word-char-p
                       (emacs-edit--char-at p)))
            (setq p (1+ p))))
        (setq n (1- n))))
     (t
      (while (> n 0)
        (let ((bm (nelisp-ec-point-min)))
          (while (and (> p bm)
                      (not (emacs-edit--word-char-p
                            (emacs-edit--char-at (1- p)))))
            (setq p (1- p)))
          (while (and (> p bm)
                      (emacs-edit--word-char-p
                       (emacs-edit--char-at (1- p))))
            (setq p (1- p))))
        (setq n (1- n)))))
    p))

(defun emacs-edit-kill-word-direct (&optional arg)
  "Kill ARG words from point and return an edit plist.
ARG defaults to 1.  Positive ARG kills forward; negative ARG kills
backward.  A zero-width movement returns a no-op plist."
  (let* ((start (nelisp-ec-point))
         (end (emacs-edit-forward-word-position start (or arg 1)))
         (beg (min start end))
         (finish (max start end)))
    (cond
     ((= beg finish)
      (list :beg nil :end nil :text nil :deleted-newline nil))
     (t
      (nelisp-ec-goto-char beg)
      (emacs-edit-kill-region-direct beg finish)))))

(defun emacs-edit-sexp-symbol-char-p (ch)
  "Return non-nil when CH is part of a Lisp-style symbol token."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (memq ch '(?- ?_ ?: ?+ ?* ?/ ?< ?> ?= ?? ?! ?& ?~ ?@ ?. ?$ ?%)))))

(defun emacs-edit-sexp-skip-forward-ws (pmax)
  "Advance point past whitespace and `;'-line-comments up to PMAX.
Return the new point."
  (let ((p (nelisp-ec-point)))
    (catch 'done
      (while (< p pmax)
        (let ((ch (emacs-edit--char-at p)))
          (cond
           ((memq ch '(?\s ?\t ?\n)) (setq p (1+ p)))
           ((eq ch ?\;)
            (while (and (< p pmax)
                        (not (eq (emacs-edit--char-at p) ?\n)))
              (setq p (1+ p))))
           (t (throw 'done nil))))))
    (nelisp-ec-goto-char (min p pmax))
    (nelisp-ec-point)))

(defun emacs-edit-sexp-skip-backward-ws (pmin)
  "Step point backward over plain whitespace down to PMIN.
Comments are not skipped on the reverse pass."
  (let ((p (nelisp-ec-point)))
    (while (and (> p pmin)
                (memq (emacs-edit--char-at (1- p)) '(?\s ?\t ?\n)))
      (setq p (1- p)))
    (nelisp-ec-goto-char (max p pmin))
    (nelisp-ec-point)))

(defun emacs-edit-scan-sexp-forward (pmax)
  "Parse one balanced sexp forward from point up to PMAX.
Move point to the parsed end and return it.  Return nil and leave point
unchanged on unmatched delimiter or unterminated string."
  (let* ((start (nelisp-ec-point))
         (ch (and (< start pmax) (emacs-edit--char-at start))))
    (cond
     ((null ch) nil)
     ((memq ch '(?\( ?\[ ?\{))
      (let* ((close (cdr (assq ch '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))))
             (depth 1)
             (p (1+ start))
             found)
        (catch 'done
          (while (< p pmax)
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ch) (setq depth (1+ depth)))
               ((eq c close)
                (setq depth (1- depth))
                (when (zerop depth)
                  (setq found (1+ p))
                  (throw 'done nil)))
               ((eq c ?\")
                (setq p (1+ p))
                (while (and (< p pmax)
                            (not (eq (emacs-edit--char-at p) ?\")))
                  (when (eq (emacs-edit--char-at p) ?\\)
                    (setq p (1+ p)))
                  (setq p (1+ p))))
               ((eq c ?\;)
                (while (and (< p pmax)
                            (not (eq (emacs-edit--char-at p) ?\n)))
                  (setq p (1+ p))))))
            (setq p (1+ p))))
        (when found
          (nelisp-ec-goto-char found)
          found)))
     ((eq ch ?\")
      (let ((p (1+ start))
            found)
        (catch 'done
          (while (< p pmax)
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ?\\) (setq p (+ p 2)))
               ((eq c ?\") (setq found (1+ p)) (throw 'done nil))
               (t (setq p (1+ p)))))))
        (when found
          (nelisp-ec-goto-char found)
          found)))
     ((emacs-edit-sexp-symbol-char-p ch)
      (let ((p start))
        (while (and (< p pmax)
                    (emacs-edit-sexp-symbol-char-p
                     (emacs-edit--char-at p)))
          (setq p (1+ p)))
        (nelisp-ec-goto-char p)
        p))
     (t
      (nelisp-ec-goto-char (1+ start))
      (1+ start)))))

(defun emacs-edit-scan-sexp-backward (pmin)
  "Parse one balanced sexp backward from point down to PMIN.
Move point to the parsed start and return it.  Return nil on unmatched
delimiter scan failure."
  (let* ((end (nelisp-ec-point))
         (ch (and (> end pmin) (emacs-edit--char-at (1- end)))))
    (cond
     ((null ch) nil)
     ((memq ch '(?\) ?\] ?\}))
      (let* ((open (cdr (assq ch '((?\) . ?\() (?\] . ?\[) (?\} . ?\{)))))
             (depth 1)
             (p (1- end))
             found)
        (catch 'done
          (while (> p pmin)
            (setq p (1- p))
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ch) (setq depth (1+ depth)))
               ((eq c open)
                (setq depth (1- depth))
                (when (zerop depth)
                  (setq found p)
                  (throw 'done nil)))))))
        (when found
          (nelisp-ec-goto-char found)
          found)))
     ((eq ch ?\")
      (let ((p (- end 2))
            found)
        (catch 'done
          (while (>= p pmin)
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ?\") (setq found p) (throw 'done nil))
               (t (setq p (1- p)))))))
        (when found
          (nelisp-ec-goto-char found)
          found)))
     ((emacs-edit-sexp-symbol-char-p ch)
      (let ((p (1- end)))
        (while (and (> p pmin)
                    (emacs-edit-sexp-symbol-char-p
                     (emacs-edit--char-at (1- p))))
          (setq p (1- p)))
        (nelisp-ec-goto-char p)
        p))
     (t
      (nelisp-ec-goto-char (1- end))
      (1- end)))))

(defun emacs-edit-matching-paren-position-direct ()
  "Return matching paren position near point without changing point.
When point is on an opener, return the matching closer position.  When
point is just after a closer, return the matching opener position.  Return
nil when no matching paren applies."
  (let* ((saved (nelisp-ec-point))
         (pmax (nelisp-ec-point-max))
         (pmin (nelisp-ec-point-min))
         (ch-after (and (< saved pmax) (emacs-edit-char-at saved)))
         (ch-before (and (> saved pmin) (emacs-edit-char-at (1- saved))))
         match)
    (unwind-protect
        (setq match
              (cond
               ((memq ch-after '(?\( ?\[ ?\{))
                (let ((res (emacs-edit-scan-sexp-forward pmax)))
                  (and res (1- res))))
               ((memq ch-before '(?\) ?\] ?\}))
                (emacs-edit-scan-sexp-backward pmin))
               (t nil)))
      (nelisp-ec-goto-char saved))
    match))

(defun emacs-edit-kill-sexp-direct ()
  "Kill the sexp following point and return an edit plist.
Leading whitespace and `;'-line-comments are skipped.  On scan failure,
restore point and return a no-op plist with `:status' set to
`scan-error'."
  (let* ((pmax (nelisp-ec-point-max))
         (start (nelisp-ec-point)))
    (emacs-edit-sexp-skip-forward-ws pmax)
    (let* ((scan-start (nelisp-ec-point))
           (end (emacs-edit-scan-sexp-forward pmax)))
      (cond
       ((null end)
        (nelisp-ec-goto-char start)
        (list :beg nil :end nil :text nil :status 'scan-error
              :deleted-newline nil))
       (t
        (nelisp-ec-goto-char scan-start)
        (append (emacs-edit-kill-region-direct scan-start end)
                (list :status 'killed)))))))

(defun emacs-edit-forward-sexp-direct (&optional arg)
  "Move forward across ARG sexps and return a motion plist.
ARG defaults to 1.  The result contains `:point' and `:status'.  On scan
failure, point remains after the leading whitespace/comment skip and
`:status' is `scan-error'."
  (let ((n (or arg 1))
        (point (nelisp-ec-point))
        (status 'moved))
    (cond
     ((< n 0)
      (emacs-edit-backward-sexp-direct (- n)))
     (t
      (while (and (> n 0) (eq status 'moved))
        (emacs-edit-sexp-skip-forward-ws (nelisp-ec-point-max))
        (let ((res (emacs-edit-scan-sexp-forward (nelisp-ec-point-max))))
          (cond
           (res (setq point res
                      n (1- n)))
           (t
            (setq point (nelisp-ec-point)
                  status 'scan-error)))))
      (list :point point :status status)))))

(defun emacs-edit-backward-sexp-direct (&optional arg)
  "Move backward across ARG sexps and return a motion plist.
ARG defaults to 1.  The result contains `:point' and `:status'.  On scan
failure, point remains after the trailing whitespace skip and `:status'
is `scan-error'."
  (let ((n (or arg 1))
        (point (nelisp-ec-point))
        (status 'moved))
    (cond
     ((< n 0)
      (emacs-edit-forward-sexp-direct (- n)))
     (t
      (while (and (> n 0) (eq status 'moved))
        (emacs-edit-sexp-skip-backward-ws (nelisp-ec-point-min))
        (let ((res (emacs-edit-scan-sexp-backward (nelisp-ec-point-min))))
          (cond
           (res (setq point res
                      n (1- n)))
           (t
            (setq point (nelisp-ec-point)
                  status 'scan-error)))))
      (list :point point :status status)))))

(defun emacs-edit-sentence-end-char-p (ch)
  "Return non-nil when CH is a sentence-ending punctuation char."
  (memq ch '(?. ?! ??)))

(defun emacs-edit-forward-sentence-position (pos)
  "Return the position after the sentence ending at or after POS.
When no sentence terminator is found, return `point-max'."
  (let ((p pos)
        (pmax (nelisp-ec-point-max))
        found)
    (catch 'done
      (while (< p pmax)
        (let ((ch (emacs-edit--char-at p)))
          (when (emacs-edit-sentence-end-char-p ch)
            (setq p (1+ p))
            (while (and (< p pmax)
                        (memq (emacs-edit--char-at p)
                              '(?\" ?\) ?\] ?\} ?\')))
              (setq p (1+ p)))
            (setq found p)
            (throw 'done nil)))
        (setq p (1+ p))))
    (or found pmax)))

(defun emacs-edit--forward-sentence-motion-target (pos)
  "Return `(TARGET . FOUND)' for sentence motion from POS.
TARGET skips trailing closing delimiters and whitespace when a sentence
terminator is found.  FOUND is nil only when scanning reached
`point-max' without finding a sentence terminator."
  (let ((p pos)
        (pmax (nelisp-ec-point-max))
        found)
    (catch 'done
      (while (< p pmax)
        (let ((ch (emacs-edit--char-at p)))
          (when (emacs-edit-sentence-end-char-p ch)
            (setq p (1+ p))
            (while (and (< p pmax)
                        (memq (emacs-edit--char-at p)
                              '(?\" ?\) ?\] ?\} ?\')))
              (setq p (1+ p)))
            (while (and (< p pmax)
                        (memq (emacs-edit--char-at p)
                              '(?\s ?\t ?\n)))
              (setq p (1+ p)))
            (setq found p)
            (throw 'done nil)))
        (setq p (1+ p))))
    (cons (or found pmax) (and found t))))

(defun emacs-edit-forward-sentence-motion-position (pos)
  "Return the frontend motion target after the sentence from POS.
Unlike `emacs-edit-forward-sentence-position', this skips the whitespace
after the terminator so motion lands at the beginning of the next
sentence.  When no terminator is found, return `point-max'."
  (car (emacs-edit--forward-sentence-motion-target pos)))

(defun emacs-edit-backward-sentence-position (pos)
  "Return the start position of the sentence ending before POS.
This mirrors the legacy GTK scanner: trailing whitespace before POS is
skipped, then the previous sentence terminator plus following whitespace
and closing delimiters forms the boundary.  If none is found, return
`point-min'."
  (let ((end pos)
        (p pos)
        (pmin (nelisp-ec-point-min))
        found)
    (when (and (> p pmin)
               (memq (emacs-edit--char-at (1- p)) '(?\s ?\t ?\n)))
      (while (and (> p pmin)
                  (memq (emacs-edit--char-at (1- p)) '(?\s ?\t ?\n)))
        (setq p (1- p))))
    (catch 'done
      (while (> p pmin)
        (setq p (1- p))
        (when (emacs-edit-sentence-end-char-p (emacs-edit--char-at p))
          (setq found (1+ p))
          (while (and (< found end)
                      (memq (emacs-edit--char-at found)
                            '(?\s ?\t ?\n ?\" ?\) ?\] ?\} ?\')))
            (setq found (1+ found)))
          (throw 'done nil))))
    (or found pmin)))

(defun emacs-edit-forward-sentence-direct (&optional arg)
  "Move forward across ARG sentences and return a motion plist.
The result contains `:old-point', `:point', and `:status'.  `:status'
is `moved' when a sentence boundary was crossed, or `eob' when motion
lands at `point-max' without finding another sentence terminator."
  (let ((n (or arg 1))
        (status 'moved)
        (old (nelisp-ec-point))
        point)
    (cond
     ((< n 0)
      (emacs-edit-backward-sentence-direct (- n)))
     (t
      (while (> n 0)
        (let* ((start (nelisp-ec-point))
               (target-state
                (emacs-edit--forward-sentence-motion-target start))
               (target (car target-state)))
          (setq point target)
          (when (not (cdr target-state))
            (setq status 'eob))
          (nelisp-ec-goto-char target)
          (setq n (1- n))))
      (list :old-point old :point (or point old) :status status)))))

(defun emacs-edit-backward-sentence-direct (&optional arg)
  "Move backward across ARG sentences and return a motion plist.
The result contains `:old-point', `:point', and `:status'.  `:status'
is `moved' when a sentence boundary was crossed, or `bob' when motion
lands at `point-min' without finding an earlier sentence boundary."
  (let ((n (or arg 1))
        (status 'moved)
        (old (nelisp-ec-point))
        point)
    (cond
     ((< n 0)
      (emacs-edit-forward-sentence-direct (- n)))
     (t
      (while (> n 0)
        (let* ((start (nelisp-ec-point))
               (target (emacs-edit-backward-sentence-position start)))
          (setq point target)
          (when (= target (nelisp-ec-point-min))
            (setq status 'bob))
          (nelisp-ec-goto-char target)
          (setq n (1- n))))
      (list :old-point old :point (or point old) :status status)))))

(defun emacs-edit-kill-sentence-direct ()
  "Kill from point to the end of the current sentence.
Return an edit plist.  A zero-width range returns a no-op plist with
`:status' set to `empty'."
  (let* ((start (nelisp-ec-point))
         (end (emacs-edit-forward-sentence-position start)))
    (cond
     ((= end start)
      (list :beg nil :end nil :text nil :status 'empty
            :deleted-newline nil))
     (t
      (append (emacs-edit-kill-region-direct start end)
              (list :status 'killed))))))

(defun emacs-edit-backward-kill-sentence-direct ()
  "Kill from the start of the current sentence to point.
Return an edit plist.  A zero-width range returns a no-op plist with
`:status' set to `empty'."
  (let* ((end (nelisp-ec-point))
         (start (emacs-edit-backward-sentence-position end)))
    (cond
     ((= start end)
      (list :beg nil :end nil :text nil :status 'empty
            :deleted-newline nil))
     (t
      (nelisp-ec-goto-char start)
      (append (emacs-edit-kill-region-direct start end)
              (list :status 'killed))))))

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

;;;; --- skip-chars set scanning --------------------------------------

;; Reusable core for `skip-chars-forward' / `skip-chars-backward'
;; (src/syntax.c:skip_chars).  The set spec is parsed into inclusive
;; codepoint ranges plus a negate flag, then membership is tested while
;; stepping over the nelisp-ec substrate.  This is the editor-core owner
;; for the Doc 06 MISSING skip-chars primitives; the emacs-core-delegation
;; cfront MCF0-W2 probe modeled the same charset semantics as evidence.

(defun emacs-edit--skip-chars-parse (string)
  "Parse a skip-chars STRING spec into (NEGATE . RANGES).
RANGES is a list of (LO . HI) inclusive codepoint ranges.  Supports a
leading `^' negation, `A-Z' style ranges, backslash quoting of the next
character, and literal characters.  POSIX `[:class:]' forms are not yet
modeled."
  (let ((chars (append string nil))
        (negate nil)
        (ranges nil))
    (when (and (cdr chars) (eq (car chars) ?^))
      (setq negate t
            chars (cdr chars)))
    (while chars
      (let ((c (car chars)))
        (cond
         ((and (eq c ?\\) (cdr chars))
          (push (cons (cadr chars) (cadr chars)) ranges)
          (setq chars (cddr chars)))
         ((and (cddr chars) (eq (cadr chars) ?-) (not (eq c ?-)))
          (push (cons c (nth 2 chars)) ranges)
          (setq chars (nthcdr 3 chars)))
         (t
          (push (cons c c) ranges)
          (setq chars (cdr chars))))))
    (cons negate (nreverse ranges))))

(defun emacs-edit--skip-chars-member-p (ch parsed)
  "Return non-nil when CH is in the PARSED skip-chars set.
PARSED is the (NEGATE . RANGES) value from `emacs-edit--skip-chars-parse'."
  (let ((in nil))
    (dolist (r (cdr parsed))
      (when (and (>= ch (car r)) (<= ch (cdr r)))
        (setq in t)))
    (if (car parsed) (not in) in)))

(when (emacs-edit-builtins--install-function-p 'skip-chars-forward)
  (defun skip-chars-forward (string &optional lim)
    "Phase E polyfill: move point forward across characters matching STRING.
STRING uses the `skip-chars' set-spec subset modeled by
`emacs-edit--skip-chars-parse' (leading `^' negation, `A-Z' ranges,
backslash quoting, and literals; POSIX `[:class:]' is not yet modeled).
Point stops before the first non-matching character, but never moves past
LIM.  Return the (non-negative) distance traveled."
    (let* ((parsed (emacs-edit--skip-chars-parse string))
           (start (nelisp-ec-point))
           (end (or lim (nelisp-ec-point-max)))
           (p start))
      (while (and (< p end)
                  (let ((ch (emacs-edit--char-at p)))
                    (and ch (emacs-edit--skip-chars-member-p ch parsed))))
        (setq p (+ p 1)))
      (nelisp-ec-goto-char p)
      (- p start))))

(when (emacs-edit-builtins--install-function-p 'skip-chars-backward)
  (defun skip-chars-backward (string &optional lim)
    "Phase E polyfill: move point backward across characters matching STRING.
See `skip-chars-forward' for the supported STRING set-spec.  Point stops
just after the first non-matching character scanning back, but never moves
past LIM.  Return the (non-positive) distance traveled."
    (let* ((parsed (emacs-edit--skip-chars-parse string))
           (start (nelisp-ec-point))
           (begin (or lim (nelisp-ec-point-min)))
           (p start))
      (while (and (> p begin)
                  (let ((ch (emacs-edit--char-at (- p 1))))
                    (and ch (emacs-edit--skip-chars-member-p ch parsed))))
        (setq p (- p 1)))
      (nelisp-ec-goto-char p)
      (- p start))))

(when (emacs-edit-builtins--install-function-p 'kill-word)
  (defun kill-word (&optional arg)
    "MVP polyfill: kill ARG ASCII words forward from point."
    (interactive "p")
    (emacs-edit-kill-word-direct arg)
    nil))

(when (emacs-edit-builtins--install-function-p 'backward-kill-word)
  (defun backward-kill-word (&optional arg)
    "MVP polyfill: kill ARG ASCII words backward from point."
    (interactive "p")
    (emacs-edit-kill-word-direct (- (or arg 1)))
    nil))

(when (emacs-edit-builtins--install-function-p 'kill-sentence)
  (defun kill-sentence (&optional arg)
    "MVP polyfill: kill ARG sentences forward from point."
    (interactive "p")
    (let ((n (or arg 1)))
      (while (> n 0)
        (emacs-edit-kill-sentence-direct)
        (setq n (1- n))))
    nil))

(when (emacs-edit-builtins--install-function-p 'backward-kill-sentence)
  (defun backward-kill-sentence (&optional arg)
    "MVP polyfill: kill ARG sentences backward from point."
    (interactive "p")
    (let ((n (or arg 1)))
      (while (> n 0)
        (emacs-edit-backward-kill-sentence-direct)
        (setq n (1- n))))
    nil))

(when (emacs-edit-builtins--install-function-p 'forward-sentence)
  (defun forward-sentence (&optional arg)
    "MVP polyfill: move forward across ARG sentences."
    (interactive "p")
    (plist-get (emacs-edit-forward-sentence-direct arg) :point)))

(when (emacs-edit-builtins--install-function-p 'backward-sentence)
  (defun backward-sentence (&optional arg)
    "MVP polyfill: move backward across ARG sentences."
    (interactive "p")
    (plist-get (emacs-edit-backward-sentence-direct arg) :point)))

(when (emacs-edit-builtins--install-function-p 'forward-sexp)
  (defun forward-sexp (&optional arg)
    "MVP polyfill: move forward across ARG sexps."
    (interactive "p")
    (let ((edit (emacs-edit-forward-sexp-direct arg)))
      (when (eq (plist-get edit :status) 'scan-error)
        (signal 'scan-error '("Unbalanced parentheses")))
      (plist-get edit :point))))

(when (emacs-edit-builtins--install-function-p 'backward-sexp)
  (defun backward-sexp (&optional arg)
    "MVP polyfill: move backward across ARG sexps."
    (interactive "p")
    (let ((edit (emacs-edit-backward-sexp-direct arg)))
      (when (eq (plist-get edit :status) 'scan-error)
        (signal 'scan-error '("Unbalanced parentheses")))
      (plist-get edit :point))))

(when (emacs-edit-builtins--install-function-p 'kill-sexp)
  (defun kill-sexp (&optional arg)
    "MVP polyfill: kill ARG sexps forward from point."
    (interactive "p")
    (let ((n (or arg 1)))
      (while (> n 0)
        (let ((edit (emacs-edit-kill-sexp-direct)))
          (when (eq (plist-get edit :status) 'scan-error)
            (signal 'scan-error '("Unbalanced parentheses"))))
        (setq n (1- n))))
    nil))

(provide 'emacs-edit-builtins)

;;; emacs-edit-builtins.el ends here
