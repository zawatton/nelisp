;;; nemacs-main.el --- nemacs interactive runner / batch entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track N (2026-05-03) — Layer 2 production entry point.
;;
;; `nemacs-main' is the glue that turns the per-track substrates
;; into a runnable nemacs.  Layered on top of `nemacs-loadup':
;;
;;   1. (nemacs-init) handles the bootstrap (= scratch buffer +
;;      fundamental-mode + standalone init + startup-hook).
;;   2. `nemacs-main--realise-tui' brings up emacs-tui-backend +
;;      emacs-redisplay (= Track G) and binds the active handle.
;;   3. `nemacs-main--initial-paint' draws the scratch buffer's
;;      contents + a one-line status bar to the frame.
;;   4. `nemacs-main--event-loop' drains TUI events through
;;      `command-execute' (= Track B/C) until quit fires.
;;
;; Two driver entry points are exposed:
;;   - `nemacs-main' (= the interactive runner)
;;   - `nemacs-batch-main' (= the --batch equivalent)
;;
;; Both honour `nemacs-main-options' (a plist set by the
;; `bin/nemacs' shell launcher before loading this file) so the
;; same single entry covers `nemacs', `nemacs --batch', `nemacs
;; -l FILE', and `nemacs --eval FORM'.

;;; Code:

(require 'nemacs-loadup)

;;;; --- options surface ---------------------------------------------

(defvar nemacs-main-options nil
  "Plist of runtime options set by `bin/nemacs' before loading
this file.  Recognised keys:

  :batch       t when running in batch mode (= no TUI, no event loop)
  :load        a list of file paths to `load' after bootstrap
  :eval-forms  a list of sexps to evaluate after `:load'
  :no-banner   t to suppress the ready banner
  :driver      symbol describing the driver (= host or nelisp);
               purely informational, used by `nemacs-status-banner'.")

(defun nemacs-main-option (key &optional default)
  "Return the value of KEY in `nemacs-main-options', or DEFAULT."
  (or (plist-get nemacs-main-options key) default))

;;;; --- TUI realisation ----------------------------------------------

(defvar nemacs-main--backend nil
  "The current emacs-tui-backend handle (= nil before realise).")
(defvar nemacs-main--frame nil
  "The current TUI frame object.")
(defvar nemacs-main--redisplay nil
  "The current emacs-redisplay handle.")
(defvar nemacs-main--event-handle nil
  "Doc 51 (2026-05-04) — the `emacs-tui-event' source handle.
Separate from `nemacs-main--backend' (= the canvas backend);
this one owns the stdin-byte-stream → key-event parser.  The
event loop drains it via `emacs-tui-event-poll', which in turn
pumps `emacs-tui-event-input-fn' (= our
`nemacs-main--stdin-byte-fn').")

(defun nemacs-main--realise-tui ()
  "Bring up the TUI backend + redisplay engine for interactive use.

Sets the four state variables (`nemacs-main--backend', `--frame',
`--redisplay'), wires the redisplay current-handle slot (= Track G
bridge), and binds `*scratch*' into the selected window.  Returns
the redisplay handle on success, nil if the TUI side failed (= the
fallback path runs nemacs read-eval batch-style)."
  (when (and (fboundp 'emacs-tui-backend-init)
             (fboundp 'emacs-redisplay-init))
    (condition-case err
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "main"))
               (h  (emacs-redisplay-init (list :backend bk))))
          (setq nemacs-main--backend bk
                nemacs-main--frame fr
                nemacs-main--redisplay h)
          ;; Doc 51 (2026-05-04) — bring up the parallel
          ;; emacs-tui-event handle.  Its job: drain
          ;; `emacs-tui-event-input-fn' (= our stdin reader) and
          ;; turn raw bytes into key events.  Without this the
          ;; backend's event-poll only sees in-process injections
          ;; (= no actual keyboard input under the nelisp driver).
          (when (fboundp 'emacs-tui-event-init)
            (setq nemacs-main--event-handle (emacs-tui-event-init)))
          (when (fboundp 'emacs-redisplay-set-current-handle)
            (emacs-redisplay-set-current-handle h))
          ;; Bind scratch into the selected window so the first
          ;; redisplay pass has something to paint.
          (let ((w (and (fboundp 'emacs-window-selected-window)
                        (emacs-window-selected-window)))
                (b (cdr (assoc "*scratch*" nelisp-ec--buffers))))
            (when (and w b (fboundp 'emacs-window-set-window-buffer))
              (emacs-window-set-window-buffer w b)))
          h)
      (error
       (when (fboundp 'message)
         (message "nemacs: TUI realise failed: %S" err))
       nil))))

(defun nemacs-main--shutdown-tui ()
  "Tear down the TUI subsystem realised by `nemacs-main--realise-tui'."
  (when (and nemacs-main--event-handle
             (fboundp 'emacs-tui-event-shutdown))
    (condition-case _
        (emacs-tui-event-shutdown nemacs-main--event-handle)
      (error nil)))
  (when (and nemacs-main--backend
             (fboundp 'emacs-tui-backend-shutdown))
    (condition-case _
        (emacs-tui-backend-shutdown nemacs-main--backend)
      (error nil)))
  (setq nemacs-main--backend nil
        nemacs-main--frame nil
        nemacs-main--redisplay nil
        nemacs-main--event-handle nil)
  (when (fboundp 'emacs-redisplay-set-current-handle)
    (emacs-redisplay-set-current-handle nil))
  nil)

;;;; --- initial paint ------------------------------------------------

(defun nemacs-main--initial-paint ()
  "Run the first redisplay pass + flush so something appears.
Tolerant of missing pieces — any failure is logged but does not
abort the boot."
  (condition-case err
      (when (and nemacs-main--redisplay nemacs-main--frame
                 (fboundp 'emacs-redisplay-redisplay-window)
                 (fboundp 'emacs-redisplay-flush-frame))
        (let ((w (and (fboundp 'emacs-window-selected-window)
                      (emacs-window-selected-window))))
          (when w
            (emacs-redisplay-redisplay-window nemacs-main--redisplay w))
          (emacs-redisplay-flush-frame nemacs-main--redisplay
                                       nemacs-main--frame)))
    (error
     (when (fboundp 'message)
       (message "nemacs: initial paint failed: %S" err)))))

;;;; --- keymap (Doc 51 Track C) ------------------------------------------
;;
;; nemacs's own command keymap.  Bound here so consumer code can
;; rebind cleanly without touching the host Emacs global map and so
;; the nelisp driver path has a single place to look.  Track C MVP:
;;
;;   C-x C-c     nemacs-kill           — graceful exit
;;   C-c C-q     nemacs-kill           — short-form alternative
;;   C-g         keyboard-quit         — abort current key sequence
;;
;; Under host driver this map is installed as
;; `overriding-terminal-local-map' so it takes precedence over any
;; mode map that happens to be active.  Under nelisp driver the
;; `nemacs-main--event-loop' threads raw keys through it directly.

(defvar nemacs-main--global-keymap nil
  "Top-level nemacs keymap.  See `nemacs-main--init-keymap'.")

(defun nemacs-main-kill (&optional exit-code)
  "Quit nemacs gracefully.
Under host Emacs this calls `kill-emacs'; under the nelisp driver it
sets `nemacs-main--quit-flag' so the event loop unwinds normally.
EXIT-CODE defaults to 0."
  (interactive)
  (setq nemacs-main--quit-flag t)
  (cond
   ((fboundp 'kill-emacs)
    (kill-emacs (or exit-code 0)))
   (t
    (when (fboundp 'message)
      (message "nemacs: quit (exit %S)" (or exit-code 0))))))

(defun nemacs-main--init-keymap ()
  "Construct `nemacs-main--global-keymap' if not yet built.
Idempotent — safe to call multiple times.  Returns the keymap.

Doc 51 Track A (2026-05-04): bind ASCII printable + RET to
self-insert-command / newline so a freshly booted nemacs is
typeable.  C-x C-c / C-c C-q / C-g remain the kill / quit
keys."
  (unless nemacs-main--global-keymap
    (let ((m (cond
              ((fboundp 'make-sparse-keymap) (make-sparse-keymap))
              ((fboundp 'emacs-keymap-make-keymap)
               (emacs-keymap-make-keymap))
              (t (list 'keymap)))))
      (when (fboundp 'define-key)
        ;; Top-level commands.
        (define-key m (kbd "C-x C-c") 'nemacs-main-kill)
        (define-key m (kbd "C-c C-q") 'nemacs-main-kill)
        (when (fboundp 'keyboard-quit)
          (define-key m (kbd "C-g") 'keyboard-quit))
        ;; ASCII printable → self-insert-command.  We bind the
        ;; integer key directly (= what nemacs-main--key-event->key
        ;; produces for a bare ASCII char with no modifier).
        ;; Range 32..126 = SPC..~  inclusive.
        (when (fboundp 'self-insert-command)
          (let ((c 32))
            (while (<= c 126)
              (define-key m (vector c) 'self-insert-command)
              (setq c (1+ c)))))
        ;; Newline (= byte 13 = RET in raw mode).
        (when (fboundp 'newline)
          (define-key m (vector 13) 'newline))
        ;; Doc 51 Track B (2026-05-04) — motion + delete.
        (when (fboundp 'forward-char)
          (define-key m (kbd "C-f") 'forward-char))
        (when (fboundp 'backward-char)
          (define-key m (kbd "C-b") 'backward-char))
        (when (fboundp 'next-line)
          (define-key m (kbd "C-n") 'next-line))
        (when (fboundp 'previous-line)
          (define-key m (kbd "C-p") 'previous-line))
        (when (fboundp 'beginning-of-line)
          (define-key m (kbd "C-a") 'beginning-of-line))
        (when (fboundp 'end-of-line)
          (define-key m (kbd "C-e") 'end-of-line))
        (when (fboundp 'delete-char)
          (define-key m (kbd "C-d") 'delete-char))
        (when (fboundp 'kill-line)
          (define-key m (kbd "C-k") 'kill-line))
        (when (fboundp 'delete-backward-char)
          ;; DEL (= byte 127) and Ctrl+H both surface as the symbol
          ;; `backspace' through `emacs-tui-event--control-char-name'.
          (define-key m (vector 'backspace) 'delete-backward-char)
          ;; Bare byte 127 in case the symbol mapping is bypassed.
          (define-key m (vector 127) 'delete-backward-char))
        ;; Doc 51 Track C — file open / save.
        (define-key m (kbd "C-x C-f") 'nemacs-main-find-file-interactive)
        (define-key m (kbd "C-x C-s") 'nemacs-main-save-buffer-interactive))
      (setq nemacs-main--global-keymap m)))
  nemacs-main--global-keymap)

(defun nemacs-main--install-keymap-host ()
  "Install `nemacs-main--global-keymap' as the host Emacs override.
On host driver (= interactive Emacs) this lets us own `C-x C-c'
without disturbing the user's global map.  The override is bound
via `overriding-terminal-local-map' so it persists across mode
switches; callers should clear it from `nemacs-main--shutdown-tui'."
  (when (and (not noninteractive)
             (boundp 'overriding-terminal-local-map))
    (let ((m (nemacs-main--init-keymap)))
      ;; Inherit from the existing terminal map so vanilla bindings
      ;; (cursor motion, self-insert) still work.
      (when (and (fboundp 'set-keymap-parent)
                 (fboundp 'current-global-map))
        (set-keymap-parent m (current-global-map)))
      (set 'overriding-terminal-local-map m))))

(defun nemacs-main--uninstall-keymap-host ()
  "Reverse of `nemacs-main--install-keymap-host'."
  (when (boundp 'overriding-terminal-local-map)
    (set 'overriding-terminal-local-map nil)))

;;;; --- nelisp driver TTY wiring (Track E) ----------------------------
;;
;; Under the nelisp driver `bin/nemacs' (no args) needs raw TTY input
;; or every keypress is line-buffered by the kernel.  We expose three
;; NeLisp builtins (Track E, 2026-05-04):
;;
;;   (terminal-raw-mode-enter)     -> t / nil
;;   (terminal-raw-mode-leave)     -> t / nil
;;   (read-stdin-byte-available
;;        &optional TIMEOUT-MS)    -> integer / nil
;;
;; and plumb them through `emacs-tui-event-input-fn'.  Under host
;; driver these builtins are not bound — nemacs-main--install-keymap-host
;; is the active path and stdin is owned by host Emacs's command loop.
;; So everything below is gated on `(fboundp 'terminal-raw-mode-enter)'
;; or runs only under the nelisp driver branch.

(defvar nemacs-main--tty-raw-active nil
  "Non-nil when `terminal-raw-mode-enter' has been called from us.
Used by `nemacs-main--shutdown-tui' to decide whether to leave raw
mode (= we don't restore termios we didn't put into raw)." )

(defun nemacs-main--stdin-byte-fn ()
  "Input-fn for `emacs-tui-event-input-fn' (Track E).
Returns the next available byte (integer 0..255) or nil when no
input is queued.  TIMEOUT-MS=0 = pure non-blocking; the outer
event-poll's TIMEOUT-MS handles wait-for-input on idle."
  (when (fboundp 'read-stdin-byte-available)
    (read-stdin-byte-available 0)))

(defun nemacs-main--enable-tty-raw-input ()
  "Install raw-mode + the stdin reader under the nelisp driver.
Returns t on success, nil if the builtins are unavailable."
  (when (and (fboundp 'terminal-raw-mode-enter)
             (fboundp 'read-stdin-byte-available)
             (boundp 'emacs-tui-event-input-fn))
    (condition-case err
        (progn
          (terminal-raw-mode-enter)
          (setq nemacs-main--tty-raw-active t)
          (setq emacs-tui-event-input-fn 'nemacs-main--stdin-byte-fn)
          t)
      (error
       (when (fboundp 'message)
         (message "nemacs: TTY raw setup failed: %S" err))
       nil))))

(defun nemacs-main--disable-tty-raw-input ()
  "Reverse of `nemacs-main--enable-tty-raw-input'."
  (when (and nemacs-main--tty-raw-active
             (fboundp 'terminal-raw-mode-leave))
    (condition-case _
        (terminal-raw-mode-leave)
      (error nil))
    (setq nemacs-main--tty-raw-active nil))
  (when (boundp 'emacs-tui-event-input-fn)
    (setq emacs-tui-event-input-fn nil)))

;;;; --- event loop ---------------------------------------------------

(defvar nemacs-main--quit-flag nil
  "Set by the event loop's quit handler; checked each iteration.")

(defun nemacs-main--quit ()
  "Mark the event loop for termination.  Returns t."
  (setq nemacs-main--quit-flag t))

(defvar nemacs-main--prefix-keys []
  "Accumulated key prefix for multi-key sequences (= e.g. C-x C-c).
A vector of integer keys cleared after every successful keymap
lookup.  Single-key bindings clear it on each press; prefix-key
bindings keep it growing.")

(defun nemacs-main--key-event->key (ev)
  "Translate a tui-event-key plist EV into a keymap-lookup key.

Returns one of:
  - integer        plain ASCII char or symbol-as-int from :name
  - integer + bit  C-X chord (= `(logior CHAR (ash 1 26))')
  - symbol         function key (= `up', `backspace', `f1', …)
  - the plist itself  fallback for shapes we don't recognise

The plist shape from `emacs-tui-event' uses `:name' / `:modifiers'
(= `:name' is integer for ASCII, symbol for function keys).  The
test fixtures use the older `:char' / `:mods' aliases.  Both are
accepted."
  (let* ((char (or (plist-get ev :char)
                   (let ((n (plist-get ev :name)))
                     (and (integerp n) n))))
         (sym  (let ((n (plist-get ev :name)))
                 (and (symbolp n) (not (null n)) n)))
         (mods (or (plist-get ev :mods)
                   (plist-get ev :modifiers))))
    (cond
     ((and char (memq 'control mods))
      ;; control bit per upstream Emacs ASCII C- chord encoding.
      (logior char (lsh 1 26)))
     (char char)
     (sym sym)
     (t ev))))

(defun nemacs-main--lookup-key-vec (vec)
  "Look up VEC in `nemacs-main--global-keymap'.
Returns the binding (= a command symbol / keymap / nil)."
  (when (and nemacs-main--global-keymap
             (fboundp 'lookup-key))
    (lookup-key nemacs-main--global-keymap vec)))

(defun nemacs-main--dispatch-key-event (ev)
  "Process a single key EV (= tui-event-poll plist) through the keymap.
Accumulates EV into `nemacs-main--prefix-keys', looks the result up
in `nemacs-main--global-keymap', and:
  - Runs the command via `command-execute' on a non-keymap binding,
    then clears the prefix.
  - Keeps the prefix growing on a keymap binding (= prefix key).
  - Clears the prefix on an unbound sequence (= give up gracefully)."
  (let* ((key (nemacs-main--key-event->key ev))
         (next-vec (vconcat nemacs-main--prefix-keys (vector key)))
         (binding (nemacs-main--lookup-key-vec next-vec)))
    (cond
     ;; Prefix key — keep accumulating.
     ((and binding
           (or (and (fboundp 'keymapp) (keymapp binding))
               (and (fboundp 'emacs-keymap-keymapp)
                    (emacs-keymap-keymapp binding))))
      (setq nemacs-main--prefix-keys next-vec))
     ;; Bound command — execute + reset.
     ((and binding (fboundp 'command-execute))
      (setq nemacs-main--prefix-keys [])
      ;; Doc 51 Track A — `self-insert-command' looks at
      ;; `last-command-event' to know which char to insert.
      ;; Set it from the key event we just dispatched on.  Real
      ;; tui-event puts the char in :name as an integer; the test
      ;; fixtures use :char.  Accept both shapes.
      (let* ((c (or (and (consp ev) (plist-get ev :char))
                    (let ((n (and (consp ev) (plist-get ev :name))))
                      (and (integerp n) n)))))
        (when (and c (boundp 'last-command-event))
          (setq last-command-event c)))
      (condition-case _
          (command-execute binding)
        (quit (nemacs-main--quit))))
     ;; No binding — reset and ignore (= upstream "<key> is undefined").
     (t
      (setq nemacs-main--prefix-keys [])))))

(defun nemacs-main--handle-winsize ()
  "Doc 51 Track P — react to a pending SIGWINCH.
If the resize-pending flag is set, query the controlling tty's
current size, propagate to the frame, and force a redraw.  Safe
to call when the builtins are not bound (= host driver) — returns
nil silently.

`nemacs-main--frame' is an `emacs-tui-backend-frame', so we route
through `emacs-tui-backend-frame-resize' (= the TUI-side resize)
not `emacs-frame-set-frame-size' (= the higher-level frame
abstraction's resize, which expects an `emacs-frame')."
  (when (and (fboundp 'terminal-take-winsize-changed)
             (terminal-take-winsize-changed))
    (when (fboundp 'terminal-current-winsize)
      (let ((sz (terminal-current-winsize)))
        (when (and sz (consp sz)
                   (integerp (car sz)) (integerp (cdr sz))
                   nemacs-main--frame nemacs-main--backend
                   (fboundp 'emacs-tui-backend-frame-resize))
          (condition-case err
              (emacs-tui-backend-frame-resize nemacs-main--backend
                                              nemacs-main--frame
                                              (car sz) (cdr sz))
            (error
             (when (fboundp 'message)
               (message "nemacs: SIGWINCH resize failed: %S" err)))))))))

(defun nemacs-main--handle-sigcont ()
  "Doc 51 Track Q — react to a SIGCONT (= just-resumed-from-suspend).
The TSTP handler dropped raw mode before suspending; on resume we
re-enter raw mode + force a full redraw.  Safe under host driver."
  (when (and (fboundp 'terminal-take-sigcont)
             (terminal-take-sigcont))
    (when (fboundp 'nemacs-main--enable-tty-raw-input)
      (nemacs-main--enable-tty-raw-input))
    ;; Treat resume as an implicit resize too — terminal geometry
    ;; could have changed while we were suspended.
    (when (and (fboundp 'terminal-current-winsize)
               (fboundp 'emacs-tui-backend-frame-resize)
               nemacs-main--frame nemacs-main--backend)
      (let ((sz (terminal-current-winsize)))
        (when (and sz (consp sz)
                   (integerp (car sz)) (integerp (cdr sz)))
          (condition-case _
              (emacs-tui-backend-frame-resize nemacs-main--backend
                                              nemacs-main--frame
                                              (car sz) (cdr sz))
            (error nil)))))))

;;;; --- minibuffer-style line read (Doc 51 Track C) ----------------------

(defun nemacs-main--read-line-blocking (prompt)
  "Doc 51 Track C (2026-05-04) — block-read a line via TUI canvas.

Paints PROMPT at the bottom row of `nemacs-main--frame', echoes
each key as the user types, supports backspace, and returns the
typed string on RET (= byte 13).  Returns nil on C-g (= byte 7).
Blocks the event loop while reading — no other commands fire.

This is intentionally a minimal `read-from-minibuffer'-replacement
(= the full minibuffer machinery is too heavy for the boot path).
Used by `nemacs-main-find-file-interactive'."
  (let* ((h nemacs-main--backend)
         (f nemacs-main--frame)
         (height (and f (fboundp 'emacs-tui-backend-frame-height)
                      (emacs-tui-backend-frame-height f)))
         (width  (and f (fboundp 'emacs-tui-backend-frame-width)
                      (emacs-tui-backend-frame-width f)))
         (row    (and height (1- height)))
         (input  "")
         (done   nil)
         (cancel nil))
    (cl-flet ((repaint
               ()
               (when (and h f row width
                          (fboundp 'emacs-tui-backend-canvas-draw-text))
                 (let* ((line (concat prompt input))
                        (clipped (if (> (length line) width)
                                     (substring line 0 width)
                                   line))
                        ;; Pad with spaces to clear stale chars.
                        (pad-len (- width (length clipped)))
                        (full (concat clipped
                                      (if (> pad-len 0)
                                          (make-string pad-len ?\s)
                                        ""))))
                   (emacs-tui-backend-canvas-draw-text h f row 0 full)
                   (when (fboundp 'emacs-redisplay-flush-frame)
                     (emacs-redisplay-flush-frame nemacs-main--redisplay f))))))
      (repaint)
      (while (not done)
        (let ((b (and (fboundp 'read-stdin-byte-available)
                      (read-stdin-byte-available 100))))
          (when b
            (cond
             ((= b 13) (setq done t))                             ; RET
             ((= b 7)  (setq cancel t done t))                    ; C-g
             ((or (= b 127) (= b 8))                              ; BS / DEL
              (when (> (length input) 0)
                (setq input (substring input 0 (1- (length input))))
                (repaint)))
             ((and (>= b 32) (<= b 126))
              (setq input (concat input (string b)))
              (repaint))))))
      (if cancel nil input))))

(defun nemacs-main-find-file-interactive ()
  "Doc 51 Track C — prompt for a path and visit it via `find-file'."
  (interactive)
  (let ((path (nemacs-main--read-line-blocking "Find file: ")))
    (when (and path (> (length path) 0)
               (fboundp 'find-file))
      (condition-case err
          (find-file path)
        (error
         (when (fboundp 'message)
           (message "find-file failed: %S" err)))))))

(defun nemacs-main-save-buffer-interactive ()
  "Doc 51 Track C — save the current buffer via `save-buffer'.
If the buffer has no associated file, prompt for one via
`write-file' instead."
  (interactive)
  (let* ((b (and (fboundp 'nelisp-ec-current-buffer)
                 (nelisp-ec-current-buffer)))
         (f (and b (fboundp 'buffer-file-name) (buffer-file-name b))))
    (cond
     (f
      (condition-case err
          (when (fboundp 'save-buffer) (save-buffer))
        (error
         (when (fboundp 'message)
           (message "save-buffer failed: %S" err)))))
     (t
      (let ((path (nemacs-main--read-line-blocking "Write file: ")))
        (when (and path (> (length path) 0)
                   (fboundp 'write-file))
          (condition-case err
              (write-file path)
            (error
             (when (fboundp 'message)
               (message "write-file failed: %S" err))))))))))

(defun nemacs-main--drain-once (timeout-ms)
  "Pull one event and dispatch it.  Returns t when an event ran, nil
on idle.  Honours TIMEOUT-MS (= caller's poll budget).

Doc 51 (2026-05-04): polls the `emacs-tui-event' handle FIRST
(= drains stdin → key events) and falls back to the backend's
in-process queue for any test-injected events.  Without this,
stdin under the nelisp driver was never consumed — bytes piled
up in the kernel buffer until program exit."
  (let ((ev nil))
    ;; 1) stdin → key events via the tui-event handle.
    (when (and nemacs-main--event-handle
               (fboundp 'emacs-tui-event-poll))
      (setq ev (emacs-tui-event-poll nemacs-main--event-handle timeout-ms)))
    ;; 2) Fallback: in-process backend queue (= test injection).
    (when (and (null ev)
               nemacs-main--backend
               (fboundp 'emacs-tui-backend-event-poll))
      ;; Use 0 timeout so we don't double-wait — the tui-event poll
      ;; above already waited the budget when its queue was empty.
      (setq ev (emacs-tui-backend-event-poll nemacs-main--backend 0)))
    (cond
     ((null ev) nil)
     ;; tui-event key plist (= raw stdin byte translated to a key).
     ;; Route through the keymap dispatcher.
     ((and (consp ev) (eq (plist-get ev :type) 'key))
      (nemacs-main--dispatch-key-event ev)
      t)
     ;; Convention: event vector / list whose head matches a
     ;; bound-symbol command runs through `command-execute'.
     ((and (or (vectorp ev) (consp ev))
           (fboundp 'command-execute))
      (condition-case _
          (cond
           ((vectorp ev)
            (let ((cmd (aref ev 0)))
              (when (and (symbolp cmd) (commandp cmd))
                (command-execute cmd))))
           ((symbolp (car ev))
            (let ((cmd (car ev)))
              (when (commandp cmd) (command-execute cmd)))))
        (quit (nemacs-main--quit)))
      t)
     (t
      ;; Unknown event shape: log + continue.
      (when (fboundp 'message)
        (message "nemacs: ignoring event %S" ev))
      t))))

(defun nemacs-main--event-loop ()
  "Run the interactive event loop until the quit flag fires.

Returns nil immediately when no TUI backend is realised (= the
caller forgot `nemacs-main--realise-tui', or the realise step
failed) so that test fixtures + batch fallbacks do not spin
forever.  Honours `nemacs-main--quit-flag' if the caller has
pre-set it to t (= early-out before the first poll)."
  (cond
   ((null nemacs-main--backend) nil)
   (t
    ;; Doc 51 (2026-05-04) — point `emacs-tui-event-input-fn' at our
    ;; stdin reader IF raw mode is active (= the Track E install
    ;; path).  Without an active `--enable-tty-raw-input', the
    ;; tui-event handle has nothing to drain; the backend's
    ;; in-process queue still works for test injection.
    (when (and (fboundp 'nemacs-main--stdin-byte-fn)
               (boundp 'emacs-tui-event-input-fn)
               (null emacs-tui-event-input-fn))
      (setq emacs-tui-event-input-fn 'nemacs-main--stdin-byte-fn))
    (let ((budget-ms 50))
      (while (not nemacs-main--quit-flag)
        ;; Doc 51 Track P/Q — pick up signal-handler flags BEFORE
        ;; polling for input.  Resize first so the upcoming poll
        ;; uses the new geometry.
        (condition-case e1 (nemacs-main--handle-sigcont)
          (error (message "loop: sigcont err: %S" e1)))
        (condition-case e2 (nemacs-main--handle-winsize)
          (error (message "loop: winsize err: %S" e2)))
        (condition-case e3 (nemacs-main--drain-once budget-ms)
          (error (message "loop: drain err: %S" e3)))
        ;; Doc 51 Track S — re-fontify any dirty interval that the
        ;; just-dispatched edit recorded.  Cheap when nothing is
        ;; dirty (= early-exit on nil).
        (when (fboundp 'emacs-font-lock-flush-pending)
          (condition-case e4 (emacs-font-lock-flush-pending)
            (error (message "loop: flush-pending err: %S" e4))))
        ;; Doc 51 Track A — re-paint the canvas from buffer state
        ;; AFTER input dispatch, so `self-insert-command' /
        ;; `delete-backward-char' / etc. show up on the next flush.
        ;; The flush below pushes the canvas to the terminal.
        (when (and nemacs-main--redisplay nemacs-main--frame
                   (fboundp 'emacs-redisplay-redisplay-window))
          (let ((w (and (fboundp 'emacs-window-selected-window)
                        (emacs-window-selected-window))))
            (when w
              (condition-case e5
                  (emacs-redisplay-redisplay-window
                   nemacs-main--redisplay w)
                (error (message "loop: redisplay err: %S" e5))))))
        ;; Refresh the painted state after every dispatched event.
        (when (and nemacs-main--redisplay nemacs-main--frame
                   (fboundp 'emacs-redisplay-flush-frame))
          (condition-case e6
              (emacs-redisplay-flush-frame nemacs-main--redisplay
                                           nemacs-main--frame)
            (error (message "loop: flush err: %S" e6)))))))))

;;;; --- option-driven preload ----------------------------------------

(defun nemacs-main--apply-options ()
  "Honour `nemacs-main-options' (= -l files + --eval forms)."
  (dolist (path (nemacs-main-option :load))
    (when (fboundp 'load)
      (condition-case err
          (load path nil 'no-message 'no-suffix)
        (error
         (when (fboundp 'message)
           (message "nemacs: load %S failed: %S" path err))))))
  (dolist (form (nemacs-main-option :eval-forms))
    (condition-case err
        (eval form t)
      (error
       (when (fboundp 'message)
         (message "nemacs: --eval failed: %S form=%S" err form))))))

(defun nemacs-main-status-banner ()
  "Return a one-line status string suitable for the bottom of the screen."
  (let* ((ver (and (boundp 'nemacs-version) nemacs-version))
         (drv (or (nemacs-main-option :driver) 'host))
         (fc  (and (boundp 'features) (length features))))
    (format "-- nemacs %s [%s driver] features=%d -- C-x C-c quit --"
            (or ver "?") drv (or fc 0))))

;;;; --- entry points -------------------------------------------------

(defun nemacs-batch-main ()
  "--batch entry: bootstrap, run -l / --eval, exit.
Returns the exit-code symbol (= `ok' on success)."
  ;; Doc 51 Track M (2026-05-04) — install SIGINT → quit-flag handler
  ;; so a long batch eval can be interrupted with Ctrl+C without
  ;; killing the process abruptly.  Idempotent + no-op on non-Unix.
  (when (fboundp 'install-sigint-handler)
    (install-sigint-handler))
  (unless nemacs-initialized
    (nemacs-init t))
  (nemacs-main--apply-options)
  (unless (nemacs-main-option :no-banner)
    (when (fboundp 'message)
      (message "%s" (nemacs-main-status-banner))))
  'ok)

(defun nemacs-main ()
  "Interactive nemacs runner.

Boots the substrate, brings up the TUI, paints the initial frame,
then runs the event loop.  Returns the exit-code symbol when the
loop exits cleanly.

Under host Emacs (= interactive driver) this installs
`nemacs-main--global-keymap' as `overriding-terminal-local-map'
so `C-x C-c' / `C-c C-q' route to `nemacs-kill', and then defers
the read loop to host Emacs's command loop (= no nested loop).
Under nelisp driver the substrate's own `nemacs-main--event-loop'
takes over and dispatches TUI events directly."
  ;; Doc 51 Track M (2026-05-04) — install SIGINT → quit-flag handler
  ;; before we enter any long-running eval (= batch or interactive).
  ;; This is what makes Ctrl+C interrupt the evaluator instead of
  ;; killing the process.  The builtin is a no-op on non-Unix and
  ;; idempotent, so guarding by `fboundp' is the only condition.
  (when (fboundp 'install-sigint-handler)
    (install-sigint-handler))
  ;; Doc 51 Track P/Q (2026-05-04) — install SIGWINCH and
  ;; SIGTSTP/SIGCONT handlers.  All three are no-op on non-Unix
  ;; and idempotent.  The event loop polls the resulting flags.
  (when (fboundp 'install-winsize-handler)
    (install-winsize-handler))
  (when (fboundp 'install-jobctrl-handlers)
    (install-jobctrl-handlers))
  (cond
   ((nemacs-main-option :batch)
    (nemacs-batch-main))
   (t
    (unless nemacs-initialized
      (nemacs-init))
    (nemacs-main--apply-options)
    (nemacs-main--init-keymap)
    (let ((tui-ok (nemacs-main--realise-tui))
          (driver (or (nemacs-main-option :driver) 'host)))
      (unwind-protect
          (cond
           (tui-ok
            (nemacs-main--initial-paint)
            ;; Banner before yielding control.
            (unless (nemacs-main-option :no-banner)
              (when (fboundp 'message)
                (message "%s" (nemacs-main-status-banner))))
            (cond
             ;; Host driver + interactive: install keymap + return,
             ;; let host Emacs's main loop drive.  Don't enter our
             ;; own event loop (= would nest two read-event drains).
             ((and (eq driver 'host)
                   (boundp 'noninteractive)
                   (not noninteractive))
              (nemacs-main--install-keymap-host)
              'ok)
             (t
              ;; nelisp driver / non-interactive: drive our own loop.
              ;; Try to enable raw TTY input when the builtins are
              ;; available (= nelisp driver).  Failure leaves the
              ;; loop running on whatever the backend's event-queue
              ;; injected (= test mode, no real TTY needed).
              (nemacs-main--enable-tty-raw-input)
              (nemacs-main--event-loop)
              'ok)))
           (t
            ;; TUI unavailable → fall through to batch semantics.
            (when (fboundp 'message)
              (message "nemacs: TUI not available, running batch-style"))
            (nemacs-batch-main)))
        ;; Clean up only the per-process state that we installed.
        ;; Note: under interactive host driver we do NOT shutdown TUI
        ;; here, because shutdown happens when the user runs C-x C-c
        ;; → nemacs-kill → kill-emacs → at-exit hooks.
        (nemacs-main--disable-tty-raw-input)
        (when (or (nemacs-main-option :batch)
                  (and (boundp 'noninteractive) noninteractive))
          (nemacs-main--shutdown-tui)))))))

(provide 'nemacs-main)

;;; nemacs-main.el ends here
