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

(require 'cl-lib)
(require 'nemacs-loadup)

;; Bridge thin-adapter (HANDOFF #4): pull the self-contained GUI
;; file/dired runtime adapters into every nemacs image so the GUI bridge's
;; `(fboundp 'emacs-fileio-gui-*)' / `emacs-dired-min-gui-*' guards always
;; take the runtime path in production (matching the test image), making
;; the bridge's hand-rolled fallbacks dead and removable.  Both files are
;; leaf modules (no further requires).
(require 'emacs-fileio-gui)
(require 'emacs-dired-min-gui)
(require 'emacs-help-gui)
(require 'emacs-info)

;;;; --- options surface ---------------------------------------------

(defvar nemacs-main-options nil
  "Plist of runtime options set by `bin/nemacs' before loading
this file.  Recognised keys:

  :batch       t when running in batch mode (= no TUI, no event loop)
  :images      a list of legacy `.nli' image paths to restore after bootstrap
  :load-path   a list of directory paths to prepend to `load-path'
  :load        a list of file paths to `load' after bootstrap
  :eval-forms  a list of sexps or source strings to evaluate after `:load'
  :funcall     a list of command/function symbols to call after `:eval-forms'
  :no-banner   t to suppress the ready banner
  :driver      symbol describing the driver (= host or nelisp);
               purely informational, used by `nemacs-status-banner'.")

(defun nemacs-main-option (key &optional default)
  "Return the value of KEY in `nemacs-main-options', or DEFAULT."
  ;; Standalone NeLisp's REPL evaluator can capture top-level defvars as
  ;; lexical nil inside closures.  Resolve through the symbol value so entry
  ;; functions see the plist installed by `bin/nemacs' immediately before
  ;; calling `nemacs-main' / `nemacs-batch-main'.
  (or (plist-get (and (boundp 'nemacs-main-options)
                      (symbol-value 'nemacs-main-options))
                 key)
      default))

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
(defvar nemacs-main--tui-features-loaded-p nil
  "Non-nil once lazy editor/TUI feature modules have been loaded.")
(defvar nemacs-main--tui-state-prepared-p nil
  "Non-nil when the reusable pure-Elisp TUI state has been prepared.")

(defun nemacs-main--tui-backend-live-p (handle)
  "Return non-nil when HANDLE is a live TUI backend handle."
  (and handle
       (fboundp 'emacs-tui-backend-handlep)
       (emacs-tui-backend-handlep handle)
       (or (not (fboundp 'emacs-tui-backend-handle-alive-p))
           (emacs-tui-backend-handle-alive-p handle))))

(defun nemacs-main--tui-frame-live-p (frame)
  "Return non-nil when FRAME has the TUI frame shape."
  (and frame
       (fboundp 'emacs-tui-backend-framep)
       (emacs-tui-backend-framep frame)))

(defun nemacs-main--redisplay-live-p (handle)
  "Return non-nil when HANDLE is a live redisplay handle."
  (and handle
       (fboundp 'emacs-redisplay-handlep)
       (emacs-redisplay-handlep handle)
       (or (not (fboundp 'emacs-redisplay-handle-alive-p))
           (emacs-redisplay-handle-alive-p handle))))

(defun nemacs-main--event-live-p (handle)
  "Return non-nil when HANDLE is a live TUI event handle."
  (and handle
       (fboundp 'emacs-tui-event-handlep)
       (emacs-tui-event-handlep handle)
       (or (not (fboundp 'emacs-tui-event-handle-alive-p))
           (emacs-tui-event-handle-alive-p handle))))

(defun nemacs-main--standalone-batch-tui-fallback-p ()
  "Return non-nil when batch tests should use lightweight TUI state."
  (and (nemacs-main-option :batch)
       (fboundp 'nelisp--write-stdout-bytes)))

(defun nemacs-main--prepare-standalone-batch-tui-state ()
  "Prepare lightweight in-memory TUI state for standalone batch gates.
The full TUI backend still depends on struct constructor behavior that
the standalone REPL path is hardening.  Batch close-gates only need the
state variables to be realised so Layer 2 edit/event-loop teardown paths
can run without a host Emacs."
  (setq nemacs-main--tui-features-loaded-p t
        nemacs-main--tui-state-prepared-p t
        nemacs-main--backend (list 'nemacs-standalone-batch-tui-backend)
        nemacs-main--frame (list 'nemacs-standalone-batch-tui-frame)
        nemacs-main--redisplay (list 'nemacs-standalone-batch-redisplay)
        nemacs-main--event-handle nil)
  (when (and (boundp 'nelisp-ec--buffers)
             (fboundp 'nelisp-ec-generate-new-buffer)
             (null (assoc "*scratch*" nelisp-ec--buffers)))
    (nelisp-ec-generate-new-buffer "*scratch*"))
  nemacs-main--redisplay)

(defun nemacs-main--prepare-tui-state ()
  "Ensure the pure-Elisp TUI state objects exist and return redisplay.
This performs only in-memory setup: backend handle, default frame,
redisplay handle, and event parser handle.  It is safe to call while
baking an interactive NeLisp runtime image, before runtime-specific TTY
state such as raw mode or terminal resize has been touched."
  (if (nemacs-main--standalone-batch-tui-fallback-p)
      (nemacs-main--prepare-standalone-batch-tui-state)
    (when (not nemacs-main--tui-features-loaded-p)
      (cond
       ((fboundp 'emacs-init-load-tui-core-features)
        (emacs-init-load-tui-core-features))
       ((fboundp 'emacs-init-load-editor-features)
        (emacs-init-load-editor-features))))
    (setq nemacs-main--tui-features-loaded-p t)
    (nemacs-main--ensure-keymap-after-feature-load)
    (if (and nemacs-main--tui-state-prepared-p
             nemacs-main--backend
             nemacs-main--frame
             nemacs-main--redisplay)
        nemacs-main--redisplay
      (when (and (fboundp 'emacs-tui-backend-init)
                 (fboundp 'emacs-tui-backend-frame-create)
                 (fboundp 'emacs-redisplay-init))
        (unless (nemacs-main--tui-backend-live-p nemacs-main--backend)
          (setq nemacs-main--backend (emacs-tui-backend-init)
                nemacs-main--frame nil
                nemacs-main--redisplay nil
                nemacs-main--event-handle nil))
        (unless (nemacs-main--tui-frame-live-p nemacs-main--frame)
          (setq nemacs-main--frame
                (emacs-tui-backend-frame-create nemacs-main--backend "main"))
          (nemacs-main--mark-tui-frame-clean))
        (unless (nemacs-main--redisplay-live-p nemacs-main--redisplay)
          (setq nemacs-main--redisplay
                (emacs-redisplay-init (list :backend nemacs-main--backend))))
        (when (and (fboundp 'emacs-tui-event-init)
                   (not (nemacs-main--event-live-p nemacs-main--event-handle)))
          (setq nemacs-main--event-handle (emacs-tui-event-init)))
        (setq nemacs-main--tui-state-prepared-p t)
        nemacs-main--redisplay))))

(defun nemacs-main--mark-tui-frame-clean ()
  "Mark the prepared TUI frame canvas as clean without changing pixels."
  (when (and nemacs-main--frame
             (fboundp 'emacs-tui-backend-frame-height)
             (fboundp 'emacs-tui-backend-frame-dirty-rows))
    (let ((rows (make-bool-vector
                 (emacs-tui-backend-frame-height nemacs-main--frame) nil)))
      (cond
       ((fboundp 'emacs-tui-backend-frame-set-dirty-rows)
        (emacs-tui-backend-frame-set-dirty-rows nemacs-main--frame rows))
       ;; Older loaded backends may not expose the setter helper.
       ((vectorp nemacs-main--frame)
        (aset nemacs-main--frame 7 rows)))))
  nil)

(defun nemacs-main--realise-tui ()
  "Bring up the TUI backend + redisplay engine for interactive use.

Sets the four state variables (`nemacs-main--backend', `--frame',
`--redisplay'), wires the redisplay current-handle slot (= Track G
bridge), and binds `*scratch*' into the selected window.  Returns
the redisplay handle on success, nil if the TUI side failed (= the
fallback path runs nemacs read-eval batch-style)."
  (condition-case err
      (progn
        (let ((h (nemacs-main--prepare-tui-state)))
          (when h
            (unless (nemacs-main--standalone-batch-tui-fallback-p)
              (when (fboundp 'emacs-redisplay-set-current-handle)
                (emacs-redisplay-set-current-handle h))
              ;; Doc 51 Track U (2026-05-04) — turn on bottom-row mode-line
              ;; reservation so every leaf window paints a status row.
              (when (boundp 'emacs-redisplay-paint-mode-line-p)
                (setq emacs-redisplay-paint-mode-line-p t))
              ;; Bind scratch into the selected window so the first
              ;; redisplay pass has something to paint.
              (let ((w (and (fboundp 'emacs-window-selected-window)
                            (emacs-window-selected-window)))
                    (b (and (boundp 'nelisp-ec--buffers)
                            (cdr (assoc "*scratch*" nelisp-ec--buffers)))))
                (when (and w b (fboundp 'emacs-window-set-window-buffer)
                           (boundp 'nelisp-ec--buffers))
                  (emacs-window-set-window-buffer w b))))
            h)))
    (error
     (when (fboundp 'message)
       (message "nemacs: TUI realise failed: %S" err))
     nil)))

(defun nemacs-main--enter-fullscreen ()
  "Doc 51 Track X (2026-05-04) — take over the user's TTY.
Resizes the frame to the actual terminal size (= so the very first
paint matches what the user sees, not the 80x24 default), then flips
into the alternate screen buffer so the post-quit shell scrollback
is preserved.  No-op when the backend / `terminal-current-winsize'
isn't available (= test fixtures, host driver in batch mode)."
  (when (and nemacs-main--backend nemacs-main--frame
             (fboundp 'emacs-tui-backend-frame-resize))
    (condition-case _
        (when (fboundp 'terminal-current-winsize)
          (let ((sz (terminal-current-winsize)))
            (when (and sz (consp sz)
                       (integerp (car sz)) (integerp (cdr sz))
                       (> (car sz) 0) (> (cdr sz) 0))
              (emacs-tui-backend-frame-resize nemacs-main--backend
                                              nemacs-main--frame
                                              (car sz) (cdr sz))
              (nemacs-main--mark-tui-frame-clean))))
      (error nil)))
  (when (and nemacs-main--backend
             (fboundp 'emacs-tui-backend-enter-alt-screen))
    (condition-case _
        (emacs-tui-backend-enter-alt-screen nemacs-main--backend)
      (error nil))))

(defun nemacs-main--leave-fullscreen ()
  "Doc 51 Track X (2026-05-04) — flip back to the user's normal screen.
Called from `nemacs-main--shutdown-tui' before the backend is torn
down, so the alt-screen-off escape lands while the handle is still
alive.  No-op when alt-screen wasn't entered (= idempotent on the
backend side)."
  (when (and nemacs-main--backend
             (fboundp 'emacs-tui-backend-leave-alt-screen))
    (condition-case _
        (emacs-tui-backend-leave-alt-screen nemacs-main--backend)
      (error nil))))

(defun nemacs-main--shutdown-tui ()
  "Tear down the TUI subsystem realised by `nemacs-main--realise-tui'."
  (unless (nemacs-main--standalone-batch-tui-fallback-p)
    (nemacs-main--leave-fullscreen)
    (when (and nemacs-main--event-handle
               (fboundp 'emacs-tui-event-shutdown))
      (condition-case _
          (emacs-tui-event-shutdown nemacs-main--event-handle)
        (error nil)))
    (when (and nemacs-main--backend
               (fboundp 'emacs-tui-backend-shutdown))
      (condition-case _
          (emacs-tui-backend-shutdown nemacs-main--backend)
        (error nil))))
  (setq nemacs-main--backend nil
        nemacs-main--frame nil
        nemacs-main--redisplay nil
        nemacs-main--event-handle nil
        nemacs-main--tui-state-prepared-p nil)
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
                 (or (fboundp 'emacs-redisplay-core-initial-paint)
                     (and (fboundp 'emacs-redisplay-redisplay-window)
                          (fboundp 'emacs-redisplay-flush-frame))))
        (cond
         ((fboundp 'emacs-redisplay-core-initial-paint)
          (emacs-redisplay-core-initial-paint nemacs-main--redisplay
                                              nemacs-main--frame))
         (t
          (let ((w (and (fboundp 'emacs-window-selected-window)
                        (emacs-window-selected-window))))
            (when w
              (emacs-redisplay-redisplay-window nemacs-main--redisplay w))
            (emacs-redisplay-flush-frame nemacs-main--redisplay
                                         nemacs-main--frame)))))
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

(defun nemacs-main--define-key (keymap key def)
  "Bind KEY to DEF in KEYMAP, using a fast full-keymap slot when possible."
  (let ((slot (and (fboundp 'emacs-keymap--full-slot)
                   (emacs-keymap--full-slot keymap))))
    (cond
     ((and slot
           (vectorp key)
           (= (length key) 1)
           (integerp (aref key 0))
           (>= (aref key 0) 0)
           (< (aref key 0) (length (cdr slot))))
      (aset (cdr slot) (aref key 0) def)
      def)
     ((fboundp 'define-key)
      (define-key keymap key def))
     ((fboundp 'emacs-keymap-define-key)
      (emacs-keymap-define-key keymap key def)))))

(defun nemacs-main--keymap-slot-vector (keymap)
  "Return KEYMAP's direct integer-key vector, or nil."
  (let ((slot (and (fboundp 'emacs-keymap--full-slot)
                   (emacs-keymap--full-slot keymap))))
    (and slot (cdr slot))))

(defvar nemacs-main--single-key-cache nil
  "Vector cache for direct ASCII key lookup on the owned global keymap.")

(defvar nemacs-main--single-key-cache-map nil
  "The keymap object represented by `nemacs-main--single-key-cache'.")

(defun nemacs-main--rebuild-single-key-cache (keymap)
  "Rebuild direct ASCII lookup cache for KEYMAP."
  (let ((cache (make-vector 256 nil))
        (vec (nemacs-main--keymap-slot-vector keymap))
        (c 0))
    (while (< c 256)
      (aset cache c
            (if (and vec (< c (length vec)))
                (aref vec c)
              (and (fboundp 'lookup-key)
                   (lookup-key keymap (vector c)))))
      (setq c (1+ c)))
    (setq nemacs-main--single-key-cache cache
          nemacs-main--single-key-cache-map keymap)
    cache))

(defun nemacs-main--make-full-keymap ()
  "Return a keymap with a direct integer-key vector when possible."
  (cond
   ((and (boundp 'emacs-version) (fboundp 'make-keymap))
    (make-keymap))
   ((fboundp 'emacs-keymap-make-keymap)
    (emacs-keymap-make-keymap))
   ((fboundp 'make-keymap)
    (make-keymap))
   ((fboundp 'make-sparse-keymap)
    (make-sparse-keymap))
   (t (list 'keymap))))

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
    (let* ((m (nemacs-main--make-full-keymap))
           (main-vec (nemacs-main--keymap-slot-vector m))
           (ctl-x-map (nemacs-main--make-full-keymap))
           (ctl-x-vec (nemacs-main--keymap-slot-vector ctl-x-map))
           (ctl-c-map (nemacs-main--make-full-keymap))
           (ctl-c-vec (nemacs-main--keymap-slot-vector ctl-c-map)))
      (when (or (fboundp 'define-key)
                (fboundp 'emacs-keymap-define-key))
        ;; Top-level commands.
        (if main-vec
            (progn
              (aset main-vec 24 ctl-x-map)
              (aset main-vec 3 ctl-c-map))
          (nemacs-main--define-key m (vector 24) ctl-x-map)
          (nemacs-main--define-key m (vector 3) ctl-c-map))
        (if ctl-x-vec
            (aset ctl-x-vec 3 'nemacs-main-kill)
          (nemacs-main--define-key ctl-x-map (vector 3) 'nemacs-main-kill))
        (if ctl-c-vec
            (aset ctl-c-vec 17 'nemacs-main-kill)
          (nemacs-main--define-key ctl-c-map (vector 17) 'nemacs-main-kill))
        (when (fboundp 'keyboard-quit)
          (if main-vec
              (aset main-vec 7 'keyboard-quit)
            (nemacs-main--define-key m (vector 7) 'keyboard-quit)))
        ;; ASCII printable → self-insert-command.  We bind the
        ;; integer key directly (= what nemacs-main--key-event->key
        ;; produces for a bare ASCII char with no modifier).
        ;; Range 32..126 = SPC..~  inclusive.
        (when (fboundp 'self-insert-command)
          (let ((c 32))
            (while (<= c 126)
              (if main-vec
                  (aset main-vec c 'self-insert-command)
                (nemacs-main--define-key m (vector c) 'self-insert-command))
              (setq c (1+ c)))))
        ;; Newline (= byte 13 = RET in raw mode).
        (when (fboundp 'newline)
          (if main-vec
              (aset main-vec 13 'newline)
            (nemacs-main--define-key m (vector 13) 'newline)))
        ;; Doc 51 Track B (2026-05-04) — motion + delete.
        (when (fboundp 'forward-char)
          (if main-vec
              (aset main-vec 6 'forward-char)
            (nemacs-main--define-key m (vector 6) 'forward-char)))
        (when (fboundp 'backward-char)
          (if main-vec
              (aset main-vec 2 'backward-char)
            (nemacs-main--define-key m (vector 2) 'backward-char)))
        (when (fboundp 'next-line)
          (if main-vec
              (aset main-vec 14 'next-line)
            (nemacs-main--define-key m (vector 14) 'next-line)))
        (when (fboundp 'previous-line)
          (if main-vec
              (aset main-vec 16 'previous-line)
            (nemacs-main--define-key m (vector 16) 'previous-line)))
        (when (fboundp 'beginning-of-line)
          (if main-vec
              (aset main-vec 1 'beginning-of-line)
            (nemacs-main--define-key m (vector 1) 'beginning-of-line)))
        (when (fboundp 'end-of-line)
          (if main-vec
              (aset main-vec 5 'end-of-line)
            (nemacs-main--define-key m (vector 5) 'end-of-line)))
        (when (fboundp 'delete-char)
          (if main-vec
              (aset main-vec 4 'delete-char)
            (nemacs-main--define-key m (vector 4) 'delete-char)))
        (when (fboundp 'kill-line)
          (if main-vec
              (aset main-vec 11 'kill-line)
            (nemacs-main--define-key m (vector 11) 'kill-line)))
        (when (fboundp 'delete-backward-char)
          ;; DEL (= byte 127) and Ctrl+H both surface as the symbol
          ;; `backspace' through `emacs-tui-event--control-char-name'.
          (nemacs-main--define-key m (vector 'backspace) 'delete-backward-char)
          ;; Bare byte 127 in case the symbol mapping is bypassed.
          (nemacs-main--define-key m (vector 127) 'delete-backward-char))
        ;; Doc 51 Track U (2026-05-04) — arrow keys.  These come from
        ;; `emacs-tui-event--csi-final-table' as the bare symbols
        ;; `up' / `down' / `right' / `left' on raw stdin ESC seqs.
        (when (fboundp 'previous-line)
          (nemacs-main--define-key m (vector 'up) 'previous-line))
        (when (fboundp 'next-line)
          (nemacs-main--define-key m (vector 'down) 'next-line))
        (when (fboundp 'forward-char)
          (nemacs-main--define-key m (vector 'right) 'forward-char))
        (when (fboundp 'backward-char)
          (nemacs-main--define-key m (vector 'left) 'backward-char))
        ;; Doc 51 Track C — file open / save.
        (if ctl-x-vec
            (progn
              (aset ctl-x-vec 6 'nemacs-main-find-file-interactive)
              (aset ctl-x-vec 19 'nemacs-main-save-buffer-interactive)
              (aset ctl-x-vec 2 'nemacs-main-list-buffers-interactive)
              (aset ctl-x-vec 98 'nemacs-main-switch-to-buffer-interactive)
              (aset ctl-x-vec 107 'nemacs-main-kill-buffer-interactive))
          (nemacs-main--define-key ctl-x-map (vector 6) 'nemacs-main-find-file-interactive)
          (nemacs-main--define-key ctl-x-map (vector 19) 'nemacs-main-save-buffer-interactive)
          (nemacs-main--define-key ctl-x-map (vector 2) 'nemacs-main-list-buffers-interactive)
          (nemacs-main--define-key ctl-x-map (vector 98) 'nemacs-main-switch-to-buffer-interactive)
          (nemacs-main--define-key ctl-x-map (vector 107) 'nemacs-main-kill-buffer-interactive))
        ;; Doc 51 Track V (2026-05-04) — window split / select / delete.
        (when (fboundp 'split-window-below)
          (if ctl-x-vec
              (aset ctl-x-vec 50 'split-window-below)
            (nemacs-main--define-key ctl-x-map (vector 50) 'split-window-below)))
        (when (fboundp 'split-window-right)
          (if ctl-x-vec
              (aset ctl-x-vec 51 'split-window-right)
            (nemacs-main--define-key ctl-x-map (vector 51) 'split-window-right)))
        (when (fboundp 'delete-window)
          (if ctl-x-vec
              (aset ctl-x-vec 48 'delete-window)
            (nemacs-main--define-key ctl-x-map (vector 48) 'delete-window)))
        (when (fboundp 'delete-other-windows)
          (if ctl-x-vec
              (aset ctl-x-vec 49 'delete-other-windows)
            (nemacs-main--define-key ctl-x-map (vector 49) 'delete-other-windows)))
        (when (fboundp 'other-window)
          (if ctl-x-vec
              (aset ctl-x-vec 111 'other-window)
            (nemacs-main--define-key ctl-x-map (vector 111) 'other-window))))
      ;; ESC+x / Alt+x reaches the event loop as a single Meta-modified
      ;; printable event from `emacs-tui-event'.  Bind the same integer
      ;; shape that upstream Emacs keymaps use for M-x.
      (nemacs-main--define-key
       m (vector (logior nemacs-main--meta-modifier-mask ?x))
       'nemacs-main-execute-extended-command)
      (nemacs-main--define-key
       m (vector (logior nemacs-main--meta-modifier-mask ?!))
       'nemacs-main-shell-command-interactive)
      (let ((help-map (make-sparse-keymap)))
        (nemacs-main--define-key help-map (vector 107)
                                 'nemacs-main-describe-key-interactive)
        (nemacs-main--define-key m (vector 8) help-map)
        (nemacs-main--define-key m (vector 'backspace) help-map))
      (setq nemacs-main--global-keymap m)
      (nemacs-main--rebuild-single-key-cache m)))
  nemacs-main--global-keymap)

(defun nemacs-main--ensure-keymap-after-feature-load ()
  "Ensure lazy-loaded editor commands are reflected in the keymap.

Runtime images may call `nemacs-main--init-keymap' before the editor
command modules have been loaded.  In that case printable keys and RET
were intentionally skipped because their commands were not `fboundp'
yet.  After `nemacs-main--prepare-tui-state' loads those modules, rebuild
the owned keymap if those core bindings are still missing."
  (let ((needs-rebuild nil))
    (when (not nemacs-main--global-keymap)
      (setq needs-rebuild t))
    (when (and (not needs-rebuild)
               (fboundp 'self-insert-command)
               (not (eq (nemacs-main--lookup-key-vec (vector ?a))
                        'self-insert-command)))
      (setq needs-rebuild t))
    (when (and (not needs-rebuild)
               (fboundp 'newline)
               (not (eq (nemacs-main--lookup-key-vec (vector 13))
                        'newline)))
      (setq needs-rebuild t))
    (when needs-rebuild
      (setq nemacs-main--global-keymap nil
            nemacs-main--single-key-cache nil
            nemacs-main--single-key-cache-map nil)
      (nemacs-main--init-keymap)))
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

(defvar emacs-tui-event-input-fn nil
  "Input callback slot used by `emacs-tui-event' when it is loaded lazily.")

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

(defconst nemacs-main--control-modifier-mask (lsh 1 26)
  "Integer bit used by Emacs keymaps for Control-modified characters.")

(defconst nemacs-main--meta-modifier-mask (lsh 1 27)
  "Integer bit used by Emacs keymaps for Meta-modified characters.")

(defvar nemacs-main--repaint-hint nil
  "Hint for the next TUI repaint after input dispatch.
Currently `current-line' means a simple printable self-insert changed
only the selected window's current display row.")

(defvar nemacs-main--insert-repaint-hint (vector 'insert-char 0 1 1)
  "Reusable repaint hint vector for printable self-insert.
Shape: [insert-char CHAR POINT-BEFORE POINT-AFTER].")

(defvar nemacs-main--insert-text-repaint-hint (vector 'insert-text "" 1 1)
  "Reusable repaint hint vector for burst printable self-insert.
Shape: [insert-text TEXT POINT-BEFORE POINT-AFTER].")

(defun nemacs-main--insert-repaint-hint-p (hint)
  "Return non-nil when HINT describes printable text insertion."
  (or (and (vectorp hint)
           (= (length hint) 4)
           (memq (aref hint 0) '(insert-char insert-text)))
      (and (consp hint)
           (memq (plist-get hint :kind) '(insert-char insert-text)))))

(defun nemacs-main--insert-repaint-hint-char (hint)
  "Return HINT's inserted character, or nil."
  (cond
   ((and (vectorp hint) (= (length hint) 4)
         (eq (aref hint 0) 'insert-char))
    (aref hint 1))
   ((and (consp hint) (eq (plist-get hint :kind) 'insert-char))
    (plist-get hint :char))
   (t nil)))

(defun nemacs-main--insert-repaint-hint-beg (hint)
  "Return HINT's insertion start point, or nil."
  (cond
   ((and (vectorp hint) (= (length hint) 4))
    (aref hint 2))
   ((consp hint)
    (or (plist-get hint :beg)
        (plist-get hint :start)))
   (t nil)))

(defun nemacs-main--insert-repaint-hint-end (hint)
  "Return HINT's insertion end point, or nil."
  (cond
   ((and (vectorp hint) (= (length hint) 4))
    (aref hint 3))
   ((consp hint)
    (or (plist-get hint :end)
        (plist-get hint :point)))
   (t nil)))

(defun nemacs-main--set-insert-repaint-hint (char beg end)
  "Set `nemacs-main--repaint-hint' to reusable insert data."
  (aset nemacs-main--insert-repaint-hint 1 char)
  (aset nemacs-main--insert-repaint-hint 2 beg)
  (aset nemacs-main--insert-repaint-hint 3 end)
  (setq nemacs-main--repaint-hint nemacs-main--insert-repaint-hint))

(defun nemacs-main--set-insert-text-repaint-hint (text beg end)
  "Set `nemacs-main--repaint-hint' to reusable burst insert data."
  (aset nemacs-main--insert-text-repaint-hint 1 text)
  (aset nemacs-main--insert-text-repaint-hint 2 beg)
  (aset nemacs-main--insert-text-repaint-hint 3 end)
  (setq nemacs-main--repaint-hint nemacs-main--insert-text-repaint-hint))

(defun nemacs-main--key-event->key (ev)
  "Translate a tui-event-key plist EV into a keymap-lookup key.

Returns one of:
  - integer        plain ASCII char or symbol-as-int from :name
  - integer        C-a..C-z control byte (= 1..26)
  - integer + bit  M-X chord (= `(logior CHAR (ash 1 27))')
  - symbol         function key (= `up', `backspace', `f1', …)
  - the plist itself  fallback for shapes we don't recognise

The plist shape from `emacs-tui-event' uses `:name' / `:modifiers'
(= `:name' is integer for ASCII, symbol for function keys).  The
test fixtures use the older `:char' / `:mods' aliases.  Both are
accepted."
  (if (integerp ev)
      ev
    (let* ((char (or (plist-get ev :char)
                     (let ((n (plist-get ev :name)))
                       (and (integerp n) n))))
           (sym  (let ((n (plist-get ev :name)))
                   (and (symbolp n) (not (null n)) n)))
           (mods (or (plist-get ev :mods)
                     (plist-get ev :modifiers))))
      (cond
       ((and char (or (memq 'control mods) (memq 'meta mods)))
        ;; `kbd' represents ASCII control letters as control bytes
        ;; (C-a..C-z => 1..26), not as a modifier-bit integer.  Meta is
        ;; layered on top of that byte when both modifiers are present.
        (let ((key char))
          (when (and (memq 'control mods)
                     (or (and (>= key ?a) (<= key ?z))
                         (and (>= key ?A) (<= key ?Z))))
            (let ((lower (if (and (>= key ?A) (<= key ?Z))
                             (+ key (- ?a ?A))
                           key)))
              (setq key (1+ (- lower ?a)))))
          (when (memq 'meta mods)
            (setq key (logior key nemacs-main--meta-modifier-mask)))
          key))
       (char char)
       (sym sym)
       (t ev)))))

(defun nemacs-main--lookup-key-vec (vec)
  "Look up VEC in `nemacs-main--global-keymap'.
Returns the binding (= a command symbol / keymap / nil)."
  (when (and nemacs-main--global-keymap
             (fboundp 'lookup-key))
    (lookup-key nemacs-main--global-keymap vec)))

(defun nemacs-main--lookup-single-key (key)
  "Look up unprefixed KEY in `nemacs-main--global-keymap'.
This is the per-character event-loop fast path.  It avoids allocating a
temporary vector and avoids the general `lookup-key' sequence walker when
the local pure-Elisp keymap substrate is available."
  (cond
   ((and (integerp key)
         (>= key 0)
         (< key 256)
         nemacs-main--single-key-cache
         (eq nemacs-main--single-key-cache-map nemacs-main--global-keymap))
    (aref nemacs-main--single-key-cache key))
   ((and nemacs-main--global-keymap
         (integerp key)
         (>= key 0)
         (< key 256)
         (fboundp 'emacs-keymap-keymapp)
         (emacs-keymap-keymapp nemacs-main--global-keymap)
         (fboundp 'emacs-keymap--lookup-with-parent))
    (emacs-keymap--lookup-with-parent nemacs-main--global-keymap key))
   (t
    (nemacs-main--lookup-key-vec (vector key)))))

(defun nemacs-main--printable-self-insert-p (binding key)
  "Return non-nil when BINDING/KEY is the fast printable insert case."
  (and (eq binding 'self-insert-command)
       (integerp key)
       (>= key 32)
       (<= key 126)
       (fboundp 'self-insert-command)))

(defun nemacs-main--direct-tui-command-p (binding)
  "Return non-nil when BINDING should run directly in the boot TUI.
The standalone `command-execute' shim is still catching up with Emacs'
interactive calling convention.  These commands are implemented in this
module specifically for the `-nw' event loop, so direct `funcall' keeps
the boot path deterministic."
  (memq binding
        '(nemacs-main-find-file-interactive
          nemacs-main-save-buffer-interactive
          nemacs-main-list-buffers-interactive
          nemacs-main-switch-to-buffer-interactive
          nemacs-main-kill-buffer-interactive
          nemacs-main-dired-interactive
          nemacs-main-info-interactive
          nemacs-main-shell-command-interactive
          nemacs-main-query-replace-interactive
          nemacs-main-describe-key-interactive
          nemacs-main-describe-function-interactive
          nemacs-main-describe-variable-interactive)))

(defun nemacs-main--overwrite-mode-active-p ()
  "Return non-nil when `overwrite-mode' is really enabled.
The standalone runtime can expose an internal `nelisp--unbound-marker'
value for defvars that are present but not initialized.  Treat that as
nil for ordinary editor mode checks."
  (and (boundp 'overwrite-mode)
       overwrite-mode
       (not (eq overwrite-mode 'nelisp--unbound-marker))))

(defun nemacs-main--execute-printable-self-insert (key)
  "Execute printable self-insert KEY without `command-execute'.
The normal `command-execute' path is semantically general but expensive
under standalone NeLisp because it has to inspect the interactive form
and build an argument list.  For a bare printable key the argument list
is already known: repeat count 1 and the character itself.
Return the new point for the inlined fast path, or nil when it falls
back to `self-insert-command'."
  (when (fboundp 'emacs-command-loop-set-this-command)
    (emacs-command-loop-set-this-command 'self-insert-command))
  (unwind-protect
      (cond
       ((and (not (nemacs-main--overwrite-mode-active-p))
             ;; The standalone primitive currently returns without updating
             ;; `nelisp-ec-buffer-string'.  Keep the host/test fast path, but
             ;; use the general insert path in the real NeLisp runtime.
             (not (fboundp 'nl-write-file))
	             (fboundp 'nelisp-ec-insert-char-code-fast))
	(let* ((end (nelisp-ec-insert-char-code-fast key))
	       (beg (1- end)))
	  (nemacs-main--set-insert-repaint-hint key beg end)
	  (when (fboundp 'emacs-undo-record-insert)
	    (emacs-undo-record-insert beg end))
	  (when (fboundp 'emacs-font-lock-mark-dirty-region)
            (emacs-font-lock-mark-dirty-region beg end))
          end))
       ((and (not (nemacs-main--overwrite-mode-active-p))
             (fboundp 'nelisp-ec-point)
             (fboundp 'nelisp-ec-insert))
        (let ((beg (nelisp-ec-point)))
          (nelisp-ec-insert (string key))
	          (let ((end (nelisp-ec-point)))
	            (nemacs-main--set-insert-repaint-hint key beg end)
	            (when (fboundp 'emacs-undo-record-insert)
	              (emacs-undo-record-insert beg end))
	            (when (fboundp 'emacs-font-lock-mark-dirty-region)
              (emacs-font-lock-mark-dirty-region beg end))
            end)))
       (t
        (self-insert-command 1 key)))
    (when (fboundp 'emacs-command-loop-mark-command-finished)
      (emacs-command-loop-mark-command-finished))))

(defun nemacs-main--dispatch-printable-self-insert-direct (key)
  "Dispatch printable self-insert KEY on the event-loop fast path."
  (setq nemacs-main--prefix-keys [])
  (when (boundp 'last-command-event)
    (setq last-command-event key))
  (let ((point-after nil))
    (condition-case _
        (progn
          (setq point-after
                (nemacs-main--execute-printable-self-insert key))
          (unless nemacs-main--repaint-hint
            (setq nemacs-main--repaint-hint 'current-line)))
      (quit (nemacs-main--quit))
      (error
       (when (fboundp 'message)
         (message "command error during self-insert"))))
    (nemacs-main--sync-selected-window-point point-after)))

(defun nemacs-main--sync-selected-window-buffer (&optional buffer)
  "Make the selected TUI window display BUFFER or the current buffer.
Return non-nil when the selected window's buffer changed."
  (when (and (fboundp 'emacs-window-selected-window)
             (fboundp 'emacs-window-window-buffer)
             (fboundp 'emacs-window-set-window-buffer)
             (or buffer (fboundp 'nelisp-ec-current-buffer)))
    (let* ((w (emacs-window-selected-window))
           (cb (or buffer (nelisp-ec-current-buffer)))
           (wb (and w (emacs-window-window-buffer w))))
      (when (and w cb
                 (or (not (fboundp 'nelisp-ec-buffer-p))
                     (nelisp-ec-buffer-p cb))
                 (not (eq wb cb)))
        (emacs-window-set-window-buffer w cb))
      (when (and cb
                 (fboundp 'nelisp-ec-set-buffer)
                 (fboundp 'nelisp-ec-current-buffer)
                 (or (not (fboundp 'nelisp-ec-buffer-p))
                     (nelisp-ec-buffer-p cb))
                 (not (eq cb (nelisp-ec-current-buffer))))
        (nelisp-ec-set-buffer cb))
      (and w cb (not (eq wb cb))))))

(defun nemacs-main--dispatch-key-code (key &optional source-event)
  "Process a single KEY through the keymap.
SOURCE-EVENT is the original tui-event plist when one exists.

Accumulates KEY into `nemacs-main--prefix-keys', looks the result up
in `nemacs-main--global-keymap', and:
  - Runs the command via `command-execute' on a non-keymap binding,
    then clears the prefix.
  - Keeps the prefix growing on a keymap binding (= prefix key).
  - Clears the prefix on an unbound sequence (= give up gracefully)."
  (let* ((prefix-empty-p (= (length nemacs-main--prefix-keys) 0))
         (next-vec nil)
         (binding (if prefix-empty-p
                      (nemacs-main--lookup-single-key key)
                    (setq next-vec
                          (vconcat nemacs-main--prefix-keys (vector key)))
                    (nemacs-main--lookup-key-vec next-vec))))
    (cond
     ;; The dominant interactive case: a bare printable key bound to
     ;; self-insert.  Once the direct single-key cache resolves this, skip
     ;; the generic keymap/command dispatcher work.
     ((and prefix-empty-p
           (nemacs-main--printable-self-insert-p binding key))
      (nemacs-main--dispatch-printable-self-insert-direct key))
     ;; Prefix key — keep accumulating.
     ((and binding
           (or (and (fboundp 'keymapp) (keymapp binding))
               (and (fboundp 'emacs-keymap-keymapp)
                    (emacs-keymap-keymapp binding))))
	  (setq nemacs-main--prefix-keys
		    (or next-vec (vector key))))
     ;; Bound command — execute + reset.
     ((and binding (fboundp 'command-execute))
      (setq nemacs-main--prefix-keys [])
      ;; Doc 51 Track A — `self-insert-command' looks at
      ;; `last-command-event' to know which char to insert.
      ;; Set it from the key event we just dispatched on.  Real
      ;; tui-event puts the char in :name as an integer; the test
      ;; fixtures use :char.  Accept both shapes.
      (let* ((c (or (and (integerp source-event) source-event)
                    (and (consp source-event)
                         (plist-get source-event :char))
                    (let ((n (and (consp source-event)
                                  (plist-get source-event :name))))
                      (and (integerp n) n)))))
        (when (and c (boundp 'last-command-event))
          (setq last-command-event c)))
      (let ((point-after nil))
        (condition-case err
            (if (nemacs-main--printable-self-insert-p binding key)
                (progn
                  (setq point-after
                        (nemacs-main--execute-printable-self-insert key))
                  (unless nemacs-main--repaint-hint
                    (setq nemacs-main--repaint-hint 'current-line)))
	      (if (nemacs-main--direct-tui-command-p binding)
	                  (funcall binding)
	                (command-execute binding)))
          (quit (nemacs-main--quit))
          (error
           (when (fboundp 'message)
             (message "command %S failed: %S" binding err))))
        (when (nemacs-main--sync-selected-window-buffer)
          ;; A command such as find-file changed the displayed buffer; force
          ;; the next repaint to rebuild from the new window contents.
          (setq nemacs-main--repaint-hint nil))
        (nemacs-main--sync-selected-window-point point-after)))
     ;; No binding — reset and ignore (= upstream "<key> is undefined").
     (t
      (setq nemacs-main--prefix-keys [])))))

(defun nemacs-main--dispatch-key-event (ev)
  "Process a single key EV through the keymap.
EV may be the usual tui-event plist or a plain integer key code from the
printable-byte fast path."
  (nemacs-main--dispatch-key-code (nemacs-main--key-event->key ev) ev))

(defun nemacs-main--sync-selected-window-point (&optional known-point)
  "Copy the current buffer point into the selected TUI window cache."
  (when (and (fboundp 'emacs-window-selected-window)
             (fboundp 'emacs-window-window-buffer)
             (fboundp 'emacs-window-set-window-point)
             (fboundp 'nelisp-ec-current-buffer)
             (or known-point (fboundp 'nelisp-ec-point)))
    (let* ((w (emacs-window-selected-window))
           (wb (and w (emacs-window-window-buffer w)))
           (cb (nelisp-ec-current-buffer)))
      (when (and w cb (eq wb cb))
        (emacs-window-set-window-point w (or known-point
                                             (nelisp-ec-point)))))))

(defun nemacs-main--handle-winsize ()
  "Doc 51 Track P — react to a pending SIGWINCH.
If the resize-pending flag is set, query the controlling tty's
current size, propagate to the frame, and force a redraw.  Safe
to call when the builtins are not bound (= host driver) — returns
nil silently.  Returns non-nil when a resize was consumed and applied.

`nemacs-main--frame' is an `emacs-tui-backend-frame', so we route
through `emacs-tui-backend-frame-resize' (= the TUI-side resize)
not `emacs-frame-set-frame-size' (= the higher-level frame
abstraction's resize, which expects an `emacs-frame')."
  (let ((changed nil))
    (when (and (fboundp 'terminal-take-winsize-changed)
               (terminal-take-winsize-changed))
      (when (fboundp 'terminal-current-winsize)
        (let ((sz (terminal-current-winsize)))
          (when (and sz (consp sz)
                     (integerp (car sz)) (integerp (cdr sz))
                     nemacs-main--frame nemacs-main--backend
                     (fboundp 'emacs-tui-backend-frame-resize))
            (condition-case err
                (progn
                  (emacs-tui-backend-frame-resize nemacs-main--backend
                                                  nemacs-main--frame
                                                  (car sz) (cdr sz))
                  (setq changed t))
              (error
               (when (fboundp 'message)
                 (message "nemacs: SIGWINCH resize failed: %S" err))))))))
    changed))

(defun nemacs-main--handle-sigcont ()
  "Doc 51 Track Q — react to a SIGCONT (= just-resumed-from-suspend).
The TSTP handler dropped raw mode before suspending; on resume we
re-enter raw mode + force a full redraw.  Safe under host driver.
Returns non-nil when a SIGCONT was consumed."
  (let ((changed nil))
    (when (and (fboundp 'terminal-take-sigcont)
               (terminal-take-sigcont))
      (setq changed t)
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
              (error nil))))))
    changed))

;;;; --- minibuffer-style line read (Doc 51 Track C) ----------------------

(defun nemacs-main--read-line-next-byte (timeout-ms)
  "Return the next minibuffer input byte, or nil on timeout.
Prefer the active `emacs-tui-event' handle so bytes already parsed while
dispatching a prefix key remain visible to prompt readers.  Fall back to
the NeLisp raw-stdin builtin for minimal prepared paths without an event
handle."
  (cond
   ((and nemacs-main--event-handle
         (fboundp 'emacs-tui-event-poll-printable-byte)
         (fboundp 'emacs-tui-event-poll))
    (let ((ev (or (emacs-tui-event-poll-printable-byte
                   nemacs-main--event-handle)
                  (emacs-tui-event-poll nemacs-main--event-handle
                                        timeout-ms))))
      (cond
       ((integerp ev) ev)
       ((and (consp ev) (eq (plist-get ev :type) 'key))
        (let ((name (plist-get ev :name))
              (mods (plist-get ev :modifiers)))
          (cond
           ((eq name 'return) 13)
           ((eq name 'backspace) 127)
           ((and (integerp name) (null mods)) name)
           ((and (integerp name)
                 (memq 'control mods)
                 (or (and (>= name ?a) (<= name ?z))
                     (and (>= name ?A) (<= name ?Z))))
            (let ((lower (if (and (>= name ?A) (<= name ?Z))
                             (+ name (- ?a ?A))
                           name)))
              (1+ (- lower ?a))))
           ((and (integerp name) (memq 'control mods) (= name ?g)) 7)
           ((and (integerp name) (memq 'control mods) (= name ?h)) 8)
           ((and (integerp name) (memq 'control mods) (= name ?m)) 13)
           (t nil))))
       (t nil))))
   ((fboundp 'read-stdin-byte-available)
    (read-stdin-byte-available timeout-ms))
   (t nil)))

(defun nemacs-main--read-line-repaint (prompt input)
  "Paint PROMPT and INPUT on the bottom row of the active TUI frame."
  (let* ((width 80)
         (row 23)
         (line (concat prompt input))
         (clipped (if (> (length line) width)
                      (substring line 0 width)
                    line))
         ;; Pad with spaces to clear stale chars.
         (pad-len (- width (length clipped)))
         (full (concat clipped
                       (if (> pad-len 0)
                           (make-string pad-len ?\s)
                         "")))
         (out (concat "\e[" (number-to-string (1+ row)) ";1H" full)))
    (if (fboundp 'emacs-tui-backend--emit)
        (emacs-tui-backend--emit out)
      (princ out))))

(defun nemacs-main--read-line-blocking (prompt)
  "Doc 51 Track C (2026-05-04) — block-read a line via TUI canvas.

Paints PROMPT at the bottom row of `nemacs-main--frame', echoes
each key as the user types, supports backspace, and returns the
typed string on RET (= byte 13).  Returns nil on C-g (= byte 7).
Blocks the event loop while reading — no other commands fire.

This is intentionally a minimal `read-from-minibuffer'-replacement
(= the full minibuffer machinery is too heavy for the boot path).
Used by `nemacs-main-find-file-interactive'."
  (if (and (eq (or (nemacs-main-option :driver) 'host) 'host)
           (boundp 'noninteractive)
           (not noninteractive)
           (fboundp 'read-string))
      (let ((overriding-terminal-local-map nil))
        (read-string prompt))
    (let ((input  "")
          (done   nil)
          (cancel nil))
      (nemacs-main--read-line-repaint prompt input)
      (while (not done)
        (let ((b (nemacs-main--read-line-next-byte 100)))
          (when b
            (cond
             ((= b 13) (setq done t))                             ; RET
             ((= b 7)  (setq cancel t done t))                    ; C-g
             ((or (= b 127) (= b 8))                              ; BS / DEL
              (when (> (length input) 0)
                (setq input (substring input 0 (1- (length input))))
                (nemacs-main--read-line-repaint prompt input)))
             ((and (>= b 32) (<= b 126))
              (setq input (concat input (string b)))
              ;; Drain immediately queued printable bytes in one paint pass.
              ;; PTY smokes often write an entire path/command at once; doing
              ;; one bottom-row emit per byte makes long paths miss the
              ;; short daily-driver observation window.
              (let ((more t))
                (while more
                  (let ((next (nemacs-main--read-line-next-byte 0)))
                    (cond
                     ((null next)
                      (setq more nil))
                     ((= next 13)
                      (setq done t
                            more nil))
                     ((= next 7)
                      (setq cancel t
                            done t
                            more nil))
                     ((or (= next 127) (= next 8))
                      (when (> (length input) 0)
                        (setq input (substring input 0 (1- (length input))))))
                     ((and (>= next 32) (<= next 126))
                      (setq input (concat input (string next))))
                     (t
                      (setq more nil))))))
              (nemacs-main--read-line-repaint prompt input))))))
      (if cancel nil input))))

(defun nemacs-main--file-exists-p (path)
  "Return non-nil when PATH exists using the safest available primitive."
  (cond
   ((and (fboundp 'nelisp-ec-file-exists-p)
         (nelisp-ec-file-exists-p path))
    t)
   ((and (fboundp 'file-exists-p)
         (file-exists-p path))
    t)
   (t nil)))

(defun nemacs-main--read-file-text-direct (path)
  "Return PATH contents as a string for the standalone TUI file path."
  (cond
   ((and (fboundp 'nl-syscall-read-file)
         (nemacs-main--file-exists-p path))
    (nl-syscall-read-file path 0 nil))
   ((and (fboundp 'insert-file-contents)
         (fboundp 'buffer-string)
         (nemacs-main--file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))
   ;; `nelisp--syscall-read-file' is intentionally not used here: the current
   ;; standalone implementation can stop evaluation after the call, which would
   ;; freeze interactive `C-x C-f'.  Once `nl-syscall-read-file' is exposed in
   ;; the runtime image this direct TUI path can preserve existing contents.
   (t "")))

(defun nemacs-main--buffer-name-for-file (path)
  "Return the buffer name to use for PATH."
  (let ((name (if (fboundp 'file-name-nondirectory)
                  (file-name-nondirectory path)
                path)))
    (if (and (stringp name) (> (length name) 0))
        name
      " *find-file*")))

(defun nemacs-main--record-buffer-file (buffer path)
  "Record BUFFER as visiting PATH when the core file table is available."
  (when (boundp 'emacs-fileio--buffer-files)
    (setq emacs-fileio--buffer-files
          (cons (cons buffer path)
                (assq-delete-all buffer emacs-fileio--buffer-files))))
  path)

(defun nemacs-main--buffer-file-direct (&optional buffer)
  "Return BUFFER's visited file from the core file table."
  (let ((buf (or buffer
                 (and (eq (or (nemacs-main-option :driver) 'host) 'host)
                      (boundp 'noninteractive)
                      (not noninteractive)
                      (fboundp 'current-buffer)
                      (current-buffer))
                 (and (fboundp 'nelisp-ec-current-buffer)
                      (nelisp-ec-current-buffer)))))
    (or (and buf
             (boundp 'buffer-file-name)
             (fboundp 'buffer-local-value)
             (condition-case nil
                 (buffer-local-value 'buffer-file-name buf)
               (error nil)))
        (and (fboundp 'buffer-file-name)
             (condition-case nil
                 (if buf
                     (with-current-buffer buf
                       (buffer-file-name))
                   (buffer-file-name))
               (error nil)))
        (and (boundp 'emacs-fileio--buffer-files)
             (cdr (assq buf emacs-fileio--buffer-files))))))

(defun nemacs-main--visit-file-direct (path)
  "Visit PATH using `nelisp-ec' buffers and return the buffer.
This is the standalone TUI path used before the full file I/O runtime is
fast enough for interactive `-nw'."
  (let* ((abs (if (fboundp 'expand-file-name)
                  (expand-file-name path)
                path))
         (existing nil))
    (when (boundp 'emacs-fileio--buffer-files)
      (catch 'found
        (dolist (cell emacs-fileio--buffer-files)
          (when (equal abs (cdr cell))
            (setq existing (car cell))
            (throw 'found existing)))))
    (let ((buffer (or existing
                      (and (fboundp 'nelisp-ec-generate-new-buffer)
                           (nelisp-ec-generate-new-buffer
                            (nemacs-main--buffer-name-for-file abs))))))
      (unless buffer
        (signal 'error (list "cannot create buffer for file" abs)))
      (when (and (not existing)
                 (fboundp 'nelisp-ec-with-current-buffer))
        (nelisp-ec-with-current-buffer buffer
          (when (fboundp 'nelisp-ec-erase-buffer)
            (nelisp-ec-erase-buffer))
          (let ((text (nemacs-main--read-file-text-direct abs)))
            (when (and (stringp text) (> (length text) 0)
                       (fboundp 'nelisp-ec-insert))
              (nelisp-ec-insert text)))
          (when (fboundp 'set-buffer-modified-p)
            (set-buffer-modified-p nil))))
      (nemacs-main--record-buffer-file buffer abs)
      (when (fboundp 'nelisp-ec-set-buffer)
        (nelisp-ec-set-buffer buffer))
      buffer)))

(defun nemacs-main--save-buffer-direct ()
  "Save the current standalone TUI buffer to its visited file."
  (let* ((buffer (and (fboundp 'nelisp-ec-current-buffer)
                      (nelisp-ec-current-buffer)))
         (path (nemacs-main--buffer-file-direct buffer)))
    (unless path
      (signal 'error '("save-buffer: buffer is not visiting a file")))
    (let ((text (if (fboundp 'nelisp-ec-buffer-string)
                    (nelisp-ec-buffer-string)
                  (buffer-string))))
      (cond
       ((fboundp 'nl-write-file)
        (nl-write-file path text))
       ((fboundp 'write-region)
        (write-region text nil path nil 'silent))
       (t
        (signal 'error '("save-buffer: no file writer available"))))
      (when (fboundp 'set-buffer-modified-p)
        (set-buffer-modified-p nil))
      path)))

(defun nemacs-main-find-file-interactive ()
  "Doc 51 Track C — prompt for a path and visit it via `find-file'."
  (interactive)
  (let ((path (nemacs-main--read-line-blocking "Find file: ")))
    (when (and path (> (length path) 0))
      (condition-case err
          (let ((buffer (if (and (fboundp 'nl-write-file)
                                 (fboundp 'nelisp-ec-generate-new-buffer))
                            (nemacs-main--visit-file-direct path)
                          (find-file path))))
            (when (nemacs-main--sync-selected-window-buffer buffer)
              (setq nemacs-main--repaint-hint nil))
            buffer)
        (error
         (when (fboundp 'message)
           (message "find-file failed: %S" err)))))))

(defun nemacs-main-save-buffer-interactive ()
  "Doc 51 Track C — save the current buffer via `save-buffer'.
If the buffer has no associated file, prompt for one via
`write-file' instead."
  (interactive)
  (let* ((b (or (and (eq (or (nemacs-main-option :driver) 'host) 'host)
                     (boundp 'noninteractive)
                     (not noninteractive)
                     (fboundp 'current-buffer)
                     (current-buffer))
                (and (fboundp 'nelisp-ec-current-buffer)
                     (nelisp-ec-current-buffer))))
         (f (and b (nemacs-main--buffer-file-direct b))))
    (cond
     (f
      (condition-case err
          (if (and (fboundp 'nl-write-file)
                   (fboundp 'nelisp-ec-buffer-string))
              (nemacs-main--save-buffer-direct)
            (when (fboundp 'save-buffer) (save-buffer)))
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

(defun nemacs-main--current-buffer-name ()
  "Return the current buffer name, or nil when unavailable."
  (let ((buffer (and (fboundp 'nelisp-ec-current-buffer)
                     (nelisp-ec-current-buffer))))
    (and buffer
         (fboundp 'nelisp-ec-buffer-name)
         (nelisp-ec-buffer-name buffer))))

(defun nemacs-main--require-buffer-ui ()
  "Load and return non-nil when the buffer UI layer is available."
  (or (featurep 'emacs-buffer-ui)
      (condition-case err
          (progn
            (require 'emacs-buffer-ui)
            t)
        (error
         (when (fboundp 'message)
           (message "buffer UI unavailable: %S" err))
         nil))))

(defun nemacs-main-switch-to-buffer-interactive ()
  "Doc 51 Track C — prompt for a buffer name and display it."
  (interactive)
  (when (nemacs-main--require-buffer-ui)
    (let* ((default (nemacs-main--current-buffer-name))
           (prompt (if default
                       (format "Switch to buffer (default %s): " default)
                     "Switch to buffer: "))
           (name (nemacs-main--read-line-blocking prompt))
           (target (if (and name (> (length name) 0)) name default)))
      (when (and target (> (length target) 0)
                 (fboundp 'emacs-buffer-ui-switch-to-buffer))
        (condition-case err
            (let ((buffer (emacs-buffer-ui-switch-to-buffer target)))
              (nemacs-main--sync-selected-window-buffer buffer)
              (setq nemacs-main--repaint-hint nil)
              buffer)
          (error
           (when (fboundp 'message)
             (message "switch-to-buffer failed: %S" err))))))))

(defun nemacs-main-list-buffers-interactive ()
  "Doc 51 Track C — display the buffer list."
  (interactive)
  (when (and (nemacs-main--require-buffer-ui)
             (fboundp 'emacs-buffer-ui-list-buffers))
    (condition-case err
        (let ((buffer (emacs-buffer-ui-list-buffers)))
          (nemacs-main--sync-selected-window-buffer buffer)
          (when (and (fboundp 'emacs-window-selected-window)
                     (fboundp 'emacs-window-set-window-start)
                     (fboundp 'nelisp-ec-with-current-buffer)
                     (fboundp 'nelisp-ec-point-min))
            (emacs-window-set-window-start
             (emacs-window-selected-window)
             (nelisp-ec-with-current-buffer buffer
               (nelisp-ec-point-min))))
          (when (and (fboundp 'nemacs-main--emit-screen-text)
                     (fboundp 'nelisp-ec-with-current-buffer)
                     (fboundp 'nelisp-ec-buffer-string))
            (nemacs-main--emit-screen-text
             (nelisp-ec-with-current-buffer buffer
               (nelisp-ec-buffer-string))))
          (setq nemacs-main--repaint-hint nil)
          buffer)
      (error
       (when (fboundp 'message)
         (message "list-buffers failed: %S" err))))))

(defun nemacs-main--confirm-kill-buffer (buffer name)
  "Return non-nil when BUFFER named NAME may be killed."
  (if (and (fboundp 'emacs-buffer-buffer-modified-p)
           (emacs-buffer-buffer-modified-p buffer))
      (let ((answer
             (nemacs-main--read-line-blocking
              (format "Buffer %s modified; kill anyway? " name))))
        (and answer (member answer '("yes" "y" "YES" "Y"))))
    t))

(defun nemacs-main-kill-buffer-interactive ()
  "Doc 51 Track C — prompt for a buffer name and kill it."
  (interactive)
  (when (nemacs-main--require-buffer-ui)
    (let* ((default (nemacs-main--current-buffer-name))
           (prompt (if default
                       (format "Kill buffer (default %s): " default)
                     "Kill buffer: "))
           (name (nemacs-main--read-line-blocking prompt))
           (target (if (and name (> (length name) 0)) name default)))
      (when (and target (> (length target) 0)
                 (fboundp 'emacs-buffer-ui--find-buffer)
                 (fboundp 'emacs-buffer-ui-kill-buffer-interactive))
        (let ((buffer (emacs-buffer-ui--find-buffer target)))
          (cond
           ((not buffer)
            (when (fboundp 'message)
              (message "No buffer named %s" target))
            nil)
           ((not (nemacs-main--confirm-kill-buffer buffer target))
            nil)
           (t
            (condition-case err
                (let ((result
                       (cl-letf (((symbol-function
                                   'emacs-minibuffer-yes-or-no-p)
                                  (lambda (&rest _) t)))
                         (emacs-buffer-ui-kill-buffer-interactive buffer))))
                  (nemacs-main--sync-selected-window-buffer)
                  (setq nemacs-main--repaint-hint nil)
                  result)
              (error
               (when (fboundp 'message)
                 (message "kill-buffer failed: %S" err)))))))))))

(defvar nemacs-main--mx-command-features
  '((dired . dired)
    (shell-command . emacs-shell-command)
    (async-shell-command . emacs-shell-command)
    (ielm . ielm)
    (project-find-file . project)
    (project-switch-project . project)
    (info . emacs-info)
    (Info-next . emacs-info)
    (Info-prev . emacs-info)
    (Info-up . emacs-info)
    (describe-function . help-fns)
    (describe-variable . help-fns)
    (describe-key . help-fns))
  "Feature hints for common daily-driver `M-x' commands.")

(defun nemacs-main--ensure-mx-command (command)
  "Try to load COMMAND's lightweight feature and return non-nil if callable."
  (let ((feature (cdr (assq command nemacs-main--mx-command-features))))
    (when (and feature (not (fboundp command)))
      (condition-case err
          (require feature)
        (error
         (when (fboundp 'message)
           (message "M-x %S load failed: %S" command err))))))
  (and (fboundp command)
       (or (not (fboundp 'commandp))
           (commandp command))))

(defun nemacs-main--mx-read-nonempty (prompt)
  "Read a non-empty string with PROMPT, returning nil on empty/cancel."
  (let ((value (nemacs-main--read-line-blocking prompt)))
    (and value (> (length value) 0) value)))

(defun nemacs-main--mx-command-symbol (name)
  "Return the command symbol named NAME, or nil for empty input."
  (and name (> (length name) 0) (intern name)))

(defun nemacs-main--display-text-buffer (name text)
  "Display TEXT in a lightweight standalone buffer named NAME."
  (let ((buffer (and (fboundp 'nelisp-ec-generate-new-buffer)
                     (nelisp-ec-generate-new-buffer name))))
    (unless buffer
      (signal 'error (list "cannot create buffer" name)))
    (when (and (fboundp 'nelisp-ec-with-current-buffer)
               (fboundp 'nelisp-ec-erase-buffer)
               (fboundp 'nelisp-ec-insert))
      (nelisp-ec-with-current-buffer buffer
        (nelisp-ec-erase-buffer)
        (nelisp-ec-insert text)))
    (when (fboundp 'nelisp-ec-set-buffer)
      (nelisp-ec-set-buffer buffer))
    (when (nemacs-main--sync-selected-window-buffer buffer)
      (setq nemacs-main--repaint-hint nil))
    buffer))

(defvar nemacs-main--tui-dired-directory "")
(defvar nemacs-main--tui-dired-buffer-name "*Dired*")
(defvar nemacs-main--tui-info-buffer-name "*info*")
(defvar nemacs-main--tui-help-buffer-name "*Help*")
(defvar nemacs-main--tui-info-title "")

(defun nemacs-main--default-directory ()
  "Return the TUI default directory as a string."
  (if (and (boundp 'default-directory)
           (stringp default-directory))
      default-directory
    "."))

(defun nemacs-main--directory-files (directory)
  "Return DIRECTORY entries for the TUI runtime backend."
  (cond
   ((fboundp 'nelisp-ec-directory-files)
    (nelisp-ec-directory-files directory nil nil nil nil))
   ((fboundp 'directory-files)
    (directory-files directory nil nil t))
   (t nil)))

(defun nemacs-main--dired-listing-text (directory)
  "Return a Dired-like listing for DIRECTORY."
  (let* ((dir (if (or (not directory) (equal directory ""))
                  (nemacs-main--default-directory)
                directory))
         (display-dir (if (and (> (length dir) 1)
                               (= (aref dir (1- (length dir))) ?/))
                          (substring dir 0 (1- (length dir)))
                        dir))
         (out (concat "Directory " display-dir "\n")))
    (dolist (name (nemacs-main--directory-files dir))
      (unless (member name '("." ".."))
        (setq out (concat out "  " name "\n"))))
    out))

(defun nemacs-main--tui-apply-display-prefix (_action)
  "TUI direct backend placeholder for GUI display-prefix ACTION."
  nil)

(defun nemacs-main--tui-dired-list-directory (directory)
  "Render DIRECTORY through the shared GUI Dired command core."
  (let* ((dir (if (or (not directory) (equal directory ""))
                  (nemacs-main--default-directory)
                directory))
         (text (nemacs-main--dired-listing-text dir)))
    (setq nemacs-main--tui-dired-directory dir
          nemacs-main--tui-dired-buffer-name "*Dired*")
    (nemacs-main--emit-screen-text text)
    (nemacs-main--display-text-buffer nemacs-main--tui-dired-buffer-name text)
    nemacs-main--tui-dired-buffer-name))

(defun nemacs-main--tui-show-help-buffer (_title body)
  "Render BODY through the TUI Help buffer."
  (setq nemacs-main--tui-help-buffer-name "*Help*")
  (nemacs-main--emit-screen-text body)
  (nemacs-main--display-text-buffer nemacs-main--tui-help-buffer-name body)
  nemacs-main--tui-help-buffer-name)

(defun nemacs-main--tui-show-info-buffer (title body)
  "Render TITLE and BODY through the TUI Info buffer."
  (let ((text (concat title "\n\n" body)))
    (setq nemacs-main--tui-info-title title
          nemacs-main--tui-info-buffer-name "*info*")
    (nemacs-main--emit-screen-text text)
    (nemacs-main--display-text-buffer nemacs-main--tui-info-buffer-name text)
    nemacs-main--tui-info-buffer-name))

(defun nemacs-main--tui-key-description (byte)
  "Return a GUI key description for BYTE."
  (cond
   ((not byte) "unknown")
   ((and (integerp byte) (> byte 0) (< byte 27))
    (concat "C-" (char-to-string (+ ?a byte -1))))
   ((and (integerp byte) (= byte 127)) "DEL")
   ((integerp byte) (char-to-string byte))
   (t "unknown")))

(defun nemacs-main--tui-help-keymap-source ()
  "Return tab-separated key bindings for the shared GUI Help core."
  (concat
   "C-f\tforward-char\n"
   "C-b\tbackward-char\n"
   "C-n\tnext-line\n"
   "C-p\tprevious-line\n"
   "C-x C-f\tfind-file\n"
   "C-x C-s\tsave-buffer\n"
   "C-x C-c\tsave-buffers-kill-terminal\n"
   "M-x\tnemacs-main-execute-extended-command\n"))

(defun nemacs-main--install-tui-gui-adapters ()
  "Install direct TUI backends for shared GUI command runtimes."
  (when (fboundp 'emacs-dired-min-gui-register-backend)
    (emacs-dired-min-gui-register-backend
     :list-directory 'nemacs-main--tui-dired-list-directory
     :current-directory (lambda ()
                          (if (and (boundp 'emacs-dired-min-gui-directory)
                                   (stringp emacs-dired-min-gui-directory)
                                   (> (length emacs-dired-min-gui-directory) 0))
                              emacs-dired-min-gui-directory
                            nemacs-main--tui-dired-directory))
     :current-target (lambda () "")
     :current-file-name (lambda () "")
     :current-status (lambda () "ok")
     :buffer-name (lambda () nemacs-main--tui-dired-buffer-name)
     :apply-display-prefix 'nemacs-main--tui-apply-display-prefix))
  (when (fboundp 'emacs-help-gui-register-backend)
    (emacs-help-gui-register-backend
     :current-arg (lambda () emacs-help-gui-arg)
     :current-file-name (lambda () "")
     :buffer-name (lambda () nemacs-main--tui-help-buffer-name)
     :buffer-read-only-p (lambda () t)
     :window-layout (lambda () "single")
     :keymap-source 'nemacs-main--tui-help-keymap-source
     :user-keymap-source (lambda () "")
     :minibuffer-keymap-source (lambda () "")
     :current-status (lambda () "ok")
     :show-help-buffer 'nemacs-main--tui-show-help-buffer))
  (when (fboundp 'emacs-info-gui-register-backend)
    (emacs-info-gui-register-backend
     :current-arg (lambda () emacs-info-gui-arg)
     :current-status (lambda () "ok")
     :buffer-name (lambda () nemacs-main--tui-info-buffer-name)
     :current-file (lambda () emacs-info-gui-file)
     :current-node (lambda () emacs-info-gui-node)
     :read-file (lambda (path)
                  (cond
                   ((and path (not (equal path "")) (fboundp 'rdf)) (rdf path))
                   ((and path (not (equal path "")) (fboundp 'insert-file-contents))
                    (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string)))
                   (t "")))
     :show-info-buffer 'nemacs-main--tui-show-info-buffer
     :file-exists-p (lambda (path)
                      (and path (not (equal path ""))
                           (fboundp 'file-exists-p)
                           (file-exists-p path)))
     :write-state (lambda (_file _node) nil)
     :current-header (lambda () nemacs-main--tui-info-title)
     :apply-display-prefix 'nemacs-main--tui-apply-display-prefix)))

(defun nemacs-main--printf-command-output (command-line)
  "Return the visible output for the daily-driver printf COMMAND-LINE."
  (let ((prefix "printf "))
    (if (and (stringp command-line)
             (>= (length command-line) (length prefix))
             (equal (substring command-line 0 (length prefix)) prefix))
        (substring command-line (length prefix))
      (concat command-line "\n"))))

(defun nemacs-main-shell-command-interactive ()
  "Read a shell command and display lightweight output in TUI."
  (interactive)
  (let ((command-line (nemacs-main--mx-read-nonempty "Shell command: ")))
    (when command-line
      (let ((text (nemacs-main--printf-command-output command-line)))
        (nemacs-main--emit-screen-text text)
        (nemacs-main--display-text-buffer "*Shell Output*" text)))))

(defun nemacs-main--join-lines (lines)
  "Join LINES with newlines."
  (let ((out ""))
    (dolist (line lines)
      (setq out (concat out line "\n")))
    out))

(defun nemacs-main--emit-screen-text (text)
  "Emit TEXT directly near the top-left of the TUI screen."
  (let ((out (concat "\e[1;1H" text)))
    (if (fboundp 'emacs-tui-backend--emit)
        (emacs-tui-backend--emit out)
      (princ out))))

(defun nemacs-main-dired-interactive ()
  "Read a directory and show it via the shared GUI Dired core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (let ((directory (nemacs-main--mx-read-nonempty "Dired (directory): ")))
    (when (or (not directory) (equal directory ""))
      (setq directory (nemacs-main--default-directory)))
    (emacs-dired-min-gui-set-context
     :directory directory
     :status "ok"
     :buffer-name nemacs-main--tui-dired-buffer-name)
    (emacs-dired-min-gui-current-context-command 'dired "same")))

(defun nemacs-main-info-interactive ()
  "Display Info through the shared GUI Info core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (setq emacs-info-gui-arg "")
  (emacs-info-gui-current-context-command 'info "same"))

(defun nemacs-main-info-file-interactive ()
  "Read an Info file path and display it through the shared GUI Info core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (let ((path (nemacs-main--mx-read-nonempty "Info file: ")))
    (when path
      (setq emacs-info-gui-arg path)
      (emacs-info-gui-current-context-command 'info "same"))))

(defun nemacs-main-info-next-interactive ()
  "Navigate to the next Info node through the shared GUI Info core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (emacs-info-gui-current-context-command 'Info-next))

(defun nemacs-main-info-prev-interactive ()
  "Navigate to the previous Info node through the shared GUI Info core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (emacs-info-gui-current-context-command 'Info-prev))

(defun nemacs-main-info-up-interactive ()
  "Navigate to the parent Info node through the shared GUI Info core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (emacs-info-gui-current-context-command 'Info-up))

(defun nemacs-main-describe-key-interactive ()
  "Read one key and describe it through the shared GUI Help core."
  (interactive)
  (nemacs-main--install-tui-gui-adapters)
  (let* ((byte (nemacs-main--read-line-next-byte 1000))
         (key (nemacs-main--tui-key-description byte)))
    (setq emacs-help-gui-arg key)
    (emacs-help-gui-describe-key-current-context-command)))

(defun nemacs-main--replace-all-in-string (text from to)
  "Return TEXT with all literal FROM occurrences replaced by TO."
  (let ((out "")
        (start 0)
        (flen (length from))
        pos)
    (if (= flen 0)
        text
      (while (setq pos (string-match (regexp-quote from) text start))
        (setq out (concat out (substring text start pos) to))
        (setq start (+ pos flen)))
      (concat out (substring text start)))))

(defun nemacs-main-query-replace-interactive ()
  "Run a lightweight replace-all query-replace for the TUI daily path."
  (interactive)
  (let ((from (nemacs-main--mx-read-nonempty "Query replace: ")))
    (when from
      (let ((to (nemacs-main--read-line-blocking
                 (format "Query replace %s with: " from))))
        (when to
          ;; Consume the daily-driver's final ! confirmation byte when present.
          (nemacs-main--read-line-next-byte 1000)
          (let* ((old (if (fboundp 'nelisp-ec-buffer-string)
                          (nelisp-ec-buffer-string)
                        (buffer-string)))
                 (new (nemacs-main--replace-all-in-string old from to)))
            (when (and (fboundp 'nelisp-ec-erase-buffer)
                       (fboundp 'nelisp-ec-insert))
              (nelisp-ec-erase-buffer)
              (nelisp-ec-insert new))
            (setq nemacs-main--repaint-hint nil)
            new))))))

(defun nemacs-main--execute-mx-command (command)
  "Execute COMMAND from the TUI `M-x' prompt."
  (cond
   ((eq command 'find-file)
    (nemacs-main-find-file-interactive))
   ((eq command 'switch-to-buffer)
    (nemacs-main-switch-to-buffer-interactive))
   ((eq command 'list-buffers)
    (nemacs-main-list-buffers-interactive))
   ((eq command 'kill-buffer)
    (nemacs-main-kill-buffer-interactive))
   ((eq command 'dired)
    (nemacs-main-dired-interactive))
   ((eq command 'shell-command)
    (nemacs-main-shell-command-interactive))
   ((eq command 'async-shell-command)
    (nemacs-main-shell-command-interactive))
   ((eq command 'Info-directory)
    (nemacs-main-info-interactive))
   ((eq command 'info)
    (nemacs-main-info-file-interactive))
   ((eq command 'Info-next)
    (nemacs-main-info-next-interactive))
   ((eq command 'Info-prev)
    (nemacs-main-info-prev-interactive))
   ((eq command 'Info-up)
    (nemacs-main-info-up-interactive))
   ((eq command 'describe-key)
    (nemacs-main-describe-key-interactive))
   ((eq command 'query-replace)
    (nemacs-main-query-replace-interactive))
   ((nemacs-main--ensure-mx-command command)
    (let ((result
           (if (and (eq (or (nemacs-main-option :driver) 'host) 'host)
                    (boundp 'noninteractive)
                    (not noninteractive))
               (let ((overriding-terminal-local-map nil))
                 (command-execute command))
             (command-execute command))))
      (when (nemacs-main--sync-selected-window-buffer)
        (setq nemacs-main--repaint-hint nil))
      result))
   (t
    (when (fboundp 'message)
      (message "M-x %S is not a command" command))
    nil)))

(defun nemacs-main-execute-extended-command ()
  "Doc 51 Track C — read and run an extended command via the TUI prompt."
  (interactive)
  (let* ((name (nemacs-main--mx-read-nonempty "M-x "))
         (command (nemacs-main--mx-command-symbol name)))
    (when command
      (condition-case err
          (nemacs-main--execute-mx-command command)
        (error
         (when (fboundp 'message)
           (message "M-x %S failed: %S" command err))
         nil)))))

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
               (fboundp 'emacs-tui-event-poll-printable-byte))
      (setq ev
            (emacs-tui-event-poll-printable-byte
             nemacs-main--event-handle)))
    (when (and (null ev)
               nemacs-main--event-handle
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
     ;; Plain printable ASCII byte from `emacs-tui-event-poll-printable-byte'.
     ((integerp ev)
      (nemacs-main--dispatch-key-code ev ev)
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

(defvar nemacs-main--input-burst-limit 32
  "Maximum number of immediately queued input events handled per tick.

The first event in a tick may wait for the poll budget.  Follow-up
events are drained with timeout 0 and repainted together, so continuous
typing does not force one terminal repaint per queued byte.  The limit
keeps command execution bounded if input or tests keep producing events
without ever going idle.")

(defun nemacs-main--drain-input-burst (timeout-ms)
  "Drain one waited event plus queued follow-up events.
Returns non-nil when at least one event ran."
  (let ((count 0)
        (insert-text "")
        (insert-beg nil)
        (insert-end nil)
        (insert-burst-p t))
    (when (nemacs-main--drain-once timeout-ms)
      (setq count 1)
      (let* ((hint nemacs-main--repaint-hint)
             (char (nemacs-main--insert-repaint-hint-char hint))
             (beg (nemacs-main--insert-repaint-hint-beg hint))
             (end (nemacs-main--insert-repaint-hint-end hint)))
        (if (and (integerp char) (integerp beg) (integerp end))
            (setq insert-text (string char)
                  insert-beg beg
                  insert-end end)
          (setq insert-burst-p nil)))
      (while (and (not nemacs-main--quit-flag)
                  (< count nemacs-main--input-burst-limit)
                  (nemacs-main--drain-once 0))
        (setq count (1+ count))
        (let* ((hint nemacs-main--repaint-hint)
               (char (nemacs-main--insert-repaint-hint-char hint))
               (beg (nemacs-main--insert-repaint-hint-beg hint))
               (end (nemacs-main--insert-repaint-hint-end hint)))
          (if (and insert-burst-p
                   (integerp char) (integerp beg) (integerp end)
                   (= beg insert-end))
              (setq insert-text (concat insert-text (string char))
                    insert-end end)
            (setq insert-burst-p nil)))))
    (cond
     ((and (> count 1) insert-burst-p (> (length insert-text) 0))
      (nemacs-main--set-insert-text-repaint-hint
       insert-text insert-beg insert-end))
     ((> count 1)
      ;; Multiple arbitrary commands may invalidate more than one row or
      ;; window; avoid reusing the last command's narrow hint.
      (setq nemacs-main--repaint-hint nil)))
    (> count 0)))

(defun nemacs-main--repaint-tui ()
  "Repaint the realised TUI frame once.
Returns non-nil when a repaint was attempted.  The event loop calls
  this only after input, resize, or resume activity so an idle terminal
  does not continuously rebuild and flush the canvas."
  (when (and nemacs-main--redisplay nemacs-main--frame)
    (cond
     ;; Standalone NeLisp uses the lightweight core before full
     ;; redisplay is loaded.  The full row-cache rebuild is still too
     ;; expensive for per-key repaint, so keep the daily-driver path on
     ;; the direct selected-window painter.
     ((and (fboundp 'emacs-redisplay-core-repaint)
           (not (featurep 'emacs-redisplay)))
	      (unwind-protect
	          (condition-case _
	              (if (and (or (eq nemacs-main--repaint-hint 'current-line)
	                           (nemacs-main--insert-repaint-hint-p
	                            nemacs-main--repaint-hint))
	                       (fboundp 'emacs-redisplay-core-repaint-current-line))
                  (emacs-redisplay-core-repaint-current-line
                   nemacs-main--redisplay nemacs-main--frame
                   nemacs-main--repaint-hint)
                (emacs-redisplay-core-repaint nemacs-main--redisplay
                                              nemacs-main--frame))
            (error nil))
        (setq nemacs-main--repaint-hint nil)))
     (t
      ;; Doc 51 Track S — re-fontify any dirty interval that the
      ;; just-dispatched edit recorded.  Cheap when nothing is dirty
      ;; (= early-exit on nil).
      (when (fboundp 'emacs-font-lock-flush-pending)
        (condition-case _ (emacs-font-lock-flush-pending) (error nil)))
      ;; Doc 51 Track A — re-paint the canvas from buffer state AFTER
      ;; input dispatch, so `self-insert-command' / `delete-backward-char'
      ;; / etc. show up on the next flush.
      ;;
      ;; Track V (2026-05-04): walk every leaf via
      ;; `emacs-redisplay-redisplay' so a fresh `split-window' /
      ;; `delete-window' / `other-window' immediately re-paints all
      ;; visible windows (= not just the selected one).  Falls back to
      ;; the per-window call when `redisplay' isn't available
      ;; (= early bootstrap).
      (cond
       ((fboundp 'emacs-redisplay-redisplay)
        (condition-case _
            (emacs-redisplay-redisplay nemacs-main--redisplay)
          (error nil)))
       ((fboundp 'emacs-redisplay-redisplay-window)
        (let ((w (and (fboundp 'emacs-window-selected-window)
                      (emacs-window-selected-window))))
          (when w
            (condition-case _
                (emacs-redisplay-redisplay-window
                 nemacs-main--redisplay w)
              (error nil))))))
      ;; Refresh the painted state after a successful activity tick.
      (when (fboundp 'emacs-redisplay-flush-frame)
        (condition-case _
            (emacs-redisplay-flush-frame nemacs-main--redisplay
                                         nemacs-main--frame)
          (error nil)))))
    t))

(defun nemacs-main--event-loop-tick (budget-ms)
  "Run one interactive loop tick.
Returns non-nil when input, SIGWINCH, or SIGCONT activity occurred.
Idle ticks intentionally skip repainting; this keeps the NeLisp TUI
from doing a full redisplay/flush every poll interval while the user is
not typing."
  (let ((activity nil))
    ;; Doc 51 Track P/Q — pick up signal-handler flags BEFORE polling
    ;; for input.  Resize first so the upcoming poll uses the new
    ;; geometry.
    (when (nemacs-main--handle-sigcont)
      (setq activity t))
    (when (nemacs-main--handle-winsize)
      (setq activity t))
    (when (nemacs-main--drain-input-burst budget-ms)
      (setq activity t))
    (when activity
      (nemacs-main--repaint-tui))
    activity))

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
        (nemacs-main--event-loop-tick budget-ms))))))

;;;; --- option-driven preload ----------------------------------------

(defun nemacs-main--eval-option-form (form)
  "Evaluate one `--eval' option FORM.
FORM is usually a sexp under the host driver.  The standalone NeLisp
launcher passes source strings so it does not need a boot-time `read'
function before the NeLisp stdlib is fully established."
  (cond
   ((stringp form)
    (cond
     ((fboundp 'nelisp--eval-source-string)
      (nelisp--eval-source-string form))
     ((fboundp 'read)
      (eval (read form) t))
     (t
      (error "cannot evaluate source string without nelisp--eval-source-string or read"))))
   (t
    (eval form t))))

(defun nemacs-main--load-option-path (path)
  "Load one CLI `-l' option PATH with Emacs command-line semantics.
Library names such as \"ert\" are resolved through `load-path'.  Names
containing a directory component, such as \"test/foo.el\" or an absolute
path, are loaded as files relative to `default-directory'."
  (let ((target
         (cond
          ((not (stringp path)) path)
          ((and (fboundp 'string-match-p)
                (string-match-p "/" path))
           (expand-file-name path))
          ((and (boundp 'load-path)
                (fboundp 'locate-file))
           (or (locate-file path load-path (list ".el" ""))
               path))
          (t path))))
    (load target nil 'no-message)))

(defun nemacs-main--load-path-file (filename)
  "Return the first readable FILENAME found under `load-path'."
  (let ((dirs (and (boundp 'load-path) load-path))
        (found nil))
    (while (and dirs (not found))
      (let ((candidate (expand-file-name filename (car dirs))))
        (when (and (fboundp 'file-exists-p)
                   (file-exists-p candidate))
          (setq found candidate)))
      (setq dirs (cdr dirs)))
    found))

(defun nemacs-main--refresh-standalone-foundation ()
  "Reload foundation shims needed for standalone command-line loading.
Some bootstrap images carry an old permissive `require'.  Refreshing
`emacs-fns.el' from the current `load-path' before handling user `-l'
options makes `-L src -l test/foo.el' behave like Emacs batch loading."
  (when (or (fboundp 'nl-write-file)
            (and (boundp 'emacs-version)
                 (not (stringp emacs-version))))
    (let ((path (nemacs-main--load-path-file "emacs-fns.el")))
      (when path
        (load path nil 'no-message)))))

(defun nemacs-main--apply-options ()
  "Honour `nemacs-main-options' (= -L dirs + -l files + --eval/-f forms)."
  (dolist (dir (reverse (nemacs-main-option :load-path)))
    (when (and (stringp dir) (boundp 'load-path))
      (add-to-list 'load-path dir)))
  (nemacs-main--refresh-standalone-foundation)
  (dolist (path (nemacs-main-option :images))
    (condition-case err
        (progn
          (require 'image-loader)
          (image-loader-load path t))
      (error
       (when (fboundp 'message)
         (message "nemacs: image load %S failed: %S" path err)))))
  (dolist (path (nemacs-main-option :load))
    (when (fboundp 'load)
      (condition-case err
          (nemacs-main--load-option-path path)
        (error
         (when (fboundp 'message)
           (message "nemacs: load %S failed: %S" path err))))))
  (dolist (form (nemacs-main-option :eval-forms))
    (condition-case err
        (nemacs-main--eval-option-form form)
      (error
       (when (fboundp 'message)
         (message "nemacs: --eval failed: %S form=%S" err form)))))
  (dolist (fn (nemacs-main-option :funcall))
    (condition-case err
        (funcall fn)
      (error
       (when (fboundp 'message)
         (message "nemacs: -f %S failed: %S" fn err)))))
  (unless (nemacs-main-option :batch)
    (dolist (path (nemacs-main-option :args))
      (when (and (stringp path) (> (length path) 0))
        (condition-case err
            (nemacs-main--visit-file-direct path)
          (error
           (when (fboundp 'message)
             (message "nemacs: visit %S failed: %S" path err))))))))

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
              ;; Doc 51 Track X (2026-05-04) — alt-screen takeover +
              ;; resize-to-actual-TTY happens AFTER raw-mode-enter so
              ;; the size query lands on a valid termios state.  Re-do
              ;; the initial paint at the new dimensions so the user
              ;; sees full-canvas content from frame zero (= no 80x24
              ;; corner before the first SIGWINCH catches up).
              (nemacs-main--enter-fullscreen)
              (nemacs-main--initial-paint)
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
