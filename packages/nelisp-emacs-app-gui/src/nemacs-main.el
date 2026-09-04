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
(require 'emacs-replace)
(require 'emacs-shell-command)
(require 'emacs-command-loop)
(require 'emacs-fileio-builtins)
(require 'emacs-keymap)
(require 'emacs-startup-screen)

(declare-function emacs-buffer-ui-confirm-kill-buffer "emacs-buffer-ui"
                  (buffer name read-confirmation-function))
(declare-function emacs-buffer-ui-run-switch-buffer-command "emacs-buffer-ui"
                  (&rest plist))
(declare-function emacs-buffer-ui-run-list-buffers-command "emacs-buffer-ui"
                  (&rest plist))
(declare-function emacs-buffer-ui-run-kill-buffer-command "emacs-buffer-ui"
                  (&rest plist))
(declare-function emacs-fileio-buffer-file-direct "emacs-fileio-builtins"
                  (&optional buffer))
(declare-function emacs-fileio-run-find-file-command "emacs-fileio-builtins"
                  (&rest plist))
(declare-function emacs-fileio-run-save-buffer-command "emacs-fileio-builtins"
                  (&rest plist))
(declare-function emacs-fileio-visit-file-direct "emacs-fileio-builtins"
                  (path))

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
        ;; Doc 06 A1: route Elisp `read-event' through live TUI stdin.
        (when (boundp 'emacs-command-loop-input-poll-function)
          (setq emacs-command-loop-input-poll-function
                #'nemacs-main--poll-input-event))
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

(defalias 'nemacs-main--keymap-slot-vector
  #'emacs-keymap-direct-slot-vector)

(defun nemacs-main--define-key (keymap key def)
  "Bind KEY to DEF in KEYMAP, using a fast full-keymap slot when possible."
  (emacs-keymap-define-key-fast
   keymap key def (nemacs-main--keymap-slot-vector keymap)))

(defvar nemacs-main--single-key-cache nil
  "Vector cache for direct ASCII key lookup on the owned global keymap.")

(defvar nemacs-main--single-key-cache-map nil
  "The keymap object represented by `nemacs-main--single-key-cache'.")

(defun nemacs-main--rebuild-single-key-cache (keymap)
  "Rebuild direct ASCII lookup cache for KEYMAP."
  (let ((cache (emacs-keymap-build-single-key-cache keymap)))
    (setq nemacs-main--single-key-cache cache
          nemacs-main--single-key-cache-map keymap)
    cache))

(defalias 'nemacs-main--make-full-keymap
  #'emacs-keymap-make-compatible-full-keymap)

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

(defalias 'nemacs-main--init-keymap
  (lambda ()
    "Construct `nemacs-main--global-keymap' if not yet built.
Idempotent — safe to call multiple times.  Returns the keymap.

Doc 51 Track A (2026-05-04): bind ASCII printable + RET to
self-insert-command / newline so a freshly booted nemacs is
typeable.  C-x C-c / C-c C-q / C-g remain the kill / quit
keys."
    (unless nemacs-main--global-keymap
      (let ((m
             (emacs-command-loop-build-standard-keymap
              :make-full-keymap #'nemacs-main--make-full-keymap
              :slot-vector #'nemacs-main--keymap-slot-vector
              :define-key #'nemacs-main--define-key
              :quit-command 'nemacs-main-kill
              :c-x-command-alist
              '((find-file . nemacs-main-find-file-interactive)
                (save-buffer . nemacs-main-save-buffer-interactive)
                (switch-to-buffer . nemacs-main-switch-to-buffer-interactive)
                (list-buffers . nemacs-main-list-buffers-interactive)
                (kill-buffer . nemacs-main-kill-buffer-interactive)
                (quit . nemacs-main-kill)
                (split-window-below . split-window-below)
                (split-window-right . split-window-right)
                (delete-window . delete-window)
                (delete-other-windows . delete-other-windows)
                (other-window . other-window))
              ;; ESC+x / Alt+x reaches the event loop as a single
              ;; Meta-modified printable event from `emacs-tui-event'.
              :extra-bindings
              (list
               (cons (vector (logior nemacs-main--meta-modifier-mask ?x))
                     'nemacs-main-execute-extended-command)
               (cons (vector (logior nemacs-main--meta-modifier-mask ?!))
                     'nemacs-main-shell-command-interactive))
              :help-command-alist
              '((describe-key . nemacs-main-describe-key-interactive)
                (describe-bindings . emacs-help-gui-describe-bindings-current-context-command)
                (describe-function . emacs-help-gui-describe-function-prompt-command)
                (describe-variable . emacs-help-gui-describe-variable-prompt-command)
                (apropos . emacs-help-gui-apropos-command-prompt-command))
              :help-command-bound-p (lambda (_command) t))))
        (setq nemacs-main--global-keymap m)
        (nemacs-main--rebuild-single-key-cache m)))
    nemacs-main--global-keymap))

(defalias 'nemacs-main--ensure-keymap-after-feature-load
  (lambda ()
    "Ensure lazy-loaded editor commands are reflected in the keymap.

Runtime images may call `nemacs-main--init-keymap' before the editor
command modules have been loaded.  In that case printable keys and RET
were intentionally skipped because their commands were not `fboundp'
yet.  After `nemacs-main--prepare-tui-state' loads those modules, rebuild
the owned keymap if those core bindings are still missing."
    (emacs-command-loop-ensure-keymap-bindings
     :keymap nemacs-main--global-keymap
     :required-bindings
     `((,(vector ?a) . self-insert-command)
       (,(vector 13) . newline))
     :lookup-key (lambda (_keymap key)
                   (nemacs-main--lookup-key-vec key))
     :clear-keymap
     (lambda ()
       (setq nemacs-main--global-keymap nil
             nemacs-main--single-key-cache nil
             nemacs-main--single-key-cache-map nil))
     :init-keymap #'nemacs-main--init-keymap)))

(defalias 'nemacs-main--install-keymap-host
  (lambda ()
    "Install `nemacs-main--global-keymap' as the host Emacs override.
On host driver (= interactive Emacs) this lets us own `C-x C-c'
without disturbing the user's global map.  The override is bound
via `overriding-terminal-local-map' so it persists across mode
switches; callers should clear it from `nemacs-main--shutdown-tui'."
    (emacs-keymap-install-overriding-terminal-map
     (nemacs-main--init-keymap)
     (and (fboundp 'current-global-map)
          (current-global-map)))))

(defalias 'nemacs-main--uninstall-keymap-host
  (lambda ()
    "Reverse of `nemacs-main--install-keymap-host'."
    (emacs-keymap-clear-overriding-terminal-map)))

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
         (fboundp 'emacs-keymap-lookup-with-parent))
    (emacs-keymap-lookup-with-parent nemacs-main--global-keymap key))
   (t
    (nemacs-main--lookup-key-vec (vector key)))))

(defconst nemacs-main--direct-tui-commands
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
    emacs-help-gui-describe-bindings-current-context-command
    emacs-help-gui-describe-function-prompt-command
    emacs-help-gui-describe-variable-prompt-command
    emacs-help-gui-apropos-command-prompt-command
    emacs-help-gui-apropos-documentation-prompt-command)
  "Commands that should run by direct `funcall' in the boot TUI.")

(defalias 'nemacs-main--direct-tui-command-p
  (lambda (binding)
    "Return non-nil when BINDING should run directly in the boot TUI.
The standalone `command-execute' shim is still catching up with Emacs'
interactive calling convention.  These commands are implemented in this
module specifically for the `-nw' event loop, so direct `funcall' keeps
the boot path deterministic."
    (emacs-command-loop-key-dispatch-direct-command-p
     binding nemacs-main--direct-tui-commands)))

(defun nemacs-main--apply-self-insert-edit-result (key edit)
  "Apply self-insert EDIT for KEY to TUI repaint hints.
Return the edit end point, or nil when EDIT does not describe a range."
  (let ((beg (plist-get edit :beg))
        (end (plist-get edit :end)))
    (when (and beg end)
      (nemacs-main--set-insert-repaint-hint key beg end))
    end))

(defalias 'nemacs-main--execute-printable-self-insert
  (lambda (key)
    "Execute printable self-insert KEY without `command-execute'.
The normal `command-execute' path is semantically general but expensive
under standalone NeLisp because it has to inspect the interactive form
and build an argument list.  For a bare printable key the argument list
is already known: repeat count 1 and the character itself.
Return the new point for the inlined fast path, or nil when it falls
back to `self-insert-command'."
    (emacs-command-loop-key-dispatch-run-self-insert
     key
     (lambda () (emacs-edit-self-insert-direct key t))
     (lambda (edit)
       (nemacs-main--apply-self-insert-edit-result key edit)))))

(defalias 'nemacs-main--dispatch-printable-self-insert-direct
  (lambda (key)
    "Dispatch printable self-insert KEY on the event-loop fast path."
    (emacs-command-loop-key-dispatch-run-plan
     (list :kind 'self-insert
           :binding 'self-insert-command
           :event key
           :next-prefix [])
     :set-prefix (lambda (prefix)
                   (setq nemacs-main--prefix-keys prefix))
     :set-last-command-event
     (lambda (event)
       (when (boundp 'last-command-event)
         (setq last-command-event event)))
     :run-self-insert
     (lambda (event _plan)
       (nemacs-main--execute-printable-self-insert event))
     :after-self-insert
     (lambda (_point _plan)
       (unless nemacs-main--repaint-hint
         (setq nemacs-main--repaint-hint 'current-line)))
     :after-command
     (lambda (point-after _plan)
       (nemacs-main--sync-selected-window-point point-after))
     :on-quit #'nemacs-main--quit
     :on-self-insert-error
     (lambda (_binding _err)
       (when (fboundp 'message)
         (message "command error during self-insert"))))))

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

(defalias 'nemacs-main--dispatch-key-code
  (lambda (key &optional source-event)
    "Process a single KEY through the keymap.
SOURCE-EVENT is the original tui-event plist when one exists.

Accumulates KEY into `nemacs-main--prefix-keys', looks the result up
in `nemacs-main--global-keymap', and:
  - Runs the command via `command-execute' on a non-keymap binding,
    then clears the prefix.
  - Keeps the prefix growing on a keymap binding (= prefix key).
  - Clears the prefix on an unbound sequence (= give up gracefully)."
    (emacs-command-loop-key-dispatch-run-plan
     (emacs-command-loop-key-dispatch-plan
      :events (vector key)
      :prefix nemacs-main--prefix-keys
      :lookup-single #'nemacs-main--lookup-single-key
      :lookup-sequence #'nemacs-main--lookup-key-vec)
     :source-event source-event
     :set-prefix (lambda (prefix)
                   (setq nemacs-main--prefix-keys prefix))
     :set-last-command-event
     (lambda (event)
       (when (boundp 'last-command-event)
         (setq last-command-event event)))
     :run-self-insert
     (lambda (event _plan)
       (nemacs-main--execute-printable-self-insert event))
     :after-self-insert
     (lambda (_point _plan)
       (unless nemacs-main--repaint-hint
         (setq nemacs-main--repaint-hint 'current-line)))
     :inline-edit-commands '((self-insert-command . self-insert))
     :direct-command-p #'nemacs-main--direct-tui-command-p
     :command-execute (and (fboundp 'command-execute) #'command-execute)
     :after-command
     (lambda (point-after plan)
       (when (and (eq (plist-get plan :kind) 'command)
                  (nemacs-main--sync-selected-window-buffer))
         ;; A command such as find-file changed the displayed buffer; force
         ;; the next repaint to rebuild from the new window contents.
         (setq nemacs-main--repaint-hint nil))
       (nemacs-main--sync-selected-window-point point-after))
     :on-quit #'nemacs-main--quit
     :on-error
     (lambda (binding err)
       (when (fboundp 'message)
         (message "command %S failed: %S" binding err)))
     :on-direct-error
     (lambda (binding dispatch)
       (when (fboundp 'message)
         (message "command %S failed: %s"
                  binding
                  (plist-get dispatch :message)))))))

(defalias 'nemacs-main--dispatch-key-event
  (lambda (ev)
    "Process a single key EV through the keymap.
EV may be the usual tui-event plist or a plain integer key code from the
printable-byte fast path."
    (nemacs-main--dispatch-key-code (nemacs-main--key-event->key ev) ev)))

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
    (if (fboundp 'emacs-tui-backend-emit)
        (emacs-tui-backend-emit out)
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

(defun nemacs-main--run-file-visit ()
  "Run the TUI find-file command through the shared IO helper."
  (interactive)
  (emacs-fileio-run-find-file-command
   :read-string #'nemacs-main--read-line-blocking
   :sync-window (lambda (buffer)
                  (when (nemacs-main--sync-selected-window-buffer buffer)
                    (setq nemacs-main--repaint-hint nil)))
   :message-function #'message))

(defalias 'nemacs-main-find-file-interactive
  #'nemacs-main--run-file-visit)

(defun nemacs-main--run-file-save ()
  "Run the TUI save-buffer command through the shared IO helper."
  (interactive)
  (emacs-fileio-run-save-buffer-command
   :read-string #'nemacs-main--read-line-blocking
   :current-buffer
   (lambda ()
     (or (and (eq (or (nemacs-main-option :driver) 'host) 'host)
              (boundp 'noninteractive)
              (not noninteractive)
              (fboundp 'current-buffer)
              (current-buffer))
         (and (fboundp 'nelisp-ec-current-buffer)
              (nelisp-ec-current-buffer))))
   :file-function #'emacs-fileio-buffer-file-direct
   :message-function #'message))

(defalias 'nemacs-main-save-buffer-interactive
  #'nemacs-main--run-file-save)

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

(defun nemacs-main--run-switch-buffer ()
  "Run the TUI switch-buffer command through the shared BUF UI helper."
  (interactive)
  (when (nemacs-main--require-buffer-ui)
    (emacs-buffer-ui-run-switch-buffer-command
     :read-string #'nemacs-main--read-line-blocking
     :sync-window #'nemacs-main--sync-selected-window-buffer
     :after-success (lambda (_buffer)
                      (setq nemacs-main--repaint-hint nil))
     :message-function #'message)))

(defalias 'nemacs-main-switch-to-buffer-interactive
  #'nemacs-main--run-switch-buffer)

(defun nemacs-main--run-buffer-menu ()
  "Run the TUI list-buffers command through the shared BUF UI helper."
  (interactive)
  (when (nemacs-main--require-buffer-ui)
    (emacs-buffer-ui-run-list-buffers-command
     :sync-window #'nemacs-main--sync-selected-window-buffer
     :emit-text #'nemacs-main--emit-screen-text
     :after-success (lambda (_buffer)
                      (setq nemacs-main--repaint-hint nil))
     :message-function #'message)))

(defalias 'nemacs-main-list-buffers-interactive
  #'nemacs-main--run-buffer-menu)

(defun nemacs-main--run-buffer-kill ()
  "Run the TUI kill-buffer command through the shared BUF UI helper."
  (interactive)
  (when (nemacs-main--require-buffer-ui)
    (emacs-buffer-ui-run-kill-buffer-command
     :read-string #'nemacs-main--read-line-blocking
     :sync-window (lambda (_buffer)
                    (nemacs-main--sync-selected-window-buffer))
     :after-success (lambda (_buffer)
                      (setq nemacs-main--repaint-hint nil))
     :message-function #'message)))

(defalias 'nemacs-main-kill-buffer-interactive
  #'nemacs-main--run-buffer-kill)

(defvar nemacs-main--mx-command-feature-hints
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
    (describe-key . help-fns)
    (describe-bindings . help-fns)
    (apropos . help-fns)
    (apropos-command . help-fns)
    (apropos-documentation . help-fns))
  "Feature hints for common daily-driver `M-x' commands.")

(defun nemacs-main--mx-read-nonempty (prompt)
  "Read a non-empty string with PROMPT, returning nil on empty/cancel."
  (let ((value (nemacs-main--read-line-blocking prompt)))
    (and value (> (length value) 0) value)))

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

(defun nemacs-main--tui-apply-display-prefix (_action)
  "TUI direct backend placeholder for GUI display-prefix ACTION."
  nil)

(defalias 'nemacs-main--tui-dired-list-directory
  (lambda (directory)
    "Render DIRECTORY through the shared GUI Dired command core."
    (emacs-dired-min-gui-render-directory-buffer
     directory
     :default-directory #'nemacs-main--default-directory
     :directory-files #'nemacs-main--directory-files
     :emit-text #'nemacs-main--emit-screen-text
     :display-buffer #'nemacs-main--display-text-buffer
     :set-directory (lambda (dir)
                      (setq nemacs-main--tui-dired-directory dir))
     :set-buffer-name (lambda (buffer-name)
                        (setq nemacs-main--tui-dired-buffer-name buffer-name))
     :buffer-name "*Dired*")))

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

(defalias 'nemacs-main--tui-help-keymap-source
  #'emacs-help-gui-standard-keymap-source)

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
     :read-symbol-name 'nemacs-main--mx-read-nonempty
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

(declare-function emacs-shell-command-run-lightweight-command
                  "emacs-shell-command" (&rest plist))

(defun nemacs-main--run-shell ()
  "Run the TUI shell command through the shared shell helper."
  (interactive)
  (emacs-shell-command-run-lightweight-command
   :read-string #'nemacs-main--mx-read-nonempty
   :emit-function #'nemacs-main--emit-screen-text
   :display-function #'nemacs-main--display-text-buffer))

(defalias 'nemacs-main-shell-command-interactive
  #'nemacs-main--run-shell)

(defun nemacs-main--join-lines (lines)
  "Join LINES with newlines."
  (let ((out ""))
    (dolist (line lines)
      (setq out (concat out line "\n")))
    out))

(defun nemacs-main--emit-screen-text (text)
  "Emit TEXT directly near the top-left of the TUI screen."
  (let ((out (concat "\e[1;1H" text)))
    (if (fboundp 'emacs-tui-backend-emit)
        (emacs-tui-backend-emit out)
      (princ out))))

(declare-function emacs-dired-min-gui-run-directory-command
                  "emacs-dired-min-gui" (&rest plist))

(defun nemacs-main--run-directory-browser ()
  "Run TUI Dired through the shared Dired helper."
  (interactive)
  (emacs-dired-min-gui-run-directory-command
   :install-function #'nemacs-main--install-tui-gui-adapters
   :read-string #'nemacs-main--mx-read-nonempty
   :default-directory #'nemacs-main--default-directory
   :buffer-name nemacs-main--tui-dired-buffer-name))

(defalias 'nemacs-main-dired-interactive
  #'nemacs-main--run-directory-browser)

(declare-function emacs-info-run-current-context-command
                  "emacs-info" (command &rest plist))

(defun nemacs-main--run-info-directory ()
  "Run the TUI Info directory command through the shared Info helper."
  (interactive)
  (emacs-info-run-current-context-command
   'info
   :install-function #'nemacs-main--install-tui-gui-adapters))

(defalias 'nemacs-main-info-interactive
  #'nemacs-main--run-info-directory)

(defun nemacs-main--run-info-file ()
  "Run the TUI Info file command through the shared Info helper."
  (interactive)
  (emacs-info-run-current-context-command
   'info
   :install-function #'nemacs-main--install-tui-gui-adapters
   :read-string #'nemacs-main--mx-read-nonempty
   :prompt "Info file: "))

(defalias 'nemacs-main-info-file-interactive
  #'nemacs-main--run-info-file)

(defun nemacs-main--run-info-next ()
  "Run the TUI Info-next command through the shared Info helper."
  (interactive)
  (emacs-info-run-current-context-command
   'Info-next
   :install-function #'nemacs-main--install-tui-gui-adapters))

(defalias 'nemacs-main-info-next-interactive
  #'nemacs-main--run-info-next)

(defun nemacs-main--run-info-prev ()
  "Run the TUI Info-prev command through the shared Info helper."
  (interactive)
  (emacs-info-run-current-context-command
   'Info-prev
   :install-function #'nemacs-main--install-tui-gui-adapters))

(defalias 'nemacs-main-info-prev-interactive
  #'nemacs-main--run-info-prev)

(defun nemacs-main--run-info-up ()
  "Run the TUI Info-up command through the shared Info helper."
  (interactive)
  (emacs-info-run-current-context-command
   'Info-up
   :install-function #'nemacs-main--install-tui-gui-adapters))

(defalias 'nemacs-main-info-up-interactive
  #'nemacs-main--run-info-up)

(declare-function emacs-help-gui-run-key-help-command
                  "emacs-help-gui" (&rest plist))

(defun nemacs-main--run-key-help ()
  "Run TUI key help through the shared Help helper."
  (interactive)
  (emacs-help-gui-run-key-help-command
   :install-function #'nemacs-main--install-tui-gui-adapters
   :read-key #'nemacs-main--read-line-next-byte
   :key-description #'nemacs-main--tui-key-description))

(defalias 'nemacs-main-describe-key-interactive
  #'nemacs-main--run-key-help)

(declare-function emacs-query-replace-run-command
                  "emacs-replace" (&rest plist))

(defun nemacs-main--run-replace ()
  "Run TUI query-replace through the shared replace helper."
  (interactive)
  (emacs-query-replace-run-command
   :read-string #'nemacs-main--mx-read-nonempty
   :read-confirmation #'nemacs-main--read-line-next-byte
   :current-buffer #'current-buffer
   :start-function #'point
   :after-success (lambda (_session)
                    (setq nemacs-main--repaint-hint nil))))

(defalias 'nemacs-main-query-replace-interactive
  #'nemacs-main--run-replace)

(defun nemacs-main--mx-help-describe-function ()
  "Run the TUI describe-function M-x handler."
  (nemacs-main--install-tui-gui-adapters)
  (emacs-help-gui-describe-function-prompt-command))

(defun nemacs-main--mx-help-describe-variable ()
  "Run the TUI describe-variable M-x handler."
  (nemacs-main--install-tui-gui-adapters)
  (emacs-help-gui-describe-variable-prompt-command))

(defun nemacs-main--mx-help-describe-bindings ()
  "Run the TUI describe-bindings M-x handler."
  (nemacs-main--install-tui-gui-adapters)
  (emacs-help-gui-current-context-command 'describe-bindings))

(defun nemacs-main--mx-help-apropos ()
  "Run the TUI apropos-command M-x handler."
  (nemacs-main--install-tui-gui-adapters)
  (emacs-help-gui-apropos-command-prompt-command))

(defun nemacs-main--mx-help-apropos-documentation ()
  "Run the TUI apropos-documentation M-x handler."
  (nemacs-main--install-tui-gui-adapters)
  (emacs-help-gui-apropos-documentation-prompt-command))

(defvar nemacs-main--mx-handlers
  '((find-file . nemacs-main-find-file-interactive)
    (switch-to-buffer . nemacs-main-switch-to-buffer-interactive)
    (list-buffers . nemacs-main-list-buffers-interactive)
    (kill-buffer . nemacs-main-kill-buffer-interactive)
    (dired . nemacs-main-dired-interactive)
    (shell-command . nemacs-main-shell-command-interactive)
    (async-shell-command . nemacs-main-shell-command-interactive)
    (Info-directory . nemacs-main-info-interactive)
    (info . nemacs-main-info-file-interactive)
    (Info-next . nemacs-main-info-next-interactive)
    (Info-prev . nemacs-main-info-prev-interactive)
    (Info-up . nemacs-main-info-up-interactive)
    (describe-key . nemacs-main-describe-key-interactive)
    (describe-function . nemacs-main--mx-help-describe-function)
    (describe-variable . nemacs-main--mx-help-describe-variable)
    (describe-bindings . nemacs-main--mx-help-describe-bindings)
    (apropos . nemacs-main--mx-help-apropos)
    (apropos-command . nemacs-main--mx-help-apropos)
    (apropos-documentation . nemacs-main--mx-help-apropos-documentation)
    (query-replace . nemacs-main-query-replace-interactive))
  "TUI-specific handlers for commands selected through M-x.")

(defun nemacs-main--run-mx (command)
  "Run COMMAND selected by the TUI `M-x' prompt."
  (emacs-command-loop-dispatch-command-with-handlers
   command nemacs-main--mx-handlers
   :ensure-command
   (lambda (cmd)
     (emacs-command-loop-ensure-command
      cmd
      :feature-alist nemacs-main--mx-command-feature-hints
      :message-function #'message))
   :call-command
   (lambda (cmd)
     (if (and (eq (or (nemacs-main-option :driver) 'host) 'host)
              (boundp 'noninteractive)
              (not noninteractive))
         (let ((overriding-terminal-local-map nil))
           (command-execute cmd))
       (command-execute cmd)))
   :after-command
   (lambda (_cmd _result)
     (when (nemacs-main--sync-selected-window-buffer)
       (setq nemacs-main--repaint-hint nil)))
   :message-function #'message))

(defun nemacs-main--run-mx-entry ()
  "Doc 51 Track C — read and run an extended command via the TUI prompt."
  (interactive)
  (emacs-command-loop-run-extended-command
   :read-string #'nemacs-main--mx-read-nonempty
   :dispatch-command #'nemacs-main--run-mx
   :message-function #'message))

(defalias 'nemacs-main-execute-extended-command
  #'nemacs-main--run-mx-entry)

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

(defvar nemacs-main--idle-since nil
  "float-time when the current idle period began, or nil when not idle
(Doc 06 B2).")

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
    ;; Doc 06 B2: fire due regular timers every tick.
    (when (fboundp 'emacs-timer-run-pending)
      (emacs-timer-run-pending))
    (if activity
        (progn
          ;; input resets the idle clock + idle-timer fired flags.
          (setq nemacs-main--idle-since nil)
          (when (fboundp 'emacs-timer-reset-idle) (emacs-timer-reset-idle))
          (nemacs-main--repaint-tui))
      ;; Doc 06 B2: idle — fire idle timers based on elapsed idle time.
      (when (and (fboundp 'emacs-timer-run-idle) (fboundp 'float-time))
        (let ((now (float-time)))
          (unless nemacs-main--idle-since (setq nemacs-main--idle-since now))
          (emacs-timer-run-idle (- now nemacs-main--idle-since)))))
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
            (emacs-fileio-visit-file-direct path)
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

(defun nemacs-main--startup-gate-option (key)
  "Return (VALUE . t) when KEY is explicitly present in `nemacs-main-options'.
Return nil when KEY is absent.  Distinguishing an explicit nil VALUE from an
absent key is what lets `nemacs-main--apply-startup-gate' leave the global
compat variable untouched for callers that never pass these keys, instead of
clobbering it via `nemacs-main-option''s default-on-missing behaviour (that
helper cannot tell \"key maps to nil\" apart from \"key is absent\")."
  (let* ((plist (and (boundp 'nemacs-main-options)
                     (symbol-value 'nemacs-main-options)))
         (cell (plist-member plist key)))
    (when cell (cons (cadr cell) t))))

(defun nemacs-main--apply-startup-gate ()
  "Reflect `nemacs-main-options' startup-gate keys onto their globals.
Call before `nemacs-init' so the gate is in effect for the whole bootstrap.

Recognised keys:
  :init-file-user          Emacs `-q'/`-Q' semantics.  Nil makes
                            `nemacs-load-user-init-files' skip early-init.el,
                            package activation, and init.el
                            (src/nemacs-loadup.el).
  :inhibit-startup-screen  Consulted by the splash-screen owner
                            (`emacs-startup-screen-use-p'); nil is the
                            default (show splash), non-nil suppresses it.
  :args                    CLI file arguments.  Reflected onto
                            `emacs-startup-screen-file-arguments' so the
                            splash gate can suppress the splash when the
                            session starts on visited files.

All keys are optional; a key absent from `nemacs-main-options' leaves the
corresponding global untouched, so callers that never pass these options
(most existing tests and direct `nemacs-init' callers) keep today's
behaviour.  Centralising the reflection here — rather than injecting the
globals from more than one call site across `bin/nemacs' and
`nemacs-loadup' — is what avoids a double-injection ordering bug."
  (let ((init-file-user-cell
         (nemacs-main--startup-gate-option :init-file-user))
        (inhibit-startup-screen-cell
         (nemacs-main--startup-gate-option :inhibit-startup-screen))
        (args-cell (nemacs-main--startup-gate-option :args)))
    (when init-file-user-cell
      (setq init-file-user (car init-file-user-cell)))
    (when (and inhibit-startup-screen-cell (boundp 'inhibit-startup-screen))
      (setq inhibit-startup-screen (car inhibit-startup-screen-cell)))
    (when (and args-cell (boundp 'emacs-startup-screen-file-arguments))
      (setq emacs-startup-screen-file-arguments (car args-cell)))))

(defun nemacs-batch-main ()
  "--batch entry: bootstrap, run -l / --eval, exit.
Returns the exit-code symbol (= `ok' on success)."
  ;; Doc 51 Track M (2026-05-04) — install SIGINT → quit-flag handler
  ;; so a long batch eval can be interrupted with Ctrl+C without
  ;; killing the process abruptly.  Idempotent + no-op on non-Unix.
  (when (fboundp 'install-sigint-handler)
    (install-sigint-handler))
  (unless nemacs-initialized
    (nemacs-main--apply-startup-gate)
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
      (nemacs-main--apply-startup-gate)
      (nemacs-init))
    (nemacs-main--apply-options)
    (nemacs-main--init-keymap)
    (let ((tui-ok (nemacs-main--realise-tui))
          (driver (or (nemacs-main-option :driver) 'host)))
      (unwind-protect
          (cond
           (tui-ok
            (nemacs-main--install-tui-gui-adapters)
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

(defun nemacs-main--event->key (ev)
  "Convert a tui-event key plist EV into an Emacs event (char or symbol).
Control on an ASCII letter and the Meta bit (2^27) are encoded; modifiers on
non-ASCII / special keys are dropped (refinement pending)."
  (let ((name (plist-get ev :name))
        (mods (plist-get ev :modifiers)))
    (cond
     ((null mods) name)
     ((and (memq 'control mods) (integerp name) (>= name ?a) (<= name ?z))
      (let ((base (- name 96)))
        (if (memq 'meta mods) (logior base 134217728) base)))
     ((and (memq 'meta mods) (integerp name))
      (logior name 134217728))
     (t name))))

(defun nemacs-main--poll-input-event (timeout-ms)
  "Poll one input event for the standard command loop (Doc 06 A1).
Return a character / key symbol, or nil when no input is available."
  (when (and nemacs-main--event-handle (fboundp 'emacs-tui-event-poll))
    (let ((b (and (fboundp 'emacs-tui-event-poll-printable-byte)
                  (emacs-tui-event-poll-printable-byte nemacs-main--event-handle))))
      (if (integerp b)
          b
        (let ((ev (emacs-tui-event-poll nemacs-main--event-handle timeout-ms)))
          (and (consp ev) (eq (plist-get ev :type) 'key)
               (nemacs-main--event->key ev)))))))

(provide 'nemacs-main)

;;; nemacs-main.el ends here
