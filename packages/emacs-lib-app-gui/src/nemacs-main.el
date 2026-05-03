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
  (when (and nemacs-main--backend
             (fboundp 'emacs-tui-backend-shutdown))
    (condition-case _
        (emacs-tui-backend-shutdown nemacs-main--backend)
      (error nil)))
  (setq nemacs-main--backend nil
        nemacs-main--frame nil
        nemacs-main--redisplay nil)
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
Idempotent — safe to call multiple times.  Returns the keymap."
  (unless nemacs-main--global-keymap
    (let ((m (cond
              ((fboundp 'make-sparse-keymap) (make-sparse-keymap))
              ((fboundp 'emacs-keymap-make-keymap)
               (emacs-keymap-make-keymap))
              (t (list 'keymap)))))
      (when (fboundp 'define-key)
        (define-key m (kbd "C-x C-c") 'nemacs-main-kill)
        (define-key m (kbd "C-c C-q") 'nemacs-main-kill)
        (when (fboundp 'keyboard-quit)
          (define-key m (kbd "C-g") 'keyboard-quit)))
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

;;;; --- event loop ---------------------------------------------------

(defvar nemacs-main--quit-flag nil
  "Set by the event loop's quit handler; checked each iteration.")

(defun nemacs-main--quit ()
  "Mark the event loop for termination.  Returns t."
  (setq nemacs-main--quit-flag t))

(defun nemacs-main--drain-once (timeout-ms)
  "Pull one event and dispatch it.  Returns t when an event ran, nil
on idle.  Honours TIMEOUT-MS (= caller's poll budget)."
  (when (and nemacs-main--backend
             (fboundp 'emacs-tui-backend-event-poll))
    (let ((ev (emacs-tui-backend-event-poll nemacs-main--backend
                                            timeout-ms)))
      (cond
       ((null ev) nil)
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
        t)))))

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
    (let ((budget-ms 50))
      (while (not nemacs-main--quit-flag)
        (nemacs-main--drain-once budget-ms)
        ;; Refresh the painted state after every dispatched event.
        (when (and nemacs-main--redisplay nemacs-main--frame
                   (fboundp 'emacs-redisplay-flush-frame))
          (condition-case _
              (emacs-redisplay-flush-frame nemacs-main--redisplay
                                           nemacs-main--frame)
            (error nil))))))))

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
        (when (or (nemacs-main-option :batch)
                  (and (boundp 'noninteractive) noninteractive))
          (nemacs-main--shutdown-tui)))))))

(provide 'nemacs-main)

;;; nemacs-main.el ends here
