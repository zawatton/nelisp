;;; nelisp-gui-frontend.el --- Shared-command-loop XCB frontend -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-gui-pango)
(require 'emacs-frame)
(require 'emacs-keymap)
(require 'emacs-command-loop)
(require 'emacs-edit-builtins)
(defvar nelisp-gui-frontend--xcb nil)
(defvar nelisp-gui-frontend--renderer nil)
(defvar nelisp-gui-frontend--redisplay nil)
(defvar nelisp-gui-frontend--paint-needed t)
(defvar nelisp-gui-frontend--prefix [])
(defconst nelisp-gui-frontend--motion-adapters
  '((forward-char . nelisp-ec-forward-char) (backward-char . nelisp-ec-backward-char))
  "Shared pure-buffer equivalents of the reader's native-buffer commands.
Only adaptation is done here; bounds, motion and edits stay in libraries.")

(defun nelisp-gui-frontend-request-close ()
  "Request frontend teardown without terminating the shared runtime."
  (interactive)
  (set 'nemacs-main--quit-flag t))

(defun nelisp-gui-frontend--pump ()
  "Drain a bounded transport batch and feed canonical events to the shared loop."
  (let ((n 0) (go t))
    (while (and go (< n 64))
      (let ((event (nelisp-gui-xcb-poll nelisp-gui-frontend--xcb)))
        (cond
         ((null event) (setq go nil))
         ((plist-get event :key) (emacs-command-loop-feed-events (plist-get event :key)))
         ((plist-get event :expose) (setq nelisp-gui-frontend--paint-needed t))))
      (setq n (1+ n)))))

(defun nelisp-gui-frontend--pending ()
  (nelisp-gui-frontend--pump)
  (emacs-command-loop-pending-p))

(defun nelisp-gui-frontend--input (timeout-ms)
  "Supply a canonical event; retain input in the shared unread queue."
  (let ((deadline (+ (float-time) (/ (or timeout-ms 0) 1000.0))))
    (nelisp-gui-frontend--pump)
    (while (and (not (emacs-command-loop-pending-p)) (< (float-time) deadline))
      (sleep-for 0.005) (nelisp-gui-frontend--pump))
    ;; The shared reader rechecks the queue only before calling its provider.
    ;; read-event here drains that queue using the shared reader, not transport.
    (when (emacs-command-loop-pending-p)
      (let ((emacs-command-loop-input-poll-function nil))
        (emacs-command-loop-read-event)))))

(defun nelisp-gui-frontend--paint ()
  (let* ((w (emacs-window-selected-window)) (buf (emacs-window-buffer w)))
    ;; Public window API synchronizes its cached point with the shared buffer.
    (when (eq buf (nelisp-ec-current-buffer))
      (emacs-window-set-window-point w (nelisp-ec-point)))
    (let ((families (nelisp-gui-pango-paint nelisp-gui-frontend--renderer
                                         nelisp-gui-frontend--redisplay))
          (m (emacs-redisplay-glyph-matrix nelisp-gui-frontend--redisplay w)))
      (princ (format "GUI-PAINT|window=%d|point=%d|cursor=%S|families=%S|cairo=0\n"
                     (aref nelisp-gui-frontend--xcb 1) (nelisp-ec-point)
                     (emacs-redisplay-glyph-matrix-cursor m) families)))
    (setq nelisp-gui-frontend--paint-needed nil)))

(defun nelisp-gui-frontend--dispatch ()
  "Run the existing shared GUI dispatcher with shared pure-buffer adapters.
The native reader's unprefixed editing commands target its separate scratch
buffer. This is the same public dispatch/edit adapter seam used by the TUI."
  (let* ((event (emacs-command-loop-read-event))
         (plan (emacs-command-loop-key-dispatch-plan
                :events (vector event) :prefix nelisp-gui-frontend--prefix
                :lookup-sequence #'emacs-keymap-key-binding))
         (result
          (emacs-command-loop-key-dispatch-run-plan
           plan
           :set-prefix (lambda (prefix) (setq nelisp-gui-frontend--prefix prefix))
           :set-last-command-event (lambda (ev) (set 'last-command-event ev))
           :run-self-insert
           (lambda (ev _plan)
             (emacs-command-loop-key-dispatch-run-self-insert
              ev (lambda () (emacs-edit-self-insert-direct ev))
              (lambda (_edit) (nelisp-ec-point))))
           :direct-command-p
           (lambda (cmd) (assq cmd nelisp-gui-frontend--motion-adapters))
           :run-direct-command
           (lambda (cmd _plan)
             (emacs-command-loop-key-dispatch-direct-funcall
              (cdr (assq cmd nelisp-gui-frontend--motion-adapters))))
           :command-execute #'emacs-command-loop-command-execute)))
    (unless (memq (plist-get result :status) '(prefix self-insert command))
      (error "Shared GUI dispatch failed: %S" result))
    (princ (format "GUI-COMMAND|command=%S|status=%S|point=%d|text=%S\n"
                   (plist-get plan :binding) (plist-get result :status)
                   (nelisp-ec-point) (nelisp-ec-buffer-string)))))

(defun nelisp-gui-frontend-temporary-test-exit-group (code)
  "TEMPORARY R1 workaround, test opt-in only; production quit stays unmodified.
Linux SYS_exit kills only the leader after Pango starts native workers.
Use the existing syscall path; remove this helper when R1 fixes shared exit."
  (syscall-direct 231 code 0 0 0 0 0))

(defun nelisp-gui-frontend-run ()
  "Run XCB input/painting around the existing shared GUI command dispatcher.
Command lookup, execution, hooks, buffer editing and point stay in libraries."
  (let* ((frame (emacs-frame-selected-frame))
         (cols (emacs-frame-width frame)) (lines (emacs-frame-height frame))
         (old-poll emacs-command-loop-input-poll-function)
         (old-pending emacs-command-loop-input-pending-function)
         (failure nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq nelisp-gui-frontend--xcb
                    (nelisp-gui-xcb-open "NeLisp XCB" (* cols nelisp-gui-pango-cell-width)
                                         (* lines nelisp-gui-pango-line-height)))
              (setq nelisp-gui-frontend--renderer (nelisp-gui-pango-open nelisp-gui-frontend--xcb cols lines)
                    nelisp-gui-frontend--redisplay (emacs-redisplay-init)
                    emacs-command-loop-input-poll-function #'nelisp-gui-frontend--input
                    emacs-command-loop-input-pending-function #'nelisp-gui-frontend--pending
                    nelisp-gui-frontend--paint-needed t)
              (emacs-keymap-use-global-map nemacs-main--global-keymap)
              ;; Test-only lifecycle hook. Real C-x C-c still uses production quit.
              (when (getenv "NELISP_GUI_TEST_EXIT_GROUP")
                (emacs-keymap-define-key nemacs-main--global-keymap [f12]
                                        'nelisp-gui-frontend-request-close))
              (when (equal (getenv "NELISP_GUI_FAULT") "bad-window")
                (nelisp-gui-xcb-bad-window nelisp-gui-frontend--xcb))
              (nelisp-gui-frontend--paint)
              ;; Exercise external pointer lifetimes while fonts/layouts are active.
              (garbage-collect)
              (nelisp-gui-frontend--paint)
              (princ "GUI-READY|backend=xcb|shared-loop=emacs-command-loop-key-dispatch-run-plan|gc=1\n")
              (while (not (symbol-value 'nemacs-main--quit-flag))
                (nelisp-gui-frontend--pump)
                (when (emacs-command-loop-pending-p)
                  (nelisp-gui-frontend--dispatch)
                  (setq nelisp-gui-frontend--paint-needed t))
                (when (and nelisp-gui-frontend--paint-needed
                           (not (symbol-value 'nemacs-main--quit-flag)))
                  (nelisp-gui-frontend--paint))
                (when (fboundp 'emacs-timer-run-pending) (emacs-timer-run-pending))
                (sleep-for 0.01)))
          (error (setq failure err) (princ (format "GUI-ERROR|%S\n" err))))
      (setq emacs-command-loop-input-poll-function old-poll
            emacs-command-loop-input-pending-function old-pending)
      (when nelisp-gui-frontend--renderer (nelisp-gui-pango-close nelisp-gui-frontend--renderer))
      (when nelisp-gui-frontend--xcb (nelisp-gui-xcb-close nelisp-gui-frontend--xcb))
      (nl-ffi-libffi-release)
      (setq nelisp-gui-frontend--xcb nil nelisp-gui-frontend--renderer nil))
    (princ (format "GUI-CLOSED|error=%S\n" failure))
    (when (getenv "NELISP_GUI_TEST_EXIT_GROUP")
      (nelisp-gui-frontend-temporary-test-exit-group (if failure 1 0)))
    (when failure (signal (car failure) (cdr failure)))
    'ok))

(provide 'nelisp-gui-frontend)
