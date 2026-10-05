;;; nelisp-gui-frontend.el --- Shared-command-loop XCB frontend -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-gui-pango)
(require 'emacs-frame)
(require 'emacs-keymap)
(require 'emacs-command-loop)
(require 'emacs-edit-builtins)
(require 'emacs-mouse)
(require 'nelisp-gui-menu)
(require 'nelisp-gui-selection)
(defvar nelisp-gui-frontend--xcb nil)
(defvar nelisp-gui-frontend--renderer nil)
(defvar nelisp-gui-frontend--redisplay nil)
(defvar nelisp-gui-frontend--paint-needed t)
(defvar nelisp-gui-frontend--prefix [])

(defun nelisp-gui-frontend--pure-buffer-p ()
  (nelisp-ec-buffer-p (emacs-window-window-buffer (emacs-window-selected-window))))
(defun nelisp-gui-frontend--point ()
  (if (nelisp-gui-frontend--pure-buffer-p) (nelisp-ec-point) (point)))
(defun nelisp-gui-frontend--text ()
  (if (nelisp-gui-frontend--pure-buffer-p) (nelisp-ec-buffer-string) (buffer-string)))
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
  (nelisp-gui-selection-expire)
  (let ((n 0) (go t))
    (while (and go (< n 64))
      (let ((event (nelisp-gui-selection-poll)))
        (cond
         ((null event) (setq go nil))
         ((plist-get event :key) (emacs-command-loop-feed-events (plist-get event :key)))
         ((plist-get event :focus) (emacs-command-loop-feed-events (plist-get event :focus)))
         ((plist-get event :pointer)
          (let* ((raw (plist-get event :pointer))
                 (ev (unless (nelisp-gui-menu-pointer raw)
                       (apply #'emacs-mouse-transport-event
                            (append (list (cdr (assq (car raw) '((4 . press) (5 . release) (6 . motion)))))
                                    (cdr raw))))))
            (when ev
              (princ (format "GUI-MOUSE|type=%S|pos=%S|xy=%S|\n" (car ev) (nth 1 (nth 1 ev))
                             (nth 2 (nth 1 ev))))
              (emacs-command-loop-feed-events ev))))
         ((plist-get event :resize)
          (let* ((size (plist-get event :resize)) (frame (emacs-frame-selected-frame)))
            (unless (and (= (car size) (emacs-frame-frame-pixel-width frame))
                         (= (cdr size) (emacs-frame-frame-pixel-height frame)))
              (let ((shape (emacs-frame-pixels-resize frame (car size) (cdr size))))
                (nelisp-gui-xcb-call "cairo_xcb_surface_set_size" [:void :pointer :sint32 :sint32]
                                     (aref nelisp-gui-frontend--renderer 0) (car size) (cdr size))
                (setq nelisp-gui-frontend--paint-needed t)
                (princ (format "GUI-RESIZE|width=%d|height=%d|cols=%d|lines=%d|\n"
                               (car size) (cdr size) (car shape) (cdr shape)))))))
         ((plist-get event :expose) (setq nelisp-gui-frontend--paint-needed t))))
      (setq n (1+ n)))))

(defun nelisp-gui-frontend--pending ()
  (nelisp-gui-frontend--pump)
  (emacs-command-loop-pending-p))

(defun nelisp-gui-frontend--input (timeout-ms)
  "Supply a canonical event; nil TIMEOUT-MS means nonblocking."
  (let ((deadline (and timeout-ms (+ (float-time) (/ timeout-ms 1000.0)))))
    (nelisp-gui-frontend--pump)
    (while (and deadline (not (emacs-command-loop-pending-p))
                (< (float-time) deadline))
      (nelisp-gui-frontend--service)
      (unless (emacs-command-loop-pending-p)
        (nelisp-gui-xcb-wait nelisp-gui-frontend--xcb
                             (min (nelisp-gui-frontend--wait-ms)
                                  (* 1000 (max 0 (- deadline (float-time)))))))
      (nelisp-gui-frontend--pump))
    ;; The shared reader rechecks the queue only before calling its provider.
    ;; read-event here drains that queue using the shared reader, not transport.
    (when (emacs-command-loop-pending-p)
      (let ((emacs-command-loop-input-poll-function nil))
        (emacs-command-loop-read-event)))))

(defun nelisp-gui-frontend--service ()
  "Service shared callbacks between bounded OS waits, without dispatching keys."
  (let ((changed nil))
    (when (fboundp 'emacs-process-dispatch-pending)
      (when (emacs-process-dispatch-pending) (setq changed t)))
    (when (fboundp 'emacs-timer-run-pending)
      (when (> (emacs-timer-run-pending) 0) (setq changed t)))
    (when (and (fboundp 'emacs-timer-run-idle) (fboundp 'emacs-timer-idle-seconds))
      (when (> (emacs-timer-run-idle (emacs-timer-idle-seconds)) 0) (setq changed t)))
    (when changed (setq nelisp-gui-frontend--paint-needed t))))

(defun nelisp-gui-frontend--wait-ms ()
  "Use the shared timer deadline; periodically service active process callbacks."
  (let ((maximum (if (and (fboundp 'emacs-process-wait-source-p)
                          (emacs-process-wait-source-p)) 0.05 1.0)))
    (* 1000 (if (fboundp 'emacs-timer-next-delay)
                (emacs-timer-next-delay maximum) maximum))))

(defun nelisp-gui-frontend--paint ()
  (let* ((w (emacs-window-selected-window)) (buf (emacs-window-window-buffer w)))
    ;; Public window API synchronizes its cached point with the shared buffer.
    (when (eq buf (nelisp-ec-current-buffer))
      (emacs-window-set-window-point w (nelisp-ec-point)))
    (when (eq buf (current-buffer)) (set-window-point w (point)))
    (let ((families (nelisp-gui-pango-paint nelisp-gui-frontend--renderer
                                         nelisp-gui-frontend--redisplay))
          (m (emacs-redisplay-glyph-matrix nelisp-gui-frontend--redisplay w)))
      (when (equal (getenv "NELISP_GUI_FIXTURE") "metrics") (nelisp-gui-metrics-snapshot))
      (princ (format "GUI-PAINT|window=%d|point=%d|cursor=%S|families=%S|cairo=0|start=%d|popup=%S|\n"
                     (aref nelisp-gui-frontend--xcb 1) (nelisp-gui-frontend--point)
                     (emacs-redisplay-glyph-matrix-cursor m) families
                     (emacs-window-window-start w) (and nelisp-gui-menu--popup t))))
    (setq nelisp-gui-frontend--paint-needed nil)))

(defun nelisp-gui-frontend--dispatch-pure ()
  "Run the existing shared GUI dispatcher with shared pure-buffer adapters.
The native reader's unprefixed editing commands target its separate scratch
  buffer. This is the same public dispatch/edit adapter seam used by the TUI."
  (let* ((event (emacs-command-loop-read-event))
         ;; The legacy plan callback only publishes integer command events.
         ;; Retain canonical symbol/positioned events for shared commands too.
         (_publish (set 'last-command-event event))
         (plan (emacs-command-loop-key-dispatch-plan
                :events (vector event) :prefix nelisp-gui-frontend--prefix
                :lookup-sequence #'emacs-keymap-menu-binding))
         (result
          (emacs-command-loop-key-dispatch-run-plan
           plan
           :set-prefix (lambda (prefix) (setq nelisp-gui-frontend--prefix prefix))
           :set-last-command-event (lambda (value) (set 'last-command-event value)
                                                  (set 'last-input-event value))
           :source-event event
           :run-self-insert
           (lambda (ev _plan)
             (emacs-command-loop-key-dispatch-run-self-insert
              ev (lambda ()
                   (if (nelisp-gui-frontend--pure-buffer-p)
                       (emacs-edit-self-insert-direct ev)
                     (emacs-command-loop-command-execute 'self-insert-command)))
              (lambda (_edit) (nelisp-gui-frontend--point))))
           :direct-command-p
           (lambda (cmd) (and (nelisp-gui-frontend--pure-buffer-p)
                              (assq cmd nelisp-gui-frontend--motion-adapters)))
           :run-direct-command
           (lambda (cmd _plan)
             (emacs-command-loop-key-dispatch-direct-funcall
              (cdr (assq cmd nelisp-gui-frontend--motion-adapters))))
           :command-execute #'emacs-command-loop-command-execute)))
    (unless (memq (plist-get result :status) '(prefix self-insert command))
      (error "Shared GUI dispatch failed: %S" result))
    (princ (format "GUI-COMMAND|command=%S|status=%S|point=%d|mark=%S|start=%d|focus=%S|text=%S\n"
                   (plist-get plan :binding) (plist-get result :status)
                   (nelisp-gui-frontend--point) (emacs-mouse-mark) (emacs-window-window-start) (and (emacs-frame-frame-focus) t)
                   (nelisp-gui-frontend--text)))))

(defun nelisp-gui-frontend--dispatch ()
  "Use the ordinary shared loop for native buffer consumers such as ddskk."
  (if (nelisp-gui-frontend--pure-buffer-p)
      (nelisp-gui-frontend--dispatch-pure)
    (emacs-command-loop-step)
    (princ (format "GUI-COMMAND|command=%S|status=command|point=%d|mark=%S|start=%d|focus=%S|text=%S\n"
                   emacs-command-loop--last-command (point) (mark t) (emacs-window-window-start)
                   (and (emacs-frame-frame-focus) t) (buffer-string)))))

(defun nelisp-gui-frontend-run ()
  "Run XCB input/painting around the existing shared GUI command dispatcher.
Command lookup, execution, hooks, buffer editing and point stay in libraries."
  (let* ((frame (emacs-frame-selected-frame))
         (cols (emacs-frame-width frame)) (lines (emacs-frame-height frame))
         (old-poll emacs-command-loop-input-poll-function)
         (old-pending emacs-command-loop-input-pending-function)
         (old-fd emacs-command-loop-input-file-descriptor)
         (failure nil))
    (unwind-protect
        (condition-case err
            (progn
              (when (getenv "NELISP_GUI_DPI")
                (nelisp-gui-pango-configure (string-to-number (getenv "NELISP_GUI_DPI")))
                (setq nelisp-gui-pango-fringe (round (* 8 (/ nelisp-gui-pango-dpi 96.0)))))
              (setq nelisp-gui-frontend--xcb
                    (nelisp-gui-xcb-open "NeLisp XCB" (* cols nelisp-gui-pango-cell-width)
                                         (* lines nelisp-gui-pango-line-height)))
              (nelisp-gui-selection-open nelisp-gui-frontend--xcb)
              (when (getenv "NELISP_GUI_SELECTION_FIXTURE") (nelisp-gui-selections-fixture))
              (setq nelisp-gui-frontend--renderer (nelisp-gui-pango-open nelisp-gui-frontend--xcb cols lines)
                    nelisp-gui-frontend--redisplay (emacs-redisplay-init)
                    emacs-command-loop-input-poll-function #'nelisp-gui-frontend--input
                    emacs-command-loop-input-pending-function #'nelisp-gui-frontend--pending
                    emacs-command-loop-input-file-descriptor
                    (nelisp-gui-xcb-file-descriptor nelisp-gui-frontend--xcb)
                    nelisp-gui-frontend--paint-needed t)
              (emacs-keymap-use-global-map nemacs-main--global-keymap)
              (setf (emacs-frame-backend frame) 'xcb)
              (emacs-frame-set-frame-parameter frame 'display-depth
                                               (ptr-read-u8 (aref nelisp-gui-frontend--xcb 3) 38))
              (emacs-frame-pixels-install frame nelisp-gui-pango-cell-width nelisp-gui-pango-line-height
                                         #'nelisp-gui-pango-provider)
              (emacs-frame-pixels-install-builtins)
              (emacs-mouse-install-bindings nemacs-main--global-keymap)
              (emacs-keymap-define-key nemacs-main--global-keymap [focus-in] 'emacs-frame-input-focus-in)
              (emacs-keymap-define-key nemacs-main--global-keymap [focus-out] 'emacs-frame-input-focus-out)
              ;; Test-only lifecycle hook. Real C-x C-c still uses production quit.
              (when (getenv "NELISP_GUI_TEST_EXIT_GROUP")
                (emacs-keymap-define-key nemacs-main--global-keymap [f12]
                                        'nelisp-gui-frontend-request-close))
              (when (equal (getenv "NELISP_GUI_FAULT") "bad-window")
                (nelisp-gui-xcb-bad-window nelisp-gui-frontend--xcb))
              ;; Mapping has already generated Expose/Focus events. Consume
              ;; those before the first complete paint, so READY does not
              ;; announce a frame with an obsolete full repaint queued.
              (nelisp-gui-frontend--pump)
              (while (emacs-command-loop-pending-p) (nelisp-gui-frontend--dispatch))
              (nelisp-gui-frontend--paint)
              (let ((buffer (emacs-window-window-buffer (emacs-window-selected-window))))
                (princ (format "GUI-STARTUP|buffer=%S|major=%S|mode=%S|\n"
                               (if (nelisp-ec-buffer-p buffer) (nelisp-ec-buffer-name buffer)
                                 (buffer-name buffer))
                               major-mode mode-name)))
              ;; Exercise external pointer lifetimes while fonts/layouts are active.
              (garbage-collect)
              (nelisp-gui-frontend--paint)
              (princ (format "GUI-READY|backend=xcb|shared-loop=%S|gc=1\n"
                             (if (nelisp-gui-frontend--pure-buffer-p)
                                 'emacs-command-loop-key-dispatch-run-plan
                               'emacs-command-loop-step)))
              (while (not (symbol-value 'nemacs-main--quit-flag))
                (nelisp-gui-frontend--pump)
                (when (emacs-command-loop-pending-p)
                  (when (fboundp 'emacs-timer-reset-idle) (emacs-timer-reset-idle))
                  (nelisp-gui-frontend--dispatch)
                  (unless (memq last-input-event '(focus-in focus-out))
                    (setq nelisp-gui-frontend--paint-needed t)))
                (when (and nelisp-gui-frontend--paint-needed
                           (not (emacs-command-loop-pending-p))
                           (not (symbol-value 'nemacs-main--quit-flag)))
                  (nelisp-gui-frontend--paint))
                (nelisp-gui-frontend--service)
                (unless (or (emacs-command-loop-pending-p)
                            nelisp-gui-frontend--paint-needed
                            (symbol-value 'nemacs-main--quit-flag))
                  (nelisp-gui-xcb-wait nelisp-gui-frontend--xcb
                                       (nelisp-gui-frontend--wait-ms)))))
          (error (setq failure err) (princ (format "GUI-ERROR|%S\n" err))))
      (setq emacs-command-loop-input-poll-function old-poll
            emacs-command-loop-input-pending-function old-pending
            emacs-command-loop-input-file-descriptor old-fd)
      (when nelisp-gui-frontend--renderer (nelisp-gui-pango-close nelisp-gui-frontend--renderer))
      (when (nelisp-gui-selection-active-p) (nelisp-gui-selection-close))
      (when nelisp-gui-frontend--xcb (nelisp-gui-xcb-close nelisp-gui-frontend--xcb))
      (nl-ffi-libffi-release)
      (setq nelisp-gui-frontend--xcb nil nelisp-gui-frontend--renderer nil))
    (princ (format "GUI-CLOSED|error=%S\n" failure))
    (when failure (kill-emacs 1))
    'ok))

(provide 'nelisp-gui-frontend)
