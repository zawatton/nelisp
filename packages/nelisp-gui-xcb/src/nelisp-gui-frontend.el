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
(defvar nelisp-gui-frontend--logged-echo nil
  "Echo-area text last reported on the GUI diagnostic stream.")
(defvar nelisp-gui-frontend--paint-deadline nil)
(defvar nelisp-gui-frontend--transport-pending nil)
(defvar nelisp-gui-frontend-maximum-pump-time 0.025
  "Maximum time to decode one X transport batch before returning to commands.")
(defvar nelisp-gui-frontend-maximum-frame-delay 0.5
  "Maximum time to defer a dirty frame while more input is queued.
Commands and their hooks still run in order; a continuous input stream
must not starve screen updates indefinitely.")
(defvar nelisp-gui-frontend--prefix [])
(defvar nelisp-gui-frontend--timing nil)
(defvar nelisp-gui-frontend--timing-keys 0)
(defvar nelisp-gui-frontend--timing-decode 0.0)
(defvar nelisp-gui-frontend--timing-command 0.0)
(defvar nelisp-gui-frontend--timing-redisplay 0.0)
(defvar nelisp-gui-frontend--timing-gc-start nil)
(defvar nelisp-gui-frontend--timing-collections 0)
(defvar nelisp-gui-frontend--trace-text nil)
(defvar nelisp-gui-frontend--latency-check nil)
(defvar nelisp-gui-frontend--profile nil)
(defvar nelisp-gui-frontend--profile-saved nil)
(defvar nelisp-gui-frontend--profile-spans nil)
(defvar nelisp-gui-frontend--soak-interval nil)
(defvar nelisp-gui-frontend--soak-keys 0)
(defvar nelisp-gui-frontend--soak-next nil)

(defun nelisp-gui-frontend--soak-collect ()
  "Opt-in collection census after a completed, drained soak batch.
The ordinary latency/production run never enables this diagnostic: explicit
collection pauses must not be hidden in send-to-visible measurements."
  (when (and nelisp-gui-frontend--soak-next
             (>= nelisp-gui-frontend--soak-keys nelisp-gui-frontend--soak-next)
             (not (emacs-command-loop-pending-p))
             (not nelisp-gui-frontend--transport-pending)
             (not (nelisp-gui-selection-input-pending-p))
             (not nelisp-gui-frontend--paint-needed))
    (let ((start (float-time))
          (before (and (fboundp 'nelisp--arena-stats) (nelisp--arena-stats))))
      (princ (format "GUI-SOAK-GC-BEGIN|keys=%d|\n" nelisp-gui-frontend--soak-keys))
      (let ((stats (garbage-collect)))
        (princ (format "GUI-SOAK-GC|keys=%d|point=%d|seconds=%.6f|stats=%S|before=%S|after=%S|layouts=%d|\n"
                       nelisp-gui-frontend--soak-keys (nelisp-gui-frontend--point)
                       (- (float-time) start) stats before
                       (and before (nelisp--arena-stats))
                       (length (aref nelisp-gui-frontend--renderer 10)))))
      (setq nelisp-gui-frontend--soak-next
            (+ nelisp-gui-frontend--soak-keys nelisp-gui-frontend--soak-interval)))))

(defun nelisp-gui-frontend--profile-install ()
  "Opt-in inclusive phase timings; never alter the evaluator or collector."
  (dolist (name '(emacs-redisplay--snapshot-fingerprint emacs-redisplay--snapshot-line-spans
                 emacs-redisplay--viewport-text emacs-redisplay--source-entries
                 emacs-redisplay--display-tokens emacs-redisplay--token-rows
                 emacs-redisplay--fill-row emacs-redisplay--cursor-for-point
                 emacs-redisplay--redisplay-window-rebuild
                 nelisp-gui-pango-row nelisp-gui-pango--layout
                 nelisp-gui-pango--families nelisp-gui-menu-paint
                 nelisp-gui-xcb-call nelisp-gui-frontend--service))
    (let ((original (symbol-function name)) (phase name))
      (push (cons name original) nelisp-gui-frontend--profile-saved)
      (fset name
            (lambda (&rest args)
              (let ((start (float-time)))
                (prog1 (apply original args)
                  (let* ((elapsed (- (float-time) start))
                         (cell (assq phase nelisp-gui-frontend--profile-spans)))
                    (if cell
                        (setcdr cell (cons (1+ (cadr cell)) (+ elapsed (cddr cell))))
                      (push (cons phase (cons 1 elapsed)) nelisp-gui-frontend--profile-spans))))))))))

(defun nelisp-gui-frontend--profile-report ()
  (dolist (span nelisp-gui-frontend--profile-spans)
    (princ (format "GUI-PROFILE|phase=%S|calls=%d|seconds=%.6f|\n"
                   (car span) (cadr span) (cddr span))))
  (setq nelisp-gui-frontend--profile-spans nil))

(defun nelisp-gui-frontend--gc-counter ()
  "Read the existing reader collection counter without changing the collector."
  (when (fboundp 'nelisp--debug-switch) (nth 7 (nelisp--debug-switch 0))))

(defun nelisp-gui-frontend--timing-dispatch ()
  "Dispatch one shared command, optionally recording execution and hooks."
  (let ((collections (and nelisp-gui-frontend--timing (nelisp-gui-frontend--gc-counter)))
        (start (and nelisp-gui-frontend--timing (float-time))))
    (nelisp-gui-frontend--dispatch)
    (when (and nelisp-gui-frontend--soak-interval
               (not (memq last-input-event '(focus-in focus-out))))
      (setq nelisp-gui-frontend--soak-keys (1+ nelisp-gui-frontend--soak-keys)))
    (when start
      (let ((elapsed (- (float-time) start))
            (collected (and collections (- (nelisp-gui-frontend--gc-counter) collections))))
        (when collected (setq nelisp-gui-frontend--timing-collections
                              (+ nelisp-gui-frontend--timing-collections collected)))
        (setq nelisp-gui-frontend--timing-keys (1+ nelisp-gui-frontend--timing-keys)
              nelisp-gui-frontend--timing-command (+ nelisp-gui-frontend--timing-command elapsed))
        (princ (format "GUI-KEY-TIME|event=%S|command=%.6f|collections=%S|\n" last-input-event elapsed collected))))))

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
  ;; Ready INCR acknowledgements beat idle expiry after a scheduling/GC
  ;; pause.  Enforce the independent total cap even under continuous input.
  (nelisp-gui-selection-expire t)
  (let ((n 0) (go t) (deadline (+ (float-time) nelisp-gui-frontend-maximum-pump-time))
        (collections (and nelisp-gui-frontend--timing (nelisp-gui-frontend--gc-counter)))
        (start (and nelisp-gui-frontend--timing (float-time))))
    (while (and go (< n 64) (< (float-time) deadline))
      (let ((event (nelisp-gui-selection-poll)))
        (cond
         ((null event)
          (nelisp-gui-selection-expire)
          (setq go nil))
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
                (setq nelisp-gui-frontend--paint-needed t nelisp-gui-pango-force-paint t)
                (princ (format "GUI-RESIZE|width=%d|height=%d|cols=%d|lines=%d|\n"
                               (car size) (cdr size) (car shape) (cdr shape)))))))
         ((plist-get event :expose)
          (setq nelisp-gui-frontend--paint-needed t nelisp-gui-pango-force-paint t))))
      (setq n (1+ n)))
    ;; XCB can already hold unread events even when the socket is quiet.
    ;; A truncated batch must return here again before entering poll(2).
    (setq nelisp-gui-frontend--transport-pending go)
    (when start
      (setq nelisp-gui-frontend--timing-decode
            (+ nelisp-gui-frontend--timing-decode (- (float-time) start)))
      (when collections (setq nelisp-gui-frontend--timing-collections
                              (+ nelisp-gui-frontend--timing-collections
                                 (- (nelisp-gui-frontend--gc-counter) collections)))))))

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
      (unless (or (emacs-command-loop-pending-p)
                  (nelisp-gui-selection-input-pending-p)
                  nelisp-gui-frontend--transport-pending)
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
    (when (and (fboundp 'emacs-timer-run-pending)
               (or (not (boundp 'timer-list)) timer-list))
      (when (> (emacs-timer-run-pending) 0) (setq changed t)))
    (when (and (fboundp 'emacs-timer-run-idle) (fboundp 'emacs-timer-idle-seconds)
               (or (not (boundp 'timer-idle-list)) timer-idle-list))
      (when (> (emacs-timer-run-idle (emacs-timer-idle-seconds)) 0) (setq changed t)))
    (when changed (setq nelisp-gui-frontend--paint-needed t))))

(defun nelisp-gui-frontend--wait-ms ()
  "Use the shared timer deadline; periodically service active process callbacks."
  (let ((maximum (if (and (fboundp 'emacs-process-wait-source-p)
                          (emacs-process-wait-source-p)) 0.05 1.0)))
    (when (fboundp 'nelisp-gui-selection-next-delay)
      (setq maximum (nelisp-gui-selection-next-delay maximum)))
    (* 1000 (if (fboundp 'emacs-timer-next-delay)
                (emacs-timer-next-delay maximum) maximum))))

(defun nelisp-gui-frontend--paint ()
  (let ((collections (and nelisp-gui-frontend--timing (nelisp-gui-frontend--gc-counter)))
        (start (and nelisp-gui-frontend--timing (float-time)))
        (redisplay-before nelisp-gui-frontend--timing-redisplay))
    (let* ((w (emacs-window-selected-window)) (buf (emacs-window-window-buffer w)))
      ;; Public window API synchronizes its cached point with the shared buffer.
      (when (eq buf (nelisp-ec-current-buffer))
        (emacs-window-set-window-point w (nelisp-ec-point)))
      (when (eq buf (current-buffer)) (set-window-point w (point)))
      (let ((families (nelisp-gui-pango-paint nelisp-gui-frontend--renderer
                                              nelisp-gui-frontend--redisplay))
            (m (emacs-redisplay-glyph-matrix nelisp-gui-frontend--redisplay w)))
        (when nelisp-gui-frontend--latency-check
          (let ((cursor (emacs-redisplay-glyph-matrix-cursor m)))
            (when cursor
              (princ (format "GUI-MATRIX-TEXT|row=%d|text=%S|\n" (car cursor)
                             (emacs-redisplay-glyph-row-text
                              (aref (emacs-redisplay-glyph-matrix-rows m) (car cursor))))))))
        (when (equal (getenv "NELISP_GUI_FIXTURE") "metrics")
          (nelisp-gui-pango--ensure-cells nelisp-gui-frontend--renderer)
          (nelisp-gui-metrics-snapshot))
        (princ (format "GUI-PAINT|window=%d|point=%d|cursor=%S|families=%S|cairo=0|start=%d|popup=%S|\n"
                       (aref nelisp-gui-frontend--xcb 1) (nelisp-gui-frontend--point)
                       (emacs-redisplay-glyph-matrix-cursor m) families
                       (emacs-window-window-start w) (and nelisp-gui-menu--popup t)))
        ;; A live session shows `message' text in the echo area, not on
        ;; stderr (as GNU's GUI does).  Report each painted change.
        (let ((echo (and (boundp 'emacs-special-buffers-echo-message)
                         emacs-special-buffers-echo-message)))
          (unless (equal echo nelisp-gui-frontend--logged-echo)
            (setq nelisp-gui-frontend--logged-echo echo)
            (princ (format "GUI-ECHO|text=%S|\n" (or echo ""))))))
      (setq nelisp-gui-frontend--paint-needed nil
            nelisp-gui-frontend--paint-deadline nil))
    (when start
      (when collections (setq nelisp-gui-frontend--timing-collections
                              (+ nelisp-gui-frontend--timing-collections
                                 (- (nelisp-gui-frontend--gc-counter) collections))))
      (princ (format "GUI-BATCH-TIME|keys=%d|decode=%.6f|command=%.6f|redisplay=%.6f|paint=%.6f|gc=%S|collections=%S|\n"
                     nelisp-gui-frontend--timing-keys nelisp-gui-frontend--timing-decode
                     nelisp-gui-frontend--timing-command
                     (- nelisp-gui-frontend--timing-redisplay redisplay-before)
                     (- (- (float-time) start) (- nelisp-gui-frontend--timing-redisplay redisplay-before))
                     (if (and (boundp 'gc-elapsed) nelisp-gui-frontend--timing-gc-start)
                         (- gc-elapsed nelisp-gui-frontend--timing-gc-start)
                       (if (and collections (= nelisp-gui-frontend--timing-collections 0)) 0.0 'unavailable))
                     (and collections nelisp-gui-frontend--timing-collections)))
      (setq nelisp-gui-frontend--timing-keys 0 nelisp-gui-frontend--timing-collections 0
            nelisp-gui-frontend--timing-decode 0.0
            nelisp-gui-frontend--timing-command 0.0
            nelisp-gui-frontend--timing-gc-start (and (boundp 'gc-elapsed) gc-elapsed))))
  (when (equal (getenv "NELISP_GUI_STATE_LOG") "1")
    (princ (concat "GUI-DAILY-STATE|" (json-encode (gui-daily-state-snapshot)) "\n")))
  (when nelisp-gui-frontend--profile (nelisp-gui-frontend--profile-report))
  (when (or nelisp-gui-frontend--timing nelisp-gui-frontend--latency-check)
    (princ (format "GUI-FRAME-DONE|point=%d|\n" (nelisp-gui-frontend--point)))))

(defvar nelisp-gui-frontend--self-insert-event nil)
(defun nelisp-gui-frontend--set-prefix (prefix)
  (setq nelisp-gui-frontend--prefix prefix))
(defun nelisp-gui-frontend--set-command-event (value)
  (set 'last-command-event value) (set 'last-input-event value))
(defun nelisp-gui-frontend--self-insert-edit ()
  (if (nelisp-gui-frontend--pure-buffer-p)
      (emacs-edit-self-insert-direct nelisp-gui-frontend--self-insert-event)
    (emacs-command-loop-command-execute 'self-insert-command)))
(defun nelisp-gui-frontend--self-insert-point (_edit)
  (nelisp-gui-frontend--point))
(defun nelisp-gui-frontend--run-self-insert (event _plan)
  (let ((nelisp-gui-frontend--self-insert-event event))
    (emacs-command-loop-key-dispatch-run-self-insert
     event #'nelisp-gui-frontend--self-insert-edit #'nelisp-gui-frontend--self-insert-point)))
(defun nelisp-gui-frontend--direct-command-p (command)
  (and (nelisp-gui-frontend--pure-buffer-p)
       (assq command nelisp-gui-frontend--motion-adapters)))
(defun nelisp-gui-frontend--run-direct-command (command _plan)
  (emacs-command-loop-key-dispatch-direct-funcall
   (cdr (assq command nelisp-gui-frontend--motion-adapters))))
(defun nelisp-gui-frontend--command-error (command condition)
  "Keep the original shared-command condition visible in diagnostics."
  (princ (format "GUI-COMMAND-ERROR|command=%S|error=%S\n" command condition)))
(defconst nelisp-gui-frontend--dispatch-adapters
  '(:set-prefix nelisp-gui-frontend--set-prefix
    :set-last-command-event nelisp-gui-frontend--set-command-event
    :run-self-insert nelisp-gui-frontend--run-self-insert
    :direct-command-p nelisp-gui-frontend--direct-command-p
    :run-direct-command nelisp-gui-frontend--run-direct-command
    :command-execute emacs-command-loop-command-execute
    :on-error nelisp-gui-frontend--command-error))
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
          (apply #'emacs-command-loop-key-dispatch-run-plan
           plan
           :source-event event nelisp-gui-frontend--dispatch-adapters)))
    (unless (memq (plist-get result :status) '(prefix self-insert command))
      (error "Shared GUI dispatch failed: %S" result))
    (princ (format "GUI-COMMAND|command=%S|status=%S|point=%d|mark=%S|start=%d|focus=%S|text=%S\n"
                   (plist-get plan :binding) (plist-get result :status)
                   (nelisp-gui-frontend--point) (emacs-mouse-mark) (emacs-window-window-start) (and (emacs-frame-frame-focus) t)
                   (if nelisp-gui-frontend--trace-text (nelisp-gui-frontend--text) "")))))


(defun nelisp-gui-frontend--dispatch ()
  "Use the ordinary shared loop for native buffer consumers such as ddskk."
  (if (nelisp-gui-frontend--pure-buffer-p)
      (nelisp-gui-frontend--dispatch-pure)
    (emacs-command-loop-step)
    (princ (format "GUI-COMMAND|command=%S|status=command|point=%d|mark=%S|start=%d|focus=%S|text=%S\n"
                   emacs-command-loop--last-command (point) (mark t) (emacs-window-window-start)
                   (and (emacs-frame-frame-focus) t) (if nelisp-gui-frontend--trace-text (buffer-string) "")))))

(defun nelisp-gui-frontend-run ()
  "Run XCB input/painting around the existing shared GUI command dispatcher.
Command lookup, execution, hooks, buffer editing and point stay in libraries."
  (let* ((frame (emacs-frame-selected-frame))
         (cols (emacs-frame-width frame)) (lines (emacs-frame-height frame))
         (old-poll emacs-command-loop-input-poll-function)
         (old-pending emacs-command-loop-input-pending-function)
         (old-fd emacs-command-loop-input-file-descriptor)
         (old-mini-paint emacs-minibuffer-redisplay-function)
         (old-mini-key emacs-minibuffer--key-fn)
         (failure nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq nelisp-gui-frontend--timing (equal (getenv "NELISP_GUI_TIMING") "1")
                    nelisp-gui-frontend--profile (equal (getenv "NELISP_GUI_PROFILE") "1")
                    nelisp-gui-xcb-trace-events (equal (getenv "NELISP_GUI_XEVENTS") "1")
                    nelisp-gui-frontend--timing-gc-start (and (boundp 'gc-elapsed) gc-elapsed)
                    nelisp-gui-frontend--latency-check (equal (getenv "NELISP_GUI_LATENCY_CHECK") "1")
                    nelisp-gui-frontend--trace-text
                    (or (getenv "NELISP_GUI_FIXTURE") (equal (getenv "NELISP_GUI_TRACE_TEXT") "1")))
              (setq nelisp-gui-frontend--soak-interval
                    (and (getenv "NELISP_GUI_SOAK_GC")
                         (string-to-number (getenv "NELISP_GUI_SOAK_GC")))
                    nelisp-gui-frontend--soak-keys 0
                    nelisp-gui-frontend--soak-next nelisp-gui-frontend--soak-interval)
              (when (and nelisp-gui-frontend--soak-interval
                         (<= nelisp-gui-frontend--soak-interval 0))
                (error "GUI soak collection interval must be positive"))
              (when (getenv "NELISP_GUI_DPI")
                (nelisp-gui-pango-configure (string-to-number (getenv "NELISP_GUI_DPI")))
                (setq nelisp-gui-pango-fringe (round (* 8 (/ nelisp-gui-pango-dpi 96.0)))))
              (when nelisp-gui-frontend--profile (nelisp-gui-frontend--profile-install))
              (setq nelisp-gui-pango-force-paint t)
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
                    nelisp-gui-frontend--paint-needed t
                    emacs-minibuffer-redisplay-function #'nelisp-gui-frontend--paint
                    emacs-minibuffer--key-fn
                    (lambda (prompt)
                      (nelisp-gui-frontend--paint)
                      (emacs-command-loop-read-event prompt t)))
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
              ;; Startup notifications are a finite setup batch, not paced
              ;; keyboard input. Let map/expose/focus reach the first paint.
              (let ((nelisp-gui-frontend-maximum-pump-time 1.0))
                (nelisp-gui-frontend--pump))
              (while (emacs-command-loop-pending-p) (nelisp-gui-frontend--timing-dispatch))
              (nelisp-gui-frontend--paint)
              (let ((buffer (emacs-window-window-buffer (emacs-window-selected-window))))
                (princ (format "GUI-STARTUP|buffer=%S|major=%S|mode=%S|\n"
                               (if (nelisp-ec-buffer-p buffer) (nelisp-ec-buffer-name buffer)
                                 (buffer-name buffer))
                               major-mode mode-name)))
              ;; Exercise external pointer lifetimes while fonts/layouts are
              ;; active.  This is a test probe (the S3.2 gate asks for it):
              ;; a full collection here cost ~9 s on every daily launch, and
              ;; GNU does not collect before its first command loop.
              (let ((collect (equal (getenv "NELISP_GUI_STARTUP_GC") "1")))
                (when collect
                  (let ((start (and nelisp-gui-frontend--timing (float-time))))
                    (garbage-collect)
                    (when start
                      (princ (format "GUI-GC-TIME|explicit=%.6f|\n" (- (float-time) start))))))
                (nelisp-gui-frontend--paint)
                (princ (format "GUI-READY|backend=xcb|shared-loop=%S|gc=%d\n"
                               (if (nelisp-gui-frontend--pure-buffer-p)
                                   'emacs-command-loop-key-dispatch-run-plan
                                 'emacs-command-loop-step)
                               (if collect 1 0))))
              (while (not (symbol-value 'nemacs-main--quit-flag))
                (when (and nelisp-gui-frontend--paint-needed
                           (not nelisp-gui-frontend--paint-deadline))
                  (setq nelisp-gui-frontend--paint-deadline
                        (+ (float-time) nelisp-gui-frontend-maximum-frame-delay)))
                (nelisp-gui-frontend--pump)
                ;; Coalesce queued commands, but bound frame starvation at
                ;; human typing pace. A burst keeps its normal command/hook
                ;; order and continuous input still gets completed frames.
                (while (and (emacs-command-loop-pending-p)
                            (or (not nelisp-gui-frontend--paint-deadline)
                                (< (float-time) nelisp-gui-frontend--paint-deadline))
                            (not (symbol-value 'nemacs-main--quit-flag)))
                  (let ((start (float-time)))
                    (when (fboundp 'emacs-timer-reset-idle) (emacs-timer-reset-idle))
                    (nelisp-gui-frontend--timing-dispatch)
                    (unless (memq last-input-event '(focus-in focus-out))
                      (unless nelisp-gui-frontend--paint-needed
                        (setq nelisp-gui-frontend--paint-deadline
                              (+ start nelisp-gui-frontend-maximum-frame-delay)))
                      (setq nelisp-gui-frontend--paint-needed t))))
                (nelisp-gui-frontend--pump)
                (when (and nelisp-gui-frontend--paint-needed
                           (or (not (emacs-command-loop-pending-p))
                               (and nelisp-gui-frontend--paint-deadline
                                    (>= (float-time) nelisp-gui-frontend--paint-deadline)))
                           (not (symbol-value 'nemacs-main--quit-flag)))
                  (nelisp-gui-frontend--paint))
                (nelisp-gui-frontend--service)
                (when (and nelisp-gui-frontend--soak-interval
                           (not (symbol-value 'nemacs-main--quit-flag)))
                  (nelisp-gui-frontend--soak-collect))
                (unless (or (emacs-command-loop-pending-p)
                            (nelisp-gui-selection-input-pending-p)
                            nelisp-gui-frontend--transport-pending
                            nelisp-gui-frontend--paint-needed
                            (symbol-value 'nemacs-main--quit-flag))
                  (nelisp-gui-xcb-wait nelisp-gui-frontend--xcb
                                       (nelisp-gui-frontend--wait-ms)))))
          (error (setq failure err) (princ (format "GUI-ERROR|%S\n" err))))
      (setq emacs-minibuffer-redisplay-function old-mini-paint
            emacs-minibuffer--key-fn old-mini-key
            emacs-command-loop-input-poll-function old-poll
            emacs-command-loop-input-pending-function old-pending
            emacs-command-loop-input-file-descriptor old-fd)
      (dolist (entry nelisp-gui-frontend--profile-saved) (fset (car entry) (cdr entry)))
      (setq nelisp-gui-frontend--profile-saved nil)
      (when nelisp-gui-frontend--renderer (nelisp-gui-pango-close nelisp-gui-frontend--renderer))
      (when (nelisp-gui-selection-active-p) (nelisp-gui-selection-close))
      (when nelisp-gui-frontend--xcb (nelisp-gui-xcb-close nelisp-gui-frontend--xcb))
      (nl-ffi-libffi-release)
      (setq nelisp-gui-frontend--xcb nil nelisp-gui-frontend--renderer nil))
    (princ (format "GUI-CLOSED|error=%S\n" failure))
    (when failure (kill-emacs 1))
    'ok))

(provide 'nelisp-gui-frontend)
