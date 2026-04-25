;;; emacs-tui-backend-test.el --- ERT for emacs-tui-backend.el  -*- lexical-binding: t; -*-

;; Phase 2 module ERT per nelisp-emacs Doc 01 (LOCKED v2 §3.2),
;; mirroring NeLisp Doc 43 v2 §3.1 Phase 11.A TUI MVP reference impl.
;;
;; Coverage:
;;   A. backend lifecycle  (init / shutdown / handlep)
;;   B. capability query   (Doc 43 §2.5 + §2.5a)
;;   C. frame management   (Doc 34 §2.11 swap-in)
;;   D. canvas drawing     (ANSI escape verification)
;;   E. event polling      (Doc 43 §2.6 pull-on-demand)
;;   F. cursor             (show / hide / clamp)
;;   G. resize listener    (callback dispatch)
;;   H. cross-cutting      (handle/frame errors, version constants)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-tui-backend)

;;; Test fixture: capture ANSI output into a buffer

(defvar emacs-tui-backend-test--captured ""
  "Accumulator for ANSI escape output captured by tests.")

(defun emacs-tui-backend-test--capture-fn (string)
  "Append STRING to `emacs-tui-backend-test--captured'."
  (setq emacs-tui-backend-test--captured
        (concat emacs-tui-backend-test--captured string)))

(defmacro emacs-tui-backend-test--with-capture (&rest body)
  "Run BODY with `emacs-tui-backend-output-fn' bound to the capture sink.
Resets `emacs-tui-backend-test--captured' to \"\" before BODY."
  (declare (indent 0))
  `(let ((emacs-tui-backend-output-fn
          #'emacs-tui-backend-test--capture-fn)
         (emacs-tui-backend-test--captured ""))
     ,@body))

;;; A. backend lifecycle

(ert-deftest emacs-tui-backend-test-init-returns-handle-with-capabilities ()
  "init returns an alive handle with the MVP minimum capability set."
  (let ((h (emacs-tui-backend-init)))
    (should (emacs-tui-backend-handlep h))
    (should (emacs-tui-backend-handle-alive-p h))
    (should (symbolp (emacs-tui-backend-handle-id h)))
    (let ((caps (emacs-tui-backend-capabilities h)))
      (should (memq 'text caps))
      (should (memq 'basic-color caps))
      (should (memq 'keyboard caps))
      (should (memq 'resize caps))
      (should (memq 'layout-box caps))
      (should (memq 'layout-grid caps)))))

(ert-deftest emacs-tui-backend-test-init-handles-have-unique-ids ()
  "Two consecutive init calls produce distinct ids."
  (let ((h1 (emacs-tui-backend-init))
        (h2 (emacs-tui-backend-init)))
    (should-not (eq (emacs-tui-backend-handle-id h1)
                    (emacs-tui-backend-handle-id h2)))))

(ert-deftest emacs-tui-backend-test-init-with-explicit-capabilities ()
  "Passing CAPABILITIES overrides the color-mode-derived default."
  (let ((h (emacs-tui-backend-init '(text keyboard))))
    (should (eq t   (emacs-tui-backend-get-capability h 'text)))
    (should (eq t   (emacs-tui-backend-get-capability h 'keyboard)))
    (should (eq nil (emacs-tui-backend-get-capability h 'resize)))))

(ert-deftest emacs-tui-backend-test-shutdown-marks-dead ()
  "shutdown clears the alive flag and rejects later ops."
  (emacs-tui-backend-test--with-capture
    (let ((h (emacs-tui-backend-init)))
      (should (eq t (emacs-tui-backend-shutdown h)))
      (should-not (emacs-tui-backend-handle-alive-p h))
      (should-error (emacs-tui-backend-capabilities h)
                    :type 'emacs-tui-backend-bad-handle))))

(ert-deftest emacs-tui-backend-test-shutdown-restores-terminal ()
  "shutdown emits the cursor-show + SGR-reset escapes."
  (emacs-tui-backend-test--with-capture
    (let ((h (emacs-tui-backend-init)))
      (emacs-tui-backend-shutdown h)
      ;; cursor show + reset must appear at least once.
      (should (string-match-p (regexp-quote "\e[?25h")
                              emacs-tui-backend-test--captured))
      (should (string-match-p (regexp-quote "\e[0m")
                              emacs-tui-backend-test--captured)))))

;;; B. capability query (Doc 43 §2.5 / §2.5a)

(ert-deftest emacs-tui-backend-test-default-capabilities-mvp-subset ()
  "Default capability list = Doc 43 §2.5 TUI MVP subset (16-color)."
  (let ((emacs-tui-backend-color-mode '16-color))
    (let ((h (emacs-tui-backend-init)))
      (should (equal (sort (emacs-tui-backend-capabilities h) #'string<)
                     (sort (copy-sequence
                            emacs-tui-backend-base-capabilities)
                           #'string<))))))

(ert-deftest emacs-tui-backend-test-capabilities-256-color-mode ()
  "Setting color-mode to 256-color adds the `256-color' capability."
  (let ((emacs-tui-backend-color-mode '256-color))
    (let ((h (emacs-tui-backend-init)))
      (should (eq t (emacs-tui-backend-get-capability h '256-color)))
      (should (eq nil (emacs-tui-backend-get-capability h 'truecolor))))))

(ert-deftest emacs-tui-backend-test-capabilities-truecolor-mode ()
  "Setting color-mode to truecolor adds 256-color + truecolor caps."
  (let ((emacs-tui-backend-color-mode 'truecolor))
    (let ((h (emacs-tui-backend-init)))
      (should (eq t (emacs-tui-backend-get-capability h '256-color)))
      (should (eq t (emacs-tui-backend-get-capability h 'truecolor))))))

(ert-deftest emacs-tui-backend-test-get-capability-returns-bool ()
  "get-capability is t/nil only, never raises for unknown caps."
  (let ((h (emacs-tui-backend-init)))
    (should (eq t   (emacs-tui-backend-get-capability h 'text)))
    (should (eq t   (emacs-tui-backend-get-capability h 'keyboard)))
    (should (eq nil (emacs-tui-backend-get-capability h 'mouse)))
    (should (eq nil (emacs-tui-backend-get-capability h 'image-png)))
    (should (eq nil (emacs-tui-backend-get-capability h 'completely-bogus)))))

(ert-deftest emacs-tui-backend-test-degrade-contract-signal ()
  "API for an undeclared capability signals display-spec-unsupported
with the Doc 43 §2.5a plist data."
  (let* ((h (emacs-tui-backend-init '(keyboard)))   ; no `text'
         (f (emacs-tui-backend-frame-create h "F")))
    (let ((err (should-error
                (emacs-tui-backend-canvas-draw-text h f 0 0 "x")
                :type 'display-spec-unsupported)))
      (let ((data (cdr err)))
        (should (eq 'text             (plist-get data :capability)))
        (should (eq 'canvas-draw-text (plist-get data :api)))
        (should (eq 'tui              (plist-get data :backend)))))))

(ert-deftest emacs-tui-backend-test-degrade-contract-resize ()
  "frame-resize signals display-spec-unsupported when `resize' missing."
  (let* ((h (emacs-tui-backend-init '(text keyboard)))   ; no `resize'
         (f (emacs-tui-backend-frame-create h "F")))
    (let ((err (should-error
                (emacs-tui-backend-frame-resize h f 100 30)
                :type 'display-spec-unsupported)))
      (let ((data (cdr err)))
        (should (eq 'resize       (plist-get data :capability)))
        (should (eq 'frame-resize (plist-get data :api)))
        (should (eq 'tui          (plist-get data :backend)))))))

;;; C. frame management (Doc 34 §2.11 swap-in)

(ert-deftest emacs-tui-backend-test-frame-create-default-80x24 ()
  "Default frame width / height match the Doc 34 §2.11 invariant."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create h "main")))
    (should (emacs-tui-backend-framep f))
    (should (equal "main" (emacs-tui-backend-frame-name f)))
    (should (= 80 (emacs-tui-backend-frame-width f)))
    (should (= 24 (emacs-tui-backend-frame-height f)))))

(ert-deftest emacs-tui-backend-test-frame-create-with-explicit-size ()
  "PARAMS :width / :height override the defaults."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create
             h "wide" '((:width . 120) (:height . 40)))))
    (should (= 120 (emacs-tui-backend-frame-width f)))
    (should (=  40 (emacs-tui-backend-frame-height f)))))

(ert-deftest emacs-tui-backend-test-frame-ids-unique-and-monotonic ()
  "Each create yields a fresh integer id, monotonically increasing."
  (let* ((h (emacs-tui-backend-init))
         (f1 (emacs-tui-backend-frame-create h "a"))
         (f2 (emacs-tui-backend-frame-create h "b"))
         (f3 (emacs-tui-backend-frame-create h "c")))
    (should (= 1 (emacs-tui-backend-frame-id f1)))
    (should (= 2 (emacs-tui-backend-frame-id f2)))
    (should (= 3 (emacs-tui-backend-frame-id f3)))))

(ert-deftest emacs-tui-backend-test-frame-destroy-removes-and-clears ()
  "Last frame destroy emits a clear-screen escape."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (should (eq t (emacs-tui-backend-frame-destroy h f)))
      (should (string-match-p (regexp-quote "\e[2J")
                              emacs-tui-backend-test--captured))
      (should-error (emacs-tui-backend-frame-destroy h f)
                    :type 'emacs-tui-backend-bad-frame))))

(ert-deftest emacs-tui-backend-test-frame-resize-applied ()
  "frame-resize updates dimensions and reallocates the canvas."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create h "x")))
    (emacs-tui-backend-frame-resize h f 100 30)
    (should (= 100 (emacs-tui-backend-frame-width f)))
    (should (=  30 (emacs-tui-backend-frame-height f)))
    ;; canvas is reallocated to the new dimensions
    (should (= 30 (length (emacs-tui-backend-frame-canvas f))))
    (should (= 100 (length (aref (emacs-tui-backend-frame-canvas f) 0))))))

(ert-deftest emacs-tui-backend-test-frame-resize-rejects-non-positive ()
  "Negative or zero WIDTH/HEIGHT raises wrong-type-argument."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create h "x")))
    (should-error (emacs-tui-backend-frame-resize h f 0   30)
                  :type 'wrong-type-argument)
    (should-error (emacs-tui-backend-frame-resize h f 100 -1)
                  :type 'wrong-type-argument)))

;;; D. canvas drawing

(ert-deftest emacs-tui-backend-test-canvas-draw-text-emits-ansi ()
  "draw-text + flush emits a CUP escape for the painted row plus the
literal text.  Because face-runs span the entire row when the
default face matches the surrounding spaces, the CUP lands at col 1
(1-based); we only assert the row index and the text payload."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-canvas-draw-text h f 2 5 "hi")
      (emacs-tui-backend-canvas-flush h f)
      ;; Row 2 → CUP row index 3 (1-based), col offset is run-start
      ;; (col 1 in this case because all cells share the nil face).
      (should (string-match-p (regexp-quote "\e[3;1H")
                              emacs-tui-backend-test--captured))
      ;; Text appears prefixed by the leading blanks of the run.
      (should (string-match-p "     hi"
                              emacs-tui-backend-test--captured)))))

(ert-deftest emacs-tui-backend-test-canvas-draw-text-cup-on-face-boundary ()
  "When a different face starts mid-row, that run gets its own CUP
positioned at the run start (1-based)."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-canvas-draw-text
       h f 0 5 "X" '((:foreground . red)))
      (emacs-tui-backend-canvas-flush h f)
      ;; The colored run starts at col 5 → CUP "\e[1;6H".
      (should (string-match-p (regexp-quote "\e[1;6H")
                              emacs-tui-backend-test--captured))
      (should (string-match-p (regexp-quote "\e[31m")
                              emacs-tui-backend-test--captured)))))

(ert-deftest emacs-tui-backend-test-canvas-draw-clips-out-of-bounds ()
  "Writes that overflow the row are clipped silently to the edge."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create h "x")))
    ;; col 78, 5 chars → only 2 fit (cols 78,79).
    (should (= 2 (emacs-tui-backend-canvas-draw-text h f 0 78 "abcde")))
    ;; row out-of-range is fully clipped.
    (should (= 0 (emacs-tui-backend-canvas-draw-text h f 999 0 "x")))))

(ert-deftest emacs-tui-backend-test-canvas-draw-stamps-cells ()
  "draw-text mutates the canvas with (CHAR . FACE) cons cells."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create h "x"))
         (face '((:foreground . red) (:bold . t))))
    (emacs-tui-backend-canvas-draw-text h f 0 0 "AB" face)
    (let ((row (aref (emacs-tui-backend-frame-canvas f) 0)))
      (should (eq ?A (car (aref row 0))))
      (should (equal face (cdr (aref row 0))))
      (should (eq ?B (car (aref row 1))))
      (should (equal face (cdr (aref row 1)))))))

(ert-deftest emacs-tui-backend-test-canvas-clear-emits-ansi ()
  "canvas-clear emits the clear-screen escape and resets cells."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-canvas-draw-text h f 1 1 "ABC")
      (emacs-tui-backend-canvas-clear h f)
      (should (string-match-p (regexp-quote "\e[2J")
                              emacs-tui-backend-test--captured))
      (let ((row (aref (emacs-tui-backend-frame-canvas f) 1)))
        (dotimes (c 80)
          (should (eq ?\s (car (aref row c)))))))))

(ert-deftest emacs-tui-backend-test-canvas-flush-tracks-dirty ()
  "flush returns painted-row count, clears dirty bits, idempotent."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      ;; fresh frame is fully dirty (24 rows).
      (should (= 24 (emacs-tui-backend-canvas-flush h f)))
      ;; second flush has nothing new.
      (should (= 0  (emacs-tui-backend-canvas-flush h f)))
      ;; touching one row marks only that row dirty.
      (emacs-tui-backend-canvas-draw-text h f 5 0 "z")
      (should (= 1  (emacs-tui-backend-canvas-flush h f))))))

(ert-deftest emacs-tui-backend-test-canvas-flush-color-16-applied ()
  "Painting with a basic color emits SGR 31 (foreground red)."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-canvas-draw-text
       h f 0 0 "X" '((:foreground . red)))
      (emacs-tui-backend-canvas-flush h f)
      (should (string-match-p (regexp-quote "\e[31m")
                              emacs-tui-backend-test--captured))
      ;; reset is appended after the colored chunk
      (should (string-match-p (regexp-quote "\e[0m")
                              emacs-tui-backend-test--captured)))))

(ert-deftest emacs-tui-backend-test-canvas-flush-color-bright-bg ()
  "Bright background `bright-blue' emits SGR 104."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-canvas-draw-text
       h f 0 0 "Y" '((:background . bright-blue)))
      (emacs-tui-backend-canvas-flush h f)
      (should (string-match-p (regexp-quote "\e[104m")
                              emacs-tui-backend-test--captured)))))

(ert-deftest emacs-tui-backend-test-canvas-flush-attribute-bold ()
  "Bold attribute emits SGR 1."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-canvas-draw-text
       h f 0 0 "Z" '((:bold . t)))
      (emacs-tui-backend-canvas-flush h f)
      (should (string-match-p (regexp-quote "\e[1m")
                              emacs-tui-backend-test--captured)))))

(ert-deftest emacs-tui-backend-test-canvas-flush-batches-runs ()
  "Consecutive cells with the same face share one CUP+SGR header."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x"))
           (face '((:foreground . green))))
      (emacs-tui-backend-canvas-draw-text h f 0 0 "AAA" face)
      (emacs-tui-backend-canvas-flush h f)
      ;; Only one SGR 32 should appear in the flush of this row's run.
      ;; (Other rows still get their own headers but with no SGR.)
      (let ((count 0)
            (start 0))
        (while (string-match (regexp-quote "\e[32m")
                             emacs-tui-backend-test--captured start)
          (setq count (1+ count)
                start (match-end 0)))
        (should (= 1 count))))))

;;; E. event polling (Doc 43 §2.6 pull-on-demand)

(ert-deftest emacs-tui-backend-test-event-poll-empty-returns-nil ()
  "poll on an empty queue returns nil immediately."
  (let ((h (emacs-tui-backend-init)))
    (should (eq nil (emacs-tui-backend-event-poll h)))))

(ert-deftest emacs-tui-backend-test-event-poll-keypress ()
  "inject + poll round-trips events in FIFO order."
  (let ((h (emacs-tui-backend-init)))
    (emacs-tui-backend-event-inject h '(key . ?a))
    (emacs-tui-backend-event-inject h '(key . ?b))
    (emacs-tui-backend-event-inject h 'C-c)
    (should (equal '(key . ?a) (emacs-tui-backend-event-poll h)))
    (should (equal '(key . ?b) (emacs-tui-backend-event-poll h)))
    (should (eq    'C-c        (emacs-tui-backend-event-poll h)))
    (should (eq    nil         (emacs-tui-backend-event-poll h)))))

(ert-deftest emacs-tui-backend-test-event-poll-timeout-returns-nil ()
  "poll with TIMEOUT-MS times out cleanly when no events arrive."
  (let* ((h (emacs-tui-backend-init))
         (start (float-time))
         (ev (emacs-tui-backend-event-poll h 50))
         (elapsed (- (float-time) start)))
    (should (eq nil ev))
    ;; should have actually waited (~50ms, allow plenty of slack)
    (should (>= elapsed 0.04))))

;;; F. cursor

(ert-deftest emacs-tui-backend-test-cursor-show-emits-show-and-cup ()
  "cursor-show emits the DECTCEM show + CUP escape."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-cursor-show h f 3 7)
      (should (string-match-p (regexp-quote "\e[?25h")
                              emacs-tui-backend-test--captured))
      ;; CUP for (3,7) = (4,8) 1-based
      (should (string-match-p (regexp-quote "\e[4;8H")
                              emacs-tui-backend-test--captured)))))

(ert-deftest emacs-tui-backend-test-cursor-show-clamps-to-frame ()
  "cursor-show clamps OOB coordinates to the frame edge."
  (let* ((h (emacs-tui-backend-init))
         (f (emacs-tui-backend-frame-create h "x")))
    ;; (999, 999) clamped to (23, 79)
    (let ((pos (emacs-tui-backend-cursor-show h f 999 999)))
      (should (equal pos (cons 23 79))))))

(ert-deftest emacs-tui-backend-test-cursor-hide-emits-hide ()
  "cursor-hide emits the DECTCEM hide escape and clears stored pos."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-cursor-show h f 1 1)
      (emacs-tui-backend-cursor-hide h f)
      (should (string-match-p (regexp-quote "\e[?25l")
                              emacs-tui-backend-test--captured))
      (should (eq nil (emacs-tui-backend-frame-cursor-row f)))
      (should (eq nil (emacs-tui-backend-frame-cursor-col f))))))

(ert-deftest emacs-tui-backend-test-cursor-position-restored-after-flush ()
  "When a cursor pos is set, flush re-parks the cursor after painting."
  (emacs-tui-backend-test--with-capture
    (let* ((h (emacs-tui-backend-init))
           (f (emacs-tui-backend-frame-create h "x")))
      (emacs-tui-backend-cursor-show h f 4 9)
      ;; Reset capture buffer to focus on flush output.
      (setq emacs-tui-backend-test--captured "")
      (emacs-tui-backend-canvas-draw-text h f 0 0 "x")
      (emacs-tui-backend-canvas-flush h f)
      ;; The flush trailer must emit a CUP back to (4,9) = (5,10) 1-based.
      (should (string-match-p (regexp-quote "\e[5;10H")
                              emacs-tui-backend-test--captured)))))

;;; G. resize listener

(ert-deftest emacs-tui-backend-test-resize-listen-callback-fires ()
  "Registered callback is invoked by --dispatch-resize with (W H)."
  (let* ((h (emacs-tui-backend-init))
         (received nil))
    (emacs-tui-backend-resize-listen
     h (lambda (w hgt) (setq received (cons w hgt))))
    (emacs-tui-backend--dispatch-resize h 132 50)
    (should (equal (cons 132 50) received))))

(ert-deftest emacs-tui-backend-test-resize-listen-replaces-callback ()
  "resize-listen returns previous callback on overwrite."
  (let* ((h (emacs-tui-backend-init))
         (cb1 (lambda (_w _h) 'one))
         (cb2 (lambda (_w _h) 'two)))
    (should (eq nil (emacs-tui-backend-resize-listen h cb1)))
    (should (eq cb1 (emacs-tui-backend-resize-listen h cb2)))))

(ert-deftest emacs-tui-backend-test-resize-listen-rejects-non-function ()
  "Non-function CALLBACK raises wrong-type-argument."
  (let ((h (emacs-tui-backend-init)))
    (should-error (emacs-tui-backend-resize-listen h 'not-a-fn)
                  :type 'wrong-type-argument)))

;;; H. cross-cutting

(ert-deftest emacs-tui-backend-test-bad-handle-rejected-everywhere ()
  "Non-handle inputs raise emacs-tui-backend-bad-handle."
  (dolist (fn '(emacs-tui-backend-capabilities
                emacs-tui-backend-event-poll))
    (should-error (funcall fn 'not-a-handle)
                  :type 'emacs-tui-backend-bad-handle)))

(ert-deftest emacs-tui-backend-test-bad-frame-rejected ()
  "Frames from a different handle raise emacs-tui-backend-bad-frame."
  (let* ((h1 (emacs-tui-backend-init))
         (h2 (emacs-tui-backend-init))
         (f1 (emacs-tui-backend-frame-create h1 "x")))
    (should-error (emacs-tui-backend-frame-destroy h2 f1)
                  :type 'emacs-tui-backend-bad-frame)))

(ert-deftest emacs-tui-backend-test-contract-version-constants ()
  "The LOCKED contract-version constants match Phase 1 baseline."
  (should (= 1 emacs-tui-backend-frame-stub-invariant-version))
  (should (= 1 emacs-tui-backend-degrade-contract-version))
  (should (= 1 emacs-tui-backend-event-source-contract-version))
  (should (= 80 emacs-tui-backend-frame-default-width))
  (should (= 24 emacs-tui-backend-frame-default-height)))

(provide 'emacs-tui-backend-test)

;;; emacs-tui-backend-test.el ends here
