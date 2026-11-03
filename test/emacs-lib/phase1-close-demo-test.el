;;; phase1-close-demo-test.el --- Integration ERT for Phase 1 close demo  -*- lexical-binding: t; -*-

;; nelisp-emacs Phase 1 close gate (Doc 34 §9.5 + gate-M9) integration
;; checks.  Drives `phase1-close-demo-run' and asserts the observable
;; state at six checkpoints covering every Phase 1 §4 sub-task in the
;; same run.

;;; Code:

(require 'ert)
(require 'phase1-close-demo)

(defvar phase1-close-demo-test--result nil
  "Cached `phase1-close-demo-run' return value.
Populated lazily so the six checkpoint tests share one execution.")

(defun phase1-close-demo-test--state ()
  "Return the cached demo result, running the demo on first access."
  (or phase1-close-demo-test--result
      (setq phase1-close-demo-test--result
            (phase1-close-demo-run))))

(ert-deftest phase1-close-demo-buffers-created ()
  ;; Checkpoint 1: two named buffers with non-empty content.
  (let* ((state (phase1-close-demo-test--state))
         (buffers (plist-get state :buffers)))
    (should (= 2 (length buffers)))
    (should (assoc "draft.txt" buffers))
    (should (assoc "notes.txt" buffers))
    ;; Both buffers carry inserted text (= non-zero size).
    (should (> (cdr (assoc "draft.txt" buffers)) 0))
    (should (> (cdr (assoc "notes.txt" buffers)) 0))))

(ert-deftest phase1-close-demo-window-split-and-cycle ()
  ;; Checkpoint 2: split-window-below yields exactly 2 live leaves
  ;; and each shows the buffer it was assigned.
  (let* ((state (phase1-close-demo-test--state))
         (wbufs (plist-get state :window-buffers)))
    (should (= 2 (plist-get state :windows)))
    (should (= 2 (length wbufs)))
    (should (member "draft.txt" wbufs))
    (should (member "notes.txt" wbufs))))

(ert-deftest phase1-close-demo-overlay-face-via-get-char-property ()
  ;; Checkpoint 3: overlay on b2 wins via get-char-property — exercises
  ;; both the F. overlay section and B. text-property fallback.
  (let ((state (phase1-close-demo-test--state)))
    (should (eq 'phase1-demo-overlay-face
                (plist-get state :overlay-face)))))

(ert-deftest phase1-close-demo-textprop-category-inheritance ()
  ;; Checkpoint 4: text-property `category' symbol inheritance returns
  ;; the symbol's plist value for `face' at the queried pos.
  (let ((state (phase1-close-demo-test--state)))
    (should (eq 'phase1-demo-face
                (plist-get state :textprop-face)))))

(ert-deftest phase1-close-demo-keymap-newer-api-binds ()
  ;; Checkpoint 5: keymap-set ("C-x C-s") + keymap-lookup round-trip.
  (let ((state (phase1-close-demo-test--state)))
    (should (eq 'phase1-close-demo-save
                (plist-get state :keymap-binding)))))

(ert-deftest phase1-close-demo-minibuffer-roundtrip ()
  ;; Checkpoint 6: minibuffer plug-in reader returns the fed string.
  (let ((state (phase1-close-demo-test--state)))
    (should (string-equal "saved" (plist-get state :minibuffer-out)))))

(ert-deftest phase1-close-demo-tui-stub-canvas-flushed ()
  ;; Checkpoint 7 (= TUI stub gate): canvas drew one row per window
  ;; and the handle stayed live until shutdown.
  (let ((state (phase1-close-demo-test--state)))
    (should (= 2 (plist-get state :canvas-rows)))
    ;; handle was live during the run; shutdown happens after the
    ;; result is built so the captured flag is t.
    (should (eq t (plist-get state :tui-handle-live)))))

(provide 'phase1-close-demo-test)
;;; phase1-close-demo-test.el ends here
