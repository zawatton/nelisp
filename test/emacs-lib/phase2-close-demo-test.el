;;; phase2-close-demo-test.el --- Integration ERT for Phase 2 close demo  -*- lexical-binding: t; -*-

;; nelisp-emacs Phase 2 close gate (Doc 43 §3.1 Phase 11.A ship gate)
;; integration checks.  Drives `phase2-close-demo-run' and asserts the
;; observable state at the checkpoints covering every Phase 2 close
;; gate item (#1 frame swap, #2 terminfo, #3 backend dispatch, #4
;; SIGWINCH, #5 keyboard wire-up, #6 stub revert) in a single run.

;;; Code:

(require 'ert)
(require 'phase2-close-demo)

(defvar phase2-close-demo-test--result nil
  "Cached `phase2-close-demo-run' return value.
Populated lazily so all checkpoint tests share one execution.")

(defun phase2-close-demo-test--state ()
  "Return the cached demo result, running the demo on first access."
  (or phase2-close-demo-test--result
      (setq phase2-close-demo-test--result
            (phase2-close-demo-run))))

(ert-deftest phase2-close-demo-backend-swap-flips-and-reverts ()
  ;; Checkpoint 1: backend swap stub → tui → stub round-trips cleanly.
  (let ((state (phase2-close-demo-test--state)))
    (should (eq 'stub (plist-get state :backend-before)))
    (should (eq 'tui  (plist-get state :backend-after-tui)))
    (should (eq 'stub (plist-get state :backend-after-stub)))))

(ert-deftest phase2-close-demo-terminfo-detected ()
  ;; Checkpoint 2: TERM=xterm-256color exposes 256 colors via
  ;; emacs-tui-terminfo-detect → emacs-frame-tui-info.
  (let ((state (phase2-close-demo-test--state)))
    (should (equal "xterm-256color" (plist-get state :env-term)))
    (should (eq 256 (plist-get state :term-colors)))))

(ert-deftest phase2-close-demo-find-file-mini ()
  ;; Checkpoint 3: nelisp-ec-insert-file-contents reads the seed text
  ;; back ("hello " = 6 chars).
  (let* ((state (phase2-close-demo-test--state))
         (result (plist-get state :insert-file-result)))
    (should (consp result))
    (should (stringp (car result)))
    (should (= 6 (cdr result)))))

(ert-deftest phase2-close-demo-keyboard-self-insert ()
  ;; Checkpoint 4: every byte fed through the TUI event handle was
  ;; consumed by the keymap reader and inserted into the buffer.
  (let ((state (phase2-close-demo-test--state)))
    (should (= 5 (plist-get state :typed-chars)))
    (should-not (plist-get state :seen-symbols))
    (should (string-equal "hello world" (plist-get state :buffer-final)))))

(ert-deftest phase2-close-demo-sigwinch-coexists-with-keyboard ()
  ;; Checkpoint 5: a SIGWINCH dispatched mid-run resizes the frame
  ;; while the keyboard wire-up still drains the queue cleanly.
  (let ((state (phase2-close-demo-test--state)))
    (should (equal '(132 . 50) (plist-get state :resize-applied)))))

(ert-deftest phase2-close-demo-save-buffer-mini ()
  ;; Checkpoint 6: nelisp-ec-write-region persists the buffer to disk
  ;; and a fresh read of the file matches the buffer content.
  (let ((state (phase2-close-demo-test--state)))
    (should (= 11 (plist-get state :write-bytes)))
    (should (string-equal "hello world"
                          (plist-get state :disk-contents)))))

(ert-deftest phase2-close-demo-keymap-reader-restored ()
  ;; Checkpoint 7: `emacs-frame-use-stub-backend' restored the prior
  ;; `emacs-keymap--read-event-fn' value (= nil sentinel here).
  (let ((state (phase2-close-demo-test--state)))
    (should (eq t (plist-get state :keymap-reader-restored)))))

(ert-deftest phase2-close-demo-tmpfile-cleaned-up ()
  ;; Checkpoint 8: the demo's `unwind-protect' deletes the temp file
  ;; even after a successful run.
  (let* ((state (phase2-close-demo-test--state))
         (tmpfile (plist-get state :tmpfile)))
    (should (stringp tmpfile))
    (should-not (file-exists-p tmpfile))))

;;;; 3-environment validation (xterm-256color / kitty / Windows Terminal)

;; Each test runs the full demo with a different TERM / COLORTERM env
;; and asserts that the substrate detects the expected color tier *and*
;; the end-to-end keyboard + write path still ships "hello world" to
;; disk.  The cached state above is invalidated per env so each call
;; gets a fresh run.

(defun phase2-close-demo-test--run-fresh (env)
  "Run the demo against ENV (alist), bypassing the cached state."
  (let ((phase2-close-demo-test--result nil))
    (phase2-close-demo-run (list :env env))))

(ert-deftest phase2-close-demo-env-xterm-256color ()
  ;; Env 1: bare xterm-256color → 256-color tier.
  (let ((state (phase2-close-demo-test--run-fresh
                '(("TERM" . "xterm-256color")))))
    (should (eq 'tui (plist-get state :backend-after-tui)))
    (should (eq 256 (plist-get state :term-colors)))
    (should (string-equal "hello world" (plist-get state :disk-contents)))))

(ert-deftest phase2-close-demo-env-kitty ()
  ;; Env 2: xterm-kitty (= kitty's canonical TERM) → table baseline is
  ;; 256-color but the `kitty' substring matches
  ;; `emacs-tui-terminfo-extra-color-terminals' which escalates the
  ;; detection to truecolor.  We assert the escalation explicitly so a
  ;; future heuristic change is caught here.
  (let ((state (phase2-close-demo-test--run-fresh
                '(("TERM" . "xterm-kitty")))))
    (should (eq 'tui (plist-get state :backend-after-tui)))
    (should (eq 16777216 (plist-get state :term-colors)))
    (should (string-equal "hello world" (plist-get state :disk-contents)))))

(ert-deftest phase2-close-demo-env-windows-terminal ()
  ;; Env 3: Windows Terminal ships TERM=xterm-256color + COLORTERM=truecolor;
  ;; the COLORTERM hint should escalate the detection to truecolor while
  ;; the rest of the pipeline behaves identically.
  (let ((state (phase2-close-demo-test--run-fresh
                '(("TERM"      . "xterm-256color")
                  ("COLORTERM" . "truecolor")))))
    (should (eq 'tui (plist-get state :backend-after-tui)))
    (should (eq 16777216 (plist-get state :term-colors)))
    (should (string-equal "hello world" (plist-get state :disk-contents)))))

(provide 'phase2-close-demo-test)
;;; phase2-close-demo-test.el ends here
