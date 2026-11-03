;;; emacs-tui-stub-test.el --- ERT for emacs-tui-stub.el  -*- lexical-binding: t; -*-

;; Phase 1 module 6/6 ERT per nelisp-emacs Doc 01 (LOCKED v2).
;; Doc 34 v2 §2.11 stub mode invariant + Doc 43 v2 §2.5a degrade
;; contract + Doc 43 v2 §2.5 capability matrix coverage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-tui-stub)

;;; A. backend lifecycle

(ert-deftest emacs-tui-stub-test-init-returns-handle ()
  "init returns an alive handle satisfying the predicate."
  (let ((h (emacs-tui-stub-init)))
    (should (emacs-tui-stub-handlep h))
    (should (emacs-tui-stub-handle-alive-p h))
    (should (symbolp (emacs-tui-stub-handle-id h)))))

(ert-deftest emacs-tui-stub-test-init-handles-have-unique-ids ()
  "Two consecutive init calls produce distinct ids."
  (let ((h1 (emacs-tui-stub-init))
        (h2 (emacs-tui-stub-init)))
    (should-not (eq (emacs-tui-stub-handle-id h1)
                    (emacs-tui-stub-handle-id h2)))))

(ert-deftest emacs-tui-stub-test-shutdown-marks-dead ()
  "shutdown clears the alive-p flag and rejects subsequent ops."
  (let ((h (emacs-tui-stub-init)))
    (should (eq t (emacs-tui-stub-shutdown h)))
    (should-not (emacs-tui-stub-handle-alive-p h))
    (should-error (emacs-tui-stub-capabilities h)
                  :type 'emacs-tui-stub-bad-handle)))

;;; B. capability query (Doc 43 §2.5 + §2.5a)

(ert-deftest emacs-tui-stub-test-default-capabilities-mvp-subset ()
  "Default capability list = Doc 43 §2.5 TUI MVP subset."
  (let ((h (emacs-tui-stub-init)))
    (should (equal (sort (emacs-tui-stub-capabilities h) #'string<)
                   (sort (copy-sequence
                          emacs-tui-stub-default-capabilities)
                         #'string<)))))

(ert-deftest emacs-tui-stub-test-get-capability-returns-bool ()
  "get-capability returns t for declared and nil for undeclared, never raises."
  (let ((h (emacs-tui-stub-init)))
    (should (eq t   (emacs-tui-stub-get-capability h 'text)))
    (should (eq t   (emacs-tui-stub-get-capability h 'keyboard)))
    (should (eq nil (emacs-tui-stub-get-capability h '256-color)))
    (should (eq nil (emacs-tui-stub-get-capability h 'image-kitty-graphics)))
    (should (eq nil (emacs-tui-stub-get-capability h 'completely-bogus)))))

(ert-deftest emacs-tui-stub-test-degrade-contract-signal ()
  "Calling an API that requires an undeclared capability signals
display-spec-unsupported with the Doc 43 §2.5a plist data."
  (let* ((h (emacs-tui-stub-init '(keyboard)))   ; no `text'
         (f (emacs-tui-stub-frame-create h "F")))
    (let ((err (should-error
                (emacs-tui-stub-canvas-draw-text h f 0 0 "x")
                :type 'display-spec-unsupported)))
      (let ((data (cdr err)))
        (should (eq 'text             (plist-get data :capability)))
        (should (eq 'canvas-draw-text (plist-get data :api)))
        (should (eq 'stub             (plist-get data :backend)))))))

;;; C. frame management (Doc 34 v2 §2.11 invariant)

(ert-deftest emacs-tui-stub-test-frame-create-default-80x24 ()
  "Frame width/height match the LOCKED 80x24 invariant."
  (let* ((h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "main")))
    (should (emacs-tui-stub-framep f))
    (should (equal "main" (emacs-tui-stub-frame-name f)))
    (should (= 80 (emacs-tui-stub-frame-width f)))
    (should (= 24 (emacs-tui-stub-frame-height f)))))

(ert-deftest emacs-tui-stub-test-frame-ids-unique-and-monotonic ()
  "Each create yields a fresh integer id, monotonically increasing."
  (let* ((h (emacs-tui-stub-init))
         (f1 (emacs-tui-stub-frame-create h "a"))
         (f2 (emacs-tui-stub-frame-create h "b"))
         (f3 (emacs-tui-stub-frame-create h "c")))
    (should (= 1 (emacs-tui-stub-frame-id f1)))
    (should (= 2 (emacs-tui-stub-frame-id f2)))
    (should (= 3 (emacs-tui-stub-frame-id f3)))))

(ert-deftest emacs-tui-stub-test-frame-destroy-removes-from-registry ()
  "destroy purges the entry; subsequent ops on same record raise."
  (let* ((h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    (should (eq t (emacs-tui-stub-frame-destroy h f)))
    (should-error (emacs-tui-stub-frame-destroy h f)
                  :type 'emacs-tui-stub-bad-frame)))

(ert-deftest emacs-tui-stub-test-frame-resize-default-no-op ()
  "Default `emacs-tui-stub-resize-allowed' = nil keeps 80x24."
  (let* ((emacs-tui-stub-resize-allowed nil)
         (h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    (emacs-tui-stub-frame-resize h f 200 50)
    (should (= 80 (emacs-tui-stub-frame-width f)))
    (should (= 24 (emacs-tui-stub-frame-height f)))))

(ert-deftest emacs-tui-stub-test-frame-resize-applied-when-allowed ()
  "When `emacs-tui-stub-resize-allowed' is t, dimensions update."
  (let* ((emacs-tui-stub-resize-allowed t)
         (h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    (emacs-tui-stub-frame-resize h f 100 30)
    (should (= 100 (emacs-tui-stub-frame-width f)))
    (should (=  30 (emacs-tui-stub-frame-height f)))))

;;; D. canvas drawing

(ert-deftest emacs-tui-stub-test-canvas-draw-writes-cells ()
  "draw-text returns the number of cells written and stamps the canvas."
  (let* ((h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    (should (= 5 (emacs-tui-stub-canvas-draw-text h f 0 0 "hello")))
    (let* ((canvas (emacs-tui-stub-frame-canvas f))
           (row    (aref canvas 0)))
      (should (eq ?h (car (aref row 0))))
      (should (eq ?o (car (aref row 4))))
      (should (eq ?\s (car (aref row 5)))))))   ; untouched

(ert-deftest emacs-tui-stub-test-canvas-draw-clips-out-of-bounds ()
  "Writes that overflow the row are clipped silently to width."
  (let* ((h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    ;; col 78, 5 chars → only 2 fit (cols 78,79).
    (should (= 2 (emacs-tui-stub-canvas-draw-text h f 0 78 "abcde")))
    ;; row out-of-range is fully clipped.
    (should (= 0 (emacs-tui-stub-canvas-draw-text h f 999 0 "x")))))

(ert-deftest emacs-tui-stub-test-canvas-clear-resets-cells ()
  "After clear, every cell is back to space."
  (let* ((h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    (emacs-tui-stub-canvas-draw-text h f 1 1 "ABC")
    (emacs-tui-stub-canvas-clear h f)
    (let ((row (aref (emacs-tui-stub-frame-canvas f) 1)))
      (dotimes (c 80)
        (should (eq ?\s (car (aref row c))))))))

(ert-deftest emacs-tui-stub-test-canvas-flush-tracks-dirty-bit ()
  "flush returns t once after a draw, then nil until next mutation."
  (let* ((h (emacs-tui-stub-init))
         (f (emacs-tui-stub-frame-create h "x")))
    ;; Newly created frame is dirty (canvas allocation).
    (should (eq t   (emacs-tui-stub-canvas-flush h f)))
    (should (eq nil (emacs-tui-stub-canvas-flush h f)))
    (emacs-tui-stub-canvas-draw-text h f 0 0 "z")
    (should (eq t   (emacs-tui-stub-canvas-flush h f)))
    (should (eq nil (emacs-tui-stub-canvas-flush h f)))))

;;; E. event polling

(ert-deftest emacs-tui-stub-test-event-poll-empty-returns-nil ()
  "poll on an empty queue returns nil (Doc 43 §2.6 pull-on-demand)."
  (let ((h (emacs-tui-stub-init)))
    (should (eq nil (emacs-tui-stub-event-poll h)))))

(ert-deftest emacs-tui-stub-test-event-inject-and-poll-fifo ()
  "inject + poll round-trips events in FIFO order."
  (let ((h (emacs-tui-stub-init)))
    (emacs-tui-stub-event-inject h 'a)
    (emacs-tui-stub-event-inject h 'b)
    (emacs-tui-stub-event-inject h 'c)
    (should (eq 'a (emacs-tui-stub-event-poll h)))
    (should (eq 'b (emacs-tui-stub-event-poll h)))
    (should (eq 'c (emacs-tui-stub-event-poll h)))
    (should (eq nil (emacs-tui-stub-event-poll h)))))

;;; F. cross-cutting (handle / frame error paths, version constants)

(ert-deftest emacs-tui-stub-test-bad-handle-rejected-everywhere ()
  "Non-handle inputs raise emacs-tui-stub-bad-handle on every public op."
  (dolist (fn '(emacs-tui-stub-capabilities
                emacs-tui-stub-event-poll))
    (should-error (funcall fn 'not-a-handle)
                  :type 'emacs-tui-stub-bad-handle)))

(ert-deftest emacs-tui-stub-test-bad-frame-rejected ()
  "Frames from a different handle raise emacs-tui-stub-bad-frame."
  (let* ((h1 (emacs-tui-stub-init))
         (h2 (emacs-tui-stub-init))
         (f1 (emacs-tui-stub-frame-create h1 "x")))
    (should-error (emacs-tui-stub-frame-destroy h2 f1)
                  :type 'emacs-tui-stub-bad-frame)))

(ert-deftest emacs-tui-stub-test-contract-version-constants ()
  "The two LOCKED contract-version constants are = 1 (Phase 1 baseline)."
  (should (= 1 emacs-tui-stub-frame-stub-invariant-version))
  (should (= 1 emacs-tui-stub-degrade-contract-version))
  (should (= 80 emacs-tui-stub-frame-default-width))
  (should (= 24 emacs-tui-stub-frame-default-height)))

(provide 'emacs-tui-stub-test)

;;; emacs-tui-stub-test.el ends here
