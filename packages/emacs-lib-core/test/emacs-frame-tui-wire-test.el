;;; emacs-frame-tui-wire-test.el --- ERT tests for the TUI backend wire-up  -*- lexical-binding: t; -*-

;; T159 / Doc 43 §3.1 Phase 11.A integration completion.
;;
;; Covers `emacs-frame-use-tui-backend' /
;; `emacs-frame-use-stub-backend' and verifies the dispatch-table
;; routing per Doc 43 §2.1 step 2 + Doc 34 §2.11 swap-in invariant.

(require 'ert)
(require 'cl-lib)
(require 'emacs-frame)
(require 'emacs-keymap)
(require 'emacs-tui-backend)
(require 'emacs-tui-event)
(require 'emacs-tui-terminfo)

;;; Fixture

(defmacro emacs-frame-tui-wire-test--with-fresh-world (&rest body)
  "Run BODY with a clean emacs-frame + TUI substrate state.

The fixture also rebinds `emacs-tui-backend-output-fn' to a no-op so
ANSI escapes are silently discarded during the test, and forces a
deterministic terminfo override (`xterm-256color' env) so detection
results do not vary with the host TTY."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-frame--id-counter            0)
         (emacs-frame--registry              nil)
         (emacs-frame--selected              nil)
         (emacs-frame--focus                 nil)
         (emacs-frame--backend-dispatch      nil)
         (emacs-frame--tui-handle            nil)
         (emacs-frame--tui-event-handle      nil)
         (emacs-frame--tui-terminfo          nil)
         (emacs-tui-backend-output-fn        (lambda (_s) nil))
         (emacs-tui-backend--handle-counter  0)
         (emacs-tui-event--handle-counter    0)
         (emacs-tui-terminfo--cache          nil))
     (unwind-protect
         (progn ,@body)
       (when emacs-frame--tui-handle
         (ignore-errors (emacs-frame-use-stub-backend))))))

(defun emacs-frame-tui-wire-test--xterm-env ()
  "Return a deterministic env alist used for terminfo detection in ERT."
  '(("TERM" . "xterm-256color")))

;;;; 1. install lifecycle (4 tests)

(ert-deftest emacs-frame-tui-wire-use-tui-backend-flips-current-backend ()
  "After `emacs-frame-use-tui-backend' the backend symbol is `tui'."
  (emacs-frame-tui-wire-test--with-fresh-world
    (should (eq 'stub (emacs-frame-current-backend)))
    (let ((res (emacs-frame-use-tui-backend
                (list :env (emacs-frame-tui-wire-test--xterm-env)))))
      (should (plistp res))
      (should (eq 'tui (emacs-frame-current-backend)))
      (should (emacs-tui-backend-handlep (plist-get res :backend)))
      (should (plist-get res :event)))))

(ert-deftest emacs-frame-tui-wire-use-tui-backend-stores-handles ()
  "The accessors expose the live backend / event / info handles."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (should (emacs-frame-tui-handle))
    (should (emacs-frame-tui-event-handle))
    (let ((info (emacs-frame-tui-info)))
      (should info)
      ;; xterm-256color → 256 colors detected.
      (should (eq 256 (plist-get info :colors))))))

(ert-deftest emacs-frame-tui-wire-revert-to-stub-backend ()
  "`emacs-frame-use-stub-backend' tears down + reverts to stub."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (should (eq 'tui (emacs-frame-current-backend)))
    (should (emacs-frame-use-stub-backend))
    (should (eq 'stub (emacs-frame-current-backend)))
    (should-not (emacs-frame-tui-handle))
    (should-not (emacs-frame-tui-event-handle))
    (should-not (emacs-frame-tui-info))))

(ert-deftest emacs-frame-tui-wire-use-tui-backend-is-idempotent ()
  "Calling `use-tui-backend' twice tears down the first install cleanly."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let* ((first (emacs-frame-use-tui-backend
                   (list :env (emacs-frame-tui-wire-test--xterm-env))))
           (handle1 (plist-get first :backend))
           (second (emacs-frame-use-tui-backend
                    (list :env (emacs-frame-tui-wire-test--xterm-env))))
           (handle2 (plist-get second :backend)))
      (should-not (eq handle1 handle2))
      (should (eq handle2 (emacs-frame-tui-handle))))))

;;;; 2. dispatch routing (5 tests)

(ert-deftest emacs-frame-tui-wire-make-frame-uses-tui ()
  "`make-frame' after wire-up populates BACKEND-OBJ via TUI dispatch."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let* ((f (emacs-frame-make-frame '((width . 90) (height . 30)
                                        (name . "alpha"))))
           (obj (emacs-frame-backend-obj f)))
      (should (eq 'tui (emacs-frame-backend f)))
      (should obj)
      (should (emacs-tui-backend-framep obj))
      (should (equal "alpha" (emacs-tui-backend-frame-name obj)))
      (should (= 90 (emacs-tui-backend-frame-width  obj)))
      (should (= 30 (emacs-tui-backend-frame-height obj))))))

(ert-deftest emacs-frame-tui-wire-set-frame-size-routes-to-tui ()
  "`set-frame-size' rewrites the per-frame TUI canvas dimensions."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let* ((f   (emacs-frame-make-frame))
           (obj (emacs-frame-backend-obj f)))
      (emacs-frame-set-frame-size f 120 36)
      (should (= 120 (emacs-tui-backend-frame-width  obj)))
      (should (= 36  (emacs-tui-backend-frame-height obj))))))

(ert-deftest emacs-frame-tui-wire-delete-frame-deregisters-from-tui ()
  "`delete-frame' deregisters the TUI frame record from the handle."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let* ((handle (emacs-frame-tui-handle))
           (_root  (emacs-frame-selected-frame))
           (f      (emacs-frame-make-frame)))
      ;; Two TUI frames now live (one auto-created via ensure-initial,
      ;; one explicit).
      (should (= 2 (length (emacs-tui-backend-handle-frames handle))))
      (emacs-frame-delete-frame f)
      (should (= 1 (length (emacs-tui-backend-handle-frames handle)))))))

(ert-deftest emacs-frame-tui-wire-visibility-hides-and-shows-cursor ()
  "Toggling visibility flips the per-frame TUI cursor state."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let* ((f   (emacs-frame-make-frame))
           (obj (emacs-frame-backend-obj f)))
      (emacs-frame-make-frame-visible f)
      (should (eq 0 (emacs-tui-backend-frame-cursor-row obj)))
      (should (eq 0 (emacs-tui-backend-frame-cursor-col obj)))
      (emacs-frame-make-frame-invisible f)
      (should-not (emacs-tui-backend-frame-cursor-row obj))
      (should-not (emacs-tui-backend-frame-cursor-col obj)))))

(ert-deftest emacs-frame-tui-wire-revert-then-make-uses-stub ()
  "After `use-stub-backend' a fresh `make-frame' goes back to stub mode."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (emacs-frame-use-stub-backend)
    (let ((f (emacs-frame-make-frame)))
      (should (eq 'stub (emacs-frame-backend f)))
      (should-not (emacs-frame-backend-obj f)))))

;;;; 3. capability propagation (3 tests)

(ert-deftest emacs-frame-tui-wire-capability-query-delegates-to-tui ()
  "Stub-mode core caps stay supported; TUI declares its base set."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    ;; Core stub caps still satisfied (mandatory).
    (should (emacs-frame-capability-p 'frame-create))
    (should (emacs-frame-capability-p 'frame-destroy))
    (should (emacs-frame-capability-p 'frame-resize))
    ;; TUI base caps from Doc 43 §2.5.
    (should (emacs-frame-capability-p 'text))
    (should (emacs-frame-capability-p 'basic-color))
    (should (emacs-frame-capability-p 'keyboard))
    (should (emacs-frame-capability-p 'layout-box))
    ;; Undeclared cap returns nil (no truecolor on bare xterm-256).
    (should-not (emacs-frame-capability-p 'truecolor))
    ;; IME / image-* are explicitly out of MVP scope.
    (should-not (emacs-frame-capability-p 'ime))
    (should-not (emacs-frame-capability-p 'image-png))))

(ert-deftest emacs-frame-tui-wire-capabilities-override-arg ()
  "Explicit `:capabilities' override skips terminfo detection."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((res (emacs-frame-use-tui-backend
                (list :capabilities '(text keyboard truecolor)))))
      ;; No terminfo plist when caps came in explicitly.
      (should-not (plist-get res :info))
      (should-not (emacs-frame-tui-info))
      (should     (emacs-frame-capability-p 'truecolor))
      (should-not (emacs-frame-capability-p 'basic-color)))))

(ert-deftest emacs-frame-tui-wire-capability-elevation-truecolor ()
  "A truecolor-capable env (COLORTERM=truecolor) elevates capabilities."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env '(("TERM" . "xterm")
                  ("COLORTERM" . "truecolor"))))
    (should (emacs-frame-capability-p 'truecolor))
    (should (emacs-frame-capability-p '256-color))))

;;;; 4. event integration (2 tests)

(ert-deftest emacs-frame-tui-wire-event-handle-is-pollable ()
  "Polling the wired event handle yields the injected event."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let ((eh (emacs-frame-tui-event-handle)))
      (emacs-tui-event-feed-bytes eh "a")
      (let ((ev (emacs-tui-event-poll eh)))
        (should ev)
        (should (eq 'key (plist-get ev :type)))
        (should (eq ?a (plist-get ev :name)))))))

(ert-deftest emacs-frame-tui-wire-resize-dispatch-routes-to-event ()
  "Dispatching a resize event populates the event handle's queue."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let ((eh (emacs-frame-tui-event-handle)))
      (emacs-tui-event-dispatch-resize eh 132 50)
      (let ((ev (emacs-tui-event-poll eh)))
        (should ev)
        (should (eq 'resize (plist-get ev :type)))
        (should (eq 132 (plist-get ev :width)))
        (should (eq 50  (plist-get ev :height)))))))

;;;; 5. SIGWINCH wire-up (T160 / Doc 43 §3.1 Phase 11.A close gate #4)

(ert-deftest emacs-frame-tui-wire-sigwinch-installed-on-use-tui ()
  "`use-tui-backend' installs the SIGWINCH callback on the event handle."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let ((eh (emacs-frame-tui-event-handle)))
      (should (eq #'emacs-frame--tui-on-sigwinch
                  (emacs-tui-event-handle-sigwinch-cb eh)))
      (should (memq eh emacs-tui-event--installed-handles)))))

(ert-deftest emacs-frame-tui-wire-sigwinch-uninstalled-on-use-stub ()
  "`use-stub-backend' removes the handle from the resize subscriber list."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let ((eh (emacs-frame-tui-event-handle)))
      (should (memq eh emacs-tui-event--installed-handles))
      (emacs-frame-use-stub-backend)
      (should-not (memq eh emacs-tui-event--installed-handles)))))

(ert-deftest emacs-frame-tui-wire-sigwinch-resizes-live-tui-frames ()
  "Dispatching a resize event updates each live TUI frame's size."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let* ((f (emacs-frame-make-frame '((width . 80) (height . 24)))))
      (emacs-tui-event-dispatch-resize
       (emacs-frame-tui-event-handle) 132 50)
      (should (= 132 (emacs-frame-frame-width  f)))
      (should (= 50  (emacs-frame-frame-height f)))
      ;; The per-frame TUI canvas is also resized through the dispatch
      ;; table since `emacs-frame-set-frame-size' cascades into
      ;; `:frame-resize'.
      (let ((obj (emacs-frame-backend-obj f)))
        (should (= 132 (emacs-tui-backend-frame-width  obj)))
        (should (= 50  (emacs-tui-backend-frame-height obj)))))))

(ert-deftest emacs-frame-tui-wire-sigwinch-invokes-resize-hook ()
  "`emacs-frame-tui-resize-hook' fires once per live TUI frame."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((seen nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (let* ((emacs-frame-tui-resize-hook
              (list (lambda (frame w h) (push (list frame w h) seen))))
             (f1 (emacs-frame-make-frame '((width . 80) (height . 24)
                                           (name . "alpha"))))
             (f2 (emacs-frame-make-frame '((width . 80) (height . 24)
                                           (name . "beta")))))
        (emacs-tui-event-dispatch-resize
         (emacs-frame-tui-event-handle) 100 40)
        ;; Root frame auto-created by `ensure-initial' plus f1 + f2
        ;; = 3 hook calls.
        (should (= 3 (length seen)))
        (dolist (rec seen)
          (should (eq 100 (nth 1 rec)))
          (should (eq 40  (nth 2 rec))))
        (let ((resized-frames (mapcar #'car seen)))
          (should (memq f1 resized-frames))
          (should (memq f2 resized-frames)))))))

(ert-deftest emacs-frame-tui-wire-sigwinch-clamps-to-minimum ()
  "Sub-minimum WIDTH / HEIGHT are clamped before resize cascades."
  (emacs-frame-tui-wire-test--with-fresh-world
    (emacs-frame-use-tui-backend
     (list :env (emacs-frame-tui-wire-test--xterm-env)))
    (let ((f (emacs-frame-make-frame '((width . 80) (height . 24)))))
      ;; Width / height of 1 should be clamped up to the configured
      ;; minimums (`emacs-frame--min-cols' = 2,
      ;; `emacs-frame--min-lines' = 1).
      (emacs-tui-event-dispatch-resize
       (emacs-frame-tui-event-handle) 1 1)
      (should (>= (emacs-frame-frame-width  f) emacs-frame--min-cols))
      (should (>= (emacs-frame-frame-height f) emacs-frame--min-lines)))))

;;;; 6. Keyboard wire-up (T161 / Doc 43 §3.1 Phase 11.A close gate #5)

(ert-deftest emacs-frame-tui-wire-keyboard-installed-on-use-tui ()
  "`use-tui-backend' installs `emacs-frame--tui-read-event' as the keymap reader."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (should (eq #'emacs-frame--tui-read-event
                  emacs-keymap--read-event-fn)))))

(ert-deftest emacs-frame-tui-wire-keyboard-restored-on-use-stub ()
  "`use-stub-backend' restores the prior `emacs-keymap--read-event-fn'."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let* ((sentinel (lambda () 'sentinel))
           (emacs-keymap--read-event-fn sentinel))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (should (eq #'emacs-frame--tui-read-event
                  emacs-keymap--read-event-fn))
      (emacs-frame-use-stub-backend)
      (should (eq sentinel emacs-keymap--read-event-fn)))))

(ert-deftest emacs-frame-tui-wire-keyboard-restores-nil-default ()
  "A nil prior value is faithfully restored (= no leak into stub mode)."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (emacs-frame-use-stub-backend)
      (should-not emacs-keymap--read-event-fn))))

(ert-deftest emacs-frame-tui-wire-keyboard-translates-bare-char ()
  "A bare ASCII byte fed to the event handle is returned as an integer."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (emacs-tui-event-feed-bytes (emacs-frame-tui-event-handle) "a")
      (should (eq ?a (funcall emacs-keymap--read-event-fn))))))

(ert-deftest emacs-frame-tui-wire-keyboard-translates-control-char ()
  "C-a (= 0x01) is folded into an integer with the Emacs control bit set."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (emacs-tui-event-feed-bytes
       (emacs-frame-tui-event-handle) (string 1))
      (let ((elem (funcall emacs-keymap--read-event-fn)))
        (should (integerp elem))
        (should (/= 0 (logand elem ?\C-\^@)))
        (should (= ?a (logxor elem ?\C-\^@)))))))

(ert-deftest emacs-frame-tui-wire-keyboard-translates-arrow-symbol ()
  "An ESC `[A' CSI sequence surfaces as the `up' symbol."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (emacs-tui-event-feed-bytes
       (emacs-frame-tui-event-handle) (concat (string ?\e) "[A"))
      (should (eq 'up (funcall emacs-keymap--read-event-fn))))))

(ert-deftest emacs-frame-tui-wire-keyboard-drops-resize-events ()
  "Resize events are silently consumed before the next key surfaces."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (emacs-tui-event-dispatch-resize
       (emacs-frame-tui-event-handle) 132 50)
      (emacs-tui-event-feed-bytes (emacs-frame-tui-event-handle) "z")
      (should (eq ?z (funcall emacs-keymap--read-event-fn))))))

(ert-deftest emacs-frame-tui-wire-keyboard-forwards-read-timeout ()
  "`emacs-frame-tui-read-event-timeout-ms' is passed to the event poller."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil)
          (emacs-frame-tui-read-event-timeout-ms 17)
          (seen-handle nil)
          (seen-timeout nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (cl-letf (((symbol-function 'emacs-tui-event-poll)
                 (lambda (handle timeout-ms)
                   (setq seen-handle handle
                         seen-timeout timeout-ms)
                   '(:type key :name ?k :modifiers nil))))
        (should (eq ?k (funcall emacs-keymap--read-event-fn))))
      (should (eq (emacs-frame-tui-event-handle) seen-handle))
      (should (= 17 seen-timeout)))))

(ert-deftest emacs-frame-tui-wire-keyboard-empty-queue-signals ()
  "An empty queue signals `emacs-keymap-error' (= default-reader contract)."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (should-error (funcall emacs-keymap--read-event-fn)
                    :type 'emacs-keymap-error))))

(ert-deftest emacs-frame-tui-wire-keyboard-no-handle-signals ()
  "`emacs-frame--tui-read-event' signals when no event handle is installed."
  (emacs-frame-tui-wire-test--with-fresh-world
    (should-not emacs-frame--tui-event-handle)
    (should-error (emacs-frame--tui-read-event)
                  :type 'emacs-keymap-error)))

(ert-deftest emacs-frame-tui-wire-keyboard-key-hook-fires ()
  "`emacs-frame-tui-key-hook' receives the raw key-event plist."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((seen nil)
          (emacs-keymap--read-event-fn nil))
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (let ((emacs-frame-tui-key-hook
             (list (lambda (ev) (push ev seen)))))
        (emacs-tui-event-feed-bytes (emacs-frame-tui-event-handle) "Q")
        (funcall emacs-keymap--read-event-fn)
        (should (= 1 (length seen)))
        (let ((ev (car seen)))
          (should (eq 'key (plist-get ev :type)))
          (should (eq ?Q (plist-get ev :name))))))))

(ert-deftest emacs-frame-tui-wire-keyboard-via-read-key-sequence ()
  "`emacs-keymap-read-key-sequence' consumes events through the TUI bridge."
  (emacs-frame-tui-wire-test--with-fresh-world
    (let ((emacs-keymap--read-event-fn nil)
          (emacs-keymap-global-map (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key emacs-keymap-global-map [?x] 'self-insert)
      (emacs-frame-use-tui-backend
       (list :env (emacs-frame-tui-wire-test--xterm-env)))
      (emacs-tui-event-feed-bytes (emacs-frame-tui-event-handle) "x")
      (let ((seq (emacs-keymap-read-key-sequence nil)))
        (should (equal [?x] seq))))))

(provide 'emacs-frame-tui-wire-test)
;;; emacs-frame-tui-wire-test.el ends here
