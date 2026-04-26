;;; emacs-frame-tui-wire-test.el --- ERT tests for the TUI backend wire-up  -*- lexical-binding: t; -*-

;; T159 / Doc 43 §3.1 Phase 11.A integration completion.
;;
;; Covers `emacs-frame-use-tui-backend' /
;; `emacs-frame-use-stub-backend' and verifies the dispatch-table
;; routing per Doc 43 §2.1 step 2 + Doc 34 §2.11 swap-in invariant.

(require 'ert)
(require 'cl-lib)
(require 'emacs-frame)
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

(provide 'emacs-frame-tui-wire-test)
;;; emacs-frame-tui-wire-test.el ends here
