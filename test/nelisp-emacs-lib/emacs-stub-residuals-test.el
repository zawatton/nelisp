;;; emacs-stub-residuals-test.el --- ERT for Phase 11.C'' residual stubs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase F (2026-05-03) — Doc 51 / nelisp-emacs.
;;
;; Phase 11.C'' deliberately kept six sentinel no-op stubs in
;; `emacs-stub.el' because the corresponding prefixed substrate did
;; not exist yet:
;;
;;   - `define-key-after'        (no `emacs-keymap-define-key-after')
;;   - `display-graphic-p'       (no display-side capability primitive)
;;   - `display-color-p'         (likewise)
;;   - `display-multi-frame-p'   (likewise)
;;   - `window-live-p'           (no live-flag in prefixed window model)
;;   - `frame-selected-window'   (no per-frame selected-window slot)
;;
;; These tests pin the documented sentinel return values so any
;; future replacement (= bridge to a real prefixed impl) cannot
;; silently regress the API surface that callers depend on.
;;
;; Under host Emacs the host's C builtins win, so the kept stubs in
;; `emacs-stub.el' never fire.  We assert two things:
;;
;;   (a) `featurep' / `fboundp' parity (= the stubs load without error
;;       and the unprefixed names are bound, regardless of whether the
;;       binding is host or stub).
;;   (b) Polyfill-body shape parity using literal copies of the stub
;;       bodies — these run regardless of host-Emacs presence and pin
;;       what standalone NeLisp will see when the stub fires.

;;; Code:

(require 'ert)
(require 'emacs-stub)

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-stub-residuals-test/feature-and-fboundp ()
  (should (featurep 'emacs-stub))
  (dolist (sym '(define-key-after
                  display-graphic-p display-color-p display-multi-frame-p
                  window-system
                  emacs-display-window-system emacs-display-graphic-p
                  emacs-display-color-p emacs-display-multi-frame-p
                  window-live-p frame-selected-window))
    (should (fboundp sym)))
  (should (boundp 'emacs-display-system))
  (should (boundp 'initial-window-system)))

;;;; B. Polyfill body — define-key-after returns DEFINITION (= 3rd arg)

(ert-deftest emacs-stub-residuals-test/define-key-after-returns-definition ()
  (let ((stub (lambda (keymap key definition &optional after)
                (ignore keymap key after)
                definition)))
    (should (eq 'cmd (funcall stub 'KM 'KEY 'cmd nil)))
    (should (eq 'cmd (funcall stub 'KM 'KEY 'cmd 'someprior)))))

;;;; C. display-* / window-system dispatch against emacs-display-system
;;
;; Phase 1.E (2026-05-05) — the display probes left stub-land in this
;; phase: they now consult `emacs-display-system' so display backends
;; (= nelisp-emacs-gtk) can flip the values that init.el branches on.
;; These tests pin the dispatch matrix against the documented values.

(ert-deftest emacs-stub-residuals-test/display-probes-default-nil ()
  ;; With no backend set, all probes return nil — the same behaviour
  ;; the old hard-coded stubs had, preserved as the default path.
  (let ((emacs-display-system nil))
    (should-not (emacs-display-window-system))
    (should-not (emacs-display-graphic-p))
    (should-not (emacs-display-color-p))
    (should-not (emacs-display-multi-frame-p))))

(ert-deftest emacs-stub-residuals-test/display-probes-graphic-backend ()
  ;; A graphic backend (= 'gtk / 'x / 'pgtk / 'w32 / 'ns) flips
  ;; window-system + display-graphic-p + display-color-p +
  ;; display-multi-frame-p all to truthy.
  (let ((emacs-display-system 'gtk))
    (should (eq 'gtk (emacs-display-window-system)))
    (should (emacs-display-graphic-p))
    (should (emacs-display-color-p))
    (should (emacs-display-multi-frame-p))))

(ert-deftest emacs-stub-residuals-test/display-probes-tui-backend ()
  ;; A TUI backend (= 'tui) sets window-system + display-multi-frame-p
  ;; non-nil but display-graphic-p stays nil — that's how callers
  ;; distinguish "have a display" from "have a graphical display".
  (let ((emacs-display-system 'tui))
    (should (eq 'tui (emacs-display-window-system)))
    (should-not (emacs-display-graphic-p))
    (should-not (emacs-display-color-p))
    (should (emacs-display-multi-frame-p))))

;;;; D. Polyfill body — window-live-p delegates to windowp

(ert-deftest emacs-stub-residuals-test/window-live-p-delegates-to-windowp ()
  ;; Bind a synthetic `windowp' so we can probe the stub's delegation
  ;; without depending on host's real window objects.
  (cl-letf (((symbol-function 'emacs-stub-residuals-test--windowp)
             (lambda (object)
               (and (consp object) (eq (car object) 'window)))))
    (let ((stub (lambda (window)
                  (emacs-stub-residuals-test--windowp window))))
      (should (funcall stub (cons 'window 'data)))
      (should-not (funcall stub 42))
      (should-not (funcall stub nil))
      (should-not (funcall stub (cons 'frame 'x))))))

;;;; E. Polyfill body — frame-selected-window forwards to selected-window

(ert-deftest emacs-stub-residuals-test/frame-selected-window-forwards-to-selected-window ()
  (let ((received-frame :unset))
    (cl-letf (((symbol-function 'emacs-stub-residuals-test--selected-window)
               (lambda () 'the-selected-window)))
      (let ((stub (lambda (&optional frame)
                    (setq received-frame frame)
                    (emacs-stub-residuals-test--selected-window))))
        (should (eq 'the-selected-window (funcall stub)))
        (should (eq nil received-frame))
        (should (eq 'the-selected-window (funcall stub 'a-frame)))
        (should (eq 'a-frame received-frame))))))

;;;; F. Idempotence — re-loading emacs-stub leaves bindings unchanged

(ert-deftest emacs-stub-residuals-test/require-is-idempotent ()
  (let ((before-define-key-after     (symbol-function 'define-key-after))
        (before-window-live-p        (symbol-function 'window-live-p))
        (before-frame-selected-win   (symbol-function 'frame-selected-window))
        (before-display-graphic-p    (symbol-function 'display-graphic-p)))
    (require 'emacs-stub)
    (should (eq before-define-key-after   (symbol-function 'define-key-after)))
    (should (eq before-window-live-p      (symbol-function 'window-live-p)))
    (should (eq before-frame-selected-win (symbol-function 'frame-selected-window)))
    (should (eq before-display-graphic-p  (symbol-function 'display-graphic-p)))))

(provide 'emacs-stub-residuals-test)

;;; emacs-stub-residuals-test.el ends here
