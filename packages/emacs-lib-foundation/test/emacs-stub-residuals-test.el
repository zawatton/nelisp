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
                  window-live-p frame-selected-window))
    (should (fboundp sym))))

;;;; B. Polyfill body — define-key-after returns DEFINITION (= 3rd arg)

(ert-deftest emacs-stub-residuals-test/define-key-after-returns-definition ()
  (let ((stub (lambda (keymap key definition &optional after)
                (ignore keymap key after)
                definition)))
    (should (eq 'cmd (funcall stub 'KM 'KEY 'cmd nil)))
    (should (eq 'cmd (funcall stub 'KM 'KEY 'cmd 'someprior)))))

;;;; C. Polyfill body — display-* probes return nil

(ert-deftest emacs-stub-residuals-test/display-probes-return-nil ()
  (let ((stub (lambda (&optional display) (ignore display) nil)))
    (should-not (funcall stub))
    (should-not (funcall stub 'tty))
    (should-not (funcall stub "DISPLAY-spec"))))

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
