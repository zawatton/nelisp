;;; emacs-stub-residuals-test.el --- ERT for Phase 11.C'' residual stubs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase F (2026-05-03) — Doc 51 / nelisp-emacs.
;;
;; Phase 11.C'' deliberately kept sentinel compatibility stubs in
;; `emacs-stub.el' for names whose corresponding prefixed substrate did
;; not exist yet.  The former display probes now use a small capability
;; map keyed by `emacs-display-system'.
;;
;; `define-key-after' has since moved out of residual-stub status via
;; `emacs-keymap-define-key-after' and `emacs-keymap-builtins.el'; the
;; `emacs-stub.el' fallback remains only for minimal load-order
;; compatibility.
;; `window-live-p' and `frame-selected-window' likewise moved to
;; `emacs-window-builtins.el' once the prefixed window model grew real
;; live/deleted predicates and selected-window access.
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

(defconst emacs-stub-residuals-test--builtin-bridge-libraries
  '("emacs-buffer-builtins"
    "emacs-fileio-builtins"
    "emacs-edit-builtins"
    "emacs-keymap-builtins"
    "emacs-frame-builtins"
    "emacs-window-builtins"
    "emacs-line-builtins"
    "emacs-minibuffer-builtins"
    "emacs-search-builtins"
    "emacs-command-loop-builtins"
    "emacs-process-builtins"
    "emacs-undo-builtins"
    "emacs-mode-builtins"
    "emacs-faces-builtins"
    "emacs-font-lock-builtins"
    "emacs-redisplay-builtins")
  "Builtin bridge libraries that must install over standalone stubs.")

(defun emacs-stub-residuals-test--source-file (library)
  "Return source .el path for LIBRARY."
  (let ((file (locate-library library)))
    (when (and file (string-match-p "\\.elc\\'" file))
      (setq file (concat (substring file 0 (- (length file) 1)))))
    file))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-stub-residuals-test/feature-and-fboundp ()
  (should (featurep 'emacs-stub))
  (dolist (sym '(define-key-after
                  display-graphic-p display-color-p display-multi-frame-p
                  window-system
                  emacs-display-window-system emacs-display-graphic-p
                  emacs-display-color-p emacs-display-multi-frame-p
                  window-live-p frame-selected-window
                  custom-add-option custom-add-frequent-value
                  custom-variable-p))
    (should (fboundp sym)))
  (should (boundp 'emacs-display-system))
  (should (boundp 'initial-window-system)))

;;;; B. define-key-after has a real keymap substrate

(ert-deftest emacs-stub-residuals-test/define-key-after-bridged-by-keymap-builtins ()
  (require 'emacs-keymap-builtins)
  (let ((map (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (should (fboundp 'emacs-keymap-define-key-after))
    (emacs-keymap-define-key map "a" 'cmd-a)
    (emacs-keymap-define-key map "b" 'cmd-b)
    (should (eq 'cmd-c
                (emacs-keymap-define-key-after map "c" 'cmd-c ?b)))
    (emacs-keymap-map-keymap (lambda (k _v) (push k seen)) map)
    (should (equal (nreverse seen) (list ?b ?c ?a)))))

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

(ert-deftest emacs-stub-residuals-test/display-probe-install-overwrites-standalone-stubs ()
  ;; The display map lives in `emacs-stub.el' itself, after the old
  ;; no-op stubs.  Standalone NeLisp must overwrite those earlier
  ;; definitions; host Emacs must keep its C builtins.
  (should (fboundp 'emacs-stub--install-function-p))
  (should-not (emacs-stub--install-function-p 'display-graphic-p))
  (let* ((file (locate-library "emacs-stub"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(window-system display-graphic-p display-color-p
                                   display-multi-frame-p))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-stub--install-function-p '%s)" sym)
                 nil t))))))

(ert-deftest emacs-stub-residuals-test/builtin-bridges-have-standalone-install-gates ()
  "Every builtin bridge must have an install gate aware of standalone NeLisp.

This is a coarse regression guard for the old `(unless (fboundp ...))'
pattern: host Emacs should keep its builtins, but standalone NeLisp
must be able to overwrite early bootstrap stubs with real substrates."
  (dolist (library emacs-stub-residuals-test--builtin-bridge-libraries)
    (let* ((gate (format "%s--install-function-p" library))
           (file (emacs-stub-residuals-test--source-file library)))
      (should (and file (file-exists-p file)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (should (search-forward (concat "(defun " gate) nil t))
        (goto-char (point-min))
        (should (search-forward "(boundp 'emacs-version)" nil t))))))

;;;; D. window-live-p has a real window substrate

(ert-deftest emacs-stub-residuals-test/window-live-p-bridged-by-window-builtins ()
  (require 'emacs-window-builtins)
  (emacs-window-reset)
  (unwind-protect
      (let* ((w1 (emacs-window-selected-window))
             (w2 (emacs-window-split-window-vertically)))
        (should (fboundp 'emacs-window-window-live-p))
        (should (emacs-window-window-live-p w1))
        (should (emacs-window-window-live-p w2))
        (emacs-window-delete-window w2)
        (should (emacs-window-window-live-p w1))
        (should-not (emacs-window-window-live-p w2)))
    (emacs-window-reset)))

;;;; E. frame-selected-window has a real window substrate

(ert-deftest emacs-stub-residuals-test/frame-selected-window-bridged-by-window-builtins ()
  (require 'emacs-window-builtins)
  (emacs-window-reset)
  (unwind-protect
      (let ((w1 (emacs-window-selected-window))
            (w2 (emacs-window-split-window-vertically)))
        (should (fboundp 'emacs-window-frame-selected-window))
        (should (eq w1 (emacs-window-frame-selected-window)))
        (emacs-window-select-window w2)
        (should (eq w2 (emacs-window-frame-selected-window 'ignored-frame))))
    (emacs-window-reset)))

;;;; F. Custom metadata helpers

(ert-deftest emacs-stub-residuals-test/custom-add-option-deduplicates ()
  (let ((sym (make-symbol "nelisp-emacs-custom-option")))
    (custom-add-option sym 'turn-on-auto-fill)
    (custom-add-option sym 'turn-on-auto-fill)
    (custom-add-option sym 'flyspell-mode)
    (should (equal (get sym 'custom-options)
                   '(flyspell-mode turn-on-auto-fill)))))

(ert-deftest emacs-stub-residuals-test/custom-variable-p-metadata ()
  (let ((standard (make-symbol "nelisp-emacs-custom-standard"))
        (autoloaded (make-symbol "nelisp-emacs-custom-autoloaded"))
        (plain (make-symbol "nelisp-emacs-custom-plain")))
    (put standard 'standard-value '(42))
    (put autoloaded 'custom-autoload t)
    (should (custom-variable-p standard))
    (should (custom-variable-p autoloaded))
    (should-not (custom-variable-p plain))
    (should-not (custom-variable-p "not-a-symbol"))))

;;;; G. Idempotence — re-loading emacs-stub leaves bindings unchanged

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
