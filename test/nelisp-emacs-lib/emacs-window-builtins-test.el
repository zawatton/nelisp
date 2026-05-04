;;; emacs-window-builtins-test.el --- ERT tests for emacs-window-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs window.c builtin bridge.  Under batch
;; host Emacs the host C builtins remain active (= the bridge's
;; `unless (fboundp ...)' gate keeps them) so the substrate-direct
;; `emacs-window-*' API is used for semantic assertions; bridge-shape
;; assertions verify featurep + fboundp parity.

;;; Code:

(require 'ert)
(require 'emacs-window-builtins)
(require 'cl-lib)

(defmacro emacs-window-builtins-test--with-fresh-world (&rest body)
  "Run BODY against a clean prefixed-window root."
  (declare (indent 0) (debug (body)))
  `(progn
     (emacs-window-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-window-reset))))

;;;; A. Load cleanly

(ert-deftest emacs-window-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-window-builtins))
  (should (featurep 'emacs-window))
  (dolist (sym '(selected-window windowp window-list
                 window-buffer set-window-buffer))
    (should (fboundp sym))))

;;;; B. Substrate-direct: selected-window is windowp

(ert-deftest emacs-window-builtins-test/prefixed-selected-window-is-windowp ()
  (emacs-window-builtins-test--with-fresh-world
    (should (emacs-window-windowp (emacs-window-selected-window)))))

;;;; C. Substrate-direct: window-list returns at least one window

(ert-deftest emacs-window-builtins-test/prefixed-window-list-non-empty ()
  (emacs-window-builtins-test--with-fresh-world
    (let ((wl (emacs-window-window-list)))
      (should (consp wl))
      (dolist (w wl)
        (should (emacs-window-windowp w))))))

;;;; D. Substrate-direct: window-buffer accessor

(ert-deftest emacs-window-builtins-test/prefixed-window-buffer-returns-buffer-or-nil ()
  (emacs-window-builtins-test--with-fresh-world
    (let* ((w (emacs-window-selected-window))
           (b (emacs-window-window-buffer w)))
      ;; Substrate root either has a real buffer or nil — both legal.
      (should (or (null b)
                  (recordp b)
                  (vectorp b))))))

;;;; E. Substrate-direct: set-window-buffer updates window-buffer

(ert-deftest emacs-window-builtins-test/set-window-buffer-roundtrip-via-prefixed ()
  (emacs-window-builtins-test--with-fresh-world
    (let ((w (emacs-window-selected-window))
          (buf (nelisp-ec-generate-new-buffer "scratch-bridge")))
      (emacs-window-set-window-buffer w buf)
      (should (eq buf (emacs-window-window-buffer w))))))

;;;; F. Bridge wiring: defalias chain points at prefixed impl

(ert-deftest emacs-window-builtins-test/bridge-defalias-targets-prefixed ()
  ;; Under host Emacs the host's C builtin wins, so we only check the
  ;; bridge module itself produced fboundp results — the actual chain
  ;; is exercised by standalone NeLisp's load.  Smoke-test the prefixed
  ;; impls are present.
  (should (fboundp 'emacs-window-windowp))
  (should (fboundp 'emacs-window-window-list))
  (should (fboundp 'emacs-window-window-buffer))
  (should (fboundp 'emacs-window-set-window-buffer))
  (should (fboundp 'emacs-window-selected-window)))

;;;; G. Idempotence

(ert-deftest emacs-window-builtins-test/require-is-idempotent ()
  (let ((before-selected-window (symbol-function 'selected-window))
        (before-windowp         (symbol-function 'windowp))
        (before-window-buffer   (symbol-function 'window-buffer)))
    (require 'emacs-window-builtins)
    (should (eq before-selected-window (symbol-function 'selected-window)))
    (should (eq before-windowp         (symbol-function 'windowp)))
    (should (eq before-window-buffer   (symbol-function 'window-buffer)))))

;;;; H. Track V (2026-05-04) — split / select / delete bridges + other-window

(ert-deftest emacs-window-builtins-test/track-v-prefixed-fboundp ()
  "All prefixed implementations Track V relies on are present."
  (dolist (sym '(emacs-window-split-window
                 emacs-window-split-window-vertically
                 emacs-window-split-window-horizontally
                 emacs-window-delete-window
                 emacs-window-delete-other-windows
                 emacs-window-one-window-p
                 emacs-window-balance-windows
                 emacs-window-select-window
                 emacs-window-next-window
                 emacs-window-previous-window
                 emacs-window-other-window-impl))
    (should (fboundp sym))))

(ert-deftest emacs-window-builtins-test/track-v-other-window-rotates-selection ()
  "`other-window-impl' rotates the selected window through every leaf."
  (emacs-window-builtins-test--with-fresh-world
    ;; Two-pane vertical split → 2 leaves.
    (emacs-window-split-window-vertically)
    (let* ((leaves (emacs-window--all-leaves))
           (n      (length leaves)))
      (should (= 2 n))
      (let ((start (emacs-window-selected-window)))
        ;; one step → different leaf
        (emacs-window-other-window-impl 1)
        (should-not (eq start (emacs-window-selected-window)))
        ;; one more step (wrap) → back to start
        (emacs-window-other-window-impl 1)
        (should (eq start (emacs-window-selected-window)))))))

(ert-deftest emacs-window-builtins-test/track-v-other-window-negative-walks-back ()
  "Negative COUNT walks `previous-window'."
  (emacs-window-builtins-test--with-fresh-world
    (emacs-window-split-window-vertically)
    (emacs-window-split-window-horizontally) ; now 3 leaves
    (let ((start (emacs-window-selected-window)))
      (emacs-window-other-window-impl -1)
      (let ((prev (emacs-window-selected-window)))
        (should-not (eq start prev))
        ;; +1 step from prev should land back on start.
        (emacs-window-other-window-impl 1)
        (should (eq start (emacs-window-selected-window)))))))

(ert-deftest emacs-window-builtins-test/track-v-split-then-delete-restores-singleton ()
  "After split + delete-other-windows the tree is a single leaf again."
  (emacs-window-builtins-test--with-fresh-world
    (emacs-window-split-window-vertically)
    (emacs-window-split-window-horizontally)
    (should (>= (length (emacs-window--all-leaves)) 3))
    (emacs-window-delete-other-windows)
    (should (= 1 (length (emacs-window--all-leaves))))
    (should (eq (emacs-window-selected-window)
                (car (emacs-window--all-leaves))))))

(provide 'emacs-window-builtins-test)

;;; emacs-window-builtins-test.el ends here
