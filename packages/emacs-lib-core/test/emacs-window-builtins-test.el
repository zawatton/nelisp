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
  (dolist (sym '(selected-window windowp window-live-p window-valid-p
                 frame-selected-window window-list
                 window-buffer set-window-buffer))
    (should (fboundp sym))))

;;;; B. Substrate-direct: selected-window is windowp

(ert-deftest emacs-window-builtins-test/prefixed-selected-window-is-windowp ()
  (emacs-window-builtins-test--with-fresh-world
    (should (emacs-window-windowp (emacs-window-selected-window)))))

(ert-deftest emacs-window-builtins-test/prefixed-window-live-p-tracks-delete ()
  (emacs-window-builtins-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window-vertically)))
      (should (emacs-window-window-live-p w1))
      (should (emacs-window-window-live-p w2))
      (emacs-window-delete-window w2)
      (should (emacs-window-window-live-p w1))
      (should-not (emacs-window-window-live-p w2))
      (should-not (emacs-window-window-valid-p w2)))))

(ert-deftest emacs-window-builtins-test/prefixed-frame-selected-window-follows-selection ()
  (emacs-window-builtins-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window))
          (w2 (emacs-window-split-window-vertically)))
      (should (eq w1 (emacs-window-frame-selected-window)))
      (emacs-window-select-window w2)
      (should (eq w2 (emacs-window-frame-selected-window 'ignored-frame))))))

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
  (should (fboundp 'emacs-window-window-live-p))
  (should (fboundp 'emacs-window-window-valid-p))
  (should (fboundp 'emacs-window-frame-selected-window))
  (should (fboundp 'emacs-window-window-list))
  (should (fboundp 'emacs-window-window-buffer))
  (should (fboundp 'emacs-window-set-window-buffer))
  (should (fboundp 'emacs-window-selected-window)))

;;;; G. Idempotence

(ert-deftest emacs-window-builtins-test/require-is-idempotent ()
  (let ((before-selected-window (symbol-function 'selected-window))
        (before-windowp         (symbol-function 'windowp))
        (before-window-live-p   (symbol-function 'window-live-p))
        (before-frame-selected  (symbol-function 'frame-selected-window))
        (before-window-buffer   (symbol-function 'window-buffer)))
    (require 'emacs-window-builtins)
    (should (eq before-selected-window (symbol-function 'selected-window)))
    (should (eq before-windowp         (symbol-function 'windowp)))
    (should (eq before-window-live-p   (symbol-function 'window-live-p)))
    (should (eq before-frame-selected  (symbol-function 'frame-selected-window)))
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

;;;; Doc 51 Track X audit — keymap-bound window cmds have interactive form

(defun emacs-window-builtins-test--read-defun (file marker)
  "Return the source of the form starting at MARKER (a regexp) in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward marker nil t)
      (let* ((form-start (match-beginning 0))
             (form-end (save-excursion
                         (goto-char form-start)
                         (forward-sexp)
                         (point))))
        (buffer-substring form-start form-end)))))

(ert-deftest emacs-window-builtins-test/keymap-bound-cmd-shape-audit ()
  "Doc 51 Track X (2026-05-04) audit: window commands bound in
`nemacs-main-keymap' (= split-window-below / split-window-right /
delete-window / delete-other-windows) must be wrapper polyfills with
`(interactive ...)' so `call-interactively' produces a well-formed
arg list under keymap dispatch.

The previous `(defalias FOO #'emacs-window-FOO)' shape inherited
no interactive form from the underlying `emacs-window-*' helper, so
prefix-arg paths (`C-u 10 C-x 2' etc) silently dropped their arg."
  (let* ((file (locate-library "emacs-window-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (dolist (spec '(("(when (emacs-window-builtins--install-function-p 'split-window-below)"
                     "split-window-below (&optional size)" "P")
                    ("(when (emacs-window-builtins--install-function-p 'split-window-right)"
                     "split-window-right (&optional size)" "P")
                    ("(when (emacs-window-builtins--install-function-p 'delete-window)"
                     "delete-window (&optional window)" "")
                    ("(when (emacs-window-builtins--install-function-p 'delete-other-windows)"
                     "delete-other-windows (&optional window)" "")))
      (let ((s (emacs-window-builtins-test--read-defun file (nth 0 spec))))
        (should s)
        (should (string-match-p (regexp-quote (nth 1 spec)) s))
        (should (string-match-p
                 (concat "(interactive"
                         (if (equal (nth 2 spec) "")
                             ")"
                           (concat " \"" (nth 2 spec) "\")")))
                 s))))))

(provide 'emacs-window-builtins-test)

;;; emacs-window-builtins-test.el ends here
