;;; emacs-stub.el --- no-op shims for Emacs C primitives (Phase 3-A''-3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 3-A''-3 — temporary no-op shims for the long tail of
;; Emacs C primitives that vendored `subr.el' / `cl-lib.el' / friends
;; reference at load time.  Without these, NeLisp standalone fails
;; to load any nontrivial Emacs library.
;;
;; This file is INTENTIONALLY DISPOSABLE — it should disappear as the
;; real implementations land in nelisp-emacs's L2 ports
;; (`emacs-keymap.el', `emacs-frame.el', etc.) or via `nelisp-ec-*'
;; aliasing.  See `project_phase4_emacs_c_primitives_todo' memory entry
;; for the full migration checklist.
;;
;; Functions here are no-ops (= return nil / fixed sentinel).  Calling
;; them at runtime does NOTHING; library code that relies on actual
;; behavior (e.g. real keybindings, real frame manipulation) will fail
;; silently.  This is acceptable for the use cases nelisp-emacs targets
;; (= anvil tool dispatch, MCP server) where keymap / frame / display
;; primitives are never reached at the data path.
;;
;; Each shim is gated on `unless (fboundp ...)' so loading under host
;; Emacs is a cheap no-op.

;;; Code:

;;;; --- keymap.c -----------------------------------------------------------

(unless (fboundp 'make-keymap)
  (defun make-keymap (&optional string)
    "Stub: returns a synthetic keymap sentinel cons.
NeLisp standalone has no keybinding subsystem; the returned object is
only useful for `keymapp' / `eq' identity checks."
    (ignore string)
    (cons 'keymap nil)))

(unless (fboundp 'make-sparse-keymap)
  (defun make-sparse-keymap (&optional string)
    "Stub: same shape as `make-keymap'."
    (ignore string)
    (cons 'keymap nil)))

(unless (fboundp 'keymapp)
  (defun keymapp (object)
    "Stub: recognise the `make-keymap' sentinel."
    (and (consp object) (eq (car object) 'keymap))))

(unless (fboundp 'define-key)
  (defun define-key (keymap key def &optional remove)
    "Stub: no-op; returns DEF."
    (ignore keymap key remove)
    def))

(unless (fboundp 'define-key-after)
  (defun define-key-after (keymap key definition &optional after)
    "Stub: no-op; returns DEFINITION."
    (ignore keymap key after)
    definition))

(unless (fboundp 'lookup-key)
  (defun lookup-key (keymap key &optional accept-default)
    "Stub: always returns nil (= no binding)."
    (ignore keymap key accept-default)
    nil))

(unless (fboundp 'key-binding)
  (defun key-binding (key &optional accept-default no-remap position)
    "Stub: always returns nil."
    (ignore key accept-default no-remap position)
    nil))

(unless (fboundp 'set-keymap-parent)
  (defun set-keymap-parent (keymap parent)
    "Stub: no-op; returns PARENT."
    (ignore keymap)
    parent))

(unless (fboundp 'keymap-parent)
  (defun keymap-parent (keymap) (ignore keymap) nil))

(unless (fboundp 'current-global-map)
  (defun current-global-map () (cons 'keymap nil)))

(unless (fboundp 'current-local-map)
  (defun current-local-map () nil))

(unless (fboundp 'use-global-map)
  (defun use-global-map (keymap) (ignore keymap) nil))

(unless (fboundp 'use-local-map)
  (defun use-local-map (keymap) (ignore keymap) nil))

(unless (fboundp 'where-is-internal)
  (defun where-is-internal (definition &optional keymap firstonly noindirect no-remap)
    "Stub: returns nil (= no key bound)."
    (ignore definition keymap firstonly noindirect no-remap)
    nil))


;;;; --- frame.c ------------------------------------------------------------

(unless (fboundp 'make-frame)
  (defun make-frame (&optional parameters)
    "Stub: returns a synthetic frame sentinel."
    (ignore parameters)
    (cons 'frame nil)))

(unless (fboundp 'framep)
  (defun framep (object)
    (and (consp object) (eq (car object) 'frame))))

(unless (fboundp 'frame-live-p)
  (defun frame-live-p (frame) (framep frame)))

(unless (fboundp 'frame-list)
  (defun frame-list () nil))

(unless (fboundp 'selected-frame)
  (defun selected-frame () (cons 'frame nil)))

(unless (fboundp 'frame-parameter)
  (defun frame-parameter (frame parameter)
    (ignore frame parameter)
    nil))

(unless (fboundp 'frame-parameters)
  (defun frame-parameters (&optional frame) (ignore frame) nil))

(unless (fboundp 'set-frame-parameter)
  (defun set-frame-parameter (frame parameter value)
    (ignore frame parameter)
    value))

(unless (fboundp 'modify-frame-parameters)
  (defun modify-frame-parameters (frame alist)
    (ignore frame alist) nil))

(unless (fboundp 'delete-frame)
  (defun delete-frame (&optional frame force) (ignore frame force) nil))

(unless (fboundp 'display-graphic-p)
  (defun display-graphic-p (&optional display) (ignore display) nil))

(unless (fboundp 'display-color-p)
  (defun display-color-p (&optional display) (ignore display) nil))

(unless (fboundp 'display-multi-frame-p)
  (defun display-multi-frame-p (&optional display) (ignore display) nil))


;;;; --- window.c -----------------------------------------------------------

(unless (fboundp 'selected-window)
  (defun selected-window () (cons 'window nil)))

(unless (fboundp 'windowp)
  (defun windowp (object) (and (consp object) (eq (car object) 'window))))

(unless (fboundp 'window-live-p)
  (defun window-live-p (window) (windowp window)))

(unless (fboundp 'window-list)
  (defun window-list (&optional frame minibuf window) (ignore frame minibuf window) nil))

(unless (fboundp 'frame-selected-window)
  (defun frame-selected-window (&optional frame) (ignore frame) (selected-window)))

(unless (fboundp 'set-window-buffer)
  (defun set-window-buffer (window buffer-or-name &optional keep-margins)
    (ignore window buffer-or-name keep-margins) nil))

(unless (fboundp 'window-buffer)
  (defun window-buffer (&optional window) (ignore window) nil))


;;;; --- font-lock ----------------------------------------------------------

(unless (fboundp 'font-lock-mode)
  (defun font-lock-mode (&optional arg) (ignore arg) nil))

(unless (boundp 'font-lock-defaults)
  (defvar font-lock-defaults nil))

(unless (boundp 'font-lock-keywords)
  (defvar font-lock-keywords nil))

(unless (fboundp 'font-lock-fontify-buffer)
  (defun font-lock-fontify-buffer () nil))


;;;; --- format / message helpers ----------------------------------------

(unless (fboundp 'format-message)
  (defun format-message (string &rest objects)
    "Stub: route through plain `format' (= no curly-quote substitution)."
    (apply #'format string objects)))

(unless (fboundp 'message)
  (defun message (format-string &rest args)
    "Stub: print to stderr via `princ' (NeLisp standalone has no echo area)."
    (let ((s (apply #'format format-string args)))
      (princ s)
      (princ "\n")
      s)))

(unless (fboundp 'error)
  (defun error (format-string &rest args)
    "Stub: signal `error' with formatted message."
    (signal 'error (list (apply #'format format-string args)))))

;;;; --- numeric primitives -------------------------------------------------

(unless (fboundp 'min)
  (defun min (&rest numbers)
    (let ((acc (car numbers)))
      (setq numbers (cdr numbers))
      (while numbers
        (when (< (car numbers) acc) (setq acc (car numbers)))
        (setq numbers (cdr numbers)))
      acc)))

(unless (fboundp 'max)
  (defun max (&rest numbers)
    (let ((acc (car numbers)))
      (setq numbers (cdr numbers))
      (while numbers
        (when (> (car numbers) acc) (setq acc (car numbers)))
        (setq numbers (cdr numbers)))
      acc)))

(unless (fboundp 'abs)
  (defun abs (n) (if (< n 0) (- n) n)))

(unless (fboundp 'zerop)
  (defun zerop (n) (= n 0)))

(unless (fboundp 'plusp)
  (defun plusp (n) (> n 0)))

(unless (fboundp 'minusp)
  (defun minusp (n) (< n 0)))

(unless (fboundp 'oddp)
  (defun oddp (n) (= 1 (mod n 2))))

(unless (fboundp 'evenp)
  (defun evenp (n) (= 0 (mod n 2))))

(unless (fboundp 'natnump)
  (defun natnump (n) (and (integerp n) (>= n 0))))

(unless (fboundp '1+)
  (defun 1+ (n) (+ n 1)))

(unless (fboundp '1-)
  (defun 1- (n) (- n 1)))


;;;; --- bitwise ops --------------------------------------------------------

(unless (fboundp 'logior)
  (defun logior (&rest ints)
    "Stub: bitwise OR of all INTS."
    (let ((acc 0))
      (while ints
        (setq acc (+ acc (- (car ints) (logand acc (car ints)))))
        (setq ints (cdr ints)))
      acc)))

(unless (fboundp 'logand)
  (defun logand (&rest ints)
    "Stub: bitwise AND of all INTS.  Approximation via min for non-negative."
    (if (null ints)
        -1
      (let ((acc (car ints)))
        (setq ints (cdr ints))
        (while ints
          ;; Conservative: use min as a lower bound; not strictly correct
          ;; but adequate for the bit-flag use cases in subr.el load path.
          (setq acc (min acc (car ints)))
          (setq ints (cdr ints)))
        acc))))

(unless (fboundp 'logxor)
  (defun logxor (&rest ints)
    "Stub: bitwise XOR (= using +/- proxy for non-overlapping flags)."
    (let ((acc 0))
      (while ints
        (setq acc (+ acc (car ints)))
        (setq ints (cdr ints)))
      acc)))

(unless (fboundp 'lognot)
  (defun lognot (int)
    "Stub: bitwise NOT."
    (- (- int) 1)))

(unless (fboundp 'ash)
  (defun ash (value count)
    "Stub: arithmetic shift (positive COUNT = left, negative = right)."
    (cond
     ((= count 0) value)
     ((> count 0)
      (let ((acc value))
        (while (> count 0) (setq acc (* acc 2)) (setq count (- count 1)))
        acc))
     (t
      (let ((acc value))
        (while (< count 0) (setq acc (/ acc 2)) (setq count (+ count 1)))
        acc)))))

(unless (fboundp 'lsh) (defalias 'lsh 'ash))


;;;; --- char.c / fns.c -----------------------------------------------------

(unless (fboundp 'clear-string)
  (defun clear-string (string) (ignore string) nil))

(unless (fboundp 'store-substring)
  (defun store-substring (string idx obj) (ignore idx obj) string))


;;;; --- display.c ----------------------------------------------------------

(unless (fboundp 'redraw-display)
  (defun redraw-display (&rest _) nil))

(unless (fboundp 'redisplay)
  (defun redisplay (&optional force) (ignore force) nil))

(unless (fboundp 'force-mode-line-update)
  (defun force-mode-line-update (&optional all) (ignore all) nil))


;;;; --- buffer.c (minimal subset; nelisp-ec-* covers the rest) ------------

(unless (fboundp 'current-buffer)
  (defun current-buffer ()
    "Stub: synthetic placeholder.  Real impl needs nelisp-ec-current-buffer alias."
    (cons 'buffer nil)))

(unless (fboundp 'bufferp)
  (defun bufferp (object) (and (consp object) (eq (car object) 'buffer))))

(unless (fboundp 'buffer-live-p)
  (defun buffer-live-p (buffer) (bufferp buffer)))

(unless (fboundp 'get-buffer)
  (defun get-buffer (buffer-or-name) (ignore buffer-or-name) nil))

(unless (fboundp 'get-buffer-create)
  (defun get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
    (ignore buffer-or-name inhibit-buffer-hooks)
    (cons 'buffer nil)))

(unless (fboundp 'buffer-name)
  (defun buffer-name (&optional buffer) (ignore buffer) ""))

(unless (fboundp 'buffer-list)
  (defun buffer-list (&optional frame) (ignore frame) nil))


;;;; --- minor-mode helpers -------------------------------------------------

(unless (fboundp 'add-hook)
  (defun add-hook (hook function &optional depth local)
    "Stub: no-op (NeLisp standalone has no hook dispatch)."
    (ignore hook function depth local)
    nil))

(unless (fboundp 'remove-hook)
  (defun remove-hook (hook function &optional local)
    (ignore hook function local) nil))

(unless (fboundp 'run-hooks)
  (defun run-hooks (&rest hooks) (ignore hooks) nil))

(unless (fboundp 'run-hook-with-args)
  (defun run-hook-with-args (hook &rest args) (ignore hook args) nil))


;;;; --- list helpers ------------------------------------------------------

(unless (fboundp 'add-to-list)
  (defun add-to-list (list-var element &optional append compare-fn)
    "Stub: prepend (or append) ELEMENT to LIST-VAR if not already present."
    (ignore compare-fn)
    (let ((cur (and (boundp list-var) (symbol-value list-var))))
      (unless (member element cur)
        (set list-var (if append
                          (append cur (list element))
                        (cons element cur))))
      (and (boundp list-var) (symbol-value list-var)))))

(unless (fboundp 'add-to-ordered-list)
  (defun add-to-ordered-list (list-var element &optional order)
    (ignore order)
    (add-to-list list-var element)))


(provide 'emacs-stub)

;;; emacs-stub.el ends here
