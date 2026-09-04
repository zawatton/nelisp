;;; emacs-parity-flycheck.el --- flycheck-def-option-var hard-abort parity fix -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fix (b1k21 frontier).  In the standalone NeLisp
;; self-host runtime, loading Flycheck aborted every one of its 125
;; `flycheck-def-option-var' (110) and `flycheck-def-args-var' (15) forms with
;; `nelisp-bare-abort'.  Runtime marker-localization probing (bootstrap replay +
;; `nelisp--eval-source-string' on isolated forms, against the b1k21 binary)
;; pinned a SINGLE root, distinct from the two CC Mode roots:
;;
;; ROOT -- the standalone `cl-pushnew' aborts on a GENERALIZED (non-symbol)
;;   place.  `flycheck-def-args-var' expands into `flycheck-def-option-var',
;;   and `flycheck-def-option-var' (flycheck.el:6691) expands to
;;
;;     (progn (defcustom SYMBOL ...) (flycheck-register-option-var 'SYMBOL 'CHECKERS))
;;
;;   The `defcustom' half evaluates cleanly; the abort is in
;;   `flycheck-register-option-var' (flycheck.el:6679), whose body is
;;
;;     (cl-pushnew var (flycheck-checker-get checker 'option-vars))
;;
;;   The standalone prelude installs a symbol-only `cl-pushnew' that expands
;;   `(cl-pushnew ITEM PLACE)' to `(setq PLACE ...)'.  For the non-symbol place
;;   `(flycheck-checker-get ...)' that is literally `(setq (flycheck-checker-get
;;   ...) ...)' -- a `setq' whose target is not a symbol -- which is a
;;   catch-less `nelisp-bare-abort' on the fast source evaluator.  (Isolation
;;   proof: `(cl-pushnew 'y (get 'sym 'prop))' hard-aborts, while `(setf (get
;;   'sym 'prop) v)' works.)  Every `flycheck-def-option-var'/`-args-var' call
;;   in flycheck.el reaches this one broken `cl-pushnew', hence all 125.
;;
;;   `emacs-cl-macros.el' (emacs-cl-macros.el:1326) already carries the correct
;;   definition -- it routes a non-symbol place through `setf' -- but gates it
;;   behind `emacs-cl-macros--define-p', which declines to install because the
;;   broken prelude `cl-pushnew' is already `fboundp'.  So the good version is
;;   never reached and the broken one stays live when Flycheck loads.
;;
;; FIX (two clobber-immune pieces; Flycheck redefines neither of the touched
;; symbols, so ordering versus Flycheck's own load is irrelevant):
;;
;;   1. Force-install the generalized-place-aware `cl-pushnew' (identical in
;;      shape to the gated `emacs-cl-macros.el' version): symbol places still
;;      expand to `setq'; non-symbol places route through `setf'.  This is
;;      strictly non-regressive -- non-symbol places previously ABORTED.
;;
;;   2. Register a `cl-simple-setter' for `flycheck-checker-get' so the `setf'
;;      that piece 1 now emits can resolve the place.  `flycheck-checker-get'
;;      is `(get CHECKER (flycheck--checker-property-name PROPERTY))', so the
;;      setter is the corresponding `put'.  `cl-simple-setter' is the only
;;      generalized-place setter table the standalone `setf' polyfill
;;      (`nelisp--setf-1') consults; Flycheck's own runtime `gv-define-setter'
;;      is ignored by that polyfill, which is why `setf'-on-`flycheck-checker-get'
;;      raised "setf: unsupported place" before this shim.
;;
;;   As a bonus, piece 2 also repairs the ~40 separate
;;   `("setf: unsupported place" flycheck-checker-get)' errors flycheck.el
;;   raises from `flycheck-define-command-checker' / `flycheck-define-checker'
;;   (which do `(setf (flycheck-checker-get symbol prop) value)').
;;
;; Runtime-verified on the b1k21 binary: with this shim loaded, Flycheck's
;; UNCHANGED `flycheck-register-option-var', `flycheck-def-option-var' and
;; `flycheck-def-args-var' produce bound, registered option variables with zero
;; `nelisp-bare-abort'.
;;
;; This shim is a no-op outside the standalone NeLisp runtime: the setter helper
;; is inert and nothing is installed, so host Emacs keeps its real `cl-pushnew',
;; `setf', and Flycheck behaviour.

;;; Code:

(defconst emacs-parity-flycheck--standalone-p
  (fboundp 'nelisp--eval-source-string)
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards activation so host Emacs is left untouched (it keeps the real
`cl-pushnew', `setf', and Flycheck behaviour).")

;; --- Piece 2 helper: interpreter-safe checker-property setter ------------
;; Defined unconditionally (inert unless the `cl-simple-setter' property below
;; points at it) so the symbol is always resolvable.
(defun emacs-parity-flycheck--checker-set (checker property value)
  "Set CHECKER's PROPERTY to VALUE; the `setf' setter for `flycheck-checker-get'.
Mirrors `flycheck-checker-get' = `(get CHECKER
\(flycheck--checker-property-name PROPERTY))' with the corresponding `put'."
  (put checker
       (if (fboundp 'flycheck--checker-property-name)
           (flycheck--checker-property-name property)
         (intern (concat "flycheck-" (symbol-name property))))
       value)
  value)

;; --- Piece 1: generalized-place-aware `cl-pushnew' -----------------------
(defun emacs-parity-flycheck--install-cl-pushnew ()
  "Force-install a `cl-pushnew' that handles generalized (non-symbol) places.
Symbol places expand to `setq' (unchanged); non-symbol places route through
`setf'.  Matches the (gated-out) definition in `emacs-cl-macros.el'."
  (defmacro cl-pushnew (item place &rest _keys)
    "Cons ITEM onto PLACE unless it is already `member' of PLACE.
PLACE may be a symbol (expands to `setq') or a generalized place handled by
the substrate `setf' polyfill."
    (list 'unless (list 'member item place)
          (if (symbolp place)
              (list 'setq place (list 'cons item place))
            (list 'setf place (list 'cons item place))))))

;; --- Activation: standalone runtime only ---------------------------------
(when emacs-parity-flycheck--standalone-p
  (put 'flycheck-checker-get 'cl-simple-setter
       'emacs-parity-flycheck--checker-set)
  (emacs-parity-flycheck--install-cl-pushnew))

(provide 'emacs-parity-flycheck)

;;; emacs-parity-flycheck.el ends here
