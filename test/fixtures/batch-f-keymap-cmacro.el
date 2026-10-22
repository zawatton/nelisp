;;; batch-f-keymap-cmacro.el --- keymap.el compiler macros under eager macroexpansion -*- lexical-binding: t; -*-

;; GNU keymap.el declares a compiler macro on `keymap-set' (it calls
;; `keymap--compile-check') and on `define-keymap' (`define-keymap--compile').
;; Both only check their constant arguments at compile time and leave the call
;; as it is.  `load' expands every top-level form eagerly (lread.c
;; readevalloop_eager_expand_eval), so the handlers run while this file loads.
;; With valid keys they print nothing, on stderr or anywhere else, and the
;; forms behave as plain calls.  The check script also requires an empty stderr.

(defvar bkc-map (make-sparse-keymap))
(keymap-set bkc-map "C-c a" #'ignore)
(keymap-set bkc-map "C-c b" "C-c a")
(define-keymap :keymap bkc-map "C-c c" #'ignore "<f5>" #'ignore)
(defvar bkc-full (define-keymap :full t :suppress t :name "Menu" "C-x C-a" #'ignore))
(defvar-keymap bkc-var-map :doc "A keymap." "C-c d" #'ignore)

(defun bkc-show (label map key)
  (let ((binding (lookup-key map (key-parse key))))
    (princ (format "%-10s %-8s %S\n" label key binding))))

(princ "bindings after load\n")
(bkc-show "keymap-set" bkc-map "C-c a")
(bkc-show "keymap-set" bkc-map "C-c b")
(bkc-show "define-key" bkc-map "C-c c")
(bkc-show "define-key" bkc-map "<f5>")
(bkc-show "full" bkc-full "C-x C-a")
(bkc-show "defvar-km" bkc-var-map "C-c d")

;; The handlers must leave the calls as they are.
(princ "expansion\n")
(dolist (form '((keymap-set m "C-c a" #'ignore)
                (keymap-set m key def)
                (keymap-set m "C-c b" "C-c a")
                (define-keymap "C-c a" #'ignore)
                (define-keymap :full t :parent nil "C-c a" #'ignore "<f5>" #'ignore)
                (define-keymap :keymap m key def)))
  (princ (format "%S\n  unchanged %S\n" form (equal (macroexpand-all form) form))))
