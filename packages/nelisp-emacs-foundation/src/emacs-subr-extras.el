;;; emacs-subr-extras.el --- subr.el primitive shims for cl-lib bootstrap  -*- lexical-binding: t; -*-

;; Phase B2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;;
;; Standalone NeLisp ships nelisp-stdlib's string-prefix-p /
;; string-suffix-p / split-string / plist-get etc., but four
;; primitives that vendor `cl-lib.el' / `subr-x.el' reach for at
;; load time are missing:
;;
;;   `number-sequence' / `assoc-default' / `string-join' /
;;   `member-ignore-case'
;;
;; Plus the four-level `caaaar' .. `cddddr' family — fifteen entries
;; (`cadddr' is provided by NeLisp itself; the other 15 are absent).
;; `cl-lib.el' line 424 onwards does `(defalias 'cl-caaaar 'caaaar)'
;; etc. and trips `void-function caaaar' on standalone NeLisp without
;; them.
;;
;; Those nineteen `defun's are gathered here so `(require 'cl-lib)' /
;; `(require 'subr-x)' load cleanly.  Once that is in place, the rest
;; of the `anvil-server.el' load chain (= json + anvil-server-metrics
;; + anvil-server itself) goes through.  See memory entry
;; `project_anvil_runtime_phase_b1_breakthroughs' for the full audit.
;;
;; Each shim is gated on `unless (fboundp ...)' so loading under
;; host Emacs is a cheap no-op.

;;; Code:

;; ---- subr.el primitives missing from nelisp-stdlib ----

(unless (fboundp 'number-sequence)
  (defun number-sequence (from &optional to inc)
    "Return a sequence of numbers from FROM to TO (inclusive) by INC.
INC defaults to 1.  TO can be nil (= return single-element list).
Negative INC is supported when FROM > TO."
    (let ((step (or inc 1))
          (acc nil)
          (cur from))
      (cond
       ((null to) (list from))
       ((> step 0)
        (while (<= cur to)
          (setq acc (cons cur acc))
          (setq cur (+ cur step)))
        (nreverse acc))
       (t
        (while (>= cur to)
          (setq acc (cons cur acc))
          (setq cur (+ cur step)))
        (nreverse acc))))))

(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in pseudo-alist ALIST.
Each ALIST entry is either a cons (KEY . VALUE) or a bare KEY.
TEST is called with the element (or its car) and KEY; defaults
to `equal'.  When a match is found, return the cdr if the
element is a cons, otherwise DEFAULT.  When no element matches,
return nil."
    (let (found
          (tail alist)
          value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (and elt
                     (funcall (or test #'equal)
                              (if (consp elt) (car elt) elt)
                              key))
            (setq found t)
            (setq value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

(unless (fboundp 'string-join)
  (defun string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR (default empty string)."
    (mapconcat #'identity strings (or separator ""))))

(unless (fboundp 'member-ignore-case)
  (defun member-ignore-case (elt list)
    "Like `member', but case-insensitive on string elements."
    (while (and list
                (not (eq t (compare-strings elt 0 nil (car list) 0 nil t))))
      (setq list (cdr list)))
    list))

;; ---- four-level caaaar..cddddr (cadddr is NeLisp builtin) ----

(unless (fboundp 'caaaar) (defun caaaar (x) (car (car (car (car x))))))
(unless (fboundp 'caaadr) (defun caaadr (x) (car (car (car (cdr x))))))
(unless (fboundp 'caadar) (defun caadar (x) (car (car (cdr (car x))))))
(unless (fboundp 'caaddr) (defun caaddr (x) (car (car (cdr (cdr x))))))
(unless (fboundp 'cadaar) (defun cadaar (x) (car (cdr (car (car x))))))
(unless (fboundp 'cadadr) (defun cadadr (x) (car (cdr (car (cdr x))))))
(unless (fboundp 'caddar) (defun caddar (x) (car (cdr (cdr (car x))))))
(unless (fboundp 'cdaaar) (defun cdaaar (x) (cdr (car (car (car x))))))
(unless (fboundp 'cdaadr) (defun cdaadr (x) (cdr (car (car (cdr x))))))
(unless (fboundp 'cdadar) (defun cdadar (x) (cdr (car (cdr (car x))))))
(unless (fboundp 'cdaddr) (defun cdaddr (x) (cdr (car (cdr (cdr x))))))
(unless (fboundp 'cddaar) (defun cddaar (x) (cdr (cdr (car (car x))))))
(unless (fboundp 'cddadr) (defun cddadr (x) (cdr (cdr (car (cdr x))))))
(unless (fboundp 'cdddar) (defun cdddar (x) (cdr (cdr (cdr (car x))))))
(unless (fboundp 'cddddr) (defun cddddr (x) (cdr (cdr (cdr (cdr x))))))

(provide 'emacs-subr-extras)
;;; emacs-subr-extras.el ends here
