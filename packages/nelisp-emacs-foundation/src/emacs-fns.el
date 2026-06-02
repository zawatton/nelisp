;;; emacs-fns.el --- NeLisp port of Emacs C core fns.c primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2 (= Emacs C core in Elisp on NeLisp).
;;
;; Ports the standard sequence + property-list primitives that
;; `fns.c' provides in Emacs's C core.  These are foundation
;; functions every Elisp library assumes; they cannot be collapsed into
;; the NeLisp core without violating the "minimal substrate" rule
;; (user 2026-05-02 directive), and they cannot live in any single
;; application (= anvil.el, etc.) without forcing every other
;; nelisp-emacs consumer to duplicate them.
;;
;; Each definition is gated on `unless (fboundp ...)` so loading
;; this file under regular Emacs (= where the real C primitives
;; already exist) is a cheap no-op.  Implementations use only
;; bootstrap-eval primitives (no dependency on the very functions
;; being defined here, no `cl-lib', no `subr-x' tricks).
;;
;; Symbols ported: mapcar, mapconcat, mapc, nreverse, reverse,
;; plist-get, plist-put, plist-member, provide.
;;
;; Out of scope here: cl-* generic versions (= live in
;; `nelisp-emacs/src/emacs-cl-seq.el', not yet shipped).  Hash
;; table, string, and number primitives ship in their own
;; emacs-X.el files.

;;; Code:

;;;; --- trivial primitives -----------------------------------------------

;; Emacs's C primitive accepts an optional SUBFEATURES argument:
;; `(provide 'files '(remote-wildcards))' appears in vendored files.el.
;; NeLisp's standalone prelude may expose `provide' / `featurep' before it
;; has created the user-visible `features' variable.  Define the registry
;; at top level first: some standalone eval paths do not reliably handle
;; `defvar' inside a function body before the following `setq'.
(unless (boundp 'features)
  (defvar features nil))

;; NeLisp v2's bootstrap stdlib used to expose a one-argument `provide',
;; so vendor `require' could load a file and still report "feature not
;; provided" after arity failure.  Host Emacs keeps its native primitive;
;; the polyfill is only installed on the standalone NeLisp path, before
;; `emacs-version' exists.
(unless (boundp 'emacs-version)
  (defun provide (feature &optional _subfeatures)
    "Mark FEATURE as available and return FEATURE.
Optional SUBFEATURES are accepted for Emacs compatibility and ignored."
    (unless (memq feature features)
      (setq features (cons feature features)))
    feature)

  (defun featurep (feature &optional _subfeature)
    "Return non-nil if FEATURE has been provided.
Optional SUBFEATURE is accepted for Emacs compatibility and ignored."
    (if (memq feature features) t nil)))

(unless (fboundp 'ignore)
  (defun ignore (&rest _ignore-args)
    "Polyfill: do nothing, return nil regardless of arguments."
    nil))

(unless (fboundp 'identity)
  (defun identity (arg)
    "Polyfill: return ARG unchanged."
    arg))

(unless (fboundp 'null)
  (defun null (object)
    "Polyfill: return t iff OBJECT is nil."
    (eq object nil)))

;; Some NeLisp eval paths look up function symbols as values too;
;; defvar them as nil so `symbol-value' / bare-symbol-eval succeed.
(defvar null nil
  "Polyfill alias of nil — works around NeLisp eval paths that fall
back to `symbol-value' lookup for symbols whose function cell is
bound but value cell is unbound.")

(unless (fboundp 'numberp)
  (defun numberp (obj) (or (integerp obj) (floatp obj))))


;;;; --- list iteration -----------------------------------------------------

(unless (fboundp 'mapcar)
  (defun mapcar (function sequence)
    "Apply FUNCTION to each element of SEQUENCE, return list of results.
SEQUENCE here is restricted to a proper list (= terminated by nil).
A vector-aware port belongs in `emacs-fns-seq.el' (Phase 2)."
    (let ((result nil)
          (cur sequence))
      (while cur
        (setq result (cons (funcall function (car cur)) result))
        (setq cur (cdr cur)))
      ;; Manual reverse — `nreverse' may not yet be defined when the
      ;; loader installs this file before its reverse primitive.
      (let ((reversed nil))
        (while result
          (setq reversed (cons (car result) reversed))
          (setq result (cdr result)))
        reversed))))

(unless (fboundp 'mapc)
  (defun mapc (function sequence)
    "Apply FUNCTION to each element of SEQUENCE for side effects.
Returns SEQUENCE unchanged."
    (let ((cur sequence))
      (while cur
        (funcall function (car cur))
        (setq cur (cdr cur))))
    sequence))

(unless (fboundp 'mapconcat)
  (defun mapconcat (function sequence separator)
    "Apply FUNCTION to each element of SEQUENCE, concatenate with SEPARATOR.
Each FUNCTION result must be a string; SEPARATOR is a string.  Returns
the empty string when SEQUENCE is nil (matches Emacs C behaviour)."
    (if (null sequence)
        ""
      (let ((parts nil)
            (cur sequence))
        (while cur
          (setq parts (cons (funcall function (car cur)) parts))
          (setq cur (cdr cur)))
        ;; parts is reverse-order; build forward list, then concat.
        (let ((forward nil))
          (while parts
            (setq forward (cons (car parts) forward))
            (setq parts (cdr parts)))
          ;; Interleave SEPARATOR.
          (let ((out (car forward))
                (rest (cdr forward)))
            (while rest
              (setq out (concat out separator (car rest)))
              (setq rest (cdr rest)))
            out))))))


;;;; --- list reversal ------------------------------------------------------

(unless (fboundp 'reverse)
  (defun reverse (sequence)
    "Return a new list with the elements of SEQUENCE in reverse order.
Does NOT mutate SEQUENCE.  Proper-list only (vector port: Phase 2)."
    (let ((result nil)
          (cur sequence))
      (while cur
        (setq result (cons (car cur) result))
        (setq cur (cdr cur)))
      result)))

(unless (fboundp 'nreverse)
  (defun nreverse (sequence)
    "Return SEQUENCE reversed.  In Emacs this destructively
re-uses the cons cells; the polyfill here behaves identically as
far as the return value is concerned but allocates a fresh list,
because mutating cons cells from Lisp without `setcdr' availability
would be unsafe.  Callers that depend on the original SEQUENCE
becoming garbage should not be affected because the original list
is no longer reachable through the variable they used to bind it."
    (reverse sequence)))


;;;; --- property list access -----------------------------------------------

(unless (fboundp 'plist-get)
  (defun plist-get (plist property)
    "Return the value of PROPERTY in PLIST.
PLIST is a flat alternating-key/value list `(KEY1 VAL1 KEY2 VAL2 ...)'.
Comparison uses `eq' (Emacs default).  Returns nil when PROPERTY is
absent — caller must distinguish nil-as-value from missing-property
using `plist-member'."
    (let ((cur plist)
          (found nil)
          (result nil))
      (while (and cur (not found))
        (if (eq (car cur) property)
            (progn (setq result (car (cdr cur)))
                   (setq found t))
          (setq cur (cdr (cdr cur)))))
      result)))

(unless (fboundp 'plist-member)
  (defun plist-member (plist property)
    "Return the cdr cell whose car is PROPERTY in PLIST, or nil.
The returned cell is the (PROPERTY VAL ...) sub-list, not just the
value; callers can distinguish missing from nil-valued via this."
    (let ((cur plist)
          (found nil))
      (while (and cur (not found))
        (if (eq (car cur) property)
            (setq found cur)
          (setq cur (cdr (cdr cur)))))
      found)))

(unless (fboundp 'plist-put)
  (defun plist-put (plist property value)
    "Change the value of PROPERTY in PLIST to VALUE; return the modified PLIST.
If PROPERTY is absent, append (PROPERTY VALUE) to PLIST.  This polyfill
returns a fresh list rather than mutating in place — callers that depend
on identity should re-bind the variable holding PLIST."
    (let ((acc nil)
          (cur plist)
          (replaced nil))
      ;; Walk PLIST in pairs, copying.  Replace VALUE when key matches.
      (while cur
        (let ((k (car cur))
              (v (car (cdr cur))))
          (if (eq k property)
              (progn (setq acc (cons v (cons k acc)))
                     (setq replaced t))
            (setq acc (cons v (cons k acc)))))
        (setq cur (cdr (cdr cur))))
      ;; Reverse acc back to forward order.
      (let ((forward nil))
        (while acc
          (setq forward (cons (car acc) forward))
          (setq acc (cdr acc)))
        (if replaced
            forward
          ;; Append fresh (PROPERTY VALUE).
          (let ((tail (cons property (cons value nil))))
            (if (null forward)
                tail
              ;; Build (forward... PROPERTY VALUE).  No `append' dependency.
              (let ((out nil)
                    (rev nil))
                ;; First copy forward into out via reversal.
                (let ((c forward))
                  (while c
                    (setq rev (cons (car c) rev))
                    (setq c (cdr c))))
                ;; Now reverse rev into out, prepending tail.
                (setq out tail)
                (while rev
                  (setq out (cons (car rev) out))
                  (setq rev (cdr rev)))
                out))))))))


;;;; --- coding-system polyfill (Doc 51 Track B Phase 2) ----------------
;;
;; Under host Emacs `encode-coding-string' / `decode-coding-string' /
;; `multibyte-string-p' are C builtins.  Under the nelisp driver strings
;; are internally valid UTF-8, so
;; for `'utf-8' / `'utf-8-emacs' / `nil' (= no conversion) the encode/
;; decode operations are identity.  We provide minimal polyfills here
;; because `nelisp-text-buffer.el' calls them at runtime and we are
;; loaded before that file's functions are first invoked.

(unless (fboundp 'encode-coding-string)
  (defun encode-coding-string (string _coding-system &optional _nocopy &rest _)
    (if (stringp string) string "")))

(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (string _coding-system &optional _nocopy &rest _)
    (if (stringp string) string "")))

(unless (fboundp 'multibyte-string-p)
  (defun multibyte-string-p (string)
    (when (stringp string)
      (let ((i 0) (n (length string)) found)
        (while (and (not found) (< i n))
          (when (>= (aref string i) 128) (setq found t))
          (setq i (1+ i)))
        found))))

(unless (fboundp 'string-as-multibyte)
  (defun string-as-multibyte (string)
    (if (stringp string) string "")))

(unless (fboundp 'string-as-unibyte)
  (defun string-as-unibyte (string)
    (if (stringp string) string "")))

(unless (fboundp 'string-make-multibyte)
  (defalias 'string-make-multibyte 'string-as-multibyte))

(unless (fboundp 'string-make-unibyte)
  (defalias 'string-make-unibyte 'string-as-unibyte))

(provide 'emacs-fns)

;;; emacs-fns.el ends here
