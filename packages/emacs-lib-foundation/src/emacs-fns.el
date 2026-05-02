;;; emacs-fns.el --- NeLisp port of Emacs C core fns.c primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2 (= Emacs C core in Elisp on NeLisp).
;;
;; Ports the standard sequence + property-list primitives that
;; `fns.c' provides in Emacs's C core.  These are foundation
;; functions every Elisp library assumes; they cannot live in the
;; NeLisp Rust core without violating the "minimal substrate" rule
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
;; plist-get, plist-put, plist-member.
;;
;; Out of scope here: cl-* generic versions (= live in
;; `nelisp-emacs/src/emacs-cl-seq.el', not yet shipped).  Hash
;; table, string, and number primitives ship in their own
;; emacs-X.el files.

;;; Code:

;;;; --- trivial primitives -----------------------------------------------

(unless (fboundp 'ignore)
  (defun ignore (&rest _ignore-args)
    "Polyfill: do nothing, return nil regardless of arguments."
    nil))

(unless (fboundp 'identity)
  (defun identity (arg)
    "Polyfill: return ARG unchanged."
    arg))


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


(provide 'emacs-fns)

;;; emacs-fns.el ends here
