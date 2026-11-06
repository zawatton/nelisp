;;; nl-typecheck-prims.el --- primitive signature table lookup and cross-check -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 236 Stage 1.  The table in `nl-typecheck-prims-data' maps a
;; primitive symbol to (function (ARGS...) RESULT), generated from GNU's
;; `comp-primitive-type-specifiers' (the Emacs 31 name of the older
;; `comp-known-type-specifiers') by tools/gen-prims.el.  The checker never
;; needs comp-common at run time.  `nl-typecheck-prims-crosscheck'
;; compares the table with the runtime and reports disagreements; it never
;; repairs them.

;;; Code:

(require 'nl-typecheck-types)
(require 'nl-typecheck-prims-data)

(defvar nl-typecheck-prims--index nil
  "Hash table built lazily from `nl-typecheck-prims-table'.")

(defun nl-typecheck-prims-signature (symbol)
  "Return the (function (ARGS...) RESULT) signature of SYMBOL, or nil."
  (unless nl-typecheck-prims--index
    (let ((h (make-hash-table :test 'eq)))
      (dolist (e nl-typecheck-prims-table)
        (puthash (car e) (car (cdr e)) h))
      (setq nl-typecheck-prims--index h)))
  (gethash symbol nl-typecheck-prims--index))

(defun nl-typecheck-prims-spec-arity (spec)
  "Return (MIN . MAX) for signature SPEC; MAX is `many' with &rest."
  (let ((min 0) (max 0) (mode 'req))
    (dolist (a (car (cdr spec)))
      (cond ((eq a '&optional) (setq mode 'opt))
            ((eq a '&rest) (setq mode 'rest))
            ((eq mode 'req) (setq min (1+ min) max (1+ max)))
            ((eq mode 'opt) (setq max (1+ max)))))
    (cons min (if (eq mode 'rest) 'many max))))

(defun nl-typecheck-prims--arity-equal-p (spec-arity fn-arity)
  "Non-nil when SPEC-ARITY and FN-ARITY are the same (MIN . MAX)."
  (equal spec-arity fn-arity))

(defun nl-typecheck-prims--spec-types (spec)
  "Return every argument and result type mentioned by SPEC."
  (let ((types nil))
    (dolist (a (car (cdr spec)))
      (unless (memq a '(&optional &rest)) (setq types (cons a types))))
    (nreverse (cons (car (cdr (cdr spec))) types))))

(defun nl-typecheck-prims--runtime-arity (symbol)
  "Return `func-arity' of SYMBOL, or nil when it has no usable arity."
  (condition-case nil
      (let ((a (func-arity symbol)))
        (and (consp a) (integerp (car a)) a))
    (error nil)))

(defun nl-typecheck-prims-crosscheck (&optional table builtins nelisp-arities)
  "Return a sorted list of disagreement lines for TABLE.
TABLE defaults to `nl-typecheck-prims-table'; BUILTINS to
`nl-typecheck-prims-nelisp-builtins'; NELISP-ARITIES to
`nl-typecheck-prims-nelisp-fixed-arities'.  Each line is
\"CATEGORY SYMBOL DETAIL\" with CATEGORY one of missing-spec,
arity-mismatch, nelisp-arity-mismatch, unknown-type, not-fboundp."
  (let ((table (or table nl-typecheck-prims-table))
        (builtins (or builtins nl-typecheck-prims-nelisp-builtins))
        (fixed (or nelisp-arities nl-typecheck-prims-nelisp-fixed-arities))
        (lines nil))
    (dolist (name builtins)
      (unless (assq (intern name) table)
        (setq lines (cons (format "missing-spec %s no GNU spec for NeLisp builtin"
                                  name)
                          lines))))
    (dolist (e table)
      (let* ((sym (car e)) (spec (car (cdr e)))
             (spec-arity (nl-typecheck-prims-spec-arity spec))
             (fn-arity (nl-typecheck-prims--runtime-arity sym)))
        (cond
         ((not (fboundp sym))
          (setq lines (cons (format "not-fboundp %s" sym) lines)))
         ((and fn-arity
               (not (nl-typecheck-prims--arity-equal-p spec-arity fn-arity)))
          (setq lines (cons (format "arity-mismatch %s spec=%S func-arity=%S"
                                    sym spec-arity fn-arity)
                            lines))))
        (let ((fx (assoc (symbol-name sym) fixed)))
          (when (and fx (or (< (cdr fx) (car spec-arity))
                            (and (integerp (cdr spec-arity))
                                 (> (cdr fx) (cdr spec-arity)))))
            (setq lines (cons (format "nelisp-arity-mismatch %s spec=%S nelisp-fixed=%d"
                                      sym spec-arity (cdr fx))
                              lines))))
        (dolist (ty (nl-typecheck-prims--spec-types spec))
          (unless (nl-typecheck-type-known-p ty)
            (setq lines (cons (format "unknown-type %s %S" sym ty) lines))))))
    (sort lines #'string<)))

(provide 'nl-typecheck-prims)
;;; nl-typecheck-prims.el ends here
