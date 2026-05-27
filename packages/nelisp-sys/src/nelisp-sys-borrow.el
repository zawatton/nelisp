;;; nelisp-sys-borrow.el --- Lexical borrow + slice-bounds checker for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.4 / 130.S3-S4: lexical borrow checker and static slice-bounds
;; checker for nelisp-sys modules.
;;
;; Borrow rules (MVP, explicit with-borrow forms only):
;;   E-SYS-BORROW-001  A new borrow (shared or exclusive) conflicts with an
;;                      already-active exclusive (&mut) borrow of the same place.
;;   E-SYS-BORROW-002  A new exclusive (&mut) borrow conflicts with one or more
;;                      already-active shared (&) borrows of the same place.
;;   E-SYS-BORROW-003  A defun declares a borrow type ((& T) or (&mut T)) as its
;;                      return type; escaping a borrow is forbidden in MVP.
;;
;; Bounds rule (Stage 130.S4, static-only):
;;   E-SYS-BOUNDS-001  A slice-ref or slice-set! node carries a negative integer
;;                      literal as its index; this is always out of bounds.
;;
;; Active borrows are tracked as an immutable alist of (PLACE-KEY . KIND) pairs
;; where KIND is `mut' or `shared'.  The alist is extended (non-destructively)
;; when entering a with-borrow body and restored on exit.  Only plain `var'
;; places are given a symbolic key; all other places use a fresh opaque cons.
;;
;; Uses only `cl-lib' and core special forms.  No `pcase' (repo shadows it).

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-check)

;;; Error type.

(define-error 'nelisp-sys-borrow-error
  "nelisp-sys borrow/bounds error" 'nelisp-sys-error)

(defun nelisp-sys-borrow--fail (code form fmt &rest args)
  "Signal `nelisp-sys-borrow-error' with CODE, FORM, and message FMT/ARGS."
  (signal 'nelisp-sys-borrow-error
          (list code (apply #'format fmt args) :form form)))

(defun nelisp-sys-borrow-error-code (err)
  "Return the stable diagnostic code from a `nelisp-sys-borrow-error' ERR."
  (nth 1 err))

(defun nelisp-sys-borrow-error-message (err)
  "Return the message string from a `nelisp-sys-borrow-error' ERR."
  (nth 2 err))

;;; Place identity.

(defun nelisp-sys-borrow--place-key (place-node)
  "Derive a stable identity key for PLACE-NODE.
For a `var' node the key is the :name symbol, which gives readable
diagnostics.  For any other node a fresh opaque cons is returned; this
means two different non-var places never alias (conservative but sound
for the MVP gate where places are always plain variables)."
  (if (eq (nelisp-sys-ast-kind place-node) 'var)
      (nelisp-sys-ast-prop place-node :name)
    (cons 'opaque place-node)))

;;; Active-borrow queries.

(defun nelisp-sys-borrow--find-mut (active place-key)
  "Return the first entry in ACTIVE whose key is PLACE-KEY and kind is `mut'.
Returns nil when no such entry exists."
  (cl-find-if (lambda (entry)
                (and (equal (car entry) place-key)
                     (eq (cdr entry) 'mut)))
              active))

(defun nelisp-sys-borrow--find-shared (active place-key)
  "Return the first entry in ACTIVE whose key is PLACE-KEY and kind is `shared'.
Returns nil when no such entry exists."
  (cl-find-if (lambda (entry)
                (and (equal (car entry) place-key)
                     (eq (cdr entry) 'shared)))
              active))

;;; Walker.

(defun nelisp-sys-borrow--walk (node active)
  "Walk NODE recursively, checking borrow and bounds rules under ACTIVE.
ACTIVE is an alist of (PLACE-KEY . KIND) pairs representing the set of
currently live borrows.  Signals `nelisp-sys-borrow-error' on the first
violation.  Returns nothing meaningful."
  (let ((kind (nelisp-sys-ast-kind node)))
    (cond

     ;; --- with-borrow: apply borrow rules, then recurse body with updated active.
     ((eq kind 'with-borrow)
      (let* ((mut-p (nelisp-sys-ast-prop node :mut))
             (place (nelisp-sys-ast-prop node :place))
             (body  (nelisp-sys-ast-prop node :body))
             (form  (nelisp-sys-ast-prop node :form))
             (pkey  (nelisp-sys-borrow--place-key place))
             (new-kind (if mut-p 'mut 'shared)))
        ;; Walk the place expression itself with the current active set.
        (nelisp-sys-borrow--walk place active)
        ;; Rule: any borrow of a place already exclusively borrowed is E-001.
        (when (nelisp-sys-borrow--find-mut active pkey)
          (nelisp-sys-borrow--fail
           'E-SYS-BORROW-001 form
           "borrow of `%S' conflicts with active mutable borrow" pkey))
        ;; Rule: exclusive borrow of a place with active shared borrows is E-002.
        (when (and mut-p (nelisp-sys-borrow--find-shared active pkey))
          (nelisp-sys-borrow--fail
           'E-SYS-BORROW-002 form
           "mutable borrow of `%S' conflicts with existing shared borrow" pkey))
        ;; Push the new borrow for the body and recurse.
        (let ((active* (cons (cons pkey new-kind) active)))
          (dolist (b body)
            (nelisp-sys-borrow--walk b active*)))))

     ;; --- slice-ref / slice-set!: check for obviously-invalid negative index.
     ((memq kind '(slice-ref slice-set!))
      (let* ((index (nelisp-sys-ast-prop node :index))
             (form  (nelisp-sys-ast-prop node :form)))
        (when (and index
                   (eq (nelisp-sys-ast-kind index) 'int)
                   (< (nelisp-sys-ast-prop index :value 0) 0))
          (nelisp-sys-borrow--fail
           'E-SYS-BOUNDS-001 form
           "negative constant slice index %S is always out of bounds"
           (nelisp-sys-ast-prop index :value)))
        ;; Recurse children as usual.
        (dolist (child (nelisp-sys-ast-children node))
          (nelisp-sys-borrow--walk child active))))

     ;; --- all other nodes: recurse children with the same active set.
     (t
      (dolist (child (nelisp-sys-ast-children node))
        (nelisp-sys-borrow--walk child active))))))

;;; Defun-level checks.

(defun nelisp-sys-borrow--check-defun (item)
  "Check one defun ITEM for borrow/bounds violations.
Signals `nelisp-sys-borrow-error' on the first violation."
  ;; Rule E-003: defun return type must not be a borrow.
  (let ((ret  (nelisp-sys-ast-prop item :ret))
        (form (nelisp-sys-ast-prop item :form))
        (body (nelisp-sys-ast-prop item :body)))
    (when (nelisp-sys-type-ref-p ret)
      (nelisp-sys-borrow--fail
       'E-SYS-BORROW-003 form
       "returning/escaping a borrow is forbidden in MVP (function return type %S)"
       ret))
    ;; Walk the body with an empty active-borrow set.
    (dolist (node body)
      (nelisp-sys-borrow--walk node '()))))

;;; Public API.

(defun nelisp-sys-borrow-check-module (module)
  "Check MODULE for borrow and bounds violations.
Signals `nelisp-sys-borrow-error' with a stable code on the first
violation.  Returns t when the module is clean."
  (dolist (item (nelisp-sys-ast-prop module :items))
    (when (eq (nelisp-sys-ast-kind item) 'defun)
      (nelisp-sys-borrow--check-defun item)))
  t)

(defun nelisp-sys-borrow-check-collect (module)
  "Check MODULE for borrow and bounds violations.
Returns a list of (CODE MESSAGE FORM) diagnostics, or nil when clean.
Stops at the first error (MVP: no error recovery)."
  (condition-case err
      (progn (nelisp-sys-borrow-check-module module) nil)
    (nelisp-sys-borrow-error
     (list (list (nelisp-sys-borrow-error-code err)
                 (nelisp-sys-borrow-error-message err)
                 (plist-get (nthcdr 3 err) :form))))))

(provide 'nelisp-sys-borrow)

;;; nelisp-sys-borrow.el ends here
