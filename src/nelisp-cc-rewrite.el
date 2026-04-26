;;; nelisp-cc-rewrite.el --- T161 :call-indirect → :call rewrite pre-pass  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T161 — *call-indirect → call rewrite pre-pass*, the missing piece
;; uncovered by the T158 bench gate.  Doc 42 LOCKED v2 §3.3 GO-scenario
;; pivot:
;;
;; Background — the bench-form `fib` is shaped
;;
;;   (lambda ()
;;     (letrec ((fib (lambda (n)
;;                     (if (< n 2) n
;;                       (+ (funcall fib (- n 1))
;;                          (funcall fib (- n 2)))))))
;;       (funcall fib 30)))
;;
;; The Phase 7.1 frontend lowers `(funcall fib …)` inside the inner
;; lambda body as a `:call-indirect' whose operand[0] is the def of a
;; `:load-var fib' instruction (the inner lambda body has no lexical
;; reference to the letrec scope; `fib' is therefore free inside it
;; and resolves via `:load-var').  The Phase 7.7.2 simple inliner and
;; the Phase 7.7.3 recursive unroller only inspect `:call' opcodes
;; with `:fn NAME' meta — neither pass ever sees the recursion sites
;; in this shape.  T158 surfaced this as `simple=0 / rec=0 / lifted=0'
;; pipeline-stats and a fib(30) speedup of 11.87× (= ABORT scenario in
;; the Doc 42 §3.3 outcome matrix).
;;
;; T161 closes the gap with a *targeted IR rewrite* that runs *before*
;; the inliner suite.  Every `:call-indirect' whose operand[0]'s
;; def-point is a `:load-var SYM' AND whose SYM is registered in the
;; pipeline's letrec-callee registry is rewritten in place to
;; `:call' with `(:fn SYM :unresolved t :rewritten-from-call-indirect t)'
;; meta.  The closure operand is removed from the operand list (it
;; carried no semantic information for a direct call) and its
;; `:load-var' producer's USE-LIST entry is dropped.
;;
;; *Why pre-inliner*: the inliner consults `(plist-get meta :fn)' to
;; resolve the callee — once the call-indirect is rewritten the simple
;; / recursive inliners can fire on the same instruction without any
;; further changes.  The lambda-lift pass (T155) also benefits — the
;; rewritten `:call' carries `:lifted nil' so its escape gate sees no
;; remaining `:call-indirect' use of the closure value (uses dropped),
;; which combined with the T161-companion lambda-lift gate fix (see
;; `nelisp-cc-lambda-lift.el' `nelisp-cc--lift-def-escapes-besides-callee-p'
;; +`:letrec-init' exclusion) lets self-recursive letrec lambdas reach
;; `:go'.
;;
;; *Conservative scope* — only rewrites when:
;;   1a. operand[0]'s def-point is a `:load-var' instruction whose
;;       `:name' meta matches a registry alist key, OR
;;   1b. operand[0]'s def-point is a `:store-var :letrec-init t'
;;       instruction whose `:name' meta matches a registry alist key
;;       (this catches the *outer-lambda* funcall path where the
;;       letrec scope binds NAME directly to the store-var's def
;;       rather than to a load-var indirection).
;;
;; Opaque callees (operand[0]'s def-point is anything else: `:closure',
;; `:phi', `:call', `:call-indirect', `:param', `:const', other
;; `:store-var' shapes, etc.) are left untouched — they are genuinely
;; unknown to the rewrite.
;;
;; Public API:
;;
;;   `nelisp-cc-rewrite-call-indirect-to-call'
;;       (FUNCTION REGISTRY)
;;       → (FUNCTION . REWRITE-COUNT).  Mutates FUNCTION in place;
;;       REWRITE-COUNT is the number of `:call-indirect' instructions
;;       converted.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)

(define-error 'nelisp-cc-rewrite-error
  "NeLisp call-indirect rewrite error" 'nelisp-cc-error)

;;; load-var introspection -----------------------------------------

(defun nelisp-cc--rewrite-load-var-name (val)
  "Return the SYM that VAL was produced by a `:load-var' for, or nil.

VAL is an `nelisp-cc--ssa-value'.  Returns nil when VAL has no
def-point (= `:param') or when the def-point is not a `:load-var'
instruction."
  (let ((def (and val (nelisp-cc--ssa-value-def-point val))))
    (and (nelisp-cc--ssa-instr-p def)
         (eq (nelisp-cc--ssa-instr-opcode def) 'load-var)
         (plist-get (nelisp-cc--ssa-instr-meta def) :name))))

(defun nelisp-cc--rewrite-letrec-store-name (val)
  "Return the letrec NAME that VAL was produced by a letrec store-var for, or nil.

VAL is an `nelisp-cc--ssa-value'.  Returns the `:name' meta of VAL's
def-point when the def-point is a `:store-var' instruction tagged
`:letrec-init t' (= the outer-side letrec slot write whose def is
threaded into the post-letrec scope).  Returns nil otherwise.

This covers the outer-lambda funcall path — `(letrec ((fib LAMBDA))
(funcall fib 30))' lowers `funcall fib' as `:call-indirect' whose
operand[0] is the store-var's def, not a `:load-var' (the outer
scope binds `fib' directly to the store-var def, see
`nelisp-cc--lower-letrec' Pass 2)."
  (let ((def (and val (nelisp-cc--ssa-value-def-point val))))
    (and (nelisp-cc--ssa-instr-p def)
         (eq (nelisp-cc--ssa-instr-opcode def) 'store-var)
         (plist-get (nelisp-cc--ssa-instr-meta def) :letrec-init)
         (plist-get (nelisp-cc--ssa-instr-meta def) :name))))

(defun nelisp-cc--rewrite-callee-name (val registry)
  "Return the registry NAME for VAL when it resolves to a known callee.

Tries two def-point patterns in order: `:load-var' (inner-body free
reference) then `:store-var :letrec-init' (outer-scope letrec slot
def).  Returns the matching NAME when REGISTRY has it, nil otherwise."
  (let ((sym (or (nelisp-cc--rewrite-load-var-name val)
                 (nelisp-cc--rewrite-letrec-store-name val))))
    (and sym (assq sym registry) sym)))

(defun nelisp-cc--rewrite-list-call-indirect-sites (function)
  "Return every `call-indirect' instruction in FUNCTION, in block / instr order."
  (let (acc)
    (dolist (b (nelisp-cc--ssa-function-blocks function))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (when (eq (nelisp-cc--ssa-instr-opcode instr) 'call-indirect)
          (push instr acc))))
    (nreverse acc)))

;;; per-site rewrite -----------------------------------------------

(defun nelisp-cc--rewrite-rewrite-one (call-instr callee-name)
  "Mutate CALL-INSTR in place — opcode `call-indirect' → `call'.

The closure operand (= operand[0]) is removed from the operand
list and its USE-LIST entry for CALL-INSTR is dropped so the
post-rewrite IR is consistent.  META is rebuilt as
`(:fn CALLEE-NAME :unresolved t :rewritten-from-call-indirect t)'
so the inliner's `:fn'-meta lookup succeeds and any dashboard
surface can attribute the call to the rewrite step.

Returns CALL-INSTR (mutated)."
  (unless (eq (nelisp-cc--ssa-instr-opcode call-instr) 'call-indirect)
    (signal 'nelisp-cc-rewrite-error
            (list :not-a-call-indirect call-instr)))
  (let* ((operands (nelisp-cc--ssa-instr-operands call-instr))
         (closure-val (car operands))
         (rest-args (cdr operands)))
    (unless closure-val
      (signal 'nelisp-cc-rewrite-error
              (list :empty-call-indirect call-instr)))
    ;; Drop CALL-INSTR from closure-val's USE-LIST.
    (setf (nelisp-cc--ssa-value-use-list closure-val)
          (delq call-instr
                (nelisp-cc--ssa-value-use-list closure-val)))
    (setf (nelisp-cc--ssa-instr-opcode call-instr) 'call
          (nelisp-cc--ssa-instr-operands call-instr) rest-args
          (nelisp-cc--ssa-instr-meta call-instr)
          (list :fn callee-name :unresolved t
                :rewritten-from-call-indirect t))
    call-instr))

;;; top-level entry ------------------------------------------------

;;;###autoload
(defun nelisp-cc-rewrite-call-indirect-to-call (function registry)
  "Rewrite eligible `:call-indirect' sites in FUNCTION to direct `:call'.

REGISTRY is the standard letrec-callee alist `((NAME . SSA-FUNCTION)
...)' produced by
`nelisp-cc-pipeline--collect-letrec-callee-registry'.

A site is *eligible* when its operand[0] is the def of a
`:load-var' instruction AND that load-var's `:name' meta is `assq'
in REGISTRY.

Returns `(FUNCTION . REWRITE-COUNT)'.  FUNCTION is mutated in
place; REWRITE-COUNT is the number of sites rewritten (0 when no
site matches)."
  (unless (nelisp-cc--ssa-function-p function)
    (signal 'nelisp-cc-rewrite-error
            (list :not-a-function function)))
  (let ((sites (nelisp-cc--rewrite-list-call-indirect-sites function))
        (count 0))
    (dolist (site sites)
      (let* ((operands (nelisp-cc--ssa-instr-operands site))
             (callee-val (car operands))
             (sym (nelisp-cc--rewrite-callee-name callee-val registry)))
        (when sym
          (nelisp-cc--rewrite-rewrite-one site sym)
          (cl-incf count))))
    (cons function count)))

(provide 'nelisp-cc-rewrite)

;;; nelisp-cc-rewrite.el ends here
