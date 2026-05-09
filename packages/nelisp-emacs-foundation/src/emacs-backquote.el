;;; emacs-backquote.el --- NeLisp port of Emacs backquote macro  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2 — Layer 2.
;;
;; Ports the `backquote' macro from Emacs's `lisp/emacs-lisp/backquote.el'
;; (which itself is the runtime support for the ` reader syntax).  Under
;; regular Emacs the C-level reader emits `(\` ...)' / `(\, ...)' /
;; `(\,@ ...)' forms and the host `backquote' macro handles them, so the
;; `unless (fboundp 'backquote)' guard below keeps this polyfill inert.
;;
;; Under NeLisp standalone the reader emits `(backquote X)' / `(comma X)' /
;; `(comma-at X)' (= per nelisp/build-tool/src/reader/parser.rs symbol
;; conventions).  This file provides the macro expander for those forms
;; using only bootstrap-eval primitives — no `cl-lib', no recursive
;; helper macros, no internal abuse of `\`' to bootstrap itself.
;;
;; Coverage: SINGLE-LEVEL backquote.  Nested ``\`(... \`(...))' forms
;; will mis-expand at the inner level — that case never appears in the
;; anvil-memory / anvil-worklog corpus we are targeting (= grep
;; confirmed zero nested-backquote sites).  Phase 2.1 will add nested
;; support if a concrete need materializes.
;;
;; Algorithm:
;;
;;   `backquote' as a macro on FORM expands to elisp that, when
;;   evaluated, reproduces FORM with `(comma X)' replaced by the value
;;   of X and `(comma-at X)' splicing the elements of X into the
;;   surrounding list.
;;
;;   - Atom F  → `(quote F)`
;;   - `(comma X)`    → X (= eval at expansion target)
;;   - `(comma-at X)` outside list context → error
;;   - List F → recursive cons / append construction:
;;       For each cell:
;;         * head is `(comma-at Y)` → splice (= use `append Y' on tail)
;;         * head is anything else  → cons (= use `cons HEAD TAIL')

;;; Code:

(defun emacs-backquote--expand (form)
  "Return an elisp form that, when evaluated, reproduces FORM with
backquote semantics.  Used by the `backquote' macro below; exposed
for testability of the recursive walker."
  (cond
   ;; Non-cons (= atom): straight quote.
   ((not (consp form))
    (list 'quote form))
   ;; (comma X) at top level → X (eval target).
   ((eq (car form) 'comma)
    (car (cdr form)))
   ;; (comma-at X) is only valid INSIDE a list context.  Top-level
   ;; usage is a programmer bug.
   ((eq (car form) 'comma-at)
    (error "emacs-backquote: (comma-at X) used at top level"))
   ;; Cons cell — walk it.
   (t
    (emacs-backquote--list form))))

(defun emacs-backquote--list (form)
  "Build an elisp form that constructs the list FORM, honouring comma /
comma-at substitutions inside its cells.

Phase B5 fix (= 2026-05-09): detect `(... . ,Y)' / `(... . ,@Y)' tails
specifically.  The reader represents `(a . ,x)' as `(a comma x)' — a
3-element proper list — and `(a . ,@x)' as `(a comma-at x)'.  Without
the dotted-tail detection below the recursive walker treated those as
`(a comma x)' / `(a comma-at x)' literal lists and produced
`(list 'a 'comma 'x)' instead of the intended `(cons 'a x)'."
  (cond
   ;; Empty list: just nil.
   ((null form)
    nil)
   ;; Improper list terminator (= non-cons tail): quote it.
   ((not (consp form))
    (list 'quote form))
   ;; Dotted-tail comma form: `(... HEAD . ,Y)' read as
   ;; `(... HEAD comma Y)'.  Stop recursion and emit (cons HEAD-EXPR Y).
   ((and (consp (cdr form))
         (eq (car (cdr form)) 'comma)
         (consp (cdr (cdr form)))
         (null (cdr (cdr (cdr form)))))
    (let ((head-expr (emacs-backquote--expand (car form)))
          (tail-value (car (cdr (cdr form)))))
      (list 'cons head-expr tail-value)))
   ;; Dotted-tail splicing form: `(... HEAD . ,@Y)' read as
   ;; `(... HEAD comma-at Y)'.  Real Emacs would error here at read-time
   ;; (`,@' after `.' is not a list), but we just emit `(append HEAD
   ;; Y)' for forwards-compat with tests that pre-construct the form.
   ((and (consp (cdr form))
         (eq (car (cdr form)) 'comma-at)
         (consp (cdr (cdr form)))
         (null (cdr (cdr (cdr form)))))
    (let ((head-expr (emacs-backquote--expand (car form)))
          (tail-value (car (cdr (cdr form)))))
      (list 'append (list 'list head-expr) tail-value)))
   (t
    (let* ((head (car form))
           (tail (cdr form))
           (tail-expr (emacs-backquote--list tail))
           (is-splice (and (consp head) (eq (car head) 'comma-at))))
      (cond
       (is-splice
        ;; head is (comma-at Y).  Result: append Y to tail expansion.
        (let ((spliced (car (cdr head))))
          (if (null tail-expr)
              ;; `(... ,@Y)` → just Y itself.
              spliced
            (list 'append spliced tail-expr))))
       (t
        (let ((head-expr (emacs-backquote--expand head)))
          (cond
           ((null tail-expr)
            ;; `(... HEAD)` → (list HEAD-EXPR).
            (list 'list head-expr))
           (t
            ;; `(... HEAD . TAIL)` → (cons HEAD-EXPR TAIL-EXPR).
            (list 'cons head-expr tail-expr))))))))))

(unless (fboundp 'backquote)
  (defmacro backquote (form)
    "Polyfill: expand FORM under backquote semantics.
Walks FORM looking for `(comma X)' (= replace with X's value at
expansion target) and `(comma-at X)' (= splice X's elements into the
surrounding list).  Single-level only; nested backquotes are not
supported by this polyfill (Phase 2.1)."
    (emacs-backquote--expand form)))


(provide 'emacs-backquote)

;;; emacs-backquote.el ends here
