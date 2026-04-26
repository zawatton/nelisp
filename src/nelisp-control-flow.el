;;; nelisp-control-flow.el --- T153 / Phase 7+E control flow + advice  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; T153 / Doc 40 §3.E LOCKED v2 — Phase 7+E control flow + advice.
;;
;; This module ports Emacs's control-flow special forms and the
;; nadvice subsystem onto the Phase 7+B evaluator namespace.  The
;; module is split into two layers:
;;
;;   1. control-flow special-forms  (`nelisp-cf-' internal namespace)
;;      - condition-case  handler dispatch + parent-error chain match
;;      - unwind-protect  cleanup runs on every exit (normal / throw /
;;                        signal), exceptions in cleanup propagate
;;      - catch / throw   tagged non-local exit, eq tag match
;;      - signal          host signal piggyback so condition-case
;;                        catches it and `error-conditions` propagation
;;                        works through normal Elisp parent chain
;;
;;      Each form registers itself into `nelisp-special-forms--dispatch'
;;      at load time so `nelisp-special-forms-eval' picks them up
;;      transparently — no caller-side dispatch table edits required.
;;
;;   2. advice subsystem  (`nelisp-advice-' public namespace)
;;      - advice-add      install :before / :after / :around /
;;                        :before-while / :before-until / :after-while /
;;                        :after-until / :filter-args / :filter-return /
;;                        :override (10 HOW values per Emacs nadvice
;;                        precedent + Doc 40 §3.E §6.7 audit)
;;      - advice-remove   uninstall by FUNCTION value or symbol name
;;      - advice-mapc     iterate installed advice cells
;;      - advice-member-p predicate
;;
;;      Implementation = function-cell wrap.  The original function is
;;      saved on the symbol's `nelisp-advice--original' property, and
;;      the symbol's function cell is replaced with a closure that
;;      walks the advice chain and dispatches per HOW.  Removing all
;;      advice restores the original function and clears the property.
;;
;;      Advice is recorded in `nelisp-advice--registry' (sym -> list
;;      of `(HOW FUNCTION PROPS)' cells, ordered by install time;
;;      first added = innermost).  Re-adding the same FUNCTION under
;;      the same HOW updates PROPS without duplicating the entry —
;;      mirrors `advice-add' idempotence in Emacs nadvice.
;;
;; Public API surface (Doc 40 §4.5):
;;
;;   (nelisp-advice-add SYMBOL HOW FUNCTION &optional PROPS)
;;   (nelisp-advice-remove SYMBOL FUNCTION)
;;   (nelisp-advice-mapc FN SYMBOL)
;;   (nelisp-advice-member-p FUNCTION SYMBOL)
;;   (nelisp-advice-clear-all)                      -- test convenience
;;
;;   (nelisp-cf-signal SYMBOL DATA)                 -- explicit helper
;;
;; Out of scope (carry-forward per Doc 40 LOCKED v2):
;;   - defadvice (= legacy CL macro, not part of this milestone).
;;   - error-symbol propagation across NeLisp <-> host bridge in
;;     non-trivial cases (= Phase 7+F M3 milestone, anvil-server bridge
;;     glue).  Local condition-case <-> signal works in this module;
;;     bridge-level translation is wired by Phase 7+F.

;;; Code:

(require 'cl-lib)
(require 'nelisp-special-forms)

;;; -----------------------------------------------------------------
;;; Errors
;;; -----------------------------------------------------------------

(define-error 'nelisp-cf-error
  "NeLisp control-flow error (Phase 7+E)")

(define-error 'nelisp-cf-bad-handler
  "NeLisp control-flow: malformed condition-case handler"
  'nelisp-cf-error)

(define-error 'nelisp-advice-error
  "NeLisp advice error (Phase 7+E)")

(define-error 'nelisp-advice-bad-how
  "NeLisp advice: HOW value is not one of the supported keywords"
  'nelisp-advice-error)


;;; =================================================================
;;; PART 1: control-flow special forms
;;; =================================================================

(defun nelisp-cf--handler-matches-p (spec conditions)
  "Return non-nil if SPEC matches one of CONDITIONS.
SPEC = a condition symbol, a list of condition symbols, or t (= catch
anything).  CONDITIONS is the error's `error-conditions' chain (most
specific first)."
  (cond
   ((eq spec t) t)
   ((symbolp spec) (memq spec conditions))
   ((listp spec)
    (catch 'nelisp-cf--match-done
      (dolist (s spec)
        (when (memq s conditions)
          (throw 'nelisp-cf--match-done t)))
      nil))
   (t nil)))

(defun nelisp-cf--eval-condition-case (args env)
  "(condition-case VAR BODYFORM (CONDITION HANDLER-BODY...)...).
VAR is bound to the signaled error data inside a matching handler
(or left unbound when VAR is nil).  Handlers are tried in order;
the first match wins.  Unmatched errors propagate to the caller.

The error data shape is `(ERROR-SYMBOL . DATA)' (Emacs convention),
so a handler bound to VAR gets the cons that `signal' would have
constructed."
  (let ((var (car args))
        (bodyform (cadr args))
        (handlers (cddr args)))
    (unless (or (null var) (symbolp var))
      (signal 'nelisp-cf-error
              (list "condition-case VAR must be symbol or nil" var)))
    (condition-case err
        (nelisp-special-forms-eval bodyform env)
      (error
       (let* ((conditions (get (car err) 'error-conditions))
              (matched nil))
         (catch 'nelisp-cf--cc-done
           (dolist (h handlers)
             (unless (consp h)
               (signal 'nelisp-cf-bad-handler (list h)))
             (when (nelisp-cf--handler-matches-p (car h) conditions)
               (setq matched h)
               (throw 'nelisp-cf--cc-done nil))))
         (if matched
             (let ((handler-env (if var
                                    (cons (cons var err) env)
                                  env)))
               (nelisp-special-forms--eval-body (cdr matched) handler-env))
           (signal (car err) (cdr err))))))))

(defun nelisp-cf--eval-unwind-protect (args env)
  "(unwind-protect BODYFORM UNWINDFORMS...) — cleanup on every exit.
Normal completion, NeLisp `throw', and any signaled error all unwind
through the cleanup block.  If the cleanup itself signals, that signal
replaces the body's outcome (Emacs semantics)."
  (unwind-protect
      (nelisp-special-forms-eval (car args) env)
    (nelisp-special-forms--eval-body (cdr args) env)))

(defun nelisp-cf--eval-catch (args env)
  "(catch TAG-FORM BODY...) — tagged non-local exit sink."
  (let ((tag (nelisp-special-forms-eval (car args) env)))
    (catch tag
      (nelisp-special-forms--eval-body (cdr args) env))))

(defun nelisp-cf--eval-throw (args env)
  "(throw TAG-FORM VALUE-FORM) — transfer control to matching catch.
If no matching catch is in scope, host Elisp signals `no-catch' which
may be caught by an enclosing `condition-case' that lists `no-catch'
or any of its parent conditions."
  (let ((tag (nelisp-special-forms-eval (car args) env))
        (val (nelisp-special-forms-eval (cadr args) env)))
    (throw tag val)))

(defun nelisp-cf-signal (symbol data)
  "Public helper that raises a NeLisp signal with SYMBOL + DATA.
Equivalent to host `signal' but routed through this module so callers
have a stable, namespaced entry point.  Returns non-locally."
  (signal symbol data))

;; Register the four new special-form handlers into the Phase 7+B
;; dispatcher table.  Doing it at load time means
;; `nelisp-special-forms-eval' picks them up the moment this module is
;; required, with no caller-side wiring.
(let ((tbl nelisp-special-forms--dispatch))
  (puthash 'condition-case  'nelisp-cf--eval-condition-case  tbl)
  (puthash 'unwind-protect  'nelisp-cf--eval-unwind-protect  tbl)
  (puthash 'catch           'nelisp-cf--eval-catch           tbl)
  (puthash 'throw           'nelisp-cf--eval-throw           tbl))


;;; =================================================================
;;; PART 2: advice subsystem
;;; =================================================================

(defconst nelisp-advice--how-values
  '(:before :after :around
    :before-while :before-until
    :after-while :after-until
    :filter-args :filter-return
    :override)
  "Supported HOW keywords for `nelisp-advice-add' (Doc 40 §3.E).")

(defvar nelisp-advice--registry (make-hash-table :test 'eq)
  "Symbol -> list of advice cells.
Each cell is a list `(HOW FUNCTION PROPS)'.  Cells are stored in
install order so the first cell installed is the innermost wrapper
during dispatch.")

(defvar nelisp-advice--originals (make-hash-table :test 'eq)
  "Symbol -> original function value before any advice was installed.
Cleared when the last advice on a symbol is removed and the symbol's
function cell is restored.")

(defvar nelisp-advice--installed (make-hash-table :test 'eq)
  "Symbol -> t when the symbol's function cell is currently a
NeLisp-installed advice trampoline.  Used to short-circuit the
re-install path on subsequent `nelisp-advice-add' calls.")


(defun nelisp-advice--validate-how (how)
  "Signal `nelisp-advice-bad-how' when HOW is not a supported keyword."
  (unless (memq how nelisp-advice--how-values)
    (signal 'nelisp-advice-bad-how
            (list how nelisp-advice--how-values))))

(defun nelisp-advice--cells (symbol)
  "Return the advice-cell list installed on SYMBOL (may be nil)."
  (gethash symbol nelisp-advice--registry))

(defun nelisp-advice--set-cells (symbol cells)
  "Replace the advice-cell list on SYMBOL with CELLS."
  (if cells
      (puthash symbol cells nelisp-advice--registry)
    (remhash symbol nelisp-advice--registry)))

(defun nelisp-advice--cell-equal (cell how function)
  "Non-nil when advice CELL targets the same HOW and FUNCTION.
Identity is `eq' on FUNCTION except symbols compare by `eq'."
  (and (eq (nth 0 cell) how)
       (eq (nth 1 cell) function)))

(defun nelisp-advice--call-original (symbol args)
  "Apply the saved original function for SYMBOL to ARGS."
  (let ((orig (gethash symbol nelisp-advice--originals 'nelisp-advice--missing)))
    (cond
     ((eq orig 'nelisp-advice--missing)
      (signal 'nelisp-advice-error
              (list "no original function recorded for" symbol)))
     ((null orig)
      (signal 'nelisp-advice-error
              (list "original function for" symbol "is nil")))
     (t (apply orig args)))))

(defun nelisp-advice--apply-one (cell core args)
  "Apply a single advice CELL on top of CORE (a thunk taking ARGS).
CELL = (HOW FUNCTION PROPS).  CORE is a function that, when called
with the eventual ARGS, runs the next-inner layer (either another
advice cell or the original function).  Returns the layer's result.

Each HOW keyword has the standard Emacs nadvice semantics."
  (let ((how (nth 0 cell))
        (fn  (nth 1 cell)))
    (pcase how
      (:before
       (apply fn args)
       (funcall core args))
      (:after
       (let ((r (funcall core args)))
         (apply fn args)
         r))
      (:around
       (apply fn (lambda (&rest inner-args) (funcall core inner-args))
              args))
      (:override
       (apply fn args))
      (:before-while
       (if (apply fn args)
           (funcall core args)
         nil))
      (:before-until
       (or (apply fn args)
           (funcall core args)))
      (:after-while
       (let ((r (funcall core args)))
         (if r (apply fn args) nil)))
      (:after-until
       (let ((r (funcall core args)))
         (or r (apply fn args))))
      (:filter-args
       ;; Per Emacs nadvice: FN receives the args list as a single
       ;; argument and returns a (possibly different) list of args
       ;; for the wrapped function.
       (let ((new-args (funcall fn args)))
         (funcall core (if (listp new-args) new-args (list new-args)))))
      (:filter-return
       (funcall fn (funcall core args)))
      (_ (signal 'nelisp-advice-bad-how (list how))))))

(defun nelisp-advice--build-trampoline (symbol)
  "Return a closure that walks the advice chain on SYMBOL.
The returned function captures SYMBOL and dispatches each call
through every cell currently installed.  The chain order is `outermost
= last installed', i.e. cells are walked in reverse of install order
so the most-recently-added advice wraps the result of the prior chain."
  (lambda (&rest args)
    (let* ((cells (nelisp-advice--cells symbol))
           ;; Build the chain by folding from the innermost (= original)
           ;; outwards.  CORE is a function-of-args returning the inner
           ;; layer's result; we wrap it once per cell, walking cells
           ;; in install order so the LAST-installed cell ends up
           ;; outermost.
           (core (lambda (a) (nelisp-advice--call-original symbol a))))
      (dolist (cell cells)
        (let ((inner core)
              (this  cell))
          (setq core (lambda (a)
                       (nelisp-advice--apply-one this inner a)))))
      (funcall core args))))

(defun nelisp-advice--install-trampoline (symbol)
  "Install the advice trampoline on SYMBOL's function cell.
Saves the original function in `nelisp-advice--originals' the first
time it runs for SYMBOL, so repeated install/remove cycles preserve
the genuine original (not a previously-installed trampoline)."
  (unless (gethash symbol nelisp-advice--installed)
    (let ((orig (and (fboundp symbol) (symbol-function symbol))))
      (puthash symbol orig nelisp-advice--originals))
    (puthash symbol t nelisp-advice--installed)
    (fset symbol (nelisp-advice--build-trampoline symbol))))

(defun nelisp-advice--uninstall-if-empty (symbol)
  "Restore the original function on SYMBOL when no advice remains."
  (unless (nelisp-advice--cells symbol)
    (when (gethash symbol nelisp-advice--installed)
      (let ((orig (gethash symbol nelisp-advice--originals)))
        (if orig
            (fset symbol orig)
          (fmakunbound symbol)))
      (remhash symbol nelisp-advice--installed)
      (remhash symbol nelisp-advice--originals))))


;;;###autoload
(defun nelisp-advice-add (symbol how function &optional props)
  "Install FUNCTION as advice on SYMBOL with combinator HOW.
HOW must be one of `nelisp-advice--how-values' (= the 10 standard
Emacs nadvice keywords).  PROPS is an optional plist recorded in the
advice cell; the only well-known key today is `:name', used by
`nelisp-advice-remove' for symbol-named removal.

Calling `nelisp-advice-add' twice with the same HOW and FUNCTION is
idempotent (PROPS is updated, no duplicate cell is created).  The
trampoline is installed on the symbol's function cell on the first
call and reused thereafter.

Return value is the advice cell `(HOW FUNCTION PROPS)' that ends up
in the registry."
  (nelisp-advice--validate-how how)
  (unless (symbolp symbol)
    (signal 'nelisp-advice-error
            (list "advice-add: SYMBOL must be a symbol" symbol)))
  (let* ((cells (nelisp-advice--cells symbol))
         (existing (cl-find-if (lambda (c)
                                 (nelisp-advice--cell-equal c how function))
                               cells)))
    (cond
     (existing
      ;; Idempotent re-install: refresh PROPS without changing order.
      (setf (nth 2 existing) props)
      (nelisp-advice--install-trampoline symbol)
      existing)
     (t
      (let ((cell (list how function props)))
        (nelisp-advice--set-cells symbol (append cells (list cell)))
        (nelisp-advice--install-trampoline symbol)
        cell)))))

;;;###autoload
(defun nelisp-advice-remove (symbol function)
  "Remove FUNCTION from the advice chain on SYMBOL.
FUNCTION is matched against each cell's FUNCTION field by `eq'.  All
HOW combinators are checked, so calling `advice-remove' once removes
every cell whose FUNCTION matches (Emacs nadvice semantics).

When the removal empties the chain, the original function is
restored on SYMBOL's function cell.  Removing FUNCTION that is not
installed is a no-op (no error).  Returns t iff at least one cell
was removed."
  (unless (symbolp symbol)
    (signal 'nelisp-advice-error
            (list "advice-remove: SYMBOL must be a symbol" symbol)))
  (let* ((cells (nelisp-advice--cells symbol))
         (kept  (cl-remove-if (lambda (c) (eq (nth 1 c) function)) cells))
         (changed (not (eq (length kept) (length cells)))))
    (when changed
      (nelisp-advice--set-cells symbol kept)
      (nelisp-advice--uninstall-if-empty symbol))
    changed))

;;;###autoload
(defun nelisp-advice-mapc (fn symbol)
  "Call FN on each advice cell installed on SYMBOL.
FN receives one argument per cell, the cell `(HOW FUNCTION PROPS)'.
Returns nil.  Iteration order = install order (innermost first)."
  (dolist (cell (nelisp-advice--cells symbol))
    (funcall fn cell))
  nil)

;;;###autoload
(defun nelisp-advice-member-p (function symbol)
  "Return non-nil if FUNCTION is currently installed as advice on SYMBOL.
Returns the matching advice cell, or nil."
  (cl-find-if (lambda (c) (eq (nth 1 c) function))
              (nelisp-advice--cells symbol)))

;;;###autoload
(defun nelisp-advice-clear-all ()
  "Remove every advice cell from every symbol.
Convenience helper for ERT setup/teardown.  Walks the registry,
restores each symbol's original function, and clears the bookkeeping
tables."
  (let ((syms nil))
    (maphash (lambda (k _v) (push k syms)) nelisp-advice--registry)
    (dolist (sym syms)
      (nelisp-advice--set-cells sym nil)
      (nelisp-advice--uninstall-if-empty sym)))
  ;; Defensive sweep: anything left in `installed' (= partial state from
  ;; a crashed test) is cleared explicitly so the next ERT starts fresh.
  (let ((leftover nil))
    (maphash (lambda (k _v) (push k leftover)) nelisp-advice--installed)
    (dolist (sym leftover)
      (let ((orig (gethash sym nelisp-advice--originals)))
        (when orig (fset sym orig)))
      (remhash sym nelisp-advice--installed)
      (remhash sym nelisp-advice--originals)))
  nil)

(provide 'nelisp-control-flow)

;;; nelisp-control-flow.el ends here
