;;; emacs-parity-macroexpand.el --- correct macroexpand for (macro CLOSURE) -*- lexical-binding: t; -*-
;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- macroexpand entry points route through the library macro table (needs parity review).

;; The standalone substrate stores an interpreted macro's function cell as
;;   (macro CLOSURE)          ; the closure is the CADR
;; but the builtin `macroexpand-1'/`macroexpand' expand by applying the CDR --
;; which here is (CLOSURE), a LIST, not a function -- and hard-abort
;; (`nelisp-bare-abort', not catchable by `condition-case') on EVERY real macro.
;; Verified on nelisp-gc-b1k21-v48: `(macroexpand-1 '(when t 1))',
;; `(macroexpand '(when t 1))' and `(macroexpand-all '(rx (group "a")))' all
;; abort, while `(apply (cadr (symbol-function 'when)) '(t 1))' correctly yields
;; `(if t (progn 1) nil)'.
;;
;; This breaks every `macroexpand-all' consumer.  The visible casualty in the
;; real-init audit is `rx-let' / `python-rx': `rx-let' expands its body with
;; `macroexpand-all' so the inner `rx' forms translate, and python.el wraps all
;; its regexps in `(python-rx ...)' = `(rx-let (...) (rx ...))'.  With the
;; broken expander the body's `rx' never expands, producing 15 "missing \) for
;; group" errors plus a cascade of void python-font-lock / python-nav /
;; python-skeleton definitions.
;;
;; Two fixes, both pure Elisp:
;;   1. Apply the macro CLOSURE correctly (CADR when the CDR is not itself a
;;      function), so one expansion layer works.
;;   2. `macroexpand-all' must dynamically bind the GLOBAL
;;      `macroexpand-all-environment' to its ENVIRONMENT argument, because
;;      macros such as `rx' read the local definitions from that global
;;      (the rx translator does `(assq :rx-locals macroexpand-all-environment)').
;;      The earlier shim only passed the env as an argument, so `:rx-locals'
;;      was invisible and local rx names were undefined.
;;
;; Override on standalone NeLisp: a broken builtin beats a guarded src defun
;; via fboundp-gating, so the standalone path must clobber rather than use
;; `(unless (fboundp ...))'.  Host Emacs skips the definitions themselves.

(when (fboundp 'nelisp--write-stdout-bytes)

(defvar macroexpand-all-environment nil
  "Environment threaded through `macroexpand-all' (macrolet / rx-let locals).")

(defun macroexpand-1 (form &optional environment)
  "Expand FORM by one macro layer and return the result.
Correct for the substrate's `(macro CLOSURE)' representation and honoring
ENVIRONMENT, the macrolet-style alist mapping a macro NAME to its
expander (either a function or a `(macro . EXPANDER)' cell)."
  (if (not (consp form))
      form
    (let ((head (car form)))
      (if (not (symbolp head))
          form
        (let ((env-def (cdr (assq head environment))))
          (cond
           ;; Local macro from ENVIRONMENT, stored as (macro . EXPANDER).
           ((eq (car-safe env-def) 'macro)
            (let ((e (cdr env-def)))
              (apply (if (functionp e) e (car e)) (cdr form))))
           ;; Local macro from ENVIRONMENT, stored as a bare expander function.
           ((functionp env-def)
            (apply env-def (cdr form)))
           ;; Global macro: symbol-function is (macro CLOSURE) or (macro . FN).
           (t
            (let ((fn (and (fboundp head) (symbol-function head))))
              ;; Resolve one level of symbol/defalias indirection.
              (while (and (symbolp fn) fn (fboundp fn))
                (setq fn (symbol-function fn)))
              (if (and (consp fn) (eq (car fn) 'macro))
                  (let ((e (cdr fn)))
                    (apply (if (functionp e) e (car e)) (cdr form)))
                form)))))))))

(defun macroexpand (form &optional environment)
  "Repeatedly expand FORM until its head is no longer a macro call."
  (let ((cur form)
        (next (macroexpand-1 form environment)))
    (while (not (eq next cur))
      (setq cur next)
      (setq next (macroexpand-1 cur environment)))
    cur))

(defun macroexpand-all--rec (form)
  "Recursively macro-expand FORM using the dynamic `macroexpand-all-environment'."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((memq (car form) '(let let*))
    ;; Binding entries are syntax, not calls.  A variable named `do' must
    ;; not invoke the CL `do' macro while expanding a pcase-generated let.
    (cons (car form)
          (cons (mapcar (lambda (binding)
                          (if (consp binding)
                              (cons (car binding)
                                    (mapcar #'macroexpand-all--rec (cdr binding)))
                            binding))
                        (cadr form))
                (mapcar #'macroexpand-all--rec (cddr form)))))
   ((eq (car form) 'cond)
    ;; Expand the expressions inside each clause, not the clause as a call.
    (cons 'cond (mapcar (lambda (clause)
                         (mapcar #'macroexpand-all--rec clause))
                       (cdr form))))
   ((eq (car form) 'condition-case)
    (append (list 'condition-case (cadr form)
                  (macroexpand-all--rec (nth 2 form)))
            (mapcar (lambda (handler)
                      (cons (car handler)
                            (mapcar #'macroexpand-all--rec (cdr handler))))
                    (cdddr form))))
   ((eq (car form) 'function)
    ;; Mirror GNU's macroexp.el `(function . REST)' case: only
    ;; `#'(lambda ARGLIST . BODY)' needs expanding, and only its BODY (the
    ;; ARGLIST is left untouched, matching `macroexp--all-forms F 2').  A
    ;; bare `#'SYMBOL' function-cell reference is left untouched.  Without
    ;; this, macros used inside a closure passed via `#'(lambda ...)' (e.g.
    ;; `inline.el's `define-inline' expands its whole body -- including any
    ;; `#'(lambda ...)' sharing helper pcase.el may introduce -- with
    ;; `macroexpand-all') are never expanded, leaving raw unexpanded macro
    ;; calls in the installed function and causing `invalid-function' at
    ;; call time.
    (if (and (eq (car-safe (cadr form)) 'lambda) (null (cddr form)))
        (let* ((lam (cadr form))
               (arglist (cadr lam))
               (body (cddr lam)))
          (list 'function
                (append (list 'lambda arglist)
                        (mapcar #'macroexpand-all--rec body))))
      form))
   ((eq (car form) 'eval-when-compile)
    (list 'quote (eval (macroexp-progn (cdr form)) t)))
   (t
    (let ((expanded (macroexpand-1 form macroexpand-all-environment)))
      (if (not (eq expanded form))
          (macroexpand-all--rec expanded)
        ;; Head is not a macro: recurse into each subform, preserving any
        ;; dotted tail (this mirrors the previous shim's structural recursion).
        (let ((out nil) (cur form))
          (while (consp cur)
            (setq out (cons (macroexpand-all--rec (car cur)) out))
            (setq cur (cdr cur)))
          (nconc (nreverse out) cur)))))))

(defun macroexpand-all (form &optional environment)
  "Recursively expand all macros in FORM, honoring ENVIRONMENT.
Binds the global `macroexpand-all-environment' during expansion so macros
that consult it (e.g. `rx' via `:rx-locals') see the local definitions."
  (let ((macroexpand-all-environment environment))
    (macroexpand-all--rec form)))

(defun macrop (object)
  "Return non-nil if OBJECT (a symbol or function object) is a macro."
  (let ((fn (cond ((and (symbolp object) (fboundp object))
                   (symbol-function object))
                  (t object))))
    (while (and (symbolp fn) fn (fboundp fn))
      (setq fn (symbol-function fn)))
    (and (consp fn) (eq (car fn) 'macro) t)))

)

(provide 'emacs-parity-macroexpand)
;;; emacs-parity-macroexpand.el ends here
