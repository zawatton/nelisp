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

(unless (boundp 'backquote-backquote-symbol)
  (defconst backquote-backquote-symbol '\`
    "Symbol used to represent a backquote or nested backquote."))

(unless (boundp 'backquote-unquote-symbol)
  (defconst backquote-unquote-symbol '\,
    "Symbol used to represent an unquote inside a backquote."))

(unless (boundp 'backquote-splice-symbol)
  (defconst backquote-splice-symbol '\,@
    "Symbol used to represent a splice inside a backquote."))

;; Emacs's `backquote-process' rebuilds vector templates by expanding their
;; elements under list semantics and wrapping the result in `vconcat'.
(unless (fboundp 'nelisp--bq-tag-p)
  (defun nelisp--bq-tag-p (form tag punct)
    "Non-nil when FORM starts with convenience TAG or punctuation PUNCT."
    (and (consp form)
         (let ((head (car form)))
           (or (eq head tag) (eq head (intern punct)))))))

(when (and (fboundp 'nelisp--bq-expand) (fboundp 'nelisp--bq-expand-list)
           ;; The standalone parity shim is the canonical final definition
           ;; in the bootstrap bundle.  Do not replace it when this file is
           ;; loaded later than `emacs-parity-macros2.el'.
           (not (boundp 'emacs-parity-macros2--depth-aware-backquote)))
  ;; Measured on NeLisp v1.1.0+1, `nelisp--bq-expand' accepts
  ;; (FORM &optional LEVEL) and `nelisp--bq-expand-list' accepts (FORM LEVEL).
  ;; Older preludes exposed the list helper with one argument, so retain that
  ;; load-path compatibility while passing LEVEL whenever the runtime accepts it.
  (defun emacs-backquote--runtime-expand-list (form level)
    "Expand runtime backquote list FORM at nesting LEVEL."
    (let ((level (or level 1)))
      (condition-case nil
          (nelisp--bq-expand-list form level)
        (wrong-number-of-arguments
         (nelisp--bq-expand-list form)))))

  (defun nelisp--bq-expand (form &optional level)
    "Return the expansion of FORM under `backquote'."
    (let ((level (or level 1)))
      (cond
       ((vectorp form)
        (list 'vconcat
              (emacs-backquote--runtime-expand-list
               (append form nil) level)))
       ((not (consp form))
        (list 'quote form))
       ((nelisp--bq-tag-p form 'comma ",") (cadr form))
       ((nelisp--bq-tag-p form 'comma-at ",@")
        (signal 'error (list "nelisp-bq: top-level ,@ not allowed")))
       ((nelisp--bq-tag-p form 'backquote "`")
        ;; Preserve nested backquote forms for the inner macro expansion
        ;; pass.  This is enough for local macros such as generator.el's
        ;; `(cl-macrolet ... `(cps-internal-yield ,value))' body.
        (list 'quote form))
       (t (emacs-backquote--runtime-expand-list form level))))))

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

;;;; --- GNU backquote.el helper API (S2 coverage batch) -------------------
;;
;; The macro above expands our own reader's `(comma X)' / `(comma-at X)'
;; convention directly and never calls the functions below -- our reader
;; does not emit GNU's `\,' / `\,@' / `` \` `` triad at all.  These are
;; faithful ports of real GNU Emacs 31.1 `lisp/emacs-lisp/backquote.el'
;; (verified against its source) for any vendored code that calls them
;; directly against that triad, independent of which macro expands `` `...` ''
;; forms.  Pure list/algorithm code; no native support needed.

(unless (fboundp 'backquote-list*-function)
  (defun backquote-list*-function (first &rest list)
    "Like `list' but the last argument is the tail of the new list.

For example (backquote-list* \\='a \\='b \\='c) => (a b . c)"
    (if list
        (let* ((rest list) (newlist (cons first nil)) (last newlist))
          (while (cdr rest)
            (setcdr last (cons (car rest) nil))
            (setq last (cdr last)
                  rest (cdr rest)))
          (setcdr last (car rest))
          newlist)
      first)))

(unless (fboundp 'backquote-list*-macro)
  (defmacro backquote-list*-macro (first &rest list)
    "Like `list' but the last argument is the tail of the new list.

For example (backquote-list* \\='a \\='b \\='c) => (a b . c)"
    (setq list (nreverse (cons first list))
          first (car list)
          list (cdr list))
    (if list
        (let* ((second (car list))
               (rest (cdr list))
               (newlist (list 'cons second first)))
          (while rest
            (setq newlist (list 'cons (car rest) newlist)
                  rest (cdr rest)))
          newlist)
      first)))

(unless (fboundp 'backquote-list*)
  (defalias 'backquote-list* (symbol-function 'backquote-list*-macro)))

(unless (fboundp 'backquote-listify)
  (defun backquote-listify (list old-tail)
    "Decide between `append', `list', `backquote-list*', and `cons' for LIST.
LIST is a list of (TAG . STRUCTURE) pairs from `backquote-process';
OLD-TAIL is the (TAG . STRUCTURE) pair for what should be appended
at the end."
    (let ((heads nil) (tail (cdr old-tail)) (list-tail list) (item nil))
      (if (= (car old-tail) 0)
          (setq tail (eval tail)
                old-tail nil))
      (while (consp list-tail)
        (setq item (car list-tail))
        (setq list-tail (cdr list-tail))
        (if (or heads old-tail (/= (car item) 0))
            (setq heads (cons (cdr item) heads))
          (setq tail (cons (eval (cdr item)) tail))))
      (cond
       (tail
        (if (null old-tail)
            (setq tail (list 'quote tail)))
        (if heads
            (let ((use-list* (or (cdr heads)
                                  (and (consp (car heads))
                                       (eq (car (car heads))
                                           backquote-splice-symbol)))))
              (cons (if use-list* 'backquote-list* 'cons)
                    (append heads (list tail))))
          tail))
       (t (cons 'list heads))))))

(unless (fboundp 'backquote-delay-process)
  (defun backquote-delay-process (s level)
    "Process a (un|back|splice)quote inside a backquote.
This simply recurses through the body."
    (let ((exp (backquote-listify (list (cons 0 (list 'quote (car s))))
                                   (backquote-process (cdr s) level))))
      (cons (if (eq (car-safe exp) 'quote) 0 1) exp))))

(unless (fboundp 'backquote-process)
  (defun backquote-process (s &optional level)
    "Process the body of a backquote.
S is the body.  Returns a cons cell whose cdr is a piece of code which
is the macro-expansion of S, and whose car is a small integer whose
value can either indicate that the code is constant (0), or not (1),
or returns a list which should be spliced into its environment (2).
LEVEL is only used internally and indicates the nesting level: 0 (the
default) is for the toplevel nested inside a single backquote.

This is a faithful port of GNU Emacs's own `backquote-process',
operating on the `\\=`' / `\\=,' / `\\=,@' triad (`backquote-backquote-symbol'
/ `backquote-unquote-symbol' / `backquote-splice-symbol' above) -- it is
independent of this file's own `(comma X)' / `(comma-at X)' reader
convention used by the `backquote' macro."
    (unless level (setq level 0))
    (cond
     ((vectorp s)
      (let ((n (backquote-process (append s ()) level)))
        (if (= (car n) 0)
            (cons 0 s)
          (cons 1 (cond
                   ((not (listp (cdr n)))
                    (list 'vconcat (cdr n)))
                   ((eq (nth 1 n) 'list)
                    (cons 'vector (nthcdr 2 n)))
                   ((eq (nth 1 n) 'append)
                    (cons 'vconcat (nthcdr 2 n)))
                   (t
                    (list 'apply '(function vector) (cdr n))))))))
     ((atom s)
      (cons 0 (if (or (null s) (eq s t) (not (symbolp s)))
                  s
                (list 'quote s))))
     ((eq (car s) backquote-unquote-symbol)
      (if (<= level 0)
          (cond
           ((> (length s) 2)
            (error "Multiple args to , are not supported: %S" s))
           (t (cons (if (eq (car-safe (nth 1 s)) 'quote) 0 1)
                     (nth 1 s))))
        (backquote-delay-process s (1- level))))
     ((eq (car s) backquote-splice-symbol)
      (if (<= level 0)
          (if (> (length s) 2)
              (error "Multiple args to ,@ are not supported: %S" s)
            (cons 2 (nth 1 s)))
        (backquote-delay-process s (1- level))))
     ((eq (car s) backquote-backquote-symbol)
      (backquote-delay-process s (1+ level)))
     (t
      (let ((rest s)
            item firstlist list lists expression)
        (while (and (consp rest)
                    (not (or (eq (car rest) backquote-unquote-symbol)
                             (eq (car rest) backquote-backquote-symbol))))
          (setq item (backquote-process (car rest) level))
          (cond
           ((= (car item) 2)
            (if (null lists)
                (setq firstlist list
                      list nil))
            (if list
                (push (backquote-listify list '(0 . nil)) lists))
            (push (cdr item) lists)
            (setq list nil))
           (t
            (setq list (cons item list))))
          (setq rest (cdr rest)))
        (if (or rest list)
            (push (backquote-listify list (backquote-process rest level))
                  lists))
        (setq expression
              (if (or (cdr lists)
                      (eq (car-safe (car lists)) backquote-splice-symbol))
                  (cons 'append (nreverse lists))
                (car lists)))
        (if firstlist
            (setq expression (backquote-listify firstlist (cons 1 expression))))
        (cons (if (eq (car-safe expression) 'quote) 0 1) expression))))))

(provide 'emacs-backquote)
(provide 'backquote)

;;; emacs-backquote.el ends here
