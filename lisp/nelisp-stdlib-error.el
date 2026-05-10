;;; nelisp-stdlib-error.el --- elisp condition system substrate (Doc 86 §86.2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 86 §86.2 (2026-05-10) — elisp-side condition / error infra.
;;
;; The Rust evaluator unwinds via `EvalError' (= `eval/error.rs') and
;; `condition-case' catches on the canonical error-tag string returned
;; by `EvalError::error_tag'.  This file provides the *elisp-visible*
;; surface of that machinery:
;;
;;   - A registry of declared conditions (= alist) so user code can
;;     `define-error' fresh tags with parent links.
;;   - `define-error' macro — the public knob for declaring a new
;;     condition with a default message and parent chain.
;;   - `error-message-string' — accessor that turns a `(SYMBOL . DATA)'
;;     cons (= what `condition-case' binds the var to) into a printable
;;     string.
;;   - `condition-of-p' — predicate that mirrors Rust's
;;     `is_error_subtype' for the elisp side.
;;   - Pre-declared canonical conditions matching the Rust
;;     `EvalError::error_tag' output: `error', `quit', `void-variable',
;;     `void-function', `wrong-type-argument', `wrong-number-of-arguments',
;;     `arith-error', `setting-constant', `no-catch', `invalid-read-syntax',
;;     `user-error'.
;;
;; SourcePos representation: a `(line . col)' cons cell.  Reader-side
;; ERT and elisp callers that need positional info encode it that way
;; (Rust `SourcePos { line, col }' → `(LINE . COL)' on the bridge).
;;
;; Load order: AFTER `nelisp-stdlib-eval-special.el' (= we need
;; `defun' / `defmacro' / `cond' macros) and BEFORE `nelisp-stdlib.el'
;; so the basic stdlib can rely on `define-error' / `error-message-string'
;; being live.

;;; Code:

;;;; --- 1. Condition registry ----------------------------------------
;;
;; Internal alist mapping each condition symbol to a cons
;; `(PARENTS . MESSAGE)' where PARENTS is a list of parent condition
;; symbols (transitively walked by `condition-of-p') and MESSAGE is the
;; default human-readable string.
;;
;; The registry is implemented as a plain alist so it works without
;; `put' / `get' / `symbol-plist' (none of which are bootstrap
;; primitives at this load order).

(setq nelisp--error-conditions-alist nil)

(defun nelisp--error-register (sym parents message)
  "Insert (or replace) condition SYM with PARENTS and MESSAGE."
  (let ((cur nelisp--error-conditions-alist)
        (found nil))
    (while (and cur (not found))
      (if (eq (car (car cur)) sym)
          (progn
            (setcdr (car cur) (cons parents message))
            (setq found t))
        (setq cur (cdr cur))))
    (unless found
      (setq nelisp--error-conditions-alist
            (cons (cons sym (cons parents message))
                  nelisp--error-conditions-alist))))
  sym)

(defun nelisp--error-lookup (sym)
  "Return `(PARENTS . MESSAGE)' cell for SYM, or nil if undeclared."
  (let ((cur nelisp--error-conditions-alist)
        (hit nil))
    (while (and cur (not hit))
      (if (eq (car (car cur)) sym)
          (setq hit (cdr (car cur)))
        (setq cur (cdr cur))))
    hit))

;;;; --- 2. Predicates ------------------------------------------------

(defun condition-of-p (clause-tag actual-tag)
  "Non-nil if CLAUSE-TAG catches an error whose tag is ACTUAL-TAG.
Mirrors Rust `is_error_subtype': exact match, the universal `t' clause
catches everything (including `quit'), and the `error' parent catches
every non-`quit' error.  Walks the registered parent chain when both
tags are declared conditions."
  (cond
   ((eq clause-tag actual-tag) t)
   ((eq clause-tag t) t)
   ((and (eq clause-tag 'error) (not (eq actual-tag 'quit))) t)
   (t
    ;; Walk ACTUAL-TAG's parent chain; if CLAUSE-TAG appears, match.
    (let* ((entry (nelisp--error-lookup actual-tag))
           (parents (if entry (car entry) nil))
           (cur parents)
           (hit nil))
      (while (and cur (not hit))
        (if (eq (car cur) clause-tag)
            (setq hit t)
          (setq cur (cdr cur))))
      hit))))

(defun error-conditions-of (sym)
  "Return SYM's parent condition chain (a list), or nil if undeclared."
  (let ((entry (nelisp--error-lookup sym)))
    (if entry (car entry) nil)))

;;;; --- 3. define-error ----------------------------------------------

(defmacro define-error (name message &rest parents)
  "Declare NAME as an error condition with MESSAGE and optional PARENTS.
PARENTS defaults to `(error)'.  This is the elisp-visible knob; the
Rust evaluator's canonical tag set (= `EvalError::error_tag' output) is
also pre-registered below so `condition-case' / `condition-of-p' see a
consistent picture across the bridge."
  (cons 'nelisp--error-register
        (cons (cons 'quote (cons name nil))
              (cons (cons 'quote
                          (cons (if parents parents '(error)) nil))
                    (cons message nil)))))

;;;; --- 4. Pre-declared canonical conditions -------------------------
;;
;; Mirror of the Rust `EvalError::error_tag' / `ReadError' tag set.
;; Parent chains follow GNU Emacs `subr.el' (= `error' is the universal
;; non-quit parent; `quit' has no parent; arith / wrong-type / etc. all
;; descend from `error').

;; Universal roots.
(nelisp--error-register 'error nil "error")
(nelisp--error-register 'quit nil "Quit")

;; EvalError variants.
(nelisp--error-register 'void-variable           '(error) "Symbol's value as variable is void")
(nelisp--error-register 'void-function           '(error) "Symbol's function definition is void")
(nelisp--error-register 'wrong-type-argument     '(error) "Wrong type argument")
(nelisp--error-register 'wrong-number-of-arguments '(error) "Wrong number of arguments")
(nelisp--error-register 'arith-error             '(error) "Arithmetic error")
(nelisp--error-register 'setting-constant        '(error) "Attempt to set a constant symbol")
(nelisp--error-register 'no-catch                '(error) "No catch for tag")
(nelisp--error-register 'user-error              '(error) "")

;; ReadError variants.
(nelisp--error-register 'invalid-read-syntax     '(error) "Invalid read syntax")
(nelisp--error-register 'end-of-file             '(error) "End of file during parsing")

;;;; --- 5. error-message-string --------------------------------------
;;
;; Accessor that turns a `(SYMBOL . DATA)' cons (= the binding shape
;; `condition-case' produces) into a human-readable string.  When
;; `concat' / `prin1-to-string' are not yet available at load order
;; the fallback is the registered default message (= what GNU Emacs
;; would call `(get SYM 'error-message)').

(defun error-message-string (err)
  "Return a string for the `(SYMBOL . DATA)' error cons ERR."
  (let* ((sym (car err))
         (entry (nelisp--error-lookup sym)))
    (if entry
        (cdr entry)
      ;; Undeclared tag — fall back to the symbol's name (always safe).
      (symbol-name sym))))

;;;; --- 6. SourcePos helpers -----------------------------------------
;;
;; The Rust reader emits `SourcePos { line, col }' which round-trips
;; over the bridge as a `(LINE . COL)' cons.  Provide trivial accessors
;; so elisp code can pattern-match on positions without re-deriving
;; the shape.

(defun nelisp--source-pos-line (pos) (car pos))
(defun nelisp--source-pos-col  (pos) (cdr pos))
(defun nelisp--make-source-pos (line col) (cons line col))

;;;; --- 7. Provide ---------------------------------------------------
;;
;; `provide' is installed by `nelisp-stdlib.el' which loads AFTER us.
;; Skipping here matches the convention in `nelisp-jit-substrate.el'
;; (= top of the bootstrap chain).

;;; nelisp-stdlib-error.el ends here
