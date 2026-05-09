;;; nelisp-jit-substrate.el --- Pre-stdlib expression substrate (Doc 80)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 80 Stage 80.1 (2026-05-09) — Pre-stdlib elisp substrate.
;;
;; Loaded as STDLIB_FILES[0] (= BEFORE `nelisp-jit-strategy.el' but in
;; the same chain).  Installs the minimal expression set that lets
;; `nelisp-jit-strategy.el' express multi-variant fall-through dispatch
;; in elisp:
;;
;;   - `cons' / `car' / `cdr' / `setcar' / `setcdr' wrappers — same
;;     bodies as `nelisp-jit-strategy.el' (`nl-jit-call-out-*' bridge
;;     calls).  Must run FIRST because the macros below construct
;;     expansion lists with `cons', so the function cell needs a real
;;     implementation before any macro body executes.  After this
;;     substrate file finishes, `nelisp-jit-strategy.el' re-installs
;;     identical wrappers (= no observable difference; the strategy
;;     file's cons wrapper section becomes redundant but harmless,
;;     left in place to keep that file self-documenting).
;;   - `defmacro' / `defun' — self-bootstrap, lifted from
;;     `nelisp-stdlib-eval-special.el' bootstrap pattern.
;;   - `cond' / `when' / `unless' — multi-arm dispatch macros built
;;     on `if' + `progn'.  Bodies spell `(eq x nil)' directly (no
;;     `null' yet at macro-expand time).
;;   - `null' / `not' — function cell `(eq x nil)' wrapper (later
;;     re-defined by `nelisp-stdlib.el').
;;   - `nelisp--signal-wrong-type' — `(signal 'wrong-type-argument
;;     (cons EXPECTED (cons GOT nil)))' helper.
;;   - `nelisp--signal-arith' — `(signal 'arith-error DATA)' helper.
;;   - `nelisp--type-tag' — `type-of' alias (= future swap point).
;;
;; Body constraints:
;; - Only Tier 1 special forms (`if', `quote', `function', `lambda',
;;   `let', `let*', `progn', `setq', `while', `condition-case',
;;   `unwind-protect', `catch', `throw') and Rust builtins (`fset',
;;   `eq', `signal', `type-of') + the cons/car/cdr/setcar/setcdr
;;   wrappers we install up top.

;;; Code:

;;;; --- 1. cons / car / cdr / setcar / setcdr wrappers ----
;;
;; Lifted from `nelisp-jit-strategy.el' lines 73-91.  Must precede
;; `defmacro' below because the macro expansions construct cons
;; chains.  These five `(fset 'NAME (lambda ...))' forms use only
;; Tier 1 special forms and the `nl-jit-call-out-*' bridge primitive,
;; both available without prior macro infrastructure.

(fset 'cons
      (lambda (a b)
        (nl-jit-call-out-2 "nelisp_jit_cons" a b)))

(fset 'car
      (lambda (x)
        (nl-jit-call-out-1 "nelisp_jit_car" x)))

(fset 'cdr
      (lambda (x)
        (nl-jit-call-out-1 "nelisp_jit_cdr" x)))

(fset 'setcar
      (lambda (cell val)
        (nl-jit-call-out-2 "nelisp_jit_setcar" cell val)))

(fset 'setcdr
      (lambda (cell val)
        (nl-jit-call-out-2 "nelisp_jit_setcdr" cell val)))

;;;; --- 2. defmacro self-bootstrap ----
;;
;; Same shape as `nelisp-stdlib-eval-special.el' lines 49-62.  Once
;; installed, `(defmacro X ...)' forms below dispatch through fcell →
;; `apply_combiner' macro path normally.

(fset 'defmacro
      (cons 'macro
            (cons (lambda (name args &rest body)
                    (let* ((lambda-form (cons 'lambda (cons args body)))
                           (qname (cons 'quote (cons name nil)))
                           (inner-cons (cons 'cons
                                             (cons lambda-form (cons nil nil))))
                           (outer-cons (cons 'cons
                                             (cons (cons 'quote (cons 'macro nil))
                                                   (cons inner-cons nil)))))
                      (cons 'progn
                            (cons (cons 'fset (cons qname (cons outer-cons nil)))
                                  (cons qname nil)))))
                  nil)))

;;;; --- 3. when / unless / cond ----

(defmacro when (test &rest body)
  (cons 'if (cons test (cons (cons 'progn body) (cons nil nil)))))

(defmacro unless (test &rest body)
  (cons 'if (cons test (cons nil (cons (cons 'progn body) nil)))))

(defmacro cond (&rest clauses)
  ;; Tier-1-only cond: `(eq clauses nil)' directly (no `null' yet at
  ;; macro-expand time during this very file's eval).
  (if (eq clauses nil)
      nil
    (let* ((clause (car clauses))
           (rest (cdr clauses))
           (test (car clause))
           (body (cdr clause)))
      (if (eq body nil)
          ;; (TEST) — yield TEST itself when truthy.
          (cons 'let
                (cons (cons (cons '--nl-cond-tmp (cons test nil)) nil)
                      (cons (cons 'if
                                  (cons '--nl-cond-tmp
                                        (cons '--nl-cond-tmp
                                              (cons (cons 'cond rest) nil))))
                            nil)))
        (cons 'if
              (cons test
                    (cons (cons 'progn body)
                          (cons (cons 'cond rest) nil))))))))

;;;; --- 4. defun (after defmacro) ----

(defmacro defun (name args &rest body)
  (let ((lambda-form (cons 'lambda (cons args body)))
        (qname (cons 'quote (cons name nil))))
    (cons 'progn
          (cons (cons 'fset (cons qname (cons lambda-form nil)))
                (cons qname nil)))))

;;;; --- 5. null / not ----

(defun null (x) (eq x nil))
(defun not (x) (eq x nil))

;;;; --- 6. signal helpers ----
;;
;; `signal' is a Rust builtin (= `eval/builtins.rs::bi_signal').  These
;; thin wrappers preserve the canonical sym/data shape used by the
;; pre-Doc-80 `strategy.rs' Rust `EvalError::WrongType' / `ArithError'
;; arms so the elisp-side replacements raise identical-shape errors.

(defun nelisp--signal-wrong-type (expected got)
  (signal 'wrong-type-argument (cons expected (cons got nil))))

(defun nelisp--signal-arith (data)
  (signal 'arith-error data))

;;;; --- 7. type-tag thin alias ----

(defun nelisp--type-tag (x) (type-of x))

;; (provide 'nelisp-jit-substrate) — omitted: this file is loaded
;; FIRST in the bootstrap chain, before `provide' is installed by
;; `nelisp-stdlib.el'.  Inclusion would error out at image-eval time.
;;; nelisp-jit-substrate.el ends here
