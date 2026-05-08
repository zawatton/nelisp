;;; nelisp-stdlib-eval-core.el --- apply / closure / lambda elisp impl  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7 Stage 7.4.b (= Doc 68): Rust =eval/mod.rs= の =apply_function=
;; / =apply_closure= / =apply_lambda= / =bind_formals= / =is_macro= /
;; =expand_macro= helper を elisp に移植する.  Stage 7.4.b 時点では
;; Rust 側 =apply_function= が runtime path を握っているので本 file の
;; defun は *dormant* (= ERT 経由でのみ叩かれる).  Stage 7.4.c で
;; =--use-elisp-apply= flag 経路が入り、Stage 7.4.e (Doc 70) で
;; =apply-lambda-inner= だけは frame-capture leak 修正のため Rust
;; builtin に降ろされた (= 本 file には残らず、Rust =bi_apply_lambda_inner=
;; が同名で expose).
;;
;; Tier 1-only body 制約: 本 file の各 defun の body は Tier 1 special
;; form (= if / let / let* / setq / while / progn / lambda / quote /
;; function / catch / throw / condition-case / unwind-protect) + Stage
;; 7.3 SHIPPED 済 Tier 2 macro (= cond / when / unless / and / or /
;; dolist / dotimes / push / pop / prog1 / prog2 / defun / defvar) +
;; Rust builtin + Stage 7.4.a/e primitive のみ.  cl-loop / seq-* /
;; mapcar 等は使わない (= bootstrap cycle).

;;; Code:

;; ---- predicates -----------------------------------------------------

(defun nelisp--builtinp (fn)
  "T if FN is a `(builtin NAME)' sentinel cons.
Mirrors the Rust apply_function dispatch arm for builtins."
  (and (consp fn) (eq (car fn) 'builtin)))

(defun nelisp--closurep (fn)
  "T if FN is a `(closure CAPTURED-ENV ARGS BODY...)' shape.
Mirrors the Rust apply_function dispatch arm for closures."
  (and (consp fn) (eq (car fn) 'closure)))

(defun nelisp--lambdap (fn)
  "T if FN is a bare `(lambda ARGS BODY...)' shape (= what defun stores).
Mirrors the Rust apply_function dispatch arm for raw lambdas."
  (and (consp fn) (eq (car fn) 'lambda)))

(defun nelisp--macrop (fn)
  "T if FN is a `(macro . LAMBDA)' shape.
Mirrors Rust eval/mod.rs is_macro: a function-cell carrying a
macro must be expanded, not called as a function."
  (and (consp fn) (eq (car fn) 'macro)))

;; ---- formal-parameter binding (compute-only helpers) ---------------
;;
;; These are *pure* — they do not touch the frame stack.  The actual
;; binding happens inline inside `nelisp--apply-lambda-inner' so that
;; `nelisp--bind-local' targets F_formals (= the frame
;; apply-lambda-inner pushed) and not the helper's own call-frame.

(defun nelisp--bind-formals--required-count (formals)
  "Count required (= pre-marker) symbols in FORMALS.
Used for arity diagnostics on `wrong-number-of-arguments' signals.
Pure: walks FORMALS, returns int."
  (let ((p formals)
        (n 0))
    (while (and (consp p)
                (not (memq (car p) '(&optional &rest))))
      (setq n (1+ n))
      (setq p (cdr p)))
    n))

(defun nelisp--bind-formals--compute (formals args)
  "Compute the formal-parameter bindings as a list of (NAME . VALUE) pairs.
Mirrors Rust eval/mod.rs bind_formals (lines 413-518) semantics:
required → must consume, &optional → default nil if exhausted,
&rest → collect tail.  Signals on: arity mismatch, multiple `&rest',
formal symbol after `&rest', non-symbol formal.

Returns the bindings as a plain list; the caller is responsible for
walking the result and installing each pair via `nelisp--bind-local'
in a context where the topmost frame is the intended target."
  (let ((mode 'required)
        (idx 0)
        (saw-rest nil)
        (consumed-rest nil)
        (required-count (nelisp--bind-formals--required-count formals))
        (n-args (length args))
        (rest-formals formals)
        (pairs nil))
    (while (consp rest-formals)
      (let ((name (car rest-formals)))
        (cond
         ((eq name '&optional)
          (when (eq mode 'rest)
            (signal 'wrong-type-argument
                    (list 'formal-after-rest name)))
          (setq mode 'optional))
         ((eq name '&rest)
          (when saw-rest
            (signal 'wrong-type-argument
                    (list 'multiple-rest name)))
          (setq mode 'rest)
          (setq saw-rest t))
         ((symbolp name)
          (cond
           ((eq mode 'required)
            (when (>= idx n-args)
              (signal 'wrong-number-of-arguments
                      (list 'lambda required-count n-args)))
            (setq pairs (cons (cons name (nth idx args)) pairs))
            (setq idx (1+ idx)))
           ((eq mode 'optional)
            (let ((v (if (< idx n-args)
                         (prog1 (nth idx args)
                           (setq idx (1+ idx)))
                       nil)))
              (setq pairs (cons (cons name v) pairs))))
           ((eq mode 'rest)
            (when consumed-rest
              (signal 'wrong-type-argument
                      (list 'symbol-after-rest name)))
            (let ((tail (nthcdr idx args)))
              (setq pairs (cons (cons name tail) pairs))
              (setq idx n-args)
              (setq consumed-rest t)))))
         (t (signal 'wrong-type-argument (list 'symbol name)))))
      (setq rest-formals (cdr rest-formals)))
    (when (and (< idx n-args) (not consumed-rest))
      (signal 'wrong-number-of-arguments
              (list 'lambda idx n-args)))
    (when (and saw-rest (not consumed-rest))
      (signal 'wrong-type-argument
              (list 'symbol-after-rest '&rest)))
    (nreverse pairs)))

;; ---- closure / lambda body apply -----------------------------------
;;
;; `nelisp--apply-lambda-inner' is registered as a Rust builtin
;; (build-tool/src/eval/builtins.rs `bi_apply_lambda_inner').  It
;; mirrors `apply_lambda_inner' (eval/mod.rs) and was demoted from an
;; elisp defun in Stage 7.4.e (Doc 70) to fix the Stage 7.4.d
;; frame-capture leak: when the defun ran user code in its body, its
;; `--nl-ali-*' formal slots leaked into closures that `defun'
;; snapshotted, corrupting the dispatcher's state on later re-entry.
;; Keeping the helper's state on the Rust call stack closes that
;; surface — `apply-closure' / `apply-lambda' below call the builtin
;; by name (= same `(nelisp--apply-lambda-inner CAPTURED FORMALS
;; BODY ARGS)' shape as the previous defun).

(defun nelisp--apply-closure (closure args)
  "Apply CLOSURE = `(closure CAPTURED FORMALS BODY...)' to ARGS.
Walks the cons head with explicit car/cdr nesting (= NOT cadr/cddr)
because the latter are themselves elisp defuns and would re-trigger
delegation through `nelisp--apply-fn' when the takeover flag is on,
causing an infinite recursion loop while extracting the very slots
the dispatcher needs to apply *itself*."
  (let* ((parts (cdr closure))                       ; (CAPTURED FORMALS BODY...)
         (rest1 (cdr parts))                         ; (FORMALS BODY...)
         (captured (car parts))
         (formals (car rest1))
         (body (cdr rest1)))
    (nelisp--apply-lambda-inner captured formals body args)))

(defun nelisp--apply-lambda (lambda-form args)
  "Apply a bare `(lambda FORMALS BODY...)' to ARGS.
Equivalent to a closure with empty CAPTURED env.  This is the shape
that defun stores in the function cell directly.  Uses only car/cdr
(= same Rust-builtin-only constraint as `nelisp--apply-closure')."
  (let* ((parts (cdr lambda-form))
         (formals (car parts))
         (body (cdr parts)))
    (nelisp--apply-lambda-inner nil formals body args)))

;; ---- top-level dispatch --------------------------------------------
;;
;; Mirrors Rust eval/mod.rs `apply_function' (lines 277-302).  4-way
;; cond on the function value's cons head: builtin / closure / lambda
;; / macro.  Macro at this entry point is an error (= functions and
;; macros are distinct call paths in elisp; macro must be expanded
;; before its body is run).

(defun nelisp--apply-fn (fn args)
  "Dispatch FN to ARGS; mirror Rust eval/mod.rs apply_function.
FN is one of: `(builtin NAME)' / `(closure ENV ARGS BODY...)' /
`(lambda ARGS BODY...)' / `(macro . LAMBDA)'.  ARGS is a list of
already-evaluated values.  Macro shape signals an error — the
caller should `nelisp--expand-macro' first and re-eval the result.

NOTE: extracts the builtin NAME with `(car (cdr fn))' rather than
`(cadr fn)' because cadr is itself an elisp defun and would re-trigger
delegation through this same dispatcher when the takeover flag is on."
  (cond
   ((nelisp--builtinp fn)
    (nelisp--apply-builtin-dispatch (car (cdr fn)) args))
   ((nelisp--closurep fn)
    (nelisp--apply-closure fn args))
   ((nelisp--lambdap fn)
    (nelisp--apply-lambda fn args))
   ((nelisp--macrop fn)
    (signal 'wrong-type-argument
            (list 'function-not-macro fn)))
   (t
    (signal 'wrong-type-argument (list 'function fn)))))

(defun nelisp--expand-macro (macro-form arg-forms)
  "Expand `(macro . LAMBDA)' shape MACRO-FORM by calling its LAMBDA on
ARG-FORMS un-evaluated.  ARG-FORMS is a list of *raw* argument forms
(= macro semantics, args are not evaluated before the call).  Returns
the expansion form; the caller is expected to re-eval it."
  (let ((inner (cdr macro-form)))
    (cond
     ((nelisp--lambdap inner)
      (nelisp--apply-lambda inner arg-forms))
     ((nelisp--closurep inner)
      (nelisp--apply-closure inner arg-forms))
     (t
      (signal 'wrong-type-argument
              (list 'malformed-macro macro-form))))))

(provide 'nelisp-stdlib-eval-core)

;;; nelisp-stdlib-eval-core.el ends here
