;;; nelisp-stdlib-eval-core.el --- apply / closure / lambda elisp impl  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7 Stage 7.4.b (= Doc 68): Rust =eval/mod.rs= の =apply_function=
;; / =apply_closure= / =apply_lambda= / =apply_lambda_inner= / =bind_formals=
;; / =is_macro= / =expand_macro= 8 helper を elisp に移植する.  Stage
;; 7.4.b 時点では Rust 側 =apply_function= が runtime path を握って
;; いるので本 file の defun は *dormant* (= ERT 経由でのみ叩かれる).
;; Stage 7.4.c で `--use-elisp-apply' flag 経路が入り、Stage 7.4.d で
;; default が flip して active 化する.
;;
;; Tier 1-only body 制約 + frame-stack 整合性制約: 本 file の各 defun の
;; body は Tier 1 special form (= if / let / let* / setq / while /
;; progn / lambda / quote / function / catch / throw / condition-case /
;; unwind-protect) + Stage 7.3 SHIPPED 済 Tier 2 macro (= cond / when /
;; unless / and / or / dolist / dotimes / push / pop / prog1 / prog2 /
;; defun / defvar) + Rust builtin + Stage 7.4.a primitive のみ.
;; cl-loop / seq-* / mapcar 等は使わない (= bootstrap cycle).
;;
;; *Frame stack 整合性*: =apply_function= for elisp lambda は call-side で
;; 必ず frame を push する (= elisp 標準 semantics).  そのため、
;; =nelisp--bind-local NAME VALUE= を defun の body から呼ぶと、その
;; defun 自身の call-frame に bind されて return 時に消える.  本 file は
;; この罠を回避するため:
;;   (1) 状態走査 + arity 検査 を *pure* helper に隔離 (= bindings の
;;       list を返すだけ、frame stack は触らない)
;;   (2) 実際の =bind-local= 呼び出しは =apply-lambda-inner= の **直接の
;;       body** で行う (= push-frame で立てた F_formals が topmost、
;;       defun 余分 frame が間に挟まらない)
;;   (3) =apply-lambda-inner= body 内では =let= / =or= / =and= 等
;;       「展開時に =let ((--nl-...))= を生む macro」を絶対に使わない
;;       (= =setq= / =if= / =progn= / =while= / 直接 =cons= で書く)
;;
;; この制約は =bind-formals--compute= 等の *pure helper* には適用しない
;; (= helper の return 値だけ使う、helper 自身の frame は破棄される).

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
;; Mirrors Rust eval/mod.rs `apply_lambda_inner' (lines 368-411).
;; Pushes the captured-env frame + a fresh formals frame, runs the
;; binding loop INLINE (= no separate defun call between push-frame
;; and bind-local), evals BODY in order returning the last value,
;; then pops both frames.  `unwind-protect' guarantees pop on signal.
;;
;; The body of this defun MUST NOT use any frame-pushing macro
;; between the `(nelisp--push-frame)' and the bind-local loop —
;; specifically no `let' / `let*' / `or' / `and' / `cond' with empty
;; body / etc.  We use only `setq', `if', `progn', `while', plain
;; cons/car/cdr access, and direct builtin calls.  See file
;; commentary for the rationale.

(defun nelisp--apply-lambda-inner (captured formals body args)
  "Apply (lambda FORMALS BODY...) with CAPTURED env to ARGS.
Push CAPTURED (an alist of `((NAME . CELL) ...)') as a lexical frame,
push another frame for FORMALS, walk a pre-computed pair list to
install each formal binding in the just-pushed frame, eval BODY in
order, return the last value.  Both frames are popped on normal and
error exit."
  (nelisp--push-captured captured)
  (nelisp--push-frame)
  (unwind-protect
      (progn
        ;; Compute pairs via a pure helper.  The helper has its own
        ;; transient call-frame, but only its return value (= the
        ;; pairs list) escapes.  Reuse the `captured' slot to hold
        ;; the pairs list — avoids a `let' which would push F_let on
        ;; top of F_formals and re-target bind-local.
        (setq captured (nelisp--bind-formals--compute formals args))
        ;; Walk pairs.  No `let' / no nested defun call between here
        ;; and bind-local — F_formals stays topmost, bindings go
        ;; into the right place.
        (while (consp captured)
          (nelisp--bind-local (car (car captured)) (cdr (car captured)))
          (setq captured (cdr captured)))
        ;; Eval body.  Reuse `formals' slot for `last result' since
        ;; we no longer need the original FORMALS value.
        (setq formals nil)
        (while (consp body)
          (setq formals (eval (car body)))
          (setq body (cdr body)))
        formals)
    (progn
      (nelisp--pop-frame)
      (nelisp--pop-frame))))

(defun nelisp--apply-closure (closure args)
  "Apply CLOSURE = `(closure CAPTURED FORMALS BODY...)' to ARGS.
Walk the cons head with car/cadr/cddr to extract the three slots,
delegate to `nelisp--apply-lambda-inner'."
  (let* ((parts (cdr closure))
         (captured (car parts))
         (formals (cadr parts))
         (body (cddr parts)))
    (nelisp--apply-lambda-inner captured formals body args)))

(defun nelisp--apply-lambda (lambda-form args)
  "Apply a bare `(lambda FORMALS BODY...)' to ARGS.
Equivalent to a closure with empty CAPTURED env.  This is the shape
that defun stores in the function cell directly."
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
caller should `nelisp--expand-macro' first and re-eval the result."
  (cond
   ((nelisp--builtinp fn)
    (nelisp--apply-builtin-dispatch (cadr fn) args))
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
