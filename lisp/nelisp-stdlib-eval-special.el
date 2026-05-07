;;; nelisp-stdlib-eval-special.el --- Tier 2 special forms as elisp macros  -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase 7 Stage 7.3.a (= Doc 67): Tier 2 special forms を elisp の
;; defmacro として実装する。Stage 7.3.a 時点では Rust 側の
;; `apply_special' match arm が先勝するため (= eval/mod.rs:191)、本 file
;; が install されても *runtime path* は Rust special form 直行のまま。
;; ERT (= `macroexpand-1' 経由) でのみ elisp macro 経路を踏む。
;; Stage 7.3.d で Rust 側 22 件削除と同 commit で elisp macros が active
;; 化する切替方式。
;;
;; Tier 1-only macro body 制約 (= Doc 67 §2.2): 本 file の各 defmacro の
;; body は Tier 1 primitive (= if / let / let* / setq / while / progn /
;; lambda / quote / cons / car / cdr / eq) + 既存 Rust builtin (= list
;; sentinel は elisp 側だが、`(cons X (cons Y nil))' で代替) のみ使用。
;; gensym 不在のため `--nl-' prefix の固定 symbol で衝突回避。
;;
;; Tier 2 22 forms のうち本 file は 18 件:
;;   cond / when / unless / and / or / prog1 / prog2 /
;;   save-excursion / save-restriction / setq-default /
;;   defvar-local / defcustom / defgroup / dolist / dotimes /
;;   push / pop / defvar / defconst
;; (= Stage 7.3.a)
;;
;; Stage 7.3.b:  defun / defmacro
;; Stage 7.3.c:  cl-defun + helper `nelisp--parse-cl-formals'

;;; Code:

;;;; --- bootstrap (Stage 7.3.d) ---------------------------------------

;; Stage 7.3.d removes the Rust `sf_defmacro' arm.  The first
;; `(defmacro X ...)' form below can no longer dispatch through
;; `apply_special' — we must install `defmacro' itself as an elisp
;; macro before any defmacro form runs.  Bootstrap shape:
;;
;;   (fset 'defmacro
;;         (cons 'macro
;;               (cons (lambda (name args &rest body) <SAME-AS-BELOW>) nil)))
;;
;; This uses only Tier 1 special forms (`lambda', `quote') and Rust
;; builtin functions (`fset', `cons') — no special forms beyond the
;; 13-form irreducible primitive set.  After this point all later
;; `(defmacro X ...)' forms (including the redundant
;; `(defmacro defmacro ...)' near the bottom of this file) dispatch
;; through fcell → `apply_combiner' macro path normally.

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

;;;; --- 単純 conditional / sequencing ----------------------------------

(defmacro when (cond &rest body)
  "If COND yields non-nil, eval BODY forms sequentially and return last value."
  (cons 'if (cons cond (cons (cons 'progn body) (cons nil nil)))))

(defmacro unless (cond &rest body)
  "If COND yields nil, eval BODY forms sequentially and return last value."
  (cons 'if (cons cond (cons nil (cons (cons 'progn body) nil)))))

(defmacro cond (&rest clauses)
  "Try each clause until one succeeds.
Each clause is `(TEST BODY...)'.  If TEST evaluates non-nil, BODY is
evaluated and its last value returned.  When BODY is empty the value
of TEST itself is returned."
  (if (null clauses)
      nil
    (let* ((clause (car clauses))
           (rest (cdr clauses))
           (test (car clause))
           (body (cdr clause)))
      (if (null body)
          ;; (TEST) → (or TEST (cond REST...))  ≈  (let ((--nl-cond-tmp TEST))
          ;;   (if --nl-cond-tmp --nl-cond-tmp (cond REST...)))
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

(defmacro and (&rest forms)
  "Eval FORMS left-to-right, short-circuiting on nil.  Empty form list = t."
  (if (null forms)
      t
    (if (null (cdr forms))
        (car forms)
      (cons 'if
            (cons (car forms)
                  (cons (cons 'and (cdr forms))
                        (cons nil nil)))))))

(defmacro or (&rest forms)
  "Eval FORMS left-to-right, returning the first non-nil value (or nil)."
  (if (null forms)
      nil
    (if (null (cdr forms))
        (car forms)
      ;; (let ((--nl-or-tmp FIRST)) (if --nl-or-tmp --nl-or-tmp (or REST...)))
      (cons 'let
            (cons (cons (cons '--nl-or-tmp (cons (car forms) nil)) nil)
                  (cons (cons 'if
                              (cons '--nl-or-tmp
                                    (cons '--nl-or-tmp
                                          (cons (cons 'or (cdr forms)) nil))))
                        nil))))))

(defmacro prog1 (first &rest rest)
  "Eval FIRST, then REST forms in order; return value of FIRST."
  ;; (let ((--nl-prog1-tmp FIRST)) REST... --nl-prog1-tmp)
  (cons 'let
        (cons (cons (cons '--nl-prog1-tmp (cons first nil)) nil)
              (append rest (cons '--nl-prog1-tmp nil)))))

(defmacro prog2 (form1 form2 &rest rest)
  "Eval FORM1, FORM2, then REST forms; return value of FORM2."
  (cons 'progn
        (cons form1
              (cons (cons 'prog1 (cons form2 rest))
                    nil))))

;;;; --- buffer-state stubs ---------------------------------------------

(defmacro save-excursion (&rest body)
  "Stub: NeLisp has no buffer concept, so this is just `progn'.
Reserved for future nemacs layer parity."
  (cons 'progn body))

(defmacro save-restriction (&rest body)
  "Stub: NeLisp has no narrowing, so this is just `progn'."
  (cons 'progn body))

;;;; --- setq alias / defvar family --------------------------------------

(defmacro setq-default (&rest pairs)
  "NeLisp has no buffer-local; equivalent to `setq'."
  (cons 'setq pairs))

(defmacro defvar (name &optional value _docstring)
  "Define NAME as a global variable, set initial VALUE if unbound.
DOCSTRING is currently ignored.  Returns NAME."
  ;; (progn (if (boundp 'NAME) nil (set 'NAME VALUE)) 'NAME)
  (cons 'progn
        (cons (cons 'if
                    (cons (cons 'boundp
                                (cons (cons 'quote (cons name nil)) nil))
                          (cons nil
                                (cons (cons 'set
                                            (cons (cons 'quote (cons name nil))
                                                  (cons value nil)))
                                      nil))))
              (cons (cons 'quote (cons name nil)) nil))))

(defmacro defvar-local (name &optional value docstring)
  "Alias for `defvar' (NeLisp lacks buffer-local distinction)."
  (cons 'defvar (cons name (cons value (cons docstring nil)))))

(defmacro defconst (name value &optional _docstring)
  "Define NAME as a constant whose initial value is VALUE.
DOCSTRING is currently ignored.  Returns NAME."
  ;; Stage 7.3.a: NeLisp has no constant-flag primitive accessible from
  ;; elisp; we simply set the value cell.  Stage 7.3.d will revisit
  ;; (= adding a `mark-constant' primitive or relaxing constness).
  (cons 'progn
        (cons (cons 'set
                    (cons (cons 'quote (cons name nil))
                          (cons value nil)))
              (cons (cons 'quote (cons name nil)) nil))))

(defmacro defcustom (name value docstring &rest _options)
  "Stub: ignore OPTIONS, behave like `defvar'."
  (cons 'defvar (cons name (cons value (cons docstring nil)))))

(defmacro defgroup (name _parent _docstring &rest _options)
  "No-op: NeLisp has no `customize' UI; returns NAME like host Emacs."
  (cons 'quote (cons name nil)))

;;;; --- iteration --------------------------------------------------------

(defmacro dolist (spec &rest body)
  "(dolist (VAR LIST [RESULT]) BODY...) — iterate VAR over LIST.
Bindings:  --nl-dolist-list = LIST cursor."
  (let* ((var (car spec))
         (list-form (car (cdr spec)))
         (result-form (car (cdr (cdr spec)))))
    ;; (let* ((--nl-dolist-list LIST-FORM) (VAR nil))
    ;;   (while --nl-dolist-list
    ;;     (setq VAR (car --nl-dolist-list))
    ;;     BODY...
    ;;     (setq --nl-dolist-list (cdr --nl-dolist-list)))
    ;;   RESULT-FORM)
    (cons 'let*
          (cons (cons (cons '--nl-dolist-list (cons list-form nil))
                      (cons (cons var (cons nil nil)) nil))
                (cons (cons 'while
                            (cons '--nl-dolist-list
                                  (cons (cons 'setq
                                              (cons var
                                                    (cons (cons 'car
                                                                (cons '--nl-dolist-list nil))
                                                          nil)))
                                        (append body
                                                (cons (cons 'setq
                                                            (cons '--nl-dolist-list
                                                                  (cons (cons 'cdr
                                                                              (cons '--nl-dolist-list nil))
                                                                        nil)))
                                                      nil)))))
                      (cons result-form nil))))))

(defmacro dotimes (spec &rest body)
  "(dotimes (VAR COUNT [RESULT]) BODY...) — iterate VAR from 0 to COUNT-1."
  (let* ((var (car spec))
         (count-form (car (cdr spec)))
         (result-form (car (cdr (cdr spec)))))
    ;; (let* ((--nl-dotimes-counter 0) (--nl-dotimes-limit COUNT) (VAR 0))
    ;;   (while (nelisp--num-lt2 --nl-dotimes-counter --nl-dotimes-limit)
    ;;     (setq VAR --nl-dotimes-counter)
    ;;     BODY...
    ;;     (setq --nl-dotimes-counter (nelisp--add2 --nl-dotimes-counter 1)))
    ;;   RESULT-FORM)
    (cons 'let*
          (cons (cons (cons '--nl-dotimes-counter (cons 0 nil))
                      (cons (cons '--nl-dotimes-limit (cons count-form nil))
                            (cons (cons var (cons 0 nil)) nil)))
                (cons (cons 'while
                            (cons (cons 'nelisp--num-lt2
                                        (cons '--nl-dotimes-counter
                                              (cons '--nl-dotimes-limit nil)))
                                  (cons (cons 'setq
                                              (cons var
                                                    (cons '--nl-dotimes-counter nil)))
                                        (append body
                                                (cons (cons 'setq
                                                            (cons '--nl-dotimes-counter
                                                                  (cons (cons 'nelisp--add2
                                                                              (cons '--nl-dotimes-counter
                                                                                    (cons 1 nil)))
                                                                        nil)))
                                                      nil)))))
                      (cons result-form nil))))))

;;;; --- cons-cell mutation macros --------------------------------------

(defmacro push (newelt place)
  "(setq PLACE (cons NEWELT PLACE))' — symbol-place fast path only.
Generalised places (= setf-style) are out of scope for Stage 7.3.a."
  (cons 'setq
        (cons place
              (cons (cons 'cons (cons newelt (cons place nil)))
                    nil))))

(defmacro pop (place)
  "(prog1 (car PLACE) (setq PLACE (cdr PLACE)))' — symbol-place only."
  (cons 'prog1
        (cons (cons 'car (cons place nil))
              (cons (cons 'setq
                          (cons place
                                (cons (cons 'cdr (cons place nil)) nil)))
                    nil))))

;;;; --- function / macro definition (Stage 7.3.b) ----------------------

(defmacro defun (name args &rest body)
  "(defun NAME ARGS BODY...) → (progn (fset 'NAME (lambda ARGS BODY...)) 'NAME).
Unlike Rust `sf_defun' which stores the raw `(lambda ...)' form
unmodified, the elisp expansion goes through evaluation of
`(lambda ARGS BODY...)' = produces a closure with the current lexical
env captured.  For top-level defun the captured env is empty so
semantics match Rust; defuns nested inside `let' would receive a
non-empty captured env in elisp but the bare form in Rust — this is
an intentional improvement, not a regression."
  (let ((lambda-form (cons 'lambda (cons args body)))
        (qname (cons 'quote (cons name nil))))
    (cons 'progn
          (cons (cons 'fset (cons qname (cons lambda-form nil)))
                (cons qname nil)))))

(defmacro defmacro (name args &rest body)
  "(defmacro NAME ARGS BODY...) →
   (progn (fset 'NAME (cons 'macro (cons (lambda ARGS BODY...) nil))) 'NAME).
The fcell shape for a macro is `(macro LAMBDA)' — a 2-element list,
matching `expand_macro' in eval/mod.rs which does `parts[1]' to grab
the lambda after stripping the `macro' tag.  As with `defun', the
embedded lambda evaluates to a closure (not a raw `(lambda ARGS ...)'
form), so `expand_macro' receives the closure and `apply_function'
dispatches via the `closure' arm."
  (let* ((lambda-form (cons 'lambda (cons args body)))
         (qname (cons 'quote (cons name nil)))
         ;; Inner cons cell: builds (LAMBDA-FORM nil) at evaluation time.
         (inner-cons (cons 'cons
                           (cons lambda-form (cons nil nil))))
         ;; Outer cons: builds (cons 'macro (LAMBDA-FORM nil)) at
         ;; evaluation time = (macro LAMBDA-FORM).
         (outer-cons (cons 'cons
                           (cons (cons 'quote (cons 'macro nil))
                                 (cons inner-cons nil)))))
    (cons 'progn
          (cons (cons 'fset (cons qname (cons outer-cons nil)))
                (cons qname nil)))))

;;;; --- cl-defun helper + macro (Stage 7.3.c) --------------------------

(defun nelisp--parse-cl-formals (formals)
  "Parse FORMALS list of a `cl-defun' form.
Returns a 4-element list (POSITIONAL OPTIONALS REST-OR-NIL KEYS) where
POSITIONAL / OPTIONALS are flat symbol lists, REST-OR-NIL is the
&rest var (or nil), and KEYS is a list of (KW PARAM DEFAULT) triples
— one per &key entry, with KW the leading-colon keyword interned from
PARAM's name.  &aux entries are silently dropped to match Rust
`sf_cl_defun' (build-tool/src/eval/special_forms.rs:389)."
  (let ((mode 'pos)
        (positional nil)
        (optionals nil)
        (rest-sym nil)
        (keys nil)
        (cursor formals))
    (while cursor
      (let ((f (car cursor)))
        (if (eq f '&optional)
            (setq mode 'opt)
          (if (eq f '&rest)
              (setq mode 'rest)
            (if (eq f '&key)
                (setq mode 'key)
              (if (eq f '&aux)
                  (setq mode 'aux)
                (if (eq mode 'pos)
                    (setq positional (cons f positional))
                  (if (eq mode 'opt)
                      (setq optionals (cons f optionals))
                    (if (eq mode 'rest)
                        (if (null rest-sym) (setq rest-sym f))
                      (if (eq mode 'key)
                          (let (param default kw)
                            (if (consp f)
                                (progn
                                  (setq param (car f))
                                  (setq default (car (cdr f))))
                              (setq param f)
                              (setq default nil))
                            (setq kw (intern (concat ":" (symbol-name param))))
                            (setq keys (cons (list kw param default) keys)))))))))))
        (setq cursor (cdr cursor))))
    (list (nreverse positional)
          (nreverse optionals)
          rest-sym
          (nreverse keys))))

(defmacro cl-defun (name formals &rest body)
  "Common-Lisp style `defun' supporting &key arguments.
With no &key entry, expands to a plain `defun'.  Otherwise rewrites
FORMALS into (POS... [&optional O...] &rest R) and wraps BODY in a
`let*' that lifts each key from R via
`(or (car (cdr (memq ':KW R))) DEFAULT)' — semantically identical to
Rust `sf_cl_defun' (build-tool/src/eval/special_forms.rs:389)."
  (let* ((parsed (nelisp--parse-cl-formals formals))
         (positional (car parsed))
         (optionals (car (cdr parsed)))
         (rest-sym (car (cdr (cdr parsed))))
         (keys (car (cdr (cdr (cdr parsed))))))
    (if (null keys)
        ;; No &key: pass through to plain defun.
        (cons 'defun (cons name (cons formals body)))
      ;; &key present: rewrite formals + wrap body.
      (let* ((rest-name (or rest-sym '--cl-keys))
             (new-formals
              (append positional
                      (if optionals (cons '&optional optionals) nil)
                      (cons '&rest (cons rest-name nil))))
             (bindings
              (mapcar
               (lambda (k)
                 ;; k = (KW PARAM DEFAULT)
                 (let ((kw (car k))
                       (param (car (cdr k)))
                       (default (car (cdr (cdr k)))))
                   (list param
                         (list 'or
                               (list 'car
                                     (list 'cdr
                                           (list 'memq
                                                 (list 'quote kw)
                                                 rest-name)))
                               default))))
               keys))
             (let-form (cons 'let* (cons bindings body))))
        (cons 'defun (cons name (cons new-formals (cons let-form nil))))))))

;; (provide 'nelisp-stdlib-eval-special) is intentionally omitted:
;; Layer A loads BEFORE `nelisp-stdlib*.el' where `provide' itself is
;; defined.  Callers should not `(require 'nelisp-stdlib-eval-special)'.

;;; nelisp-stdlib-eval-special.el ends here
