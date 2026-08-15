;;; nelisp-pcase.el --- pcase macro elisp implementation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust-min: pcase の Elisp 実装 (= Rust special form 削除に伴う migrate)。
;;
;; 対応 pattern shape:
;;   _, pcase--dontcare ワイルドカード / t, nil 真偽リテラル
;;   :keyword           keyword 自己評価リテラル (eq 比較)
;;   integer / string   数値・文字列リテラル (equal 比較)
;;   symbol             変数 binding (常に match)
;;   (quote DATUM)      literal 等価 (`equal' 比較 -- symbol/number/string/
;;                      list/vector 問わず構造比較。`eq' だと quoted list
;;                      等の compound datum が freshly-consed な runtime
;;                      値と一致しない)
;;   (cons P1 P2)       cons cell 分解
;;   (or P1 P2 ...)     どれか match
;;   (and P1 P2 ...)    全部 match
;;   (pred FN)          (FN value) → 非 nil
;;   (guard EXPR)       EXPR → 非 nil
;;   (let PAT EXPR)     PAT を EXPR に対し test
;;   `(...)             backquote pattern (cons 分解 + ,SYM binding)
;;
;; pcase 本体は (let ((--v-- EXPR)) (cond (TEST1 BODY1) ...)) に展開。

;;; Code:

(defvar nelisp-pcase--outer-bindings nil
  "Bindings hoisted out of `pcase' clauses during macro expansion.")

;; Backquote-family head symbols come in TWO spellings depending on who
;; read the source: the NeLisp reader's own macro produces
;; `backquote'/`comma'/`comma-at', while sources printed by host Emacs
;; and re-read here carry the raw punctuation symbols.  The punctuation
;; symbols are built with `intern' because writing their literal
;; backslash-escape syntax in THIS file would trip the very reader gap
;; (old `\\,'-style escapes) that keeps vendored pcase.el unloadable.
;; Before the unknown-pattern signal existed, the punctuation spelling
;; fell into the permissive catch-all and silently matched EVERYTHING
;; (surfaced by the loud error on magit bundle part 16's `(t ,_)').
(defconst nelisp-pcase--backquote-heads (list 'backquote (intern "`")))
(defconst nelisp-pcase--comma-heads (list 'comma (intern ",")))
(defconst nelisp-pcase--comma-at-heads (list 'comma-at (intern ",@")))

(defun nelisp-pcase--wildcard-p (pattern)
  "Return non-nil when PATTERN is a wildcard with no binding."
  (or (eq pattern '_) (eq pattern 'pcase--dontcare)))

(defun nelisp-pcase--test (pattern value-form)
  "Build (TEST-FORM . BINDINGS) for matching PATTERN against VALUE-FORM."
  (cond
   ((nelisp-pcase--wildcard-p pattern) (cons t nil))
   ((keywordp pattern)
    (cons (list 'eq value-form pattern) nil))
   ((or (null pattern) (eq pattern t))
    (cons (list 'eq value-form (list 'quote pattern)) nil))
   ((symbolp pattern)
    (cons t (list (list pattern value-form))))
   ((or (integerp pattern) (stringp pattern))
    (cons (list 'equal value-form pattern) nil))
   ((consp pattern)
    (let ((head (car pattern))
          (rest (cdr pattern)))
      (cond
       ((eq head 'quote)
        ;; `equal', not `eq': a `(quote DATUM)' pattern (e.g. the
        ;; literal-list clause selector `'(t t)' in vendor cond-let.el's
        ;; `cond-let--prepare-clauses') must match any value that is
        ;; STRUCTURALLY the same, not merely the same object.  `eq'
        ;; happens to work for the common case of a quoted symbol
        ;; (interned, so `eq'-comparable) but silently never matches a
        ;; quoted compound datum (list/vector/string) compared against a
        ;; freshly-consed runtime value of the same shape -- `(eq (list
        ;; t t) '(t t))' is nil in both this reader and real Emacs.  That
        ;; silent non-match let a later, structurally-overlapping
        ;; backquote-pattern clause (e.g. `` `(t ,_) '') win instead,
        ;; selecting the wrong helper macro out of a `pcase' dispatch
        ;; that assumed exact-match precedence -- root cause of the
        ;; nelisp-emacs-lib Doc 33 item 239 `cond-let*' repro
        ;; `(cond-let* ([x 1] [x (+ x 1)] x) (t 99))' => `void-variable:
        ;; x' (the wrongly-selected non-sequential `cond-let--when-let'
        ;; expands a `(+ x 1)' binding form that runs before `x' is
        ;; bound; the correctly-selected `cond-let--when-let*' does not).
        (cons (list 'equal value-form (list 'quote (car rest))) nil))
       ((eq head 'pred)
        (let ((fn (car rest)))
          (cons (list 'funcall (list 'function fn) value-form) nil)))
       ((eq head 'guard)
        (cons (car rest) nil))
       ((eq head 'let)
        (let* ((sub-pat (car rest))
               (sub-expr (car (cdr rest)))
               (built (nelisp-pcase--test sub-pat sub-expr)))
          (cons (car built) (cdr built))))
       ((eq head 'and)
        (nelisp-pcase--and rest value-form))
       ((eq head 'or)
        (nelisp-pcase--or rest value-form))
       ((eq head 'cons)
        (nelisp-pcase--cons rest value-form))
       ((memq head nelisp-pcase--backquote-heads)
        (nelisp-pcase--backquote (car rest) value-form))
       ;; (app FUN PAT) -- match PAT against (FUN value).  FUN is a
       ;; function symbol/lambda (value becomes its sole argument) or a
       ;; call form (F ARG...) where a literal `_' marks value's
       ;; position (else value is appended last, mirroring real pcase).
       ;; Mirrors the bridge polyfill's support (nelisp-emacs-lib Doc 33
       ;; item 243: EIEIO's `pcase-defmacro eieio' expands through
       ;; `app'; without it the pattern fell into the old permissive
       ;; catch-all, which binds nothing).
       ((eq head 'app)
        (let* ((fun (car rest))
               (sub-pat (car (cdr rest)))
               (call-form
                (cond
                 ((and (consp fun)
                       (not (memq (car fun) (list 'lambda 'function 'closure))))
                  (if (memq '_ fun)
                      (mapcar (lambda (x) (if (eq x '_) value-form x)) fun)
                    (append fun (list value-form))))
                 (t (list 'funcall (list 'function fun) value-form)))))
          (nelisp-pcase--test sub-pat call-form)))
       ;; (CUSTOM ...) registered via a `pcase-macroexpander' symbol
       ;; property (real pcase's `pcase-defmacro' mechanism; the bridge
       ;; polyfill's `pcase-defmacro' also registers through this
       ;; property).  Expand and recurse.
       ((and (symbolp head) (get head 'pcase-macroexpander))
        (nelisp-pcase--test (apply (get head 'pcase-macroexpander) rest)
                            value-form))
       ;; Unknown pattern head: SIGNAL, exactly like real pcase.  The
       ;; old permissive catch-all (match everything, bind nothing) was
       ;; the silent-wrong-answer shape: it hid the missing `app'
       ;; support and the unregistered `cl-type' expander -- every
       ;; value fell into transient's FIRST `(cl-type keyword)' branch,
       ;; killing the magit bundle part 16 with "Need command ... got
       ;; `nil'" (nelisp-emacs-lib Doc 33 SS9 D1, 2026-08-15).  A
       ;; missing pattern must name itself here, not corrupt match
       ;; results downstream.
       (t (error "Unknown pcase pattern `%S'" pattern)))))
   (t (cons (list 'equal value-form (list 'quote pattern)) nil))))

;; `cl-type' pattern (real Emacs registers it in cl-macs.el, which the
;; standalone substrate does not load).  `cl-typep' itself is correct
;; here, so registration is all that was missing.  Built with explicit
;; `list' calls like the rest of this file (no backquote).
(unless (get 'cl-type 'pcase-macroexpander)
  (defun nelisp-pcase--cl-type-expander (type)
    "Expand (cl-type TYPE) to a `cl-typep' pred (Doc 33 SS9 D1)."
    (list 'pred
          (list 'lambda (list 'v)
                (list 'cl-typep 'v (list 'quote type)))))
  (put 'cl-type 'pcase-macroexpander 'nelisp-pcase--cl-type-expander))

(defun nelisp-pcase--and (patterns value-form)
  "Build (TEST . BINDINGS) for an `and' pattern."
  (let ((tests nil)
        (bindings nil)
        (cur patterns))
    (while cur
      (let* ((built (nelisp-pcase--test (car cur) value-form))
             (t1 (car built))
             (b1 (cdr built)))
        (setq tests (cons t1 tests))
        (setq bindings (append bindings b1)))
      (setq cur (cdr cur)))
    (cons (cons 'and (let ((rev nil))
                       (while tests
                         (setq rev (cons (car tests) rev))
                         (setq tests (cdr tests)))
                       rev))
          bindings)))

(defun nelisp-pcase--or (patterns value-form)
  "Build (TEST . BINDINGS) for an `or' pattern.
The selected alternative is tracked with a fresh choice symbol hoisted into
the outer `pcase' let via `nelisp-pcase--outer-bindings'."
  (let ((choice (make-symbol "--pcase-choice--"))
        (idx 0)
        (tests nil)
        (alt-bindings nil)
        (vars nil)
        (bindings nil))
    (push (list choice nil) nelisp-pcase--outer-bindings)
    (while patterns
      (let* ((built (nelisp-pcase--test (car patterns) value-form))
             (test (car built))
             (bindings-for-alt (cdr built)))
        (push (list 'and test (list 'setq choice idx)) tests)
        (push bindings-for-alt alt-bindings)
        (dolist (binding bindings-for-alt)
          (unless (assq (car binding) vars)
            (push (cons (car binding) nil) vars))))
      (setq idx (1+ idx))
      (setq patterns (cdr patterns)))
    (setq tests (nreverse tests))
    (setq alt-bindings (nreverse alt-bindings))
    (setq vars (nreverse vars))
    (dolist (var-entry vars)
      (let ((var (car var-entry))
            (clauses nil)
            (alt 0)
            (cur alt-bindings))
        (while cur
          (let ((binding (assq var (car cur))))
            (when binding
              (push (list (list 'eq choice alt) (cadr binding)) clauses)))
          (setq alt (1+ alt))
          (setq cur (cdr cur)))
        (push (list var (cons 'cond (nreverse clauses))) bindings)))
    (cons (cons 'or tests) (nreverse bindings))))

(defun nelisp-pcase--cons (rest value-form)
  "Build (TEST . BINDINGS) for a `(cons P1 P2)' pattern."
  (let* ((p1 (car rest))
         (p2 (car (cdr rest)))
         (b1 (nelisp-pcase--test p1 (list 'car value-form)))
         (b2 (nelisp-pcase--test p2 (list 'cdr value-form))))
    (cons (list 'and
                (list 'consp value-form)
                (car b1)
                (car b2))
          (append (cdr b1) (cdr b2)))))

(defun nelisp-pcase--backquote (pat value-form)
  "Build (TEST . BINDINGS) for a backquote pattern."
  (cond
   ((and (consp pat) (memq (car pat) nelisp-pcase--comma-heads))
    (let ((sym (car (cdr pat))))
      (cond
       ((nelisp-pcase--wildcard-p sym) (cons t nil))
       ((symbolp sym) (cons t (list (list sym value-form))))
       (t (nelisp-pcase--test sym value-form)))))
   ((and (consp pat) (memq (car pat) nelisp-pcase--comma-at-heads))
    (let ((sym (car (cdr pat))))
      (cons t (list (list sym value-form)))))
   ((consp pat)
    (let* ((head-build (nelisp-pcase--backquote
                        (car pat) (list 'car value-form)))
           (tail-build (nelisp-pcase--backquote
                        (cdr pat) (list 'cdr value-form))))
      (cons (list 'and
                  (list 'consp value-form)
                  (car head-build)
                  (car tail-build))
            (append (cdr head-build) (cdr tail-build)))))
   ((null pat)
    (cons (list 'null value-form) nil))
   (t
    (cons (list 'equal value-form (list 'quote pat)) nil))))

(defmacro pcase (expr &rest cases)
  "Dispatch EXPR through CASES.
See `nelisp-pcase--test' for supported pattern shapes.

Rust-min migration (= moved out of build-tool/src/eval/special_forms.rs)."
  (let ((value-sym (make-symbol "--pcase-value--"))
        (cond-clauses nil)
        (nelisp-pcase--outer-bindings nil))
    (dolist (case cases)
      (let* ((pat (car case))
             (body (cdr case))
             (built (nelisp-pcase--test pat value-sym))
             (test (car built))
             (bindings (cdr built)))
        (push (list test
                    (if bindings
                        (cons 'let (cons bindings body))
                      (cons 'progn body)))
              cond-clauses)))
    (let ((forward nil)
          (outer-bindings (nreverse nelisp-pcase--outer-bindings)))
      (while cond-clauses
        (setq forward (cons (car cond-clauses) forward))
        (setq cond-clauses (cdr cond-clauses)))
      (list 'let (append (list (list value-sym expr)) outer-bindings)
            (cons 'cond forward)))))

;; nelisp-pcase.el ends here
