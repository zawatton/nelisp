;;; nelisp-pcase.el --- pcase macro elisp implementation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust-min: pcase の Elisp 実装 (= Rust special form 削除に伴う migrate)。
;;
;; 対応 pattern shape:
;;   _, t, nil          ワイルドカード / 真偽リテラル
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

;; Kept in step with the copy in scripts/nelisp-stdlib-prelude.el, which is
;; the one baked into the standalone.  `make ns-gate' reports any drift as
;; an ns-collision-divergent, and it did the moment only the prelude was
;; fixed.
(defun nelisp-pcase--test (pattern value-form)
  "Build (TEST-FORM . BINDINGS) for matching PATTERN against VALUE-FORM."
  (cond
   ((eq pattern '_) (cons t nil))
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
       ;; The reader gives a backquote pattern the head `\=`', not
       ;; `backquote', so testing only for the latter meant NO backquote
       ;; pattern was ever recognised -- and the fallback below matched
       ;; everything, so the first backquote clause in any `pcase' won
       ;; regardless of the value.  There are 47 of them in this tree.
       ;; The always-match fallback hid it completely; making that fallback
       ;; signal is what brought it out.
       ((if (eq head 'backquote) 1 (eq head '\`))
        (nelisp-pcase--backquote (car rest) value-form))
       ((eq head 'app)
        ;; (app FUN PAT): apply FUN to the value, match PAT on the result.
        (nelisp-pcase--test (car (cdr rest))
                            (list 'funcall (list 'function (car rest))
                                  value-form)))
       ;; An unrecognised pattern head used to build the test `t', so it
       ;; matched EVERYTHING: `(pcase 5 ((app 1+ 7) (quote seven))
       ;; (_ (quote other)))' answered seven where Emacs answers other, and
       ;; a made-up head matched just as happily.  A dispatch construct
       ;; quietly taking the wrong branch is about the worst failure a
       ;; dispatch construct has, and the branch body can do anything.
       ;;
       ;; Signalling instead, which is what Emacs does for a head it does
       ;; not know.  For a head Emacs DOES know and this does not, Emacs
       ;; would evaluate it and this stops -- a deviation, but a named and
       ;; loud one that says which pattern is missing, rather than a silent
       ;; wrong answer.  Measured before changing it: across scripts/,
       ;; lisp/ and src/ the only pattern heads in use are quote (245),
       ;; backquote (47) and or (32), all handled, so nothing in the tree
       ;; relies on the old always-match.
       ;; (cl-type TYPE) -- built into the engine rather than registered
       ;; through the `pcase-macroexpander' property, because this file is
       ;; spliced into the stdlib prelude and loads BEFORE `get'/`put'
       ;; exist; a load-time registration form dies with void-function: get.
       ;; Real Emacs registers this in cl-macs.el.  `cl-typep' itself is
       ;; already correct on this substrate -- (cl-typep 5 'integer) and
       ;; (cl-typep "s" 'integer) answer t and nil here exactly as they do
       ;; in stock Emacs -- so the pattern lowers straight to a `pred'.
       ((eq head 'cl-type)
        (nelisp-pcase--test
         (list 'pred (list 'lambda (list 'v)
                           (list 'cl-typep 'v
                                 (list 'quote (car rest)))))
         value-form))
       (t (error "Unknown %s pattern: %S" head pattern)))))
   (t (cons (list 'equal value-form (list 'quote pattern)) nil))))

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
    ;; The test is wrapped in the bindings collected so far, because a
    ;; later sub-pattern may READ an earlier one: `(and n (guard (> n 3)))'
    ;; binds n and then tests it.  `pcase' puts bindings around the clause
    ;; BODY only, so without this the guard ran with n unbound and every
    ;; such clause answered void-variable rather than matching or not.
    (let ((joined (cons 'and (let ((rev nil))
                               (while tests
                                 (setq rev (cons (car tests) rev))
                                 (setq tests (cdr tests)))
                               rev))))
      (cons (if bindings (list 'let bindings joined) joined)
            bindings))))

(defun nelisp-pcase--or (patterns value-form)
  "Build (TEST . BINDINGS) for an `or' pattern (no bindings)."
  (let ((tests nil)
        (cur patterns))
    (while cur
      (let* ((built (nelisp-pcase--test (car cur) value-form))
             (t1 (car built)))
        (setq tests (cons t1 tests)))
      (setq cur (cdr cur)))
    (cons (cons 'or (let ((rev nil))
                      (while tests
                        (setq rev (cons (car tests) rev))
                        (setq tests (cdr tests)))
                      rev))
          nil)))

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
  ;; The reader spells these `\=,' and `\=,@', not `comma' and `comma-at'.
  ;; Matching only the long names meant no unquote was ever recognised, so
  ;; `(,a ,b)' fell through to the literal arm and compared the VALUE
  ;; against the pattern `((\=, a) (\=, b))' -- which never matches anything.
  ;; Together with the head being `\=`' rather than `backquote', that made
  ;; the whole backquote pattern family dead, and the dispatcher's
  ;; always-match fallback hid it: every backquote clause matched, so the
  ;; first one in a `pcase' won whatever the value was.  Both spellings are
  ;; accepted now.
  (cond
   ((and (consp pat) (if (eq (car pat) 'comma) 1 (eq (car pat) '\,)))
    (let ((sym (car (cdr pat))))
      (cond
       ((eq sym '_) (cons t nil))
       ((symbolp sym) (cons t (list (list sym value-form))))
       (t (nelisp-pcase--test sym value-form)))))
   ((and (consp pat) (if (eq (car pat) 'comma-at) 1 (eq (car pat) '\,@)))
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

(defun nelisp-pcase--distribute-or (cases)
  "Split every clause whose pattern is a top-level `or' into one per arm."
  (let ((out nil))
    (dolist (c cases)
      (let ((pat (car c)))
        (if (and (consp pat) (eq (car pat) 'or) (cdr pat))
            (dolist (arm (cdr pat))
              (setq out (cons (cons arm (cdr c)) out)))
          (setq out (cons c out)))))
    (let ((rev nil))
      (while out (setq rev (cons (car out) rev)) (setq out (cdr out)))
      rev)))

(defmacro pcase (expr &rest cases)
  "Dispatch EXPR through CASES.
See `nelisp-pcase--test' for supported pattern shapes.

Rust-min migration (= moved out of build-tool/src/eval/special_forms.rs)."
  (let ((value-sym (make-symbol "--pcase-value--"))
        (cond-clauses nil))
    ;; A top-level `or' is distributed over the clause list first:
    ;;   ((or P1 P2) BODY)  ->  (P1 BODY) (P2 BODY)
    ;; `nelisp-pcase--or' builds ONE test for all the arms and drops their
    ;; bindings, because the (TEST . BINDINGS) protocol has no way to say
    ;; "these bindings only if THAT arm matched" -- so (pcase 5 ((or (and
    ;; (pred integerp) n) n) n)) reached its body with `n' unbound.  Giving
    ;; each arm its own clause is what makes the binding belong to the arm
    ;; that matched, and it costs a copy of the body.  An `or' nested inside
    ;; another pattern still goes through the binding-less builder; that case
    ;; fails loudly with `void-variable' rather than answering wrongly.
    (setq cases (nelisp-pcase--distribute-or cases))
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
    (let ((forward nil))
      (while cond-clauses
        (setq forward (cons (car cond-clauses) forward))
        (setq cond-clauses (cdr cond-clauses)))
      (list 'let (list (list value-sym expr))
            (cons 'cond forward)))))

;; nelisp-pcase.el ends here
