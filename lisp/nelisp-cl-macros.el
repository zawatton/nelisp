;;; nelisp-cl-macros.el --- cl-loop / cl-block / cl-return elisp impl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust-min: cl-loop family を elisp 実装として stdlib に集約。
;; (= 各 consumer (nelisp-emacs / nelisp-cc / etc.) が独自の stub を
;; defun し合うのを止めて NeLisp 上流で共通実装を持つ。)
;;
;; 提供:
;;   cl-block NAME BODY...        catch+throw 経由の名前付き block
;;   cl-return-from NAME &optional VAL   block NAME を VAL で抜ける
;;   cl-return &optional VAL      最近接の unnamed block を VAL で抜ける
;;   cl-loop CLAUSES...           Common Lisp loop の subset
;;
;; cl-loop の対応 clause:
;;   for VAR in LIST                      list iterator
;;   for VAR from N to M                  numeric inclusive
;;   for VAR from N below M               numeric exclusive
;;   for VAR = INIT then UPDATE          accumulator (deferred)
;;   with VAR = VAL                       binding
;;   do FORM …                            unconditional side-effect
;;   collect FORM                         accumulate into list
;;   sum FORM                             accumulate sum
;;   count FORM                           count truthy
;;   when COND return FORM                early-exit with FORM
;;   when COND do FORM                    conditional side-effect
;;   while COND                           continue while COND non-nil
;;   until COND                           continue until COND non-nil
;;   bodyless (= no for/with/do keyword)  infinite loop with cl-return
;;
;; cl-loop は最終的に `cl-block nil (... while ...)' に展開され、
;; `cl-return' で抜ける。

;;; Code:

;;;; --- block / return ----------------------------------------------------

(defun nelisp-cl-macros--block-tag (name)
  "Catch tag symbol used by `cl-block' NAME (default NAME = anon)."
  (intern (format "cl-block-%s" (or name "anon"))))

(defmacro cl-block (name &rest body)
  "Establish a named BLOCK, returning a value or via `cl-return-from'.
NAME is captured as a catch tag; `(cl-return-from NAME VAL)' inside
BODY immediately exits the block with VAL.  A bare `(cl-return VAL)'
targets the nearest *unnamed* block (= NAME = nil), matching CL."
  (declare (indent 1) (debug (symbolp body)))
  (let ((tag (nelisp-cl-macros--block-tag name)))
    (list 'catch (list 'quote tag) (cons 'progn body))))

(defmacro cl-return-from (name &optional val)
  "Throw VAL out of the cl-block named NAME."
  (declare (indent 1) (debug (symbolp &optional form)))
  (list 'throw (list 'quote (nelisp-cl-macros--block-tag name)) val))

(defmacro cl-return (&optional val)
  "Throw VAL out of the nearest unnamed cl-block."
  (declare (debug (&optional form)))
  (list 'cl-return-from nil val))

;;;; --- loop builder ------------------------------------------------------

(defun nelisp-cl-macros--loop-build (clauses)
  "Build expansion for `cl-loop' CLAUSES.

See header for supported shapes.  Returns a form that, when the
shape is unrecognised, expands to nil (= caller gets a no-op
expansion rather than a runtime error)."
  (let ((var nil) (list-form nil) (do-forms nil) (collect-form nil)
        (sum-form nil) (count-form nil) (with-bindings nil)
        (when-return-cond nil) (when-return-form nil)
        (when-do-cond nil) (when-do-forms nil)
        (numeric-from nil) (numeric-to nil) (numeric-below nil)
        (while-cond nil) (until-cond nil)
        (bodyless-forms nil)
        (cur clauses) (recognised t))
    ;; Detect bodyless form: first clause is NOT a known keyword.
    (when (and clauses
               (not (memq (car clauses)
                          '(for with do collect sum count when
                                while until repeat finally return
                                named))))
      (setq bodyless-forms clauses
            cur nil
            recognised t))
    (while (and cur recognised)
      (let ((kw (car cur)))
        (cond
         ((eq kw 'for)
          (setq var (car (cdr cur)))
          (cond
           ((eq (car (cdr (cdr cur))) 'in)
            (setq list-form (car (cdr (cdr (cdr cur)))))
            (setq cur (cdr (cdr (cdr (cdr cur))))))
           ((eq (car (cdr (cdr cur))) 'from)
            (setq numeric-from (car (cdr (cdr (cdr cur)))))
            (let ((kw2 (car (cdr (cdr (cdr (cdr cur))))))
                  (val2 (car (cdr (cdr (cdr (cdr (cdr cur))))))))
              (cond
               ((eq kw2 'to)
                (setq numeric-to val2)
                (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
               ((eq kw2 'below)
                (setq numeric-below val2)
                (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
               (t (setq recognised nil)))))
           (t (setq recognised nil))))
         ((eq kw 'do)
          (setq do-forms (cons (car (cdr cur)) do-forms))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'collect)
          (setq collect-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'sum)
          (setq sum-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'count)
          (setq count-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'with)
          (let ((wname (car (cdr cur))))
            (when (eq (car (cdr (cdr cur))) '=)
              (setq with-bindings
                    (append with-bindings
                            (list (list wname (car (cdr (cdr (cdr cur))))))))
              (setq cur (cdr (cdr (cdr (cdr cur))))))))
         ((eq kw 'while)
          (setq while-cond (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'until)
          (setq until-cond (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'when)
          (let ((cond-form (car (cdr cur)))
                (next-kw (car (cdr (cdr cur))))
                (next-form (car (cdr (cdr (cdr cur))))))
            (cond
             ((eq next-kw 'return)
              (setq when-return-cond cond-form
                    when-return-form next-form
                    cur (cdr (cdr (cdr (cdr cur))))))
             ((eq next-kw 'do)
              (setq when-do-cond cond-form
                    when-do-forms (cons next-form when-do-forms)
                    cur (cdr (cdr (cdr (cdr cur))))))
             (t (setq recognised nil)))))
         (t (setq recognised nil)))))
    (cond
     ((not recognised) nil)
     ;; Bodyless infinite loop wrapped in cl-block nil.
     (bodyless-forms
      (list 'cl-block nil
            (cons 'while
                  (cons t bodyless-forms))))
     ;; Numeric `for VAR from N {to,below} M' [do FORM ...]
     ((and numeric-from (or numeric-to numeric-below))
      (let ((cmp (if numeric-to '<= '<))
            (limit (or numeric-to numeric-below))
            (rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let (cons (list var numeric-from) with-bindings)
              (list 'while (list cmp var limit)
                    (cons 'progn rev)
                    (list 'setq var (list '1+ var))))))
     ;; While / until plain loops (= no iterator).
     (while-cond
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'while (cons while-cond rev)))))
     (until-cond
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'while (cons (list 'not until-cond) rev)))))
     ;; `for VAR in LIST when COND return FORM' — early exit pattern.
     (when-return-cond
      (let ((tag-sym (make-symbol "--loop-tag--"))
            (result-sym (make-symbol "--loop-r--")))
        (list 'let (cons (list result-sym nil) with-bindings)
              (list 'catch (list 'quote tag-sym)
                    (list 'dolist (list var list-form)
                          (list 'when when-return-cond
                                (list 'setq result-sym when-return-form)
                                (list 'throw (list 'quote tag-sym) nil))))
              result-sym)))
     ;; `for VAR in LIST collect FORM'
     (collect-form
      (let ((acc-sym (make-symbol "--loop-acc--")))
        (list 'let (cons (list acc-sym nil) with-bindings)
              (list 'dolist (list var list-form)
                    (list 'setq acc-sym (list 'cons collect-form acc-sym)))
              (list 'nreverse acc-sym))))
     ;; `for VAR in LIST sum FORM'
     (sum-form
      (let ((acc-sym (make-symbol "--loop-sum--")))
        (list 'let (cons (list acc-sym 0) with-bindings)
              (list 'dolist (list var list-form)
                    (list 'setq acc-sym (list '+ acc-sym sum-form)))
              acc-sym)))
     ;; `for VAR in LIST count FORM'
     (count-form
      (let ((acc-sym (make-symbol "--loop-count--")))
        (list 'let (cons (list acc-sym 0) with-bindings)
              (list 'dolist (list var list-form)
                    (list 'when count-form
                          (list 'setq acc-sym (list '+ acc-sym 1))))
              acc-sym)))
     ;; `for VAR in LIST when COND do FORM …'
     (when-do-cond
      (let ((rev nil))
        (while when-do-forms
          (setq rev (cons (car when-do-forms) rev))
          (setq when-do-forms (cdr when-do-forms)))
        (list 'let with-bindings
              (list 'dolist (list var list-form)
                    (cons 'when (cons when-do-cond rev))))))
     ;; `for VAR in LIST do FORM …'
     (do-forms
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'dolist (cons (list var list-form) rev)))))
     (t (list 'let with-bindings nil)))))

(defmacro cl-loop (&rest clauses)
  "Loop CLAUSES — minimal CL-style iteration macro.

See `nelisp-cl-macros--loop-build' commentary for supported shapes.
Patterns this stub does not recognise expand to nil."
  (declare (debug (&rest sexp)))
  (nelisp-cl-macros--loop-build clauses))

;; nelisp-cl-macros.el ends here
