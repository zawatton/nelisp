;;; batch-f-doc-closure.el --- (:documentation FORM) in closures -*- lexical-binding: t; -*-

;; GNU eval.c Ffunction consumes a leading (:documentation FORM) of a lambda
;; body when it builds an interpreted closure: it evaluates FORM and stores
;; the value as the closure's docstring slot (4), which is the OClosure type
;; when FORM yields a symbol (oclosure.el `oclosure--lambda' builds its
;; closures this way).  `macroexpand-all' leaves the form alone, because
;; nothing is fboundp as `:documentation', so the eager macroexpansion of a
;; loaded file must not change the closure.  Every closure below is built
;; twice: by this file's own top-level forms (eagerly macroexpanded where the
;; runtime does that) and by `eval'.
;; Not covered: a FORM that is neither a quoted value nor a constant, which
;; this runtime's closure view does not evaluate.

(defun bfd-report (tag value) (princ (format "%s %S\n" tag value)))

;; GNU oclosure.el:479 `oclosure-type', and nadvice.el `advice--p' over it.
(defun bfd-oclosure-type (f)
  (and (closurep f) (> (length f) 4)
       (let ((type (aref f 4))) (if (symbolp type) type))))
(defun bfd-advice-p (f) (and (eq (bfd-oclosure-type f) 'advice) t))

;; 1. Slots of a plain closure, a string and a symbol in the form.
(let* ((x 1)
       (documented (lambda (y) (:documentation "from the form") (+ x y)))
       (typed (lambda (y) (:documentation 'bfd-type) y))
       (plain (lambda (y) y)))
  (bfd-report "loaded documented"
              (list (length documented) (aref documented 4) (function-documentation documented)
                    (funcall documented 2)))
  (bfd-report "loaded typed" (list (length typed) (aref typed 4) (bfd-oclosure-type typed)
                                   (bfd-advice-p typed) (funcall typed 5)))
  (bfd-report "loaded plain" (list (length plain) (bfd-oclosure-type plain) (funcall plain 6))))

;; 2. A wrapper typed `advice', as nadvice.el builds them.
(defun bfd-wrap (how fn)
  (lambda (&rest args)
    (:documentation 'advice)
    (if (eq how :before) (progn (apply fn args) 0) (apply fn args))))
(let ((w (bfd-wrap :around #'1+)))
  (bfd-report "loaded wrapper" (list (bfd-advice-p w) (bfd-oclosure-type w) (funcall w 4))))

;; 3. The form survives macroexpand-all and macroexpand.
(bfd-report "macroexpand-all" (macroexpand-all '(lambda (y) (:documentation 'bfd-type) y)))
(bfd-report "macroexpand" (macroexpand '(:documentation 'bfd-type)))

;; 4. make-interpreted-closure with a symbol and with a string docstring.
(let ((s (make-interpreted-closure '(x) '(x) '((a . 1)) 'bfd-type))
      (d (make-interpreted-closure '(x) '(x) '((a . 1)) "text")))
  (bfd-report "made" (list (length s) (aref s 4) (bfd-oclosure-type s) (funcall s 3)
                           (length d) (aref d 4) (function-documentation d))))

;; The same closures through eval.
(bfd-report "eval documented"
            (eval '(let* ((x 1) (f (lambda (y) (:documentation "from the form") (+ x y))))
                     (list (length f) (aref f 4) (function-documentation f) (funcall f 2))) t))
(bfd-report "eval typed"
            (eval '(let ((f (lambda (y) (:documentation 'bfd-type) y)))
                     (list (length f) (aref f 4) (bfd-oclosure-type f) (bfd-advice-p f) (funcall f 5))) t))
(bfd-report "eval wrapper"
            (eval '(let ((w (bfd-wrap :before #'ignore))) (list (bfd-advice-p w) (funcall w 4))) t))

(kill-emacs 0)
