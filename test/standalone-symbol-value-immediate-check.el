;;; standalone-symbol-value-immediate-check.el --- a variable holding an immediate reads back -*- lexical-binding: t; -*-

;; GNU data.c:1619 find_symbol_value reads the value cell whatever it holds.
;; NeLisp's port looped on the WORD 1, which is the fixnum 0 when a store
;; (set, setq) leaves it in the cell as an immediate: (symbol-value 'x) and
;; (boundp 'x) never returned for a variable holding 0.  A `defvar'
;; initialisation stores a boxed 0, so only a later store showed it.
;;
;; Every immediate the stores keep in the cell, read back through each
;; reader of the cell: symbol-value, boundp, default-value, default-boundp,
;; a plain reference, and the bytecode varref.

(defvar sviv-failures 0)
(defvar sviv-checks 0)
(defun sviv-check (ok label)
  (setq sviv-checks (1+ sviv-checks))
  (unless ok
    (setq sviv-failures (1+ sviv-failures))
    (princ (format "SVIV FAIL %s\n" label))))

(defvar sviv-v 5)
(defvar sviv-undefined)
(dolist (value (list 0 1 -1 2 -2 4611686018427387903 -4611686018427387904 nil t 0.0 ?a ?\0
                     'sviv-sym "" '(0)))
  (let ((label (format "%S" value)))
    ;; set, setq and a global store leave the bare value in the cell.
    (set 'sviv-v value)
    (sviv-check (equal (symbol-value 'sviv-v) value) (concat "set/symbol-value " label))
    (sviv-check (boundp 'sviv-v) (concat "set/boundp " label))
    (sviv-check (equal (default-value 'sviv-v) value) (concat "set/default-value " label))
    (sviv-check (default-boundp 'sviv-v) (concat "set/default-boundp " label))
    (sviv-check (equal sviv-v value) (concat "set/reference " label))
    (setq sviv-v 7)
    (setq sviv-v value)
    (sviv-check (equal (symbol-value 'sviv-v) value) (concat "setq/symbol-value " label))
    (set-default 'sviv-v 9)
    (set-default 'sviv-v value)
    (sviv-check (equal (default-value 'sviv-v) value) (concat "set-default " label))
    ;; A dynamic binding holds it too.
    (let ((sviv-v value))
      (sviv-check (equal (symbol-value 'sviv-v) value) (concat "let/symbol-value " label))
      (sviv-check (boundp 'sviv-v) (concat "let/boundp " label)))
    ;; And a global never initialised by `defvar'.
    (set 'sviv-fresh value)
    (sviv-check (and (boundp 'sviv-fresh) (equal (symbol-value 'sviv-fresh) value))
                (concat "fresh variable " label))
    (makunbound 'sviv-fresh)
    (sviv-check (not (boundp 'sviv-fresh)) (concat "makunbound " label))))

;; The same value in the function cell and the property list.
(dolist (value '(0 1 -1))
  (fset 'sviv-f value)
  (sviv-check (and (fboundp 'sviv-f) (eql (symbol-function 'sviv-f) value))
              (format "function cell %S" value))
  (put 'sviv-p 'sviv value)
  (sviv-check (eql (get 'sviv-p 'sviv) value) (format "property %S" value)))

;; A void variable still reads as void.
(sviv-check (not (boundp 'sviv-undefined)) "a defvar without a value is void")
(sviv-check (condition-case e (progn (symbol-value 'sviv-undefined) nil)
              (void-variable t))
            "symbol-value of a void variable signals void-variable")

;; The bytecode varref: GNU 31.1 compiles (lambda () sviv-v) to varref 0,
;; return.
(dolist (value '(0 1 -1))
  (set 'sviv-v value)
  (sviv-check (eql (funcall #[0 "\10\207" [sviv-v] 1]) value)
              (format "bytecode varref of %S" value)))

(princ (format "SVIV checks=%d failures=%d %s\n" sviv-checks sviv-failures
               (if (= sviv-failures 0) "PASS" "FAIL")))
(when (> sviv-failures 0) (error "symbol-value immediate check failed"))
