;;; standalone-symbol-value-immediate-check.el --- a variable holding an immediate reads back -*- lexical-binding: t; -*-

;; GNU data.c:1619 find_symbol_value reads the value cell whatever it holds.
;; NeLisp's port looped on the WORD 1, which is the fixnum 0 when a store
;; (set, setq) leaves it in the cell as an immediate: (symbol-value 'x) and
;; (boundp 'x) never returned for a variable holding 0.  A `defvar'
;; initialisation stores a boxed 0, so only a later store showed it.
;;
;; The WORD scheme (Doc 146, Doc 215): a fixnum n is (n<<2)|1, so 0 is 1, -1
;; is -3, 1 is 5; nil is 3 and t is 7; every other WORD is a pointer, and 0
;; and 2 are never a WORD.  The accessor API's markers (0 signalled, 2 void,
;; -1 no answer yet) must stay out of that set.  Every small fixnum (-2..2),
;; nil and t, the extremes of the fixnum range and a few other immediates are
;; stored five ways -- defvar, setq, set, a dynamic let and a buffer-local
;; binding -- and read back five ways -- symbol-value, boundp, an
;; interpreted reference, an interpreted function body and a bytecode
;; varref -- each on a symbol of its own.

(defvar sviv-failures 0)
(defvar sviv-checks 0)
(defvar sviv-counter 0)
(defun sviv-check (ok label)
  (setq sviv-checks (1+ sviv-checks))
  (unless ok
    (setq sviv-failures (1+ sviv-failures))
    (princ (format "SVIV FAIL %s\n" label))))

(defun sviv-fresh ()
  (intern (format "sviv-sym-%d" (setq sviv-counter (1+ sviv-counter)))))

;; GNU 31.1 compiles (lambda () VAR) to varref 0, return.
(defun sviv-varref (sym) (make-byte-code 0 "\10\207" (vector sym) 1))

(defun sviv-readers (sym value label)
  "Check that SYM, bound to VALUE in the current buffer, reads back as VALUE."
  (sviv-check (equal (symbol-value sym) value) (concat label " symbol-value"))
  (sviv-check (boundp sym) (concat label " boundp"))
  (sviv-check (equal (eval sym t) value) (concat label " interpreted reference"))
  (sviv-check (equal (funcall (eval (list 'lambda nil sym) t)) value)
              (concat label " interpreted function body"))
  (sviv-check (equal (funcall (sviv-varref sym)) value) (concat label " bytecode varref")))

(defvar sviv-small '(-2 -1 0 1 2 nil t))
(defvar sviv-more
  (list 4611686018427387903 -4611686018427387904 0.0 ?a ?\0 'sviv-symbol "" '(0) 8 -8 3 -3))

(dolist (value (append sviv-small sviv-more))
  (let ((tag (format "%S" value)) sym)
    ;; defvar: the initial value.
    (setq sym (sviv-fresh))
    (eval (list 'defvar sym (list 'quote value)) t)
    (sviv-readers sym value (concat "defvar " tag))
    (sviv-check (equal (default-value sym) value) (concat "defvar " tag " default-value"))
    ;; set, then setq: the bare value stays in the cell.
    (setq sym (sviv-fresh))
    (set sym value)
    (sviv-readers sym value (concat "set " tag))
    (sviv-check (equal (default-value sym) value) (concat "set " tag " default-value"))
    (sviv-check (default-boundp sym) (concat "set " tag " default-boundp"))
    (setq sym (sviv-fresh))
    (eval (list 'defvar sym 99) t)
    (eval (list 'setq sym (list 'quote value)) t)
    (sviv-readers sym value (concat "setq " tag))
    (set-default sym 7)
    (set-default sym value)
    (sviv-check (equal (default-value sym) value) (concat "set-default " tag))
    ;; A dynamic let.
    (setq sym (sviv-fresh))
    (eval (list 'defvar sym 99) t)
    (eval (list 'let (list (list sym (list 'quote value)))
                (list 'sviv-readers (list 'quote sym) (list 'quote value)
                      (concat "let " tag)))
          t)
    (sviv-check (equal (symbol-value sym) 99) (concat "let " tag " restores the outer value"))
    ;; A buffer-local binding, read in its buffer.
    (setq sym (sviv-fresh))
    (eval (list 'defvar sym 99) t)
    (with-temp-buffer
      (set (make-local-variable sym) value)
      (sviv-readers sym value (concat "buffer-local " tag))
      (sviv-check (equal (buffer-local-value sym (current-buffer)) value)
                  (concat "buffer-local " tag " buffer-local-value")))
    (sviv-check (equal (symbol-value sym) 99) (concat "buffer-local " tag " leaves the default"))
    ;; A value that was void and is made void again.
    (setq sym (sviv-fresh))
    (set sym value)
    (makunbound sym)
    (sviv-check (not (boundp sym)) (concat "makunbound " tag))
    (sviv-check (condition-case nil (progn (symbol-value sym) nil) (void-variable t))
                (concat "makunbound " tag " signals void-variable"))))

;; The same value in the function cell and the property list.
(dolist (value '(-2 -1 0 1 2 nil t))
  (fset 'sviv-f value)
  (sviv-check (equal (symbol-function 'sviv-f) value) (format "function cell %S" value))
  (sviv-check (eq (and (fboundp 'sviv-f) t) (and value t)) (format "fboundp %S" value))
  (put 'sviv-p 'sviv value)
  (sviv-check (equal (get 'sviv-p 'sviv) value) (format "property %S" value)))

;; A void variable still reads as void.
(defvar sviv-undefined)
(sviv-check (not (boundp 'sviv-undefined)) "a defvar without a value is void")
(sviv-check (condition-case e (progn (symbol-value 'sviv-undefined) nil)
              (void-variable t))
            "symbol-value of a void variable signals void-variable")
(sviv-check (condition-case e (progn (funcall (sviv-varref 'sviv-undefined)) nil)
              (void-variable t))
            "a bytecode varref of a void variable signals void-variable")

(princ (format "SVIV checks=%d failures=%d %s\n" sviv-checks sviv-failures
               (if (= sviv-failures 0) "PASS" "FAIL")))
(when (> sviv-failures 0) (error "symbol-value immediate check failed"))
