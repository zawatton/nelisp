;;; standalone-prim-callfloor-check.el --- Calling floor semantics -*- lexical-binding: t; -*-

(defun pcfc-required (a &optional b &rest rest) (list a b rest))
(defun pcfc-optional (a &optional b) (list a b))
(defun pcfc-constant (a &optional b &rest rest) 17)
(when (and (fboundp 'nelisp--fx-register) (not (getenv "NELISP_FX_DISABLE")))
  (nelisp--fx-register 'pcfc-constant)
  (unless (vectorp (gethash (nthcdr 3 (symbol-function 'pcfc-constant)) nelisp--fx-table))
    (error "Missing VM coverage of unused optional/rest parameters")))
(let ((failures 0) (checks 0))
  (dolist
      (row
       (list
        (list 'literal ((lambda () nil t 17 "last")) "last")
        (list 'empty ((lambda ())) nil)
        (list 'capture (let ((saved 23)) (funcall (lambda () saved))) 23)
        (list 'effect (let ((value 0)) (funcall (lambda () (setq value 41))) value) 41)
        (list 'spread (apply #'pcfc-required 1 '(2 3 4)) '(1 2 (3 4)))
        (list 'unused-parameters (list (pcfc-constant 1) (pcfc-constant 1 2 3 4)) '(17 17))
        (list 'unused-arity (condition-case e (pcfc-constant)
                             (wrong-number-of-arguments (list (car e) (nth 2 e))))
              '(wrong-number-of-arguments 0))
        (list 'few-identity
              (condition-case e (pcfc-optional)
                (wrong-number-of-arguments
                 (list (eq (nth 1 e) (symbol-function 'pcfc-optional)) (nth 2 e))))
              '(t 0))
        (list 'many-identity
              (condition-case e (pcfc-optional 1 2 3)
                (wrong-number-of-arguments
                 (list (eq (nth 1 e) (symbol-function 'pcfc-optional)) (nth 2 e))))
              '(t 3))
        (list 'bad-spread (condition-case e (apply #'car 1) (error e))
              '(wrong-type-argument listp 1))
        (list 'dotted-spread (condition-case e (apply #'list '(1 . 2)) (error e))
              '(wrong-type-argument listp 2))
        (list 'bytecode-arity
              (condition-case e
                  (funcall (make-byte-code 257 (unibyte-string 192 135) [17] 2))
                (error e))
              '(wrong-number-of-arguments (1 . 1) 0))
        (list 'bytecode-literal
              (funcall (make-byte-code 0 (unibyte-string 192 135) [17] 1)) 17)
        (list 'bytecode-to-bytecode-literal
              (let* ((leaf (make-byte-code 0 (unibyte-string 192 135) [17] 1))
                     (caller (make-byte-code 0 (unibyte-string 192 32 135)
                                             (vector leaf) 1)))
                (funcall caller)) 17)
        (list 'literal-identity
              (let* ((reader (lambda () [1 "literal"]))
                     (object (funcall reader)))
                (garbage-collect)
                (eq object (funcall reader))) t)
        (list 'bytecode-identity
              (let* ((object (list 'constant))
                     (reader (make-byte-code 0 (unibyte-string 192 135) (vector object) 1)))
                (garbage-collect)
                (eq object (funcall reader))) t)))
    (setq checks (1+ checks))
    (unless (equal (nth 1 row) (nth 2 row))
      (setq failures (1+ failures))
      (princ (format "CALLFLOOR FAIL %S got=%S expected=%S\n"
                     (car row) (nth 1 row) (nth 2 row)))))
  (princ (format "CALLFLOOR checks=%d failures=%d %s\n"
                 checks failures (if (= failures 0) "PASS" "FAIL")))
  (kill-emacs (if (= failures 0) 0 1)))
