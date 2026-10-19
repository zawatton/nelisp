;;; standalone-abi-g4vm-word-check.el --- Bytecode WORD parity -*- lexical-binding: t; -*-

(let ((checked 0)
      (identity-code (make-byte-code 257 (unibyte-string 135) [] 2)))
  (dolist (value (list nil t 0 -7 2305843009213693952 1.25 "bytecode word"
                      '(a . b) [nil t -7]))
    (dolist (result (list (funcall identity-code value)
                          (apply identity-code (list value))
                          (byte-code (unibyte-string 192 135) (vector value) 1)
                          (funcall (let ((captured value)) (lambda () captured)))))
      (unless (equal result value) (error "G4VM result changed: %S" value))
      (setq checked (1+ checked))))
  ;; Bnth's small-index loop names the reached tail, whereas its fallback
  ;; through nthcdr names the original list after overshooting the tail.
  (dolist (case '((0 (a . b) a) (1 (a . b) (wrong-type-argument listp b))
                  (7 (a . b) (wrong-type-argument listp b))
                  (128 (a . b) (wrong-type-argument listp (a . b)))
                  (-1 (a . b) a) (-1 nil nil) (0 nil nil) (9 (a) nil)
                  (2305843009213693952 (a . b)
                   (wrong-type-argument listp (a . b)))
                  (-2305843009213693953 (a . b) a)
                  (-2305843009213693953 nil nil)
                  (bad (a) (wrong-type-argument integerp bad))))
    (let ((result (condition-case e
                      (byte-code (unibyte-string 192 193 56 135)
                                 (vector (car case) (cadr case)) 2)
                    (error e))))
      (unless (equal result (nth 2 case))
        (error "G4VM nth changed: %S => %S" case result))
      (setq checked (1+ checked))))
  (princ (format "ABI-G4VM-WORD checked=%d PASS\n" checked)))
nil
