;;; General VM integer success and numeric fallback parity. -*- lexical-binding: t; -*-
(let ((checked 0)
      (pairs '((p35-vm-increment 1+) (p35-vm-decrement 1-)
               (p35-vm-add +) (p35-vm-multiply *) (p35-vm-subtract -)
               (p35-vm-num-equal =) (p35-vm-less <) (p35-vm-greater >)
               (p35-vm-less-equal <=) (p35-vm-greater-equal >=)))
      (values '(-9223372036854775808 -2305843009213693953 -2305843009213693952
                -2305843009213693951 -1073741825 -1073741824 -2 -1 0 1 2
                1073741823 1073741824 2305843009213693950 2305843009213693951
                2305843009213693952 9223372036854775807)))
  (dolist (pair pairs)
    (unless (byte-code-function-p (symbol-function (car pair)))
      (error "Arithmetic caller is not genuine bytecode: %S" pair))
    (let ((rows (if (memq (cadr pair) '(1+ 1-))
                    (mapcar #'list values)
                  (let ((result nil))
                    (dolist (a '(-2305843009213693952 -1 0 1 2305843009213693951
                                 -9223372036854775808 9223372036854775807))
                      (dolist (b '(-2305843009213693952 -1 0 1 2305843009213693951
                                   -9223372036854775808 9223372036854775807))
                        (push (list a b) result)))
                    result))))
      (dolist (row rows)
        (unless (equal (p35-vm-condition (car pair) (list (apply #'vector row)))
                       (p35-vm-condition (cadr pair) row))
          (error "VM numeric result/condition mismatch: %S %S" pair row))
        (setq checked (1+ checked))))
    (dolist (value '(nil t symbol "string" [1] (a) 1.5))
      (let ((row (if (memq (cadr pair) '(1+ 1-)) (list value) (list value 2))))
        (unless (equal (p35-vm-condition (car pair) (list (apply #'vector row)))
                       (p35-vm-condition (cadr pair) row))
          (error "VM numeric fallback mismatch: %S %S" pair row))
        (setq checked (1+ checked))))
    ;; Dedicated bytecode operations retain primitive semantics after FSET.
    (let ((owner (symbol-function (cadr pair)))
          (expected (pcase (cadr pair) ('1+ 3) ('1- 1) ('+ 5) ('* 6)
                            ('- -1) ('= nil) ('< t) ('> nil) ('<= t) ('>= nil))))
      (unwind-protect
          (progn
            (fset (cadr pair) (lambda (&rest ignored) 'rebound))
            (unless (eq (funcall (car pair) [2 3]) expected)
              (error "VM numeric opcode followed rebound function cell")))
        (fset (cadr pair) owner))
      (setq checked (1+ checked))))
  (princ (format "P35-ARITHMETIC-VM-PASS controls=%d\n" checked)))
