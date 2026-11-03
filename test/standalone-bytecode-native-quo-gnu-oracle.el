;;; standalone-bytecode-native-quo-gnu-oracle.el --- GNU 31.1 oracle -*- lexical-binding: t; -*-

(unless (and (equal emacs-version "31.1")
             (equal (list nl-quo-fixture-positive
                          nl-quo-fixture-negative nl-quo-fixture-float)
                    '(3 -3 3.5))
             (equal (list nl-quo-fixture-negated-int
                          nl-quo-fixture-negated-float)
                    '(-7 -2.5))
             (= (aref (aref (symbol-function 'nl-quo-fixture-negate) 1) 0) 91)
             (= (aref (aref (symbol-function 'nl-quo-fixture-negate) 1) 1) 135)
             (eq (condition-case err (nl-quo-fixture-divide 1 0)
                   (error (car err))) 'arith-error)
             (eq (condition-case err (nl-quo-fixture-divide 'wrong 2)
                   (error (car err))) 'wrong-type-argument)
             (eq (condition-case err
                     (funcall (make-byte-code nil (unibyte-string 165 135) [] 2))
                   (error (car err))) 'wrong-type-argument))
  (error "GNU 31.1 bytecode oracle mismatch"))
(princ "gnu-bytecode-oracle: (quo=3,-3,3.5 negate=-7,-2.5 opcode=91 return=135 divide-zero=arith-error wrong-type=wrong-type-argument underflow)\n")

(provide 'standalone-bytecode-native-quo-gnu-oracle)
;;; standalone-bytecode-native-quo-gnu-oracle.el ends here
