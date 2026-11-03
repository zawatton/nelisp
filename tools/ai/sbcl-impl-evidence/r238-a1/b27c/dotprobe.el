(dolist (s '("-.5" "+.5" ".5" "-.5e2" "+.25" "-1.5" "1." "-1." "+1" "-1" "-" "+" "-.a" "+.x" ".e1" "a.b" "-a" "+a" "1e3" "-0.0" "(a . b)" "(-.5 +.5 .5)" "x" "nil" "t" "?a"))
  (let ((v (condition-case e (car (read-from-string s)) (error (list 'ERR (car e))))))
    (princ (format "%S => %S %S\n" s v (type-of v)))))
