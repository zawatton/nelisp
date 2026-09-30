(ccl-execute
 (condition-case e (ccl-execute 'ccl-1-missing []) (error e))
 (condition-case e (ccl-execute 17 [1 2]) (error e)))
(ccl-execute-on-string
 (condition-case e (ccl-execute-on-string 'ccl-1-missing [nil nil nil nil nil nil nil nil nil] "abc") (error e))
 (condition-case e (ccl-execute-on-string 'ccl-1-missing [] "xyz" t t) (error e)))
(ccl-program-p
 (ccl-program-p 'ccl-1-never-registered)
 (ccl-program-p (vector 1 2 3)))
(register-ccl-program
 (register-ccl-program 'ccl-1-probe-program nil)
 (list (ccl-program-p 'ccl-1-probe-program)
       (register-ccl-program 'ccl-1-probe-program nil)))
(register-code-conversion-map
 (register-code-conversion-map 'ccl-1-probe-map [1 2 3])
 (condition-case e (register-code-conversion-map 'ccl-1-bad-map nil) (error e)))
