;; Corpus for `cconv-closure-convert': FORMs exercising a bare atom, a
;; quoted constant, a lambda with no free variables, a lambda that
;; captures an outer lexical variable (the interesting closure-
;; conversion case), a free variable declared dynamically bound via
;; DYNBOUND-VARS, and a wrong-type-argument error path through a
;; non-list DYNBOUND-VARS that is actually consulted (for a
;; `let'-bound variable).  See test/nelisp-eln-s6-measure.sh.
((5)
 ((quote foo))
 ((function (lambda (x) x)))
 ((let ((y 1)) (function (lambda () y))))
 ((function (lambda () q)) (q))
 ((let ((q 1)) q) 5))
