;; Corpus for `macroexpand-1': single-step macroexpansion over an atom,
;; a non-macro call, a real macro (`when'), an ENVIRONMENT expander
;; entry that is applied, one that is a no-op (nil cdr), and a
;; wrong-type-argument error path through a non-list ENVIRONMENT.
;; See test/nelisp-eln-s6-measure.sh.
((5)
 ((foo 1 2))
 ((when x y))
 ((foo 1) ((foo . (lambda (a) (list (quote bar) a)))))
 ((foo 1) ((foo)))
 ((foo 1) 5))
