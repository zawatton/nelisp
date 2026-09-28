;; Corpus for `macroexp--all-forms': FORMS lists exercising the
;; unconditional-expand path, the SKIP-count path, a form containing
;; already-quoted/nested subforms, and a wrong-type-argument error
;; path through a non-numeric SKIP.  See test/nelisp-eln-s6-measure.sh.
((nil)
 ((1 2 3))
 ((1 2 3) 2)
 (((quote a) (quote b) (if x y z)))
 ((1 2 3) not-a-number))
