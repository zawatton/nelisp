;; Corpus for `caar': single-argument calls over nil, nested conses,
;; and a wrong-type error path. See test/nelisp-eln-s6-measure.sh.
((nil)
 ((1 . 2))
 (((3 . 4) . 5))
 ((("a") . "b"))
 (5))
