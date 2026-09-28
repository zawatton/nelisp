;; Corpus for `cconv--set-diff': set-difference calls over disjoint,
;; overlapping, and empty lists, plus a wrong-type-argument error path
;; through S1.  See test/nelisp-eln-s6-measure.sh.
((nil nil)
 ((a b c) (b))
 ((a b) nil)
 (nil (a))
 (5 nil))
