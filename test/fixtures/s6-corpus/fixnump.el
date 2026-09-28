;; Corpus for `fixnump': single-argument calls covering fixnum bounds,
;; one value just past most-positive-fixnum (a genuine bignum), a
;; float, and a non-number. `fixnump' never signals.
;; See test/nelisp-eln-s6-measure.sh.
((0)
 (1)
 (-1)
 (2305843009213693951)
 (-2305843009213693952)
 (2305843009213693952)
 (1.5)
 ("x")
 (nil))
