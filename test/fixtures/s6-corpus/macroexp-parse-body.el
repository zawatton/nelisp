;; Corpus for `macroexp-parse-body': BODY lists exercising a
;; docstring+interactive-spec prefix, a `declare' prefix, an empty
;; body, a body whose only element is nil, a body that is a single
;; string (kept as the return value, not a declaration), and a
;; wrong-type-argument error path.  See test/nelisp-eln-s6-measure.sh.
((("doc" (interactive) (+ 1 2)))
 (((declare (indent 1)) b1 b2))
 (nil)
 ((nil))
 (("only-string"))
 (5))
