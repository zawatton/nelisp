;; S6.7 corpus for the cconv--convert-function.wrapper.el entry point.
;; Args are (FREEVARS-ALIST ARGS BODY ENV PARENTFORM): a matching-car
;; freevars-alist entry (success) and a mismatched one (the genuine
;; `cl-assertion-failed' error path).  See test/nelisp-eln-s6-measure.sh
;; and README.md.
(
 ((((x))) (a) (x) nil nil)
 ((((z))) (a) (y) nil nil)
 )
