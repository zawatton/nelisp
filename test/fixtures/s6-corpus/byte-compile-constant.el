;; S6.15 corpus for the byte-compile-constant.wrapper.el entry point.
;; Args are (CONST FOR-EFFECT); (42 nil) exercises the pushed-value path,
;; (42 t) the for-effect (nothing pushed) path.  See
;; test/nelisp-eln-s6-measure.sh and README.md.
((42 nil)
 (42 t))
