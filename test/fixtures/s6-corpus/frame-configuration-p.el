;; Corpus for `frame-configuration-p': single-argument calls over
;; well-formed and malformed pseudo frame-configuration lists, plus
;; non-cons values. Never signals. See test/nelisp-eln-s6-measure.sh.
((nil)
 ((frame-configuration))
 ((frame-configuration 1 2))
 ((not-frame-configuration 1 2))
 (5)
 ("x"))
