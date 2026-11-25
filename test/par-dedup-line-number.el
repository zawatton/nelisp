;;; par-dedup-line-number.el --- GNU fns.c narrowing semantics -*- lexical-binding: t; -*-

(with-temp-buffer
  (insert "zero\none\ntwo\nthree")
  (let ((beyond (copy-marker 16)))
    (narrow-to-region 6 10)
    (prin1 (list 'narrowed-line-number
                 (line-number-at-pos 1)
                 (line-number-at-pos 16)
                 (line-number-at-pos beyond)
                 (line-number-at-pos 10)
                 (line-number-at-pos 10 t)))
    (terpri)))
