;;; -*- lexical-binding: t; -*-
;; functionp cost row (F416).  Load this file, then call (fp-time-row) from a SEPARATE top-level form so the loop
;; runs outside `load'.  Prints t when the mean cost of a 100000-call mix (symbol, closure, subr, macro, non-function)
;; is under 2 us per call.  GNU 31.1 answers t (about 0.4 us); the old chained definition costs about 7.8 us.
(defun fp-time-fn (x) x)
(defun fp-time-row ()
  (let* ((cl (lambda (x) x)) (n 100000) (i 0) (r nil) (t0 (float-time)))
    (while (< i n)
      (setq r (functionp 'fp-time-fn))
      (setq r (functionp cl))
      (setq r (functionp 'when))
      (setq r (functionp 5))
      (setq r (functionp 'car))
      (setq i (1+ i)))
    (let* ((per (/ (- (float-time) t0) (* 5.0 n))))
      (princ (format "T1 functionp < 2 us per call            %S\n" (< per 2e-6)))
      (message "   (mean %.3f us)" (* per 1e6)))))
