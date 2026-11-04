;;; probe.el --- Doc 237 region end-to-end probe -*- lexical-binding: t; -*-
;; Run from the repository root on a built image (see check.sh).  One line per
;; case: NAME RESULT.  Handles are never printed: only deterministic data.

(load "lisp/nelisp-region.el")

(defun pc-sum (e)
  (cond ((eq (car e) 'wrong-type-argument)
         (if (eq (cadr e) 'fixnump)
             (list 'wrong-type-argument 'fixnump (nth 2 e))
           (list 'wrong-type-argument (cadr e))))
        ((eq (car e) 'args-out-of-range) (cons 'args-out-of-range (cddr e)))
        (t (list (car e) (cadr e)))))

(defun pc-run (name f)
  (princ (format "%s %S\n" name (condition-case e (funcall f) (error (pc-sum e))))))

(defmacro pc (name &rest body) `(pc-run ,name (lambda () ,@body)))
(defmacro pe (&rest body) `(condition-case e (progn ,@body) (error (pc-sum e))))
(defun pc-count () (car (nelisp--region-stats)))

(pc "open-size-live"
    (let* ((r (nelisp--region-open 4096))
           (a (nelisp--region-size r)) (b (nelisp--region-live-p r)))
      (nelisp--region-close r) (list a b)))
(pc "i64-roundtrip"
    (let ((r (nelisp--region-open 4096)))
      (nelisp--region-i64-set r 0 42) (nelisp--region-i64-set r 511 -7)
      (prog1 (list (nelisp--region-i64-ref r 0) (nelisp--region-i64-ref r 511))
        (nelisp--region-close r))))
(pc "u8-roundtrip"
    (let ((r (nelisp--region-open 4096)))
      (prog1 (list (nelisp--region-u8-set r 0 255) (nelisp--region-u8-set r 4095 7)
                   (nelisp--region-u8-ref r 0) (nelisp--region-u8-ref r 4095))
        (nelisp--region-close r))))
(pc "zero-fill"
    (let ((r (nelisp--region-open 4096)))
      (prog1 (list (nelisp--region-u8-ref r 100) (nelisp--region-i64-ref r 3))
        (nelisp--region-close r))))
(pc "stale-after-close"
    (let ((r (nelisp--region-open 64)))
      (nelisp--region-close r)
      (list (nelisp--region-live-p r)
            (pe (nelisp--region-u8-ref r 0))
            (pe (nelisp--region-size r)))))
(pc "non-fixnum-handle"
    (list (nelisp--region-live-p 'a) (pe (nelisp--region-size 'a))
          (pe (nelisp--region-close "x")) (pe (nelisp--region-move nil))))
(pc "u8-index-range"
    (let ((r (nelisp--region-open 4096)))
      (prog1 (list (pe (nelisp--region-u8-ref r 4096)) (pe (nelisp--region-u8-ref r -1))
                   (pe (nelisp--region-u8-set r 4096 1)))
        (nelisp--region-close r))))
(pc "i64-index-range"
    (let ((r (nelisp--region-open 4096)))
      (prog1 (list (pe (nelisp--region-i64-ref r 512)) (pe (nelisp--region-i64-ref r -1))
                   (pe (nelisp--region-i64-set r 512 1)))
        (nelisp--region-close r))))
(pc "i64-partial-word"
    (let ((r (nelisp--region-open 12)))
      (prog1 (list (nelisp--region-i64-ref r 0) (pe (nelisp--region-i64-ref r 1)))
        (nelisp--region-close r))))
(pc "index-not-fixnum"
    (let ((r (nelisp--region-open 64)))
      (prog1 (list (pe (nelisp--region-u8-ref r 'x)) (pe (nelisp--region-i64-ref r "y")))
        (nelisp--region-close r))))
(pc "value-errors"
    (let ((r (nelisp--region-open 64)))
      (prog1 (list (pe (nelisp--region-i64-set r 0 'x)) (pe (nelisp--region-u8-set r 0 'x))
                   (pe (nelisp--region-u8-set r 0 256)) (pe (nelisp--region-u8-set r 0 -1)))
        (nelisp--region-close r))))
(pc "move-old-handle-stale"
    (let* ((r1 (nelisp--region-open 64)) (w (nelisp--region-i64-set r1 0 9))
           (r2 (nelisp--region-move r1)))
      (prog1 (list (nelisp--region-live-p r1) (nelisp--region-live-p r2)
                   (pe (nelisp--region-u8-ref r1 0)) (nelisp--region-i64-ref r2 0) w
                   (pe (nelisp--region-close r1)))
        (nelisp--region-close r2))))
(pc "double-close"
    (let ((r (nelisp--region-open 64)))
      (list (nelisp--region-close r) (pe (nelisp--region-close r)))))
(pc "open-bad-sizes"
    (list (pe (nelisp--region-open 0)) (pe (nelisp--region-open -5))
          (pe (nelisp--region-open 268435457)) (pe (nelisp--region-open 'x))))
(pc "open-max-then-over"
    (let ((r (nelisp--region-open 268435456)))
      (prog1 (list (nelisp--region-size r) (pe (nelisp--region-open 1)))
        (nelisp--region-close r))))
(pc "cumulative-budget"
    (let ((r (nelisp--region-open 209715200)))
      (prog1 (pe (nelisp--region-open 104857600))
        (nelisp--region-close r))))
(pc "slot-exhaustion"
    (let ((hs nil) (err nil) (n 0))
      (dotimes (_ 64) (push (nelisp--region-open 4096) hs))
      (setq n (pc-count))
      (setq err (pe (nelisp--region-open 4096)))
      (dolist (h hs) (nelisp--region-close h))
      (list n err (pc-count))))
(pc "with-region-normal"
    (list (nelisp-with-region (r 4096)
            (nelisp--region-i64-set r 0 42) (nelisp--region-i64-ref r 0))
          (pc-count)))
(pc "with-region-throw"
    (list (catch 'tag (nelisp-with-region (r 4096) (throw 'tag 'thrown))) (pc-count)))
(pc "with-region-signal"
    (list (pe (nelisp-with-region (r 4096) (error "boom"))) (pc-count)))
(pc "with-region-moved"
    (let (new)
      (nelisp-with-region (r 4096) (setq new (nelisp--region-move r)))
      (let ((a (nelisp--region-live-p new)) (c (pc-count)))
        (nelisp--region-close new)
        (list a c (pc-count)))))
(pc "gc-survival"
    (let ((r (nelisp--region-open 67108864)) (bad 0) (k 0))
      (dotimes (k 2048) (nelisp--region-i64-set r (* k 4096) (+ (* k 7919) 13)))
      (nelisp--region-u8-set r 67108863 99)
      (dotimes (_ 3)
        (let ((g nil))
          (dotimes (n 20000) (setq g (cons (make-vector 8 n) g)))
          (garbage-collect)))
      (dotimes (k 2048)
        (unless (= (nelisp--region-i64-ref r (* k 4096)) (+ (* k 7919) 13)) (setq bad (1+ bad))))
      (prog1 (list bad (nelisp--region-u8-ref r 67108863) (nelisp--region-i64-ref r 1))
        (nelisp--region-close r))))
(pc "final-stats"
    (let ((s (nelisp--region-stats))) (list (nth 0 s) (nth 1 s))))

(kill-emacs 0)
