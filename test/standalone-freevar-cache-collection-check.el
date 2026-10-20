;;; standalone-freevar-cache-collection-check.el --- Memo lifetime -*- lexical-binding: t; -*-

;; A free-variable filter is derived data. Its memo must not keep a dead
;; lambda's quoted constants alive after collection, or affect live captures.
(defvar fvc-collection--keep nil)
(defvar fvc-collection--before nil)
(defvar fvc-collection--after nil)
(setq fvc-collection--keep
      (let ((cell (list 40)))
        (lambda (n) (setcar cell (+ (car cell) n)))))

(when (fboundp 'nelisp--arena-stats)
  (nelisp--debug-switch 28)
  (garbage-collect)
  (garbage-collect)
  (setq fvc-collection--before (nth 4 (nelisp--arena-stats))))

;; LENGTH supplies a nonempty symbol filter, while each quoted vector is
;; reachable only from that temporary lambda's source tree.
(dotimes (_ 128)
  (eval (list 'lambda nil
              (list 'length (list 'quote (make-vector 4096 10)))) t))
(garbage-collect)
(garbage-collect)

(when (fboundp 'nelisp--arena-stats)
  (setq fvc-collection--after (nth 4 (nelisp--arena-stats)))
  (let ((retained (- fvc-collection--after fvc-collection--before)))
    (princ (format "FREEVAR-CACHE retained=%d\n" retained))
    (unless (< retained 1048576)
      (error "Free-variable memo retains dead lambda constants: %d" retained)))
  (nelisp--debug-switch 29))
(unless (= (funcall fvc-collection--keep 2) 42)
  (error "Live lexical capture changed across collection"))
(garbage-collect)
(unless (= (funcall fvc-collection--keep 1) 43)
  (error "Live lexical capture changed on repeated invocation"))
(princ "FREEVAR-CACHE-COLLECTION PASS\n")
