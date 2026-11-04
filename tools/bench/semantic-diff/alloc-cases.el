;;; alloc-cases.el --- allocation size failure differential cases  -*- lexical-binding: t -*-
;; Same "NAME => RESULT" / "DONE n=N" format as cases.el.  Each over-large
;; allocation must be a catchable `error' (GNU memory_full), not a process abort.
(defvar sd-count 0)
(defun sd-emit (name thunk)
  (let ((r (condition-case e (list 'ok (funcall thunk))
             (error (list 'err e)))))
    (setq sd-count (1+ sd-count))
    (princ (format "%s => %s\n" name
                   (if (eq (car r) 'ok) (prin1-to-string (cadr r))
                     (prin1-to-string
                      (cons 'error
                            (mapcar (lambda (x) (if (stringp x) (substring-no-properties x) x))
                                    (cadr r)))))))))
(defmacro sd-case (name form) `(sd-emit ,name (lambda () ,form)))
(sd-case "alloc-vec-mpf" (make-vector most-positive-fixnum 0))
(sd-case "alloc-vec-2p40" (make-vector (ash 1 40) 0))
(sd-case "alloc-vec-neg" (make-vector -1 0))
(sd-case "alloc-vec-2p62" (make-vector (ash 1 62) 0))
(sd-case "alloc-vec-float" (make-vector 1.5 0))
(sd-case "alloc-vec-normal" (length (make-vector 1000 0)))
(sd-case "alloc-str-mpf" (make-string most-positive-fixnum ?a))
(sd-case "alloc-str-2p40" (make-string (ash 1 40) ?a))
(sd-case "alloc-str-neg" (make-string -1 ?a))
(sd-case "alloc-str-normal" (length (make-string 1000 ?a)))
(sd-case "alloc-bv-mpf" (make-bool-vector most-positive-fixnum nil))
(sd-case "alloc-bv-2p40" (make-bool-vector (ash 1 40) nil))
(sd-case "alloc-bv-neg" (make-bool-vector -1 nil))
(sd-case "alloc-bv-normal" (length (make-bool-vector 1000 t)))
(sd-case "alloc-survives" (progn (ignore-errors (make-vector (ash 1 40) 0)) (length (make-vector 3 1))))
(princ (format "DONE n=%d\n" sd-count))
