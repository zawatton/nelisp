;;; symbols-mxlib-probe.el --- Doc 226 S2.2 mxlib timing -*- lexical-binding: t; -*-

;; Loads SYMP_ELC (GNU 31.1 bytecode of mxlib.el plus
;; symbols-mxlib-loops.el, printed as fset forms) and times SYMX_N calls of
;; mx-a and mx-b on a six-element list after 1000 warmup calls, garbage
;; collected before each loop and collections inside the loop counted.
(defvar symx-n (string-to-number (or (getenv "SYMX_N") "100010")))
(load (getenv "SYMP_ELC") nil t)
(let ((xs (list 1 2 3 4 5 6)) (res nil))
  (symx-loop-a 1000 xs)
  (symx-loop-b 1000 xs)
  (dolist (f '(symx-loop-a symx-loop-b))
    (garbage-collect)
    (let ((t0 (float-time)))
      (funcall f symx-n xs)
      (push (format "%s=%.9f" f (- (float-time) t0)) res)))
  (princ (format "SYMX %d %S %s\n" symx-n (mx-a xs)
                 (mapconcat #'identity (nreverse res) " "))))
