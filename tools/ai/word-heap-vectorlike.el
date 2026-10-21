;;; word-heap-vectorlike.el --- Doc 216 S4 retained populations -*- lexical-binding: t; -*-

;; The keeper precedes the baseline.  Separate processes and two population
;; sizes remove fixed parser/diagnostic/keeper costs, as in the S0 probe.
(defvar whv--keep nil)
(defvar whv--gnu (not (fboundp 'nelisp--arena-stats)))
(defvar whv--rows nil)
(defvar whv--kind (or (getenv "WHV_KIND") "record1"))
(defvar whv--n (string-to-number (or (getenv "WHV_N") "4000")))
(defun whv--live ()
  (if whv--gnu
      (let ((total 0))
        (dolist (row whv--rows)
          (setq total (+ total (* (nth 1 row) (nth 2 row)))))
        total)
    (nth 4 (nelisp--arena-stats))))
(defmacro whv--object ()
  (cond ((equal whv--kind "vector1") '(make-vector 1 nil))
        ((equal whv--kind "record1") '(make-record 'whv 1 nil))
        ((equal whv--kind "char-table") '(make-char-table 'whv nil))
        ((equal whv--kind "vector-slot") nil)
        (t (error "Unknown WHV_KIND %s" whv--kind))))
(unless whv--gnu (nelisp--debug-switch 28))
(unless (equal whv--kind "vector-slot")
  (setq whv--keep (make-vector whv--n nil)))
(garbage-collect)
(setq whv--rows (garbage-collect))
(defvar whv--before (whv--live))
(if (equal whv--kind "vector-slot")
    (setq whv--keep (make-vector whv--n nil))
  (dotimes (i whv--n) (aset whv--keep i (whv--object))))
(garbage-collect)
(setq whv--rows (garbage-collect))
(defvar whv--after (whv--live))
(princ (format "WHV-OBJECT %s %d %d\n" whv--kind whv--n
               (- whv--after whv--before)))
(unless (equal whv--kind "vector-slot")
  (princ (format "WHV-VALUES %S %S\n"
                 (aref whv--keep 0) (aref whv--keep (1- whv--n)))))
(when whv--gnu (princ (format "WHV-GNU %s %S\n" emacs-version (garbage-collect))))
(unless whv--gnu (nelisp--debug-switch 29))
(princ "WHV-OBJECT-DONE\n")
nil
