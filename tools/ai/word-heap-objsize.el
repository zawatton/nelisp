;;; word-heap-objsize.el --- Retained object slopes for Doc 216 -*- lexical-binding: t; -*-

;; Run unchanged on standalone and GNU Emacs. A keeper vector is allocated
;; BEFORE the baseline so neither cons cells nor slots contaminate the slope.
;; Run one type per process (WH_TYPE, WH_N); process isolation avoids retained
;; evaluator results from a previous measurement. Two population sizes expose
;; constant probe overhead. GNU reports allocated
;; type/slot bytes, including string-bytes, rather than process RSS or malloc.
(defvar os--keep nil)
(defvar os--gnu (not (fboundp 'nelisp--arena-stats)))
(defvar os--gc-rows nil)
(defun os--live ()
  (if os--gnu
        (let ((total 0))
          (dolist (row os--gc-rows) (setq total (+ total (* (nth 1 row) (nth 2 row)))))
          total)
    (nth 4 (nelisp--arena-stats))))
(unless os--gnu (nelisp--debug-switch 28))
(defvar os--type (or (getenv "WH_TYPE") "cons"))
(defvar os--n (string-to-number (or (getenv "WH_N") "4000")))
(defmacro os--object (i)
  (cond
   ((equal os--type "cons") '(cons nil nil))
   ((equal os--type "float") `(+ 0.5 ,i))
   ((equal os--type "string8") '(make-string 8 120))
   ((equal os--type "symbol") '(make-symbol "s"))
   ((equal os--type "record1") '(make-record 'wh 1 nil))
   ((equal os--type "bignum") `(+ 2305843009213693952 ,i))
   ((equal os--type "marker") '(make-marker))
   ((equal os--type "vector-slot") nil)
   (t (error "Unknown WH_TYPE: %s" os--type))))
;; Separate top-level forms ensure the baseline, population and final
;; collection have identical evaluator root lifetimes on both executables.
(unless (equal os--type "vector-slot") (setq os--keep (make-vector os--n nil)))
(garbage-collect)
(setq os--gc-rows (garbage-collect))
(defvar os--a (os--live))
(if (equal os--type "vector-slot")
    (setq os--keep (make-vector os--n nil))
  (dotimes (i os--n) (aset os--keep i (os--object i))))
(garbage-collect)
(setq os--gc-rows (garbage-collect))
(defvar os--b (os--live))
(princ (format "WH-OBJECT %s %d %d\n" os--type os--n (- os--b os--a)))
(unless (equal os--type "vector-slot")
  (princ (format "WH-VALUES %S %S\n" (aref os--keep 0) (aref os--keep (1- os--n)))))
(when os--gnu (princ (format "WH-GNU %s %S\n" emacs-version (garbage-collect))))
(unless os--gnu (nelisp--debug-switch 29))
(princ "WH-OBJECT-DONE\n")
nil
