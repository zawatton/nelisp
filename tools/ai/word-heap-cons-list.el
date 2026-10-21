;;; word-heap-cons-list.el --- Retained make-list slope for Doc 216 S2 -*- lexical-binding: t; -*-

;; Companion of word-heap-objsize.el for a list population: one retained
;; (make-list WH_N nil), so every cdr is a stored cons reference.  A
;; representation that allocates a header alias for each stored cons
;; reference pays for it here even when individually retained conses do
;; not.  Same method as word-heap-objsize.el: the keeper exists before the
;; baseline, two collections around each reading, conservative scanning
;; disabled on NeLisp, one population per process.  GNU reports the bytes
;; its collector accounts.
(defvar whl--gnu (not (fboundp 'nelisp--arena-stats)))
(defvar whl--n (string-to-number (or (getenv "WH_N") "4000")))
(defvar whl--keep nil)
(defvar whl--rows nil)
(defun whl--live ()
  (if whl--gnu
      (let ((total 0))
        (dolist (row whl--rows) (setq total (+ total (* (nth 1 row) (nth 2 row)))))
        total)
    (nth 4 (nelisp--arena-stats))))
(unless whl--gnu (nelisp--debug-switch 28))
(garbage-collect)
(setq whl--rows (garbage-collect))
(defvar whl--a (whl--live))
(setq whl--keep (make-list whl--n nil))
(garbage-collect)
(setq whl--rows (garbage-collect))
(defvar whl--b (whl--live))
(princ (format "WH-LIST %d %d\n" whl--n (- whl--b whl--a)))
(princ (format "WH-VALUES %S %S\n" (length whl--keep) (nth (1- whl--n) whl--keep)))
(unless whl--gnu (nelisp--debug-switch 29))
(princ "WH-OBJECT-DONE\n")
nil
