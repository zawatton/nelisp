;;; prim-strcmp-bench.el --- Comparable string primitive costs -*- lexical-binding: t -*-

;; Use one selected operation per process. Instruction differences include
;; loop overhead and every collection; the same harness runs under GNU.
(defvar pc-cases
  (list
   (list "string-lessp" (lambda () (string-lessp "symbol-name-0001-x" "symbol-name-0002-x")))
   (list "string<" (lambda () (string< "symbol-name-0001-x" "symbol-name-0002-x")))
   (list "string>" (lambda () (string> "symbol-name-0001-x" "symbol-name-0002-x")))
   (list "string-greaterp" (lambda () (string-greaterp "symbol-name-0001-x" "symbol-name-0002-x")))
   (list "compare-strings" (lambda () (compare-strings "symbol-name-0001-x" nil nil "symbol-name-0002-x" nil nil)))
   (list "string=" (lambda () (string= "symbol-name-0001-x" "symbol-name-0001-x")))
   (list "string-equal" (lambda () (string-equal "symbol-name-0001-x" "symbol-name-0001-x")))
   (list "string-prefix-p" (lambda () (string-prefix-p "symbol-name-0001" "symbol-name-0001-x")))))
(defun pc-run (fn n)
  (dotimes (_ n) (funcall fn)))
(dolist (row pc-cases)
  (when (or (null (getenv "PC_CASE")) (equal (getenv "PC_CASE") (car row)))
    (let* ((fn (cadr row)) (count (getenv "PC_N"))
           (n (if count (string-to-number count) 1000)))
      (funcall fn)
      (let ((start (float-time)))
        (pc-run fn n)
        (princ (format "PC-TIME %s %.6f\n" (car row)
                       (* 1000000.0 (/ (- (float-time) start) n))))))))
(princ "PC-COMPLETE\n")
