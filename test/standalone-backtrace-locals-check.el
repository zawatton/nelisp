;;; standalone-backtrace-locals-check.el --- Activation locals regression -*- lexical-binding: t; -*-
;; Run unchanged on GNU Emacs and a bare standalone reader.
(defun btl-probe (depth) (backtrace--locals depth 'btl-probe))
(defun btl-one (one) (btl-probe 1))
(defun btl-two-inner (two) (btl-probe 2))
(defun btl-two (one) (btl-two-inner 22))
(defun btl-three-inner (three) (btl-probe 3))
(defun btl-three-middle (two) (btl-three-inner 33))
(defun btl-three (one) (btl-three-middle 22))
(defun btl-promoted (a b c d e f g h i j k l m n o p)
  (garbage-collect)
  (btl-probe 1))
(defvar btl-special 1)
(defun btl-special-inner ()
  (let ((btl-special 'younger)) (backtrace--locals 1 'btl-special-inner)))
(defun btl-special-outer ()
  (let ((btl-special 2) (btl-special 3)) (btl-special-inner)))
(let ((failures 0) (checks 0))
  (dolist (row
           (list
            (list 'depth-1 (cdr (assq 'one (btl-one 11))) 11)
            (list 'depth-2 (cdr (assq 'one (btl-two 11))) 11)
            (list 'depth-3 (cdr (assq 'one (btl-three 11))) 11)
            (list 'promoted
                  (let ((locals (btl-promoted 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
                    (list (cdr (assq 'a locals)) (cdr (assq 'p locals)) (length locals)))
                  '(1 16 16))
            (list 'closure
                  (let ((locals (let ((captured 8))
                                  (funcall (lambda (parameter)
                                             (backtrace--locals 1 'backtrace--locals)) 7))))
                    (list (cdr (assq 'captured locals)) (cdr (assq 'parameter locals))))
                  '(8 7))
            (list 'shadowed-dynamic (btl-special-outer)
                  '((btl-special . 2) (btl-special . 3)))
            (list 'repeated-values
                  (let ((btl-special nil) (btl-special t) (btl-special "saved"))
                    (btl-special-inner))
                  '((btl-special) (btl-special . t) (btl-special . "saved")))))
    (setq checks (1+ checks))
    (unless (equal (nth 1 row) (nth 2 row))
      (setq failures (1+ failures))
      (princ (format "BACKTRACE-LOCALS %S actual=%S expected=%S\n"
                     (car row) (nth 1 row) (nth 2 row)))))
  (princ (format "BACKTRACE-LOCALS checks=%d failures=%d %s\n"
                 checks failures (if (= failures 0) "PASS" "FAIL")))
  (kill-emacs (if (= failures 0) 0 1)))
