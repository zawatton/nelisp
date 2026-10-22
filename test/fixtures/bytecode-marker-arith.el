;;; bytecode-marker-arith.el --- GNU-compiled arithmetic and comparison opcodes -*- lexical-binding: t; -*-

;; One function per arithmetic or comparison opcode of GNU bytecode.c, so that
;; GNU's byte compiler emits the opcode itself: Beqlsign, Bgtr, Blss, Bleq,
;; Bgeq, Bmax, Bmin, Bplus, Bdiff, Bmult, Bquo, Brem, Badd1, Bsub1 and Bnegate.
;; Every one of them coerces a marker operand through
;; CHECK_NUMBER_COERCE_MARKER, after a fixnum fast path.  The .eld beside this
;; file holds the function objects GNU compiled; regenerate it with
;; `tools/ai/batch-f-check.sh marker-fixture'.

(defun bma-eql (a b) (= a b))
(defun bma-gtr (a b) (> a b))
(defun bma-lss (a b) (< a b))
(defun bma-leq (a b) (<= a b))
(defun bma-geq (a b) (>= a b))
(defun bma-max (a b) (max a b))
(defun bma-min (a b) (min a b))
(defun bma-add (a b) (+ a b))
(defun bma-sub (a b) (- a b))
(defun bma-mul (a b) (* a b))
(defun bma-quo (a b) (/ a b))
(defun bma-rem (a b) (% a b))
(defun bma-inc (a) (1+ a))
(defun bma-dec (a) (1- a))
(defun bma-neg (a) (- a))

;; Loops of fixnum compares and arithmetic, for the paired timing of the fast
;; paths: the opcodes with a fixnum fast path (< >= + - 1+), those without
;; (= <= > % max min), and float compares and sums.
(defun bma-loop (n)
  (let ((i 0) (sum 0))
    (while (< i n)
      (if (>= i 5) (setq sum (+ sum (- i 1))))
      (setq i (1+ i)))
    sum))
(defun bma-loop2 (n)
  (let ((i 0) (acc 0))
    (while (<= i n)
      (if (= (% i 7) 3) (setq acc (+ acc (max i 5))))
      (if (> i 100) (setq acc (- acc (min i 9))))
      (setq i (+ i 1)))
    acc))
(defun bma-loopf (n)
  (let ((i 0) (x 0.0))
    (while (< i n)
      (if (> x 1000.0) (setq x 0.0))
      (setq x (+ x 1.5))
      (setq i (1+ i)))
    x))
