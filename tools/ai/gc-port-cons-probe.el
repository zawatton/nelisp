;;; gc-port-cons-probe.el --- Doc 223 cons-block probe  -*- lexical-binding: t -*-

;; Usage: nelisp --load tools/ai/gc-port-cons-probe.el
;; Prints one GCPORT-CONS line of key=value pairs read from the cons-block
;; fields that `nelisp--arena-stats' appends (12 blocks, 13 live, 14 free,
;; all counted by the last sweep_conses).  tools/ai/gc-port-check.py conses
;; judges the numbers; a runtime without cons blocks prints missing fields.
;; The drop/reuse steps run with the conservative native-stack scan off
;; (`nelisp--debug-switch' 28, back on with 29): stale stack words can keep a
;; dropped list alive on the base binary as well (its live bytes grow by the
;; list; the census merely does not count conservatively pinned objects), and
;; GNU's mark_memory behaves the same.  The probe checks the precise sweep.

(defvar gcport-keep nil)
(defvar gcport-v nil)
(defun gcport-list (n)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (cons i acc)) (setq i (1+ i)))
    acc))
(defun gcport-field (k)
  (let ((s (nelisp--arena-stats)))
    (if (> (length s) k) (nth k s) -1)))
(defun gcport-collect () (garbage-collect) (garbage-collect))
(defun gcport-note (key k) (setq gcport-v (cons (cons key (gcport-field k)) gcport-v)))

(nelisp--debug-switch 28)
(gcport-collect)
(gcport-note 'blocks0 12)
(gcport-note 'live0 13)
(setq gcport-keep (gcport-list 100000))
(gcport-collect)
(gcport-note 'blocks1 12)
(gcport-note 'live1 13)
(setq gcport-keep nil)
(gcport-collect)
(gcport-note 'live2 13)
(gcport-note 'free2 14)
(gcport-note 'blocks2 12)
;; Reuse: 50000 new conses come from the rebuilt free list, so the block
;; count does not grow past its earlier peak.
(setq gcport-keep (gcport-list 50000))
(gcport-collect)
(gcport-note 'blocks3 12)
(gcport-note 'live3 13)
(setq gcport-v (cons (cons 'len3 (length gcport-keep)) gcport-v))
(setq gcport-v (cons (cons 'sum3 (apply #'+ gcport-keep)) gcport-v))
(nelisp--debug-switch 29)
(princ (concat "GCPORT-CONS"
               (mapconcat (lambda (kv) (format " %s=%d" (car kv) (cdr kv)))
                          (reverse gcport-v) "")
               "\n"))
