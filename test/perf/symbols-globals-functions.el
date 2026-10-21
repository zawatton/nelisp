;;; symbols-globals-functions.el --- Doc 226 S2 timed functions -*- lexical-binding: t; -*-

;; Loaded as source or as its GNU 31.1-compiled .elc by
;; symbols-globals-probe.el; see there.

(defvar symp-v 1)
(defvar symp-w 0)
(defun symp-trivial1 (n s)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (symbolp s)) (setq i (1+ i)))
    acc))
(defun symp-value (n s)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (symbol-value s)) (setq i (1+ i)))
    acc))
(defun symp-trivial2 (n s)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (eq s 2)) (setq i (1+ i)))
    acc))
(defun symp-set (n s)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (set s 2)) (setq i (1+ i)))
    acc))
(defun symp-boundp (n s)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (boundp s)) (setq i (1+ i)))
    acc))
(defun symp-global-body (x) (setq symp-w (+ symp-w x)) symp-v)
(defun symp-call-global (n _s)
  (let ((i 0) (acc nil))
    (while (< i n) (setq acc (symp-global-body 1)) (setq i (1+ i)))
    acc))

(defvar symp-loops '(symp-trivial1 symp-value symp-trivial2 symp-set
                     symp-boundp symp-call-global))

(provide 'symbols-globals-functions)
