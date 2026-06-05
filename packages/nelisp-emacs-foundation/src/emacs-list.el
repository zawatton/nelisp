;;; emacs-list.el --- NeLisp port of Emacs C core list accessors  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2.1 — Layer 2.
;;
;; Ports the list-accessor primitives from `fns.c' / `data.c' that
;; subr.el and most Elisp libraries assume.  Polyfills use only
;; bootstrap-eval primitives + the earlier `emacs-fns' helpers (=
;; mapcar / nreverse).  Each is gated on `unless (fboundp ...)`.

;;; Code:

;;;; --- safe cons access ---------------------------------------------------

(unless (fboundp 'car-safe)
  (defun car-safe (object)
    "Return the car of OBJECT if it is a cons cell, otherwise nil."
    (if (consp object) (car object) nil)))

(unless (fboundp 'cdr-safe)
  (defun cdr-safe (object)
    "Return the cdr of OBJECT if it is a cons cell, otherwise nil."
    (if (consp object) (cdr object) nil)))

;;;; --- copy / shape -------------------------------------------------------

(unless (fboundp 'copy-sequence)
  (defun copy-sequence (sequence)
    "Return a shallow copy of SEQUENCE (= proper-list polyfill)."
    (let ((acc nil)
          (cur sequence))
      (while cur
        (setq acc (cons (car cur) acc))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'copy-tree)
  (defun copy-tree (tree &optional vecp)
    "Return a recursive copy of TREE.  Conses are recursively copied."
    (ignore vecp)
    (cond
     ((not (consp tree)) tree)
     (t (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))))

(unless (fboundp 'last)
  (defun last (list &optional n)
    "Return the last cons-cell of LIST (= a list of length N at the tail).
N defaults to 1.  Linear walk."
    (let ((n (or n 1))
          (len 0)
          (cur list))
      (while cur (setq len (+ len 1)) (setq cur (cdr cur)))
      (let ((skip (- len n))
            (c list))
        (while (and c (> skip 0))
          (setq c (cdr c))
          (setq skip (- skip 1)))
        c))))

(unless (fboundp 'butlast)
  (defun butlast (list &optional n)
    "Return a copy of LIST with the last N elements removed (default 1)."
    (let* ((n (or n 1))
           (len 0)
           (cur list))
      (while cur (setq len (+ len 1)) (setq cur (cdr cur)))
      (let ((keep (- len n))
            (acc nil)
            (c list))
        (while (> keep 0)
          (setq acc (cons (car c) acc))
          (setq c (cdr c))
          (setq keep (- keep 1)))
        (nreverse acc)))))


;;;; --- positional access --------------------------------------------------

(unless (fboundp 'nth)
  (defun nth (n list)
    "Return Nth element of LIST (0-indexed), or nil if out of range."
    (let ((c list))
      (while (and c (> n 0))
        (setq c (cdr c))
        (setq n (- n 1)))
      (car c))))

(unless (fboundp 'nthcdr)
  (defun nthcdr (n list)
    "Return the Nth cdr of LIST, or nil."
    (let ((c list))
      (while (and c (> n 0))
        (setq c (cdr c))
        (setq n (- n 1)))
      c)))

(unless (fboundp 'cadr)
  (defun cadr (list) "Return (car (cdr LIST))." (car (cdr list))))

(unless (fboundp 'cddr)
  (defun cddr (list) "Return (cdr (cdr LIST))." (cdr (cdr list))))

(unless (fboundp 'caddr)
  (defun caddr (list) "Return (car (cdr (cdr LIST)))." (car (cdr (cdr list)))))


;;;; --- membership ---------------------------------------------------------

(unless (fboundp 'memq)
  (defun memq (element list)
    "Return tail of LIST whose car is `eq' to ELEMENT, or nil."
    (let ((c list)
          (found nil))
      (while (and c (not found))
        (if (eq (car c) element) (setq found c)
          (setq c (cdr c))))
      found)))

(unless (fboundp 'member)
  (defun member (element list)
    "Return tail of LIST whose car is `equal' to ELEMENT, or nil."
    (let ((c list)
          (found nil))
      (while (and c (not found))
        (if (equal (car c) element) (setq found c)
          (setq c (cdr c))))
      found)))


;;;; --- alist access -------------------------------------------------------

(unless (fboundp 'assq)
  (defun assq (key alist)
    "Return first cons-cell in ALIST whose car is `eq' to KEY."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell) (eq (car cell) key))
              (setq found cell)
            (setq c (cdr c)))))
      found)))

(unless (fboundp 'assoc)
  (defun assoc (key alist &optional testfn)
    "Return first cons-cell in ALIST whose car is `equal' to KEY.
TESTFN, when supplied, is called as (TESTFN KEY CAR)."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell)
                   (if testfn
                       (funcall testfn key (car cell))
                     (equal key (car cell))))
              (setq found cell)
            (setq c (cdr c)))))
      found)))

(unless (fboundp 'rassq)
  (defun rassq (value alist)
    "Return first cons-cell in ALIST whose cdr is `eq' to VALUE."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell) (eq (cdr cell) value))
              (setq found cell)
            (setq c (cdr c)))))
      found)))

(unless (fboundp 'rassoc)
  (defun rassoc (value alist)
    "Return first cons-cell in ALIST whose cdr is `equal' to VALUE."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell) (equal (cdr cell) value))
              (setq found cell)
            (setq c (cdr c)))))
      found)))


;;;; --- removal ------------------------------------------------------------

(unless (fboundp 'delq)
  (defun delq (element list)
    "Return LIST with all `eq'-matching ELEMENT cells removed (copy)."
    (let ((acc nil)
          (cur list))
      (while cur
        (unless (eq (car cur) element)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'delete)
  (defun delete (element list)
    "Return LIST with all `equal'-matching ELEMENT cells removed (copy)."
    (let ((acc nil)
          (cur list))
      (while cur
        (unless (equal (car cur) element)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'remq)
  (defalias 'remq 'delq))

(unless (fboundp 'remove)
  (defalias 'remove 'delete))


;;;; --- length helpers -----------------------------------------------------

(unless (fboundp 'safe-length)
  (defun safe-length (list)
    "Return the length of LIST, treating dotted-pair tails as terminators."
    (let ((n 0)
          (c list))
      (while (consp c)
        (setq n (+ n 1))
        (setq c (cdr c)))
      n)))


;;;; --- alist deletion --------------------------------------------------------

(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    "Return ALIST with all entries whose car is `eq' to KEY removed.
Non-cons elements are preserved.  The returned list shares
structure with the tail of ALIST after the last removed cell."
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (eq (car cell) key))
          (push cell out)))
      (nreverse out))))

(unless (fboundp 'assoc-delete-all)
  (defun assoc-delete-all (key alist &optional test)
    "Return ALIST with all entries whose car matches KEY removed.
TEST defaults to `equal'."
    (unless test (setq test #'equal))
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (funcall test (car cell) key))
          (push cell out)))
      (nreverse out))))

(unless (fboundp 'rassoc-delete-all)
  (defun rassoc-delete-all (value alist)
    "Return ALIST with all entries whose cdr is `equal' to VALUE removed."
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (equal (cdr cell) value))
          (push cell out)))
      (nreverse out))))

(provide 'emacs-list)

;;; emacs-list.el ends here
