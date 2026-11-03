;;; emacs-hash.el --- NeLisp port of Emacs hash-table API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2.1 — Layer 2.
;;
;; Ports the hash-table primitives Emacs ships from `fns.c'
;; (`make-hash-table', `gethash', `puthash', `remhash', `clrhash',
;; `hash-table-count', `hash-table-keys', `maphash', `hash-table-p').
;;
;; Polyfill strategy: maintain a flat alist behind a tagged cons.
;; This is O(N) per access — fine for the tens-of-entries hash tables
;; anvil modules typically use; not appropriate for performance-critical
;; paths.  Phase 3 (= when NeLisp ships native hash tables) drops this
;; file entirely.

;;; Code:

(defun emacs-hash--make (test)
  "Internal: build a tagged hash-table polyfill object."
  (cons 'emacs-hash-table (cons test nil)))

(defun emacs-hash--test (table)  (car (cdr table)))
(defun emacs-hash--alist (table) (cdr (cdr table)))
(defun emacs-hash--set-alist (table alist)
  (setcdr (cdr table) alist))

(unless (fboundp 'make-hash-table)
  (defun make-hash-table (&rest args)
    "Polyfill: create an empty hash-table-like object backed by an alist.
Accepts &rest keyword args; only `:test' is honoured."
    (let ((test 'eql)
          (rest args))
      (while rest
        (when (eq (car rest) :test)
          (setq test (car (cdr rest))))
        (setq rest (cdr (cdr rest))))
      (emacs-hash--make test))))

(unless (fboundp 'hash-table-p)
  (defun hash-table-p (object)
    (and (consp object) (eq (car object) 'emacs-hash-table))))

(defun emacs-hash--cmp (test a b)
  (cond
   ((eq test 'eq)    (eq a b))
   ((eq test 'eql)   (or (eq a b) (equal a b)))
   ((eq test 'equal) (equal a b))
   (t (funcall test a b))))

(unless (fboundp 'gethash)
  (defun gethash (key table &optional default)
    (let ((alist (emacs-hash--alist table))
          (test (emacs-hash--test table))
          (found nil)
          (result default))
      (while (and alist (not found))
        (let ((cell (car alist)))
          (if (emacs-hash--cmp test (car cell) key)
              (progn (setq result (cdr cell))
                     (setq found t))
            (setq alist (cdr alist)))))
      result)))

(unless (fboundp 'puthash)
  (defun puthash (key value table)
    (let* ((alist (emacs-hash--alist table))
           (test (emacs-hash--test table))
           (acc nil)
           (replaced nil)
           (cur alist))
      (while cur
        (let ((cell (car cur)))
          (if (and (not replaced) (emacs-hash--cmp test (car cell) key))
              (progn (setq acc (cons (cons key value) acc))
                     (setq replaced t))
            (setq acc (cons cell acc))))
        (setq cur (cdr cur)))
      (let ((new (if replaced
                     (nreverse acc)
                   (cons (cons key value) (nreverse acc)))))
        (emacs-hash--set-alist table new))
      value)))

(unless (fboundp 'remhash)
  (defun remhash (key table)
    (let* ((alist (emacs-hash--alist table))
           (test (emacs-hash--test table))
           (acc nil)
           (cur alist))
      (while cur
        (let ((cell (car cur)))
          (unless (emacs-hash--cmp test (car cell) key)
            (setq acc (cons cell acc))))
        (setq cur (cdr cur)))
      (emacs-hash--set-alist table (nreverse acc))
      nil)))

(unless (fboundp 'clrhash)
  (defun clrhash (table)
    (emacs-hash--set-alist table nil)
    table))

(unless (fboundp 'hash-table-count)
  (defun hash-table-count (table)
    (let ((alist (emacs-hash--alist table))
          (n 0))
      (while alist (setq n (+ n 1)) (setq alist (cdr alist)))
      n)))

(unless (fboundp 'hash-table-keys)
  (defun hash-table-keys (table)
    (let ((alist (emacs-hash--alist table))
          (acc nil))
      (while alist
        (setq acc (cons (car (car alist)) acc))
        (setq alist (cdr alist)))
      (nreverse acc))))

(unless (fboundp 'maphash)
  (defun maphash (function table)
    (let ((alist (emacs-hash--alist table)))
      (while alist
        (let ((cell (car alist)))
          (funcall function (car cell) (cdr cell)))
        (setq alist (cdr alist))))))


(provide 'emacs-hash)

;;; emacs-hash.el ends here
