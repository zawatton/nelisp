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

(defun emacs-hash--test (table)  (car (cdr table)))
(defun emacs-hash--alist (table) (cdr (cdr table)))
(defun emacs-hash--set-alist (table alist)
  (setcdr (cdr table) alist))

(defun emacs-hash--cmp (test a b)
  (cond
   ((eq test 'eq)    (eq a b))
   ((eq test 'eql)   (or (eq a b) (equal a b)))
   ((eq test 'equal) (equal a b))
   (t (funcall test a b))))

(unless (fboundp 'hash-table-keys)
  (defun hash-table-keys (table)
    (let ((alist (emacs-hash--alist table))
          (acc nil))
      (while alist
        (setq acc (cons (car (car alist)) acc))
        (setq alist (cdr alist)))
      (nreverse acc))))


(provide 'emacs-hash)

;;; emacs-hash.el ends here
