;;; map.el --- lightweight standard map facade for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Vendor Emacs Lisp commonly requires `map' for alists, plists,
;; hash-tables, and arrays.  The full vendor map.el routes through
;; cl-generic/gv/pcase support that is heavier than the current
;; standalone path needs, so this file provides the common map API
;; directly over the standard data shapes.

;;; Code:

(require 'seq)

(if (fboundp 'define-error)
    (define-error 'map-not-inplace "Cannot modify map in-place")
  (put 'map-not-inplace 'error-conditions '(map-not-inplace error))
  (put 'map-not-inplace 'error-message "Cannot modify map in-place"))

(defun map--plist-p (list)
  "Return non-nil if LIST is a nonempty plist map."
  (and (consp list) (atom (car list))))

(defun map--plist-member (plist prop &optional predicate)
  "Return the tail of PLIST whose key matches PROP."
  (let ((test (or predicate #'eq))
        tail
        found)
    (setq tail plist)
    (while (and (consp tail) (consp (cdr tail)) (not found))
      (if (funcall test (car tail) prop)
          (setq found tail)
        (setq tail (cddr tail))))
    found))

(defun map--alist-cell (alist key &optional testfn)
  "Return ALIST cell whose key matches KEY."
  (let ((test (or testfn #'equal))
        found)
    (while (and alist (not found))
      (when (funcall test (caar alist) key)
        (setq found (car alist)))
      (setq alist (cdr alist)))
    found))

(defun map--array-p (object)
  "Return non-nil when OBJECT is an array map."
  (or (vectorp object) (stringp object)))

(defun map--array-key-p (array key)
  "Return non-nil if KEY is a valid index into ARRAY."
  (and (integerp key) (>= key 0) (< key (length array))))

(defun mapp (map)
  "Return non-nil when MAP is an alist/plist, hash-table, or array."
  (or (listp map) (hash-table-p map) (map--array-p map)))

(defun map-elt (map key &optional default testfn)
  "Look up KEY in MAP and return its value, or DEFAULT."
  (cond
   ((hash-table-p map) (gethash key map default))
   ((map--array-p map)
    (if (map--array-key-p map key) (aref map key) default))
   ((listp map)
    (if (map--plist-p map)
        (let ((tail (map--plist-member map key testfn)))
          (if tail (cadr tail) default))
      (let ((cell (map--alist-cell map key testfn)))
        (if cell (cdr cell) default))))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defmacro map-put (map key value &optional testfn)
  "Associate KEY with VALUE in MAP and return VALUE."
  `(map-put! ,map ,key ,value ,testfn))

(defun map--plist-put-existing (plist key value &optional testfn)
  "Set existing KEY in PLIST to VALUE and return VALUE."
  (let ((tail (map--plist-member plist key testfn)))
    (unless tail
      (signal 'map-not-inplace (list plist)))
    (setcar (cdr tail) value)
    value))

(defun map-put! (map key value &optional testfn)
  "Associate KEY with VALUE in MAP in-place and return VALUE."
  (cond
   ((hash-table-p map) (puthash key value map))
   ((map--array-p map)
    (unless (map--array-key-p map key)
      (signal 'map-not-inplace (list map)))
    (aset map key value))
   ((listp map)
    (if (map--plist-p map)
        (map--plist-put-existing map key value testfn)
      (let ((cell (map--alist-cell map key testfn)))
        (unless cell
          (signal 'map-not-inplace (list map)))
        (setcdr cell value)
        value)))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defalias 'map--put #'map-put!)

(defun map-delete (map key)
  "Delete KEY from MAP and return the resulting map."
  (cond
   ((hash-table-p map) (remhash key map) map)
   ((map--array-p map)
    (when (map--array-key-p map key) (aset map key nil))
    map)
   ((listp map)
    (if (map--plist-p map)
        (let (out)
          (while map
            (unless (eq (car map) key)
              (push (car map) out)
              (push (cadr map) out))
            (setq map (cddr map)))
          (nreverse out))
      (let (out)
        (dolist (cell map)
          (unless (equal (car cell) key)
            (push cell out)))
        (nreverse out))))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defun map-nested-elt (map keys &optional default)
  "Traverse MAP using KEYS and return the found value, or DEFAULT."
  (let ((value map)
        missing)
    (while (and keys (not missing))
      (if (mapp value)
          (let ((sentinel (list nil)))
            (setq value (map-elt value (car keys) sentinel))
            (when (eq value sentinel)
              (setq missing t)))
        (setq missing t))
      (setq keys (cdr keys)))
    (if missing default value)))

(defun map-do (function map)
  "Call FUNCTION for every key/value pair in MAP and return nil."
  (cond
   ((hash-table-p map) (maphash function map))
   ((map--array-p map)
    (let ((i 0)
          (n (length map)))
      (while (< i n)
        (funcall function i (aref map i))
        (setq i (1+ i)))))
   ((listp map)
    (if (map--plist-p map)
        (while map
          (funcall function (car map) (cadr map))
          (setq map (cddr map)))
      (dolist (cell map)
        (funcall function (car cell) (cdr cell)))))
   (t (signal 'wrong-type-argument (list 'mapp map))))
  nil)

(defun map-apply (function map)
  "Return a list of FUNCTION applied to each key/value pair in MAP."
  (let (out)
    (map-do (lambda (key value)
              (push (funcall function key value) out))
            map)
    (nreverse out)))

(defun map-keys (map)
  "Return MAP's keys as a list."
  (map-apply (lambda (key _value) key) map))

(defun map-values (map)
  "Return MAP's values as a list."
  (map-apply (lambda (_key value) value) map))

(defun map-pairs (map)
  "Return MAP as an alist."
  (map-apply #'cons map))

(defun map-length (map)
  "Return the number of key/value pairs in MAP."
  (cond
   ((hash-table-p map) (hash-table-count map))
   ((map--array-p map) (length map))
   ((listp map) (if (map--plist-p map) (/ (length map) 2) (length map)))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defun map-copy (map)
  "Return a shallow copy of MAP."
  (cond
   ((hash-table-p map) (copy-hash-table map))
   ((listp map) (copy-tree map))
   ((map--array-p map) (copy-sequence map))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defun map-keys-apply (function map)
  "Return the result of applying FUNCTION to each key in MAP."
  (map-apply (lambda (key _value) (funcall function key)) map))

(defun map-values-apply (function map)
  "Return the result of applying FUNCTION to each value in MAP."
  (map-apply (lambda (_key value) (funcall function value)) map))

(defun map-filter (pred map)
  "Return an alist of key/value pairs for which PRED is non-nil."
  (let (out)
    (map-do (lambda (key value)
              (when (funcall pred key value)
                (push (cons key value) out)))
            map)
    (nreverse out)))

(defun map-remove (pred map)
  "Return an alist of key/value pairs for which PRED is nil."
  (map-filter (lambda (key value) (not (funcall pred key value))) map))

(defun map-empty-p (map)
  "Return non-nil when MAP has no entries."
  (= (map-length map) 0))

(defun map-contains-key (map key &optional testfn)
  "Return non-nil when MAP contains KEY."
  (cond
   ((hash-table-p map)
    (let ((sentinel (list nil)))
      (not (eq (gethash key map sentinel) sentinel))))
   ((map--array-p map) (map--array-key-p map key))
   ((listp map)
    (if (map--plist-p map)
        (and (map--plist-member map key testfn) t)
      (and (map--alist-cell map key testfn) t)))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defun map-some (pred map)
  "Return the first non-nil value from applying PRED to MAP."
  (catch 'found
    (map-do (lambda (key value)
              (let ((result (funcall pred key value)))
                (when result (throw 'found result))))
            map)
    nil))

(defun map-every-p (pred map)
  "Return non-nil when PRED returns non-nil for every pair in MAP."
  (catch 'failed
    (map-do (lambda (key value)
              (unless (funcall pred key value)
                (throw 'failed nil)))
            map)
    t))

(defun map--into-hash (map args)
  "Convert MAP to a hash-table, forwarding ARGS to `make-hash-table'."
  (let ((table (apply #'make-hash-table args)))
    (map-do (lambda (key value) (puthash key value table)) map)
    table))

(defun map-into (map type)
  "Convert MAP into TYPE."
  (cond
   ((or (eq type 'list) (eq type 'alist)) (map-pairs map))
   ((eq type 'plist)
      (let (out)
      (map-do (lambda (key value)
                (push key out)
                (push value out))
              map)
      (nreverse out)))
   ((eq type 'hash-table)
    (map--into-hash map (list :test #'equal :size (map-length map))))
   ((and (consp type) (eq (car type) 'hash-table))
    (map--into-hash map (cdr type)))
   (t (signal 'wrong-type-argument (list 'type-specifier-p type)))))

(defun map-insert (map key value)
  "Return a new map like MAP with KEY associated to VALUE."
  (cond
   ((hash-table-p map)
    (let ((copy (copy-hash-table map)))
      (puthash key value copy)
      copy))
   ((map--array-p map)
    (let* ((len (length map))
           (size (max len (1+ key)))
           (copy (make-vector size nil))
           (i 0))
      (while (< i len)
        (aset copy i (aref map i))
        (setq i (1+ i)))
      (aset copy key value)
      copy))
   ((listp map)
    (if (map--plist-p map)
        (cons key (cons value map))
      (cons (cons key value) map)))
   (t (signal 'wrong-type-argument (list 'mapp map)))))

(defun map--merge-to-table (function maps test)
  "Merge MAPS into a hash table using FUNCTION for duplicate values."
  (let ((table (make-hash-table :test test))
        order)
    (dolist (map maps)
      (map-do (lambda (key value)
                (let ((sentinel (list nil)))
                  (let ((old (gethash key table sentinel)))
                    (when (eq old sentinel)
                      (push key order))
                    (puthash key
                             (if (eq old sentinel)
                                 value
                               (funcall function old value))
                             table))))
              map))
    (list table (nreverse order))))

(defun map--table-into (table order type)
  "Convert TABLE with insertion ORDER into TYPE."
  (cond
   ((or (eq type 'list) (eq type 'alist))
    (mapcar (lambda (key) (cons key (gethash key table))) order))
   ((eq type 'plist)
    (let (out)
      (dolist (key order)
        (push key out)
        (push (gethash key table) out))
      (nreverse out)))
   ((or (eq type 'hash-table)
        (and (consp type) (eq (car type) 'hash-table)))
    table)
   (t (map-into (map--table-into table order 'alist) type))))

(defun map-merge (type &rest maps)
  "Merge MAPS into a map of TYPE.  Later MAPS override earlier ones."
  (let* ((test (if (eq type 'plist) #'eq #'equal))
         (merged (map--merge-to-table (lambda (_old new) new) maps test)))
    (map--table-into (car merged) (cadr merged) type)))

(defun map-merge-with (type function &rest maps)
  "Merge MAPS into TYPE, combining duplicate values with FUNCTION."
  (let* ((test (if (eq type 'plist) #'eq #'equal))
         (merged (map--merge-to-table function maps test)))
    (map--table-into (car merged) (cadr merged) type)))

(provide 'map)

;;; map.el ends here
