;;; map.el --- lightweight standard map facade for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Vendor Emacs Lisp commonly requires `map' for alists, plists,
;; hash-tables, and arrays.  The full vendor map.el routes through
;; cl-generic/gv/pcase support that is heavier than the current
;; standalone path needs, so this file provides the common map API
;; directly over the standard data shapes.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- native map-elt/map-keys reject arrays; shim handles them (audit 2026-09-29).
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

;;;; --- GNU map.el plist compatibility shims (S2 coverage batch) ---------
;;
;; This facade implements its own plist helpers above (`map--plist-p',
;; `map--plist-member', `map--plist-put-existing', ...) with different
;; names/signatures, so `map-elt' / `map-put!' / `map-delete' above do
;; not call these.  These are faithful ports of the real GNU Emacs 31.1
;; `lisp/emacs-lisp/map.el' names under their own exact names, for any
;; vendored code that calls them directly.  Pure plist algorithms; no
;; native support needed.

(unless (boundp 'map--plist-has-predicate)
  (defconst map--plist-has-predicate
    (condition-case nil
        (with-no-warnings (plist-get () nil #'eq) t)
      (wrong-number-of-arguments nil)
      (error nil))
    "Non-nil means `plist-get' & co. accept a predicate in Emacs 29+.
Note that support for this predicate in map.el is patchy and
deprecated."))

(unless (fboundp 'map--plist-member-1)
  (defun map--plist-member-1 (plist prop &optional predicate)
    "Compatibility shim for the PREDICATE argument of `plist-member'.
Assumes non-nil PLIST satisfies `map--plist-p'."
    (if (or (memq predicate '(nil eq)) (null plist))
        (plist-member plist prop)
      (let ((tail plist) found)
        (while (and (not (setq found (funcall predicate (car tail) prop)))
                    (consp (setq tail (cdr tail)))
                    (consp (setq tail (cdr tail)))))
        (and tail (not found)
             (signal 'wrong-type-argument (list 'plistp plist)))
        tail))))

(unless (fboundp 'map--plist-put-1)
  (defun map--plist-put-1 (plist prop val &optional predicate)
    "Compatibility shim for the PREDICATE argument of `plist-put'.
Assumes non-nil PLIST satisfies `map--plist-p'."
    (if (or (memq predicate '(nil eq)) (null plist))
        (plist-put plist prop val)
      (let ((tail plist) prev found)
        (while (and (consp (cdr tail))
                    (not (setq found (funcall predicate (car tail) prop)))
                    (consp (setq prev tail tail (cddr tail)))))
        (cond (found (setcar (cdr tail) val))
              (tail (signal 'wrong-type-argument (list 'plistp plist)))
              (prev (setcdr (cdr prev) (cons prop (cons val (cddr prev)))))
              ((setq plist (cons prop (cons val plist)))))
        plist))))

(unless (fboundp 'map--plist-put)
  (defalias 'map--plist-put
    (if map--plist-has-predicate #'plist-put #'map--plist-put-1)
    "Compatibility shim for `plist-put' in Emacs 29+.
\n(fn PLIST PROP VAL &optional PREDICATE)"))

(unless (fboundp 'map--plist-delete)
  (defun map--plist-delete (map key)
    "Delete KEY in-place from plist MAP and return the resulting plist."
    (let ((tail map) last)
      (while (consp tail)
        (cond
         ((not (eq key (car tail)))
          (setq last tail)
          (setq tail (cddr last)))
         (last
          (setq tail (cddr tail))
          (setf (cddr last) tail))
         (t
          (setq map (cddr map))
          (setq tail map))))
      map)))

;; The real `map' pcase pattern plus `map-let', ported verbatim from GNU
;; Emacs 31.1 lisp/emacs-lisp/map.el (only the `emacs-major-version >= 30'
;; branch of `map--make-pcase-bindings' can ever run here, since this
;; facade always reports 30+; the pre-30 branch is kept for byte-for-byte
;; parity with upstream but calls `map--pcase-map-elt', which stays
;; defined -- just unreachable -- like it is upstream on a 30+ build).
(when (fboundp 'pcase-defmacro)
  (defmacro map--pcase-map-elt (key default map)
    "A macro to make MAP the last argument to `map-elt'.

This allows using default values for `map-elt', which can't be
done using `pcase--flip'.

KEY is the key sought in the map.  DEFAULT is the default value."
    ;; It's obsolete in Emacs>29, but `map.el' is distributed via GNU ELPA
    ;; for earlier Emacsen.
    (declare (obsolete _ "30.1"))
    `(map-elt ,map ,key ,default))

  (defun map--make-pcase-bindings (args)
    "Return a list of pcase bindings from ARGS to the elements of a map."
    (mapcar (if (< emacs-major-version 30)
                (lambda (elt)
                  (cond ((consp elt)
                         `(app (map--pcase-map-elt ,(car elt) ,(caddr elt))
                               ,(cadr elt)))
                        ((keywordp elt)
                         (let ((var (intern (substring (symbol-name elt) 1))))
                           `(app (pcase--flip map-elt ,elt) ,var)))
                        (t `(app (pcase--flip map-elt ',elt) ,elt))))
              (lambda (elt)
                (cond ((consp elt)
                       `(app (map-elt _ ,(car elt) ,(caddr elt))
                             ,(cadr elt)))
                      ((keywordp elt)
                       (let ((var (intern (substring (symbol-name elt) 1))))
                         `(app (map-elt _ ,elt) ,var)))
                      (t `(app (map-elt _ ',elt) ,elt)))))
            args))

  (defun map--make-pcase-patterns (args)
    "Return a list of `(map ...)' pcase patterns built from ARGS."
    (cons 'map
          (mapcar (lambda (elt)
                    (if (eq (car-safe elt) 'map)
                        (map--make-pcase-patterns elt)
                      elt))
                  args)))

  (pcase-defmacro map (&rest args)
    "Build a `pcase' pattern matching map elements.

ARGS is a list of elements to be matched in the map.

Each element of ARGS can be of the form (KEY PAT [DEFAULT]),
which looks up KEY in the map and matches the associated value
against `pcase' pattern PAT.  DEFAULT specifies the fallback
value to use when KEY is not present in the map.  If omitted, it
defaults to nil.  Both KEY and DEFAULT are evaluated.

Each element can also be a SYMBOL, which is an abbreviation of
a (KEY PAT) tuple of the form (\\='SYMBOL SYMBOL).  When SYMBOL
is a keyword, it is an abbreviation of the form (:SYMBOL SYMBOL),
useful for binding plist values.

An element of ARGS fails to match if PAT does not match the
associated value or the default value.  The overall pattern fails
to match if any element of ARGS fails to match."
    `(and (pred mapp)
          ,@(map--make-pcase-bindings args)))

  (unless (fboundp 'map-let)
    (defmacro map-let (keys map &rest body)
      "Bind the variables in KEYS to the elements of MAP, then evaluate BODY.

KEYS can be a list of symbols, in which case each element will be
bound to the looked up value in MAP.

KEYS can also be a list of (KEY VARNAME [DEFAULT]) sublists, in
which case KEY and DEFAULT are unquoted forms.

MAP can be an alist, plist, hash-table, or array."
      (declare (indent 2)
               (debug ((&rest &or symbolp ([form symbolp &optional form]))
                       form body)))
      `(pcase-let ((,(map--make-pcase-patterns keys) ,map))
         ,@body))))

(provide 'map)

;;; map.el ends here
