;;; nl-typecheck-types.el --- type relation for nl-typecheck -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 236 type language: atomic GNU types, (or ...) (and ...) (not ...)
;; (member ...) (eql V) (integer LOW HIGH) (float LOW HIGH),
;; (list-of T) (vector-of T) and (function ...).  A type is mapped to a
;; denotation (LEAVES INTERVALS): LEAVES is a set of value kinds (plus
;; (:val . V) singleton leaves from `member'), INTERVALS a sorted list of
;; disjoint (LOW . HIGH) fixnum ranges.  Every relation is conservative:
;; when a type cannot be denoted (`satisfies', `not' of a `member', an
;; unknown atom) the relation answers nil, i.e. "not proven", so the
;; checker can never raise an error from a type it does not understand.
;; `t' is the dynamic type: it is consistent with every type both ways.
;; Plain Elisp only (no cl-lib) so it also loads on standalone NeLisp.

;;; Code:

(require 'nl-static)

(defconst nl-typecheck-types-min-fixnum -2305843009213693952
  "Lower bound of the fixnum interval.")
(defconst nl-typecheck-types-max-fixnum 2305843009213693951
  "Upper bound of the fixnum interval.")

(defconst nl-typecheck--leaves
  '(null true keyword symbol-other bignum float string cons vector
         bool-vector char-table hash-table marker buffer window frame
         obarray subr closure other)
  "All value-kind leaves; the union of every leaf is the type t.")

(defun nl-typecheck--full-ints ()
  "Return the interval list covering all fixnums."
  (list (cons nl-typecheck-types-min-fixnum nl-typecheck-types-max-fixnum)))

(defun nl-typecheck--norm-ints (ints)
  "Sort INTS, drop empty ranges and merge touching ones."
  (let ((sorted nil) (out nil))
    (dolist (r ints)
      (when (<= (car r) (cdr r)) (setq sorted (cons r sorted))))
    (setq sorted (sort sorted (lambda (a b) (< (car a) (car b)))))
    (dolist (r sorted)
      (if (and out (<= (car r) (1+ (cdr (car out)))))
          (when (> (cdr r) (cdr (car out)))
            (setq out (cons (cons (car (car out)) (cdr r)) (cdr out))))
        (setq out (cons (cons (car r) (cdr r)) out))))
    (nreverse out)))

(defun nl-typecheck--ints-inter (a b)
  "Intersect interval lists A and B."
  (let ((out nil))
    (dolist (x a)
      (dolist (y b)
        (let ((lo (max (car x) (car y))) (hi (min (cdr x) (cdr y))))
          (when (<= lo hi) (setq out (cons (cons lo hi) out))))))
    (nl-typecheck--norm-ints out)))

(defun nl-typecheck--ints-comp (a)
  "Complement interval list A inside the fixnum range."
  (let ((out nil) (next nl-typecheck-types-min-fixnum))
    (dolist (r (nl-typecheck--norm-ints a))
      (when (< next (car r)) (setq out (cons (cons next (1- (car r))) out)))
      (setq next (1+ (cdr r))))
    (when (<= next nl-typecheck-types-max-fixnum)
      (setq out (cons (cons next nl-typecheck-types-max-fixnum) out)))
    (nreverse out)))

(defun nl-typecheck--ints-subset (a b)
  "Non-nil when every range of A lies inside one range of B."
  (let ((ok t))
    (dolist (x a)
      (let ((hit nil))
        (dolist (y b)
          (when (and (>= (car x) (car y)) (<= (cdr x) (cdr y)))
            (setq hit t)))
        (unless hit (setq ok nil))))
    ok))

(defun nl-typecheck--den (leaves ints)
  "Build a denotation from LEAVES and INTS."
  (list leaves (nl-typecheck--norm-ints ints)))

(defun nl-typecheck--val-leaf-p (leaf)
  "Non-nil when LEAF is a (:val . V) singleton."
  (and (consp leaf) (eq (car leaf) :val)))

(defun nl-typecheck--value-kind (value)
  "Return the kind leaf of VALUE, or nil when not representable."
  (cond ((null value) 'null)
        ((eq value t) 'true)
        ((keywordp value) 'keyword)
        ((symbolp value) 'symbol-other)
        ((stringp value) 'string)
        ((floatp value) 'float)
        (t nil)))

(defun nl-typecheck--leaf-in (leaf leaves)
  "Non-nil when LEAF is covered by LEAVES."
  (or (member leaf leaves)
      (and (nl-typecheck--val-leaf-p leaf)
           (memq (nl-typecheck--value-kind (cdr leaf)) leaves))))

(defun nl-typecheck--den-empty-p (d)
  "Non-nil when denotation D has no values."
  (and (null (car d)) (null (car (cdr d)))))

(defun nl-typecheck--union (a b)
  "Union of denotations A and B."
  (let ((leaves (copy-sequence (car a))))
    (dolist (l (car b))
      (unless (member l leaves) (setq leaves (cons l leaves))))
    (nl-typecheck--den leaves (append (car (cdr a)) (car (cdr b))))))

(defun nl-typecheck--inter (a b)
  "Intersection of denotations A and B."
  (let ((leaves nil))
    (dolist (l (car a))
      (when (nl-typecheck--leaf-in l (car b)) (setq leaves (cons l leaves))))
    (dolist (l (car b))
      (when (and (nl-typecheck--val-leaf-p l)
                 (not (member l leaves))
                 (nl-typecheck--leaf-in l (car a)))
        (setq leaves (cons l leaves))))
    (nl-typecheck--den
     leaves (nl-typecheck--ints-inter (car (cdr a)) (car (cdr b))))))

(defun nl-typecheck--comp (a)
  "Complement of denotation A, or :unsupported with singleton leaves."
  (let ((bad nil))
    (dolist (l (car a)) (when (nl-typecheck--val-leaf-p l) (setq bad t)))
    (if bad
        :unsupported
      (let ((leaves nil))
        (dolist (l nl-typecheck--leaves)
          (unless (memq l (car a)) (setq leaves (cons l leaves))))
        (nl-typecheck--den leaves
                           (nl-typecheck--ints-comp (car (cdr a))))))))

(defun nl-typecheck--subset (a b)
  "Non-nil when denotation A is contained in denotation B."
  (let ((ok t))
    (dolist (l (car a))
      (unless (nl-typecheck--leaf-in l (car b)) (setq ok nil)))
    (and ok (nl-typecheck--ints-subset (car (cdr a)) (car (cdr b))))))

(defun nl-typecheck--all ()
  "Denotation of t."
  (nl-typecheck--den (copy-sequence nl-typecheck--leaves)
                     (nl-typecheck--full-ints)))

(defun nl-typecheck--atom-den (type)
  "Return the denotation of atomic TYPE, or :unsupported."
  (let ((ints (nl-typecheck--full-ints)))
    (cond
     ((eq type t) (nl-typecheck--all))
     ((null type) (nl-typecheck--den nil nil))
     ((eq type 'null) (nl-typecheck--den '(null) nil))
     ((eq type 'boolean) (nl-typecheck--den '(null true) nil))
     ((eq type 'symbol)
      (nl-typecheck--den '(null true keyword symbol-other) nil))
     ((eq type 'keyword) (nl-typecheck--den '(keyword) nil))
     ((eq type 'fixnum) (nl-typecheck--den nil ints))
     ((eq type 'integer) (nl-typecheck--den '(bignum) ints))
     ((eq type 'bignum) (nl-typecheck--den '(bignum) nil))
     ((eq type 'natnum)
      (nl-typecheck--den '(bignum) (list (cons 0 nl-typecheck-types-max-fixnum))))
     ((eq type 'character) (nl-typecheck--den nil (list (cons 0 4194303))))
     ((eq type 'float) (nl-typecheck--den '(float) nil))
     ((memq type '(number real)) (nl-typecheck--den '(bignum float) ints))
     ((memq type '(integer-or-marker))
      (nl-typecheck--den '(bignum marker) ints))
     ((memq type '(number-or-marker))
      (nl-typecheck--den '(bignum float marker) ints))
     ((memq type '(string cons vector bool-vector char-table hash-table
                   marker buffer window frame obarray subr))
      (nl-typecheck--den (list type) nil))
     ((memq type '(list proper-list)) (nl-typecheck--den '(null cons) nil))
     ((eq type 'array)
      (nl-typecheck--den '(string vector bool-vector char-table) nil))
     ((eq type 'sequence)
      (nl-typecheck--den
       '(null cons string vector bool-vector char-table) nil))
     ((eq type 'atom)
      (let ((leaves nil))
        (dolist (l nl-typecheck--leaves)
          (unless (eq l 'cons) (setq leaves (cons l leaves))))
        (nl-typecheck--den leaves ints)))
     ;; A function designator may be a subr, a closure, a symbol or a
     ;; lambda list; stay wide so disjointness is never over-claimed.
     ((eq type 'function)
      (nl-typecheck--den
       '(subr closure true keyword symbol-other cons) nil))
     (t :unsupported))))

(defun nl-typecheck--bound (x default)
  "Return X, or DEFAULT when X is `*' or nil."
  (if (or (null x) (eq x '*)) default x))

(defun nl-typecheck--member-den (values)
  "Denotation of (member . VALUES), or :unsupported."
  (let ((leaves nil) (ints nil) (bad nil))
    (dolist (v values)
      (cond ((integerp v)
             (if (and (>= v nl-typecheck-types-min-fixnum)
                      (<= v nl-typecheck-types-max-fixnum))
                 (setq ints (cons (cons v v) ints))
               (setq bad t)))
            ((memq v '(nil t)) (setq leaves (cons (nl-typecheck--value-kind v) leaves)))
            ((nl-typecheck--value-kind v)
             (setq leaves (cons (cons :val v) leaves)))
            (t (setq bad t))))
    (if bad :unsupported (nl-typecheck--den leaves ints))))

(defun nl-typecheck--den-of (type)
  "Return the denotation of TYPE, or :unsupported when not expressible."
  (cond
   ((not (consp type)) (nl-typecheck--atom-den type))
   ((eq (car type) 'or)
    (let ((d (nl-typecheck--den nil nil)))
      (dolist (x (cdr type))
        (let ((dx (if (eq d :unsupported) :unsupported
                    (nl-typecheck--den-of x))))
          (setq d (if (or (eq d :unsupported) (eq dx :unsupported))
                      :unsupported
                    (nl-typecheck--union d dx)))))
      d))
   ((eq (car type) 'and)
    (let ((d (nl-typecheck--all)))
      (dolist (x (cdr type))
        (let ((dx (if (eq d :unsupported) :unsupported
                    (nl-typecheck--den-of x))))
          (setq d (if (or (eq d :unsupported) (eq dx :unsupported))
                      :unsupported
                    (nl-typecheck--inter d dx)))))
      d))
   ((and (eq (car type) 'not) (consp (cdr type)))
    (let ((d (nl-typecheck--den-of (car (cdr type)))))
      (if (eq d :unsupported) d (nl-typecheck--comp d))))
   ((eq (car type) 'member) (nl-typecheck--member-den (cdr type)))
   ((and (eq (car type) 'eql) (consp (cdr type)))
    (nl-typecheck--member-den (list (car (cdr type)))))
   ((and (eq (car type) 'integer) (listp (cdr type)))
    (let ((lo (nl-typecheck--bound (car (cdr type)) nil))
          (hi (nl-typecheck--bound (car (cdr (cdr type))) nil)))
      (if (and (or (null lo) (integerp lo)) (or (null hi) (integerp hi)))
          (nl-typecheck--den
           (if (null hi) '(bignum) nil)
           (list (cons (max (or lo nl-typecheck-types-min-fixnum)
                            nl-typecheck-types-min-fixnum)
                       (min (or hi nl-typecheck-types-max-fixnum)
                            nl-typecheck-types-max-fixnum))))
        :unsupported)))
   ((and (eq (car type) 'float) (listp (cdr type)))
    (nl-typecheck--den '(float) nil))
   ((memq (car type) '(list-of)) (nl-typecheck--den '(null cons) nil))
   ((memq (car type) '(vector-of)) (nl-typecheck--den '(vector) nil))
   ((eq (car type) 'function) (nl-typecheck--atom-den 'function))
   (t :unsupported)))

(defun nl-typecheck-type-known-p (type)
  "Non-nil when the type language can express TYPE."
  (and (not (eq (nl-typecheck--den-of type) :unsupported))
       (let ((ok t))
         (when (and (consp type) (memq (car type) '(or and not)))
           (dolist (x (cdr type))
             (unless (nl-typecheck-type-known-p x) (setq ok nil))))
         (when (and (consp type) (memq (car type) '(list-of vector-of)))
           (setq ok (and (consp (cdr type))
                         (nl-typecheck-type-known-p (car (cdr type))))))
         ok)))

(defun nl-typecheck-subtype-p (sub super)
  "Non-nil only when SUB is provably a subtype of SUPER."
  (cond
   ((eq super t) t)
   ((equal sub super) t)
   ((eq sub t) nil)
   (t (let ((a (nl-typecheck--den-of sub))
            (b (nl-typecheck--den-of super)))
        (and (not (eq a :unsupported)) (not (eq b :unsupported))
             (nl-typecheck--subset a b)
             (nl-typecheck--elem-ok sub super))))))

(defun nl-typecheck--elem-ok (sub super)
  "Check element types when both SUB and SUPER are list-of/vector-of."
  (if (and (consp super) (memq (car super) '(list-of vector-of)))
      (cond ((and (consp sub) (eq (car sub) (car super)))
             (nl-typecheck-subtype-p (car (cdr sub)) (car (cdr super))))
            ((and (consp sub) (memq (car sub) '(list-of vector-of))) nil)
            ;; Unconstrained list/vector/null is not provably a subtype.
            ((eq sub 'null) t)
            (t nil))
    t))

(defun nl-typecheck-disjoint-p (left right)
  "Non-nil only when LEFT and RIGHT provably share no value."
  (cond
   ((or (eq left t) (eq right t)) nil)
   ((null left) t)
   ((null right) t)
   ((and (symbolp left) (symbolp right)
         (nl-static-types-disjoint-p left right))
    t)
   (t (let ((a (nl-typecheck--den-of left))
            (b (nl-typecheck--den-of right)))
        (and (not (eq a :unsupported)) (not (eq b :unsupported))
             (nl-typecheck--den-empty-p (nl-typecheck--inter a b)))))))

(defun nl-typecheck-consistent-p (left right)
  "Gradual consistency: nil only when LEFT and RIGHT are provably clashing.
`t' is consistent with everything in both directions."
  (not (nl-typecheck-disjoint-p left right)))

(defun nl-typecheck--flatten (head args)
  "Flatten nested HEAD forms among normalized ARGS."
  (let ((out nil))
    (dolist (x args)
      (if (and (consp x) (eq (car x) head))
          (dolist (y (cdr x)) (setq out (cons y out)))
        (setq out (cons x out))))
    (nreverse out)))

(defun nl-typecheck-normalize (type)
  "Return a canonical form of TYPE.
Flattens or/and, removes duplicates and subsumed members, collapses
`t'/`nil' absorbers and singletons, and sorts members for determinism."
  (cond
   ((not (consp type)) type)
   ((memq (car type) '(or and))
    (let* ((head (car type))
           (args (nl-typecheck--flatten
                  head (mapcar #'nl-typecheck-normalize (cdr type))))
           (absorb (if (eq head 'or) t nil))
           (unit (if (eq head 'or) nil t))
           (uniq nil))
      (dolist (x args)
        (unless (or (equal x unit) (member x uniq))
          (setq uniq (cons x uniq))))
      (cond
       ((member absorb uniq) absorb)
       (t
        (let ((kept nil))
          (dolist (x uniq)
            (let ((dominated nil))
              (dolist (y uniq)
                (when (and (not (equal x y))
                           (if (eq head 'or)
                               (nl-typecheck-subtype-p x y)
                             (nl-typecheck-subtype-p y x))
                           ;; Break ties between mutual subtypes.
                           (or (not (nl-typecheck-subtype-p
                                     (if (eq head 'or) y x)
                                     (if (eq head 'or) x y)))
                               (string< (prin1-to-string y)
                                        (prin1-to-string x))))
                  (setq dominated t)))
              (unless dominated (setq kept (cons x kept)))))
          (setq kept (sort kept (lambda (a b)
                                  (string< (prin1-to-string a)
                                           (prin1-to-string b)))))
          (cond ((null kept) unit)
                ((null (cdr kept)) (car kept))
                (t (cons head kept))))))))
   ((and (eq (car type) 'not) (consp (cdr type)))
    (let ((inner (nl-typecheck-normalize (car (cdr type)))))
      (cond ((eq inner t) nil)
            ((null inner) t)
            ((and (consp inner) (eq (car inner) 'not)) (car (cdr inner)))
            (t (list 'not inner)))))
   ((and (eq (car type) 'integer)
         (memq (car (cdr type)) '(nil *))
         (memq (car (cdr (cdr type))) '(nil *)))
    'integer)
   (t type)))

(provide 'nl-typecheck-types)
;;; nl-typecheck-types.el ends here
