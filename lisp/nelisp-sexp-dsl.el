;;; nelisp-sexp-dsl.el --- Phase 47 Sexp layout DSL (§95.a + §95.b)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 95 §95.a — freestanding constructor / accessor / predicate
;; surface for the 13 NeLisp `Sexp' variants, mirroring
;; `build-tool/src/eval/sexp.rs'.  This is the first sub-stage of the
;; Phase 47 chain (Doc 91-95) that lifts the Sexp data layer from
;; Rust into elisp.
;;
;; Scope (per Doc 95 §8 §95.a):
;;
;;   - 13 constructors (= `nelisp-sexp-make-{nil,t,int,float,symbol,
;;     str,mut-str,cons,vector,char-table,bool-vector,cell,record}')
;;   - tag accessor (= `nelisp-sexp-tag') + 13 predicates
;;   - per-variant value accessors (= int / float / str / cons / vec
;;     / record)
;;   - structural equality (= `nelisp-sexp-eq')
;;   - debug printer (= `nelisp-sexp-pp')
;;
;; OOS (= deferred to §95.b/§95.c): byte serializer, mutators, full
;; box-pointer relocation plumbing, JIT bridge.
;;
;; Representation: plist (`(:tag TAG ...)') — the doc §2.2 tagged-vec
;; choice is held back to §95.c when serialization actually matters;
;; for §95.a a plist is the simplest stepping stone and keeps tests
;; readable.  All accessors go through `plist-get' / explicit type
;; check, so swapping repr later is a local edit.

;;; Code:

;;; --- variant tag inventory ----------------------------------------

(defconst nelisp-sexp-variants
  '(nil t int float symbol str mut-str
        cons vector char-table bool-vector cell record)
  "Canonical list of the 13 `Sexp' variants from sexp.rs §1.2.
Ordering matches the `SEXP_TAG_*' discriminant byte values.")

;;; --- constructors -------------------------------------------------

(defun nelisp-sexp-make-nil ()
  "Construct the `Sexp::Nil' variant."
  (list :tag nil))

(defun nelisp-sexp-make-t ()
  "Construct the `Sexp::T' variant."
  (list :tag t))

(defun nelisp-sexp-make-int (n)
  "Construct `Sexp::Int' wrapping integer N.
Signals `wrong-type-argument' unless N is an integer."
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (list :tag 'int :value n))

(defun nelisp-sexp-make-float (f)
  "Construct `Sexp::Float' wrapping float F.
Signals `wrong-type-argument' unless F is a float."
  (unless (floatp f)
    (signal 'wrong-type-argument (list 'floatp f)))
  (list :tag 'float :value f))

(defun nelisp-sexp-make-str (s)
  "Construct `Sexp::Str' wrapping immutable string S.
Signals `wrong-type-argument' unless S is a string."
  (unless (stringp s)
    (signal 'wrong-type-argument (list 'stringp s)))
  (list :tag 'str :value s))

(defun nelisp-sexp-make-mut-str (s)
  "Construct `Sexp::MutStr' wrapping mutable string S.
Signals `wrong-type-argument' unless S is a string."
  (unless (stringp s)
    (signal 'wrong-type-argument (list 'stringp s)))
  (list :tag 'mut-str :value s))

(defun nelisp-sexp-make-symbol (name)
  "Construct `Sexp::Symbol' with name NAME.
NAME may be either a string or a symbol; it is normalised to a
string so structural equality across constructor calls is total."
  (let ((str (cond ((stringp name) name)
                   ((symbolp name) (symbol-name name))
                   (t (signal 'wrong-type-argument
                              (list 'stringp-or-symbolp name))))))
    (list :tag 'symbol :value str)))

(defun nelisp-sexp-make-cons (car cdr)
  "Construct `Sexp::Cons' with CAR / CDR (both Sexp values).
No structural type check on CAR / CDR — `nelisp-sexp-eq'
recursively walks them, so well-formedness is enforced lazily."
  (list :tag 'cons :car car :cdr cdr))

(defun nelisp-sexp-make-vector (elts)
  "Construct `Sexp::Vector' from list ELTS of Sexp values.
ELTS must be a proper list."
  (unless (listp elts)
    (signal 'wrong-type-argument (list 'listp elts)))
  (list :tag 'vector :elts elts))

(defun nelisp-sexp-make-char-table (default extras)
  "Construct `Sexp::CharTable' with DEFAULT entry + EXTRAS list.
DEFAULT is the fallback Sexp; EXTRAS is a list of Sexp values
holding the extra-slot payload."
  (unless (listp extras)
    (signal 'wrong-type-argument (list 'listp extras)))
  (list :tag 'char-table :default default :extras extras))

(defun nelisp-sexp-make-bool-vector (len init)
  "Construct `Sexp::BoolVector' of length LEN, every bit = INIT.
LEN must be a non-negative integer; INIT is coerced to boolean."
  (unless (and (integerp len) (>= len 0))
    (signal 'wrong-type-argument (list 'natnump len)))
  (list :tag 'bool-vector :len len :init (and init t)))

(defun nelisp-sexp-make-cell (inner)
  "Construct `Sexp::Cell' wrapping INNER Sexp value."
  (list :tag 'cell :inner inner))

(defun nelisp-sexp-make-record (type-sym slots)
  "Construct `Sexp::Record' with TYPE-SYM tag + SLOTS list.
TYPE-SYM is a symbol (= the record type tag); SLOTS is a list of
Sexp values holding the per-slot payloads."
  (unless (symbolp type-sym)
    (signal 'wrong-type-argument (list 'symbolp type-sym)))
  (unless (listp slots)
    (signal 'wrong-type-argument (list 'listp slots)))
  (list :tag 'record :type-sym type-sym :slots slots))

;;; --- tag accessor + predicates ------------------------------------

(defun nelisp-sexp-p (sexp)
  "Return non-nil when SEXP looks like a Sexp DSL plist."
  (and (consp sexp) (keywordp (car sexp)) (eq (car sexp) :tag)))

(defun nelisp-sexp-tag (sexp)
  "Return the `:tag' value of SEXP, or nil when SEXP is not a Sexp."
  (when (nelisp-sexp-p sexp)
    (plist-get sexp :tag)))

(defun nelisp-sexp-nil-p (sexp)
  "Return non-nil when SEXP is `Sexp::Nil'."
  (and (nelisp-sexp-p sexp) (eq (plist-get sexp :tag) nil)))

(defun nelisp-sexp-t-p (sexp)
  "Return non-nil when SEXP is `Sexp::T'."
  (and (nelisp-sexp-p sexp) (eq (plist-get sexp :tag) t)))

(defun nelisp-sexp-int-p (sexp)
  "Return non-nil when SEXP is `Sexp::Int'."
  (eq (nelisp-sexp-tag sexp) 'int))

(defun nelisp-sexp-float-p (sexp)
  "Return non-nil when SEXP is `Sexp::Float'."
  (eq (nelisp-sexp-tag sexp) 'float))

(defun nelisp-sexp-str-p (sexp)
  "Return non-nil when SEXP is `Sexp::Str'."
  (eq (nelisp-sexp-tag sexp) 'str))

(defun nelisp-sexp-mut-str-p (sexp)
  "Return non-nil when SEXP is `Sexp::MutStr'."
  (eq (nelisp-sexp-tag sexp) 'mut-str))

(defun nelisp-sexp-symbol-p (sexp)
  "Return non-nil when SEXP is `Sexp::Symbol'."
  (eq (nelisp-sexp-tag sexp) 'symbol))

(defun nelisp-sexp-cons-p (sexp)
  "Return non-nil when SEXP is `Sexp::Cons'."
  (eq (nelisp-sexp-tag sexp) 'cons))

(defun nelisp-sexp-vector-p (sexp)
  "Return non-nil when SEXP is `Sexp::Vector'."
  (eq (nelisp-sexp-tag sexp) 'vector))

(defun nelisp-sexp-char-table-p (sexp)
  "Return non-nil when SEXP is `Sexp::CharTable'."
  (eq (nelisp-sexp-tag sexp) 'char-table))

(defun nelisp-sexp-bool-vector-p (sexp)
  "Return non-nil when SEXP is `Sexp::BoolVector'."
  (eq (nelisp-sexp-tag sexp) 'bool-vector))

(defun nelisp-sexp-cell-p (sexp)
  "Return non-nil when SEXP is `Sexp::Cell'."
  (eq (nelisp-sexp-tag sexp) 'cell))

(defun nelisp-sexp-record-p (sexp)
  "Return non-nil when SEXP is `Sexp::Record'."
  (eq (nelisp-sexp-tag sexp) 'record))

;;; --- per-variant accessors ----------------------------------------

(defun nelisp-sexp--check (pred sexp wanted)
  "Internal: signal `wrong-type-argument' when PRED on SEXP is nil.
WANTED is the symbol passed as the violated predicate name."
  (unless (funcall pred sexp)
    (signal 'wrong-type-argument (list wanted sexp))))

(defun nelisp-sexp-int-value (sexp)
  "Return the wrapped integer of `Sexp::Int' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-int-p sexp 'nelisp-sexp-int-p)
  (plist-get sexp :value))

(defun nelisp-sexp-float-value (sexp)
  "Return the wrapped float of `Sexp::Float' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-float-p sexp 'nelisp-sexp-float-p)
  (plist-get sexp :value))

(defun nelisp-sexp-str-value (sexp)
  "Return the wrapped string of `Sexp::Str' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-str-p sexp 'nelisp-sexp-str-p)
  (plist-get sexp :value))

(defun nelisp-sexp-mut-str-value (sexp)
  "Return the wrapped string of `Sexp::MutStr' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-mut-str-p sexp 'nelisp-sexp-mut-str-p)
  (plist-get sexp :value))

(defun nelisp-sexp-symbol-name (sexp)
  "Return the wrapped name string of `Sexp::Symbol' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-symbol-p sexp 'nelisp-sexp-symbol-p)
  (plist-get sexp :value))

(defun nelisp-sexp-cons-car (sexp)
  "Return the CAR of `Sexp::Cons' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-cons-p sexp 'nelisp-sexp-cons-p)
  (plist-get sexp :car))

(defun nelisp-sexp-cons-cdr (sexp)
  "Return the CDR of `Sexp::Cons' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-cons-p sexp 'nelisp-sexp-cons-p)
  (plist-get sexp :cdr))

(defun nelisp-sexp-vector-elts (sexp)
  "Return the element list of `Sexp::Vector' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-vector-p sexp 'nelisp-sexp-vector-p)
  (plist-get sexp :elts))

(defun nelisp-sexp-record-type-sym (sexp)
  "Return the type symbol of `Sexp::Record' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-record-p sexp 'nelisp-sexp-record-p)
  (plist-get sexp :type-sym))

(defun nelisp-sexp-record-slots (sexp)
  "Return the slot list of `Sexp::Record' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-record-p sexp 'nelisp-sexp-record-p)
  (plist-get sexp :slots))

;;; --- structural equality ------------------------------------------

(defun nelisp-sexp-eq (a b)
  "Return non-nil iff Sexp values A and B are structurally identical.
Recursively compares cons CAR/CDR, vector elements, cell inner,
and record slots; atomic variants compare via `equal' on the
wrapped value."
  (cond
   ((and (null a) (null b)) t)
   ((or (not (nelisp-sexp-p a)) (not (nelisp-sexp-p b))) nil)
   ((not (eq (plist-get a :tag) (plist-get b :tag))) nil)
   (t
    (let ((tag (plist-get a :tag)))
      (cond
       ((null tag) t)
       ((eq tag t) t)
       ((memq tag '(int float str mut-str symbol))
        (equal (plist-get a :value) (plist-get b :value)))
       ((eq tag 'cons)
        (and (nelisp-sexp-eq (plist-get a :car) (plist-get b :car))
             (nelisp-sexp-eq (plist-get a :cdr) (plist-get b :cdr))))
       ((eq tag 'vector)
        (nelisp-sexp--eq-list (plist-get a :elts) (plist-get b :elts)))
       ((eq tag 'char-table)
        (and (nelisp-sexp-eq (plist-get a :default)
                             (plist-get b :default))
             (nelisp-sexp--eq-list (plist-get a :extras)
                                   (plist-get b :extras))))
       ((eq tag 'bool-vector)
        (and (eql (plist-get a :len) (plist-get b :len))
             (eq (plist-get a :init) (plist-get b :init))))
       ((eq tag 'cell)
        (nelisp-sexp-eq (plist-get a :inner) (plist-get b :inner)))
       ((eq tag 'record)
        (and (eq (plist-get a :type-sym) (plist-get b :type-sym))
             (nelisp-sexp--eq-list (plist-get a :slots)
                                   (plist-get b :slots))))
       (t nil))))))

(defun nelisp-sexp--eq-list (xs ys)
  "Internal: pairwise `nelisp-sexp-eq' over lists XS and YS."
  (cond
   ((and (null xs) (null ys)) t)
   ((or (null xs) (null ys)) nil)
   ((nelisp-sexp-eq (car xs) (car ys))
    (nelisp-sexp--eq-list (cdr xs) (cdr ys)))
   (t nil)))

;;; --- debug printer ------------------------------------------------

(defun nelisp-sexp-pp (sexp)
  "Return a human-readable string representation of SEXP.
Used in ERT failure messages; not part of the byte-layout
serializer (= deferred to §95.c)."
  (cond
   ((not (nelisp-sexp-p sexp)) (format "<not-sexp %S>" sexp))
   (t
    (let ((tag (plist-get sexp :tag)))
      (cond
       ((null tag) "#<nil>")
       ((eq tag t) "#<t>")
       ((eq tag 'int) (format "#<int %d>" (plist-get sexp :value)))
       ((eq tag 'float) (format "#<float %g>" (plist-get sexp :value)))
       ((eq tag 'str) (format "#<str %S>" (plist-get sexp :value)))
       ((eq tag 'mut-str)
        (format "#<mut-str %S>" (plist-get sexp :value)))
       ((eq tag 'symbol)
        (format "#<symbol %s>" (plist-get sexp :value)))
       ((eq tag 'cons)
        (format "#<cons %s . %s>"
                (nelisp-sexp-pp (plist-get sexp :car))
                (nelisp-sexp-pp (plist-get sexp :cdr))))
       ((eq tag 'vector)
        (format "#<vector %s>"
                (mapconcat #'nelisp-sexp-pp
                           (plist-get sexp :elts) " ")))
       ((eq tag 'char-table)
        (format "#<char-table default=%s extras=(%s)>"
                (nelisp-sexp-pp (plist-get sexp :default))
                (mapconcat #'nelisp-sexp-pp
                           (plist-get sexp :extras) " ")))
       ((eq tag 'bool-vector)
        (format "#<bool-vector len=%d init=%s>"
                (plist-get sexp :len)
                (if (plist-get sexp :init) "t" "nil")))
       ((eq tag 'cell)
        (format "#<cell %s>"
                (nelisp-sexp-pp (plist-get sexp :inner))))
       ((eq tag 'record)
        (format "#<record %s (%s)>"
                (plist-get sexp :type-sym)
                (mapconcat #'nelisp-sexp-pp
                           (plist-get sexp :slots) " ")))
       (t (format "#<unknown-tag %S>" tag)))))))

;;; --- §95.b extensions =============================================
;;
;; Mutators, length / arity, iteration, conversion, hash, deep copy,
;; deep-eq, and type coercion (= unwrap + assert).  Per Doc 95 §8 §95.b
;; the design ROI hinges on these being available before §95.c starts
;; encoding bytes — serializer + baker need to walk and hash Sexp
;; values without re-implementing structural recursion in each call
;; site.  Plist repr from §95.a is retained; representation swap to a
;; tagged-vec stays a local edit isolated to constructors / accessors.
;;
;; Relationship with §95.a `nelisp-sexp-eq': the existing predicate is
;; already recursive, so structurally it is a deep-eq.  §95.b adds an
;; explicit alias `nelisp-sexp-deep-eq' for callers that want the
;; intent to read clearly; the two are operationally identical and a
;; trivial alias keeps the API surface predictable.

;;; --- mutators -----------------------------------------------------

(defun nelisp-sexp-cons-set-car (sexp new-car)
  "Destructively set the CAR of `Sexp::Cons' SEXP to NEW-CAR.
Returns SEXP for chaining."
  (nelisp-sexp--check #'nelisp-sexp-cons-p sexp 'nelisp-sexp-cons-p)
  (plist-put sexp :car new-car)
  sexp)

(defun nelisp-sexp-cons-set-cdr (sexp new-cdr)
  "Destructively set the CDR of `Sexp::Cons' SEXP to NEW-CDR.
Returns SEXP for chaining."
  (nelisp-sexp--check #'nelisp-sexp-cons-p sexp 'nelisp-sexp-cons-p)
  (plist-put sexp :cdr new-cdr)
  sexp)

(defun nelisp-sexp-vector-aset (sexp idx new-elt)
  "Destructively set element IDX of `Sexp::Vector' SEXP to NEW-ELT.
Signals `args-out-of-range' on bounds violation."
  (nelisp-sexp--check #'nelisp-sexp-vector-p sexp 'nelisp-sexp-vector-p)
  (let* ((elts (plist-get sexp :elts))
         (len (length elts)))
    (unless (and (integerp idx) (>= idx 0) (< idx len))
      (signal 'args-out-of-range (list sexp idx)))
    (setf (nth idx elts) new-elt)
    sexp))

(defun nelisp-sexp-record-set-slot (sexp idx new-val)
  "Destructively set slot IDX of `Sexp::Record' SEXP to NEW-VAL.
Signals `args-out-of-range' on bounds violation."
  (nelisp-sexp--check #'nelisp-sexp-record-p sexp 'nelisp-sexp-record-p)
  (let* ((slots (plist-get sexp :slots))
         (len (length slots)))
    (unless (and (integerp idx) (>= idx 0) (< idx len))
      (signal 'args-out-of-range (list sexp idx)))
    (setf (nth idx slots) new-val)
    sexp))

;;; --- length / arity -----------------------------------------------

(defun nelisp-sexp-cons-length (sexp)
  "Return proper-list length of `Sexp::Cons' chain SEXP.
The chain is terminated by `Sexp::Nil'.  Signals `wrong-type-
argument' when the chain terminates in a non-nil non-cons value."
  (let ((n 0)
        (cur sexp))
    (while (nelisp-sexp-cons-p cur)
      (setq n (1+ n)
            cur (plist-get cur :cdr)))
    (unless (nelisp-sexp-nil-p cur)
      (signal 'wrong-type-argument
              (list 'nelisp-sexp-proper-list-p sexp)))
    n))

(defun nelisp-sexp-vector-length (sexp)
  "Return length of the element list of `Sexp::Vector' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-vector-p sexp 'nelisp-sexp-vector-p)
  (length (plist-get sexp :elts)))

(defun nelisp-sexp-record-arity (sexp)
  "Return slot count of `Sexp::Record' SEXP."
  (nelisp-sexp--check #'nelisp-sexp-record-p sexp 'nelisp-sexp-record-p)
  (length (plist-get sexp :slots)))

;;; --- iteration ----------------------------------------------------

(defun nelisp-sexp-walk (sexp fn)
  "Pre-order traverse SEXP, calling FN on each Sexp sub-value.
FN's return value is ignored; traversal is for side effect."
  (when (nelisp-sexp-p sexp)
    (funcall fn sexp)
    (let ((tag (plist-get sexp :tag)))
      (cond
       ((eq tag 'cons)
        (nelisp-sexp-walk (plist-get sexp :car) fn)
        (nelisp-sexp-walk (plist-get sexp :cdr) fn))
       ((eq tag 'vector)
        (dolist (e (plist-get sexp :elts))
          (nelisp-sexp-walk e fn)))
       ((eq tag 'char-table)
        (nelisp-sexp-walk (plist-get sexp :default) fn)
        (dolist (e (plist-get sexp :extras))
          (nelisp-sexp-walk e fn)))
       ((eq tag 'cell)
        (nelisp-sexp-walk (plist-get sexp :inner) fn))
       ((eq tag 'record)
        (dolist (e (plist-get sexp :slots))
          (nelisp-sexp-walk e fn)))))))

(defun nelisp-sexp-map-cons (fn sexp)
  "Map FN over the proper Cons list SEXP, returning a new Cons list.
SEXP must be a `Sexp::Cons' chain terminated by `Sexp::Nil'.  FN
takes one Sexp argument and must return a Sexp."
  (if (nelisp-sexp-nil-p sexp)
      (nelisp-sexp-make-nil)
    (nelisp-sexp--check #'nelisp-sexp-cons-p sexp 'nelisp-sexp-cons-p)
    (nelisp-sexp-make-cons
     (funcall fn (plist-get sexp :car))
     (nelisp-sexp-map-cons fn (plist-get sexp :cdr)))))

(defun nelisp-sexp-foldl (fn init sexp)
  "Left-fold FN with seed INIT over Cons list SEXP.
FN is called as (FN ACC ELT); SEXP must be a proper Cons list."
  (let ((acc init)
        (cur sexp))
    (while (nelisp-sexp-cons-p cur)
      (setq acc (funcall fn acc (plist-get cur :car))
            cur (plist-get cur :cdr)))
    (unless (nelisp-sexp-nil-p cur)
      (signal 'wrong-type-argument
              (list 'nelisp-sexp-proper-list-p sexp)))
    acc))

;;; --- conversion ---------------------------------------------------

(defun nelisp-sexp-cons-from-list (elts-list)
  "Build a `Sexp::Cons' chain from ELTS-LIST (= elisp list of Sexps).
Empty list becomes `Sexp::Nil'.  No deep-copy of elements."
  (unless (listp elts-list)
    (signal 'wrong-type-argument (list 'listp elts-list)))
  (let ((acc (nelisp-sexp-make-nil)))
    (dolist (e (reverse elts-list))
      (setq acc (nelisp-sexp-make-cons e acc)))
    acc))

(defun nelisp-sexp-list-from-cons (sexp)
  "Flatten Cons chain SEXP into an elisp list of Sexp values.
SEXP must be a proper Cons list.  Signals on improper terminator."
  (let ((acc nil)
        (cur sexp))
    (while (nelisp-sexp-cons-p cur)
      (push (plist-get cur :car) acc)
      (setq cur (plist-get cur :cdr)))
    (unless (nelisp-sexp-nil-p cur)
      (signal 'wrong-type-argument
              (list 'nelisp-sexp-proper-list-p sexp)))
    (nreverse acc)))

(defun nelisp-sexp-to-elisp (sexp)
  "Lossy convert SEXP to a native elisp value.  Debug-only.
The mapping discards the `Sexp::MutStr' / `Sexp::Str' distinction
and collapses `Sexp::Cell' to its inner value.  Use the byte-
layout serializer (= §95.c) for anything semantic."
  (cond
   ((not (nelisp-sexp-p sexp)) sexp)
   (t
    (let ((tag (plist-get sexp :tag)))
      (cond
       ((null tag) nil)
       ((eq tag t) t)
       ((memq tag '(int float str mut-str))
        (plist-get sexp :value))
       ((eq tag 'symbol) (intern (plist-get sexp :value)))
       ((eq tag 'cons)
        (cons (nelisp-sexp-to-elisp (plist-get sexp :car))
              (nelisp-sexp-to-elisp (plist-get sexp :cdr))))
       ((eq tag 'vector)
        (apply #'vector
               (mapcar #'nelisp-sexp-to-elisp
                       (plist-get sexp :elts))))
       ((eq tag 'cell) (nelisp-sexp-to-elisp (plist-get sexp :inner)))
       ((eq tag 'record)
        (cons (plist-get sexp :type-sym)
              (mapcar #'nelisp-sexp-to-elisp
                      (plist-get sexp :slots))))
       (t sexp))))))

(defun nelisp-sexp-from-elisp (obj)
  "Sniff native elisp OBJ and wrap into a `Sexp::*' value.
Type detection order: nil → Nil, t → T, integer → Int, float →
Float, string → Str, symbol → Symbol, vector → Vector, cons →
Cons chain.  Improper cons tails are preserved verbatim."
  (cond
   ((null obj) (nelisp-sexp-make-nil))
   ((eq obj t) (nelisp-sexp-make-t))
   ((integerp obj) (nelisp-sexp-make-int obj))
   ((floatp obj) (nelisp-sexp-make-float obj))
   ((stringp obj) (nelisp-sexp-make-str obj))
   ((symbolp obj) (nelisp-sexp-make-symbol obj))
   ((vectorp obj)
    (nelisp-sexp-make-vector
     (mapcar #'nelisp-sexp-from-elisp (append obj nil))))
   ((consp obj)
    (nelisp-sexp-make-cons
     (nelisp-sexp-from-elisp (car obj))
     (nelisp-sexp-from-elisp (cdr obj))))
   (t (signal 'wrong-type-argument (list 'nelisp-sexp-from-elisp obj)))))

;;; --- hash ---------------------------------------------------------

(defun nelisp-sexp-hash (sexp)
  "Return an `eql'-compatible structural hash of SEXP.
Same SEXP shape ⇒ same hash; different shapes ⇒ different hash
with high probability.  Recursive over all variant children."
  (if (not (nelisp-sexp-p sexp))
      (sxhash-equal sexp)
    (let ((tag (plist-get sexp :tag)))
      (cond
       ((null tag) 1)
       ((eq tag t) 2)
       ((memq tag '(int float str mut-str symbol))
        (logxor (sxhash-equal tag)
                (sxhash-equal (plist-get sexp :value))))
       ((eq tag 'cons)
        (logxor (sxhash-equal tag)
                (ash (nelisp-sexp-hash (plist-get sexp :car)) 1)
                (nelisp-sexp-hash (plist-get sexp :cdr))))
       ((eq tag 'vector)
        (logxor (sxhash-equal tag)
                (nelisp-sexp--hash-list (plist-get sexp :elts))))
       ((eq tag 'char-table)
        (logxor (sxhash-equal tag)
                (nelisp-sexp-hash (plist-get sexp :default))
                (nelisp-sexp--hash-list (plist-get sexp :extras))))
       ((eq tag 'bool-vector)
        (logxor (sxhash-equal tag)
                (sxhash-equal (plist-get sexp :len))
                (if (plist-get sexp :init) 7 11)))
       ((eq tag 'cell)
        (logxor (sxhash-equal tag)
                (nelisp-sexp-hash (plist-get sexp :inner))))
       ((eq tag 'record)
        (logxor (sxhash-equal tag)
                (sxhash-equal (plist-get sexp :type-sym))
                (nelisp-sexp--hash-list (plist-get sexp :slots))))
       (t (sxhash-equal sexp))))))

(defun nelisp-sexp--hash-list (xs)
  "Internal: combine `nelisp-sexp-hash' over list XS with index mix."
  (let ((acc 0)
        (i 0))
    (dolist (x xs)
      (setq acc (logxor acc (ash (nelisp-sexp-hash x) (mod i 16)))
            i (1+ i)))
    acc))

;;; --- deep copy / deep-eq ------------------------------------------

(defun nelisp-sexp-copy (sexp)
  "Return a deep copy of SEXP.
All cons / vector / char-table / cell / record children are
recursively reconstructed; atomic variants reuse their immutable
payload (= int / float / interned string)."
  (if (not (nelisp-sexp-p sexp))
      sexp
    (let ((tag (plist-get sexp :tag)))
      (cond
       ((null tag) (nelisp-sexp-make-nil))
       ((eq tag t) (nelisp-sexp-make-t))
       ((eq tag 'int) (nelisp-sexp-make-int (plist-get sexp :value)))
       ((eq tag 'float)
        (nelisp-sexp-make-float (plist-get sexp :value)))
       ((eq tag 'str) (nelisp-sexp-make-str (plist-get sexp :value)))
       ((eq tag 'mut-str)
        (nelisp-sexp-make-mut-str (copy-sequence (plist-get sexp :value))))
       ((eq tag 'symbol)
        (nelisp-sexp-make-symbol (plist-get sexp :value)))
       ((eq tag 'cons)
        (nelisp-sexp-make-cons
         (nelisp-sexp-copy (plist-get sexp :car))
         (nelisp-sexp-copy (plist-get sexp :cdr))))
       ((eq tag 'vector)
        (nelisp-sexp-make-vector
         (mapcar #'nelisp-sexp-copy (plist-get sexp :elts))))
       ((eq tag 'char-table)
        (nelisp-sexp-make-char-table
         (nelisp-sexp-copy (plist-get sexp :default))
         (mapcar #'nelisp-sexp-copy (plist-get sexp :extras))))
       ((eq tag 'bool-vector)
        (nelisp-sexp-make-bool-vector
         (plist-get sexp :len) (plist-get sexp :init)))
       ((eq tag 'cell)
        (nelisp-sexp-make-cell
         (nelisp-sexp-copy (plist-get sexp :inner))))
       ((eq tag 'record)
        (nelisp-sexp-make-record
         (plist-get sexp :type-sym)
         (mapcar #'nelisp-sexp-copy (plist-get sexp :slots))))
       (t sexp)))))

(defalias 'nelisp-sexp-deep-eq #'nelisp-sexp-eq
  "Alias for `nelisp-sexp-eq'; reads as deep-eq at call sites.
The §95.a structural eq is already recursive over every variant,
so deep-eq and eq are operationally identical.")

;;; --- type coercion (= unwrap + assert) ----------------------------

(defun nelisp-sexp-as-int (sexp)
  "Return wrapped int of SEXP, signaling on tag mismatch."
  (nelisp-sexp--check #'nelisp-sexp-int-p sexp 'nelisp-sexp-int-p)
  (plist-get sexp :value))

(defun nelisp-sexp-as-float (sexp)
  "Return wrapped float of SEXP, signaling on tag mismatch."
  (nelisp-sexp--check #'nelisp-sexp-float-p sexp 'nelisp-sexp-float-p)
  (plist-get sexp :value))

(defun nelisp-sexp-as-str (sexp)
  "Return wrapped string of `Sexp::Str' or `Sexp::MutStr' SEXP."
  (unless (or (nelisp-sexp-str-p sexp) (nelisp-sexp-mut-str-p sexp))
    (signal 'wrong-type-argument (list 'nelisp-sexp-stringy-p sexp)))
  (plist-get sexp :value))

(defun nelisp-sexp-as-symbol (sexp)
  "Return wrapped symbol-name string of SEXP, signaling on mismatch."
  (nelisp-sexp--check #'nelisp-sexp-symbol-p sexp 'nelisp-sexp-symbol-p)
  (plist-get sexp :value))

(defun nelisp-sexp-as-cons (sexp)
  "Return SEXP itself when it is `Sexp::Cons', else signal."
  (nelisp-sexp--check #'nelisp-sexp-cons-p sexp 'nelisp-sexp-cons-p)
  sexp)

(defun nelisp-sexp-as-vector (sexp)
  "Return SEXP itself when it is `Sexp::Vector', else signal."
  (nelisp-sexp--check #'nelisp-sexp-vector-p sexp 'nelisp-sexp-vector-p)
  sexp)

(defun nelisp-sexp-as-record (sexp)
  "Return SEXP itself when it is `Sexp::Record', else signal."
  (nelisp-sexp--check #'nelisp-sexp-record-p sexp 'nelisp-sexp-record-p)
  sexp)

(provide 'nelisp-sexp-dsl)

;;; nelisp-sexp-dsl.el ends here
