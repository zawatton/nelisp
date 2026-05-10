;;; nelisp-sexp-dsl.el --- Phase 47 Sexp layout DSL (§95.a-e)  -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'cl-lib))
(require 'cl-lib)

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

;;; --- §95.c byte-layout serializer =================================
;;
;; Freestanding wire format: serialize the 13 `Sexp' variants into a
;; self-describing unibyte byte string with single-byte tag + payload,
;; and read it back losslessly.  The format is *not* the §3 32-byte
;; fixed Rust layout — that pins to the JIT trampoline ABI and is the
;; concern of §95.d (= JIT bridge).  Wire format here is variable-
;; length and minimises bytes per value so deep trees stay compact.
;;
;; Tag byte (low 8 bits, big-endian by inspection / little-endian by
;; the i64/u32/f64 payloads that follow):
;;
;;   0x00  Nil       (no payload)
;;   0x01  T         (no payload)
;;   0x02  Int       i64 LE (8 bytes, two's complement)
;;   0x03  Float     IEEE 754 binary64 LE (8 bytes)
;;   0x04  Symbol    u32 LE len + UTF-8 name
;;   0x05  Str       u32 LE len + UTF-8 content
;;   0x06  MutStr    u32 LE len + UTF-8 content
;;   0x07  Cons      <CAR> <CDR>
;;   0x08  Vector    u32 LE len + <ELT> x len
;;   0x09  CharTable <DEFAULT> u32 LE len + <EXTRA> x len
;;   0x0A  BoolVector u32 LE len + ceil(len/8) bytes packed LSB-first
;;   0x0B  Cell      <INNER>
;;   0x0C  Record    <TYPE-SYM-as-Sexp::Symbol> u32 LE len + <SLOT> x len
;;
;; Round-trip: `(equal SEXP (car (read-bytes (write-bytes SEXP) 0)))'
;; for every well-formed SEXP, modulo plist key order (= the §95.a
;; `nelisp-sexp-eq' compares :value / children, not raw plist tails).
;; All ert in §95.c additions check via `nelisp-sexp-eq', not `equal'.

;;; --- §95.c tag byte constants -------------------------------------

(defconst nelisp-sexp-wire-tag-nil         #x00)
(defconst nelisp-sexp-wire-tag-t           #x01)
(defconst nelisp-sexp-wire-tag-int         #x02)
(defconst nelisp-sexp-wire-tag-float       #x03)
(defconst nelisp-sexp-wire-tag-symbol      #x04)
(defconst nelisp-sexp-wire-tag-str         #x05)
(defconst nelisp-sexp-wire-tag-mut-str     #x06)
(defconst nelisp-sexp-wire-tag-cons        #x07)
(defconst nelisp-sexp-wire-tag-vector      #x08)
(defconst nelisp-sexp-wire-tag-char-table  #x09)
(defconst nelisp-sexp-wire-tag-bool-vector #x0A)
(defconst nelisp-sexp-wire-tag-cell        #x0B)
(defconst nelisp-sexp-wire-tag-record      #x0C)

;;; --- §95.c primitive byte helpers ---------------------------------
;;
;; The byte buffer is grown by `nelisp-sexp--emit-bytes' which appends
;; a unibyte string to BUF (a list of unibyte strings reversed at the
;; end).  Readers consume from a position cursor; helpers return
;; `(VALUE . NEW-POS)' so callers thread POS through dispatch.

(defun nelisp-sexp--emit-byte (buf b)
  "Append single byte B to BUF (= a list of unibyte strings, reversed).
Returns the updated cons cell at the head of BUF; callers store the
result back into BUF."
  (cons (unibyte-string (logand b #xFF)) buf))

(defun nelisp-sexp--write-u32 (buf val)
  "Emit u32 LE encoding of VAL into BUF, returning updated BUF.
VAL must be in [0, 2^32-1]; out-of-range signals `args-out-of-range'."
  (unless (and (integerp val) (>= val 0) (< val (ash 1 32)))
    (signal 'args-out-of-range (list 'u32 val)))
  (cons (unibyte-string (logand val           #xFF)
                        (logand (ash val  -8) #xFF)
                        (logand (ash val -16) #xFF)
                        (logand (ash val -24) #xFF))
        buf))

(defun nelisp-sexp--read-u32 (bytes pos)
  "Read u32 LE from BYTES at POS, returning (VAL . NEW-POS).
Signals `nelisp-sexp-truncated' when fewer than 4 bytes remain."
  (when (> (+ pos 4) (length bytes))
    (signal 'nelisp-sexp-truncated (list 'u32 pos (length bytes))))
  (cons (logior (aref bytes pos)
                (ash (aref bytes (+ pos 1))  8)
                (ash (aref bytes (+ pos 2)) 16)
                (ash (aref bytes (+ pos 3)) 24))
        (+ pos 4)))

(defun nelisp-sexp--write-i64 (buf val)
  "Emit i64 LE two's-complement encoding of VAL into BUF.
VAL must fit signed 64-bit; out-of-range signals `args-out-of-range'."
  (unless (integerp val)
    (signal 'wrong-type-argument (list 'integerp val)))
  (when (or (> val (1- (ash 1 63))) (< val (- (ash 1 63))))
    (signal 'args-out-of-range (list 'i64 val)))
  (let ((u (if (< val 0) (+ val (ash 1 64)) val)))
    (cons (unibyte-string (logand u           #xFF)
                          (logand (ash u  -8) #xFF)
                          (logand (ash u -16) #xFF)
                          (logand (ash u -24) #xFF)
                          (logand (ash u -32) #xFF)
                          (logand (ash u -40) #xFF)
                          (logand (ash u -48) #xFF)
                          (logand (ash u -56) #xFF))
          buf)))

(defun nelisp-sexp--read-i64 (bytes pos)
  "Read i64 LE two's-complement from BYTES at POS.
Returns (VAL . NEW-POS).  Signals `nelisp-sexp-truncated' when fewer
than 8 bytes remain."
  (when (> (+ pos 8) (length bytes))
    (signal 'nelisp-sexp-truncated (list 'i64 pos (length bytes))))
  (let ((u (logior (aref bytes pos)
                   (ash (aref bytes (+ pos 1))  8)
                   (ash (aref bytes (+ pos 2)) 16)
                   (ash (aref bytes (+ pos 3)) 24)
                   (ash (aref bytes (+ pos 4)) 32)
                   (ash (aref bytes (+ pos 5)) 40)
                   (ash (aref bytes (+ pos 6)) 48)
                   (ash (aref bytes (+ pos 7)) 56))))
    (cons (if (>= u (ash 1 63)) (- u (ash 1 64)) u)
          (+ pos 8))))

(defun nelisp-sexp--f64-to-bits (f)
  "Return u64 IEEE 754 binary64 bit pattern of float F.
Hand-rolled because elisp has no `float-to-bits' built-in.  Handles
+/-zero, +/-inf, NaN (canonical quiet NaN with payload bit), normal,
and subnormal values."
  (cond
   ;; NaN: emit canonical quiet NaN (sign 0, exp all-1, mantissa MSB 1).
   ((isnan f) #x7FF8000000000000)
   ;; +/- Infinity: sign bit + exp all-1, mantissa 0.
   ((and (> f 0) (= f (* f 2.0))) #x7FF0000000000000)
   ((and (< f 0) (= f (* f 2.0))) #xFFF0000000000000)
   ;; +/- Zero: sign bit only.
   ((= f 0.0)
    (if (< (copysign 1.0 f) 0) #x8000000000000000 0))
   (t
    (let* ((sign-bit (if (< f 0) 1 0))
           (af (abs f))
           (fe (frexp af))
           (m (car fe))               ; normalised mantissa in [0.5,1)
           (e (cdr fe))               ; binary exponent (af = m * 2^e)
           ;; IEEE has implicit-1 bit at exponent bias 1023, mantissa
           ;; in [1.0, 2.0).  frexp gives [0.5, 1.0), so shift e by 1.
           (unbiased-e (1- e))
           (biased-e (+ unbiased-e 1023)))
      (cond
       ;; Overflow → infinity (treated above already, but guard).
       ((>= biased-e #x7FF)
        (logior (ash sign-bit 63) #x7FF0000000000000))
       ;; Normal: 0 < biased-e < 2047.
       ((> biased-e 0)
        (let* ((mant-scaled (* m 2.0))    ; in [1.0, 2.0)
               (frac (- mant-scaled 1.0)) ; in [0.0, 1.0)
               (mantissa (truncate (* frac (expt 2.0 52)))))
          (logior (ash sign-bit 63)
                  (ash biased-e 52)
                  (logand mantissa #xFFFFFFFFFFFFF))))
       ;; Subnormal: biased-e <= 0; shift mantissa down accordingly.
       (t
        (let* ((shift (- 1 biased-e))    ; how many extra bits to drop
               (mant-scaled (* m 2.0))   ; [1.0, 2.0)
               (mantissa (truncate (* mant-scaled
                                      (expt 2.0 (- 52 shift))))))
          (logior (ash sign-bit 63)
                  (logand mantissa #xFFFFFFFFFFFFF)))))))))

(defun nelisp-sexp--f64-from-bits (u)
  "Decode u64 IEEE 754 binary64 bit pattern U back to a float.
Inverse of `nelisp-sexp--f64-to-bits' for finite + zero values; for
NaN any non-zero mantissa with all-1 exponent decodes to (/ 0.0 0.0)."
  (let* ((sign (ash u -63))
         (biased-e (logand (ash u -52) #x7FF))
         (mantissa (logand u #xFFFFFFFFFFFFF))
         (sign-mul (if (zerop sign) 1.0 -1.0)))
    (cond
     ;; All-1 exponent: Infinity or NaN.
     ((= biased-e #x7FF)
      (if (zerop mantissa)
          (* sign-mul (expt 10.0 400))      ; produces +/- inf
        (/ 0.0 0.0)))                       ; NaN
     ;; All-0 exponent: zero or subnormal.
     ((zerop biased-e)
      (if (zerop mantissa)
          (if (zerop sign) 0.0 -0.0)
        (* sign-mul
           (* mantissa (expt 2.0 (- 1 1023 52))))))
     ;; Normal: implicit leading 1 bit at position 52.
     (t
      (let* ((mant-frac (/ (float mantissa) (expt 2.0 52)))
             (significand (+ 1.0 mant-frac))
             (unbiased-e (- biased-e 1023)))
        (* sign-mul (ldexp significand unbiased-e)))))))

(defun nelisp-sexp--write-f64 (buf val)
  "Emit IEEE 754 binary64 LE encoding of float VAL into BUF."
  (unless (floatp val)
    (signal 'wrong-type-argument (list 'floatp val)))
  (let ((u (nelisp-sexp--f64-to-bits val)))
    (cons (unibyte-string (logand u           #xFF)
                          (logand (ash u  -8) #xFF)
                          (logand (ash u -16) #xFF)
                          (logand (ash u -24) #xFF)
                          (logand (ash u -32) #xFF)
                          (logand (ash u -40) #xFF)
                          (logand (ash u -48) #xFF)
                          (logand (ash u -56) #xFF))
          buf)))

(defun nelisp-sexp--read-f64 (bytes pos)
  "Read IEEE 754 binary64 LE from BYTES at POS, returning (F . NEW-POS).
Signals `nelisp-sexp-truncated' when fewer than 8 bytes remain."
  (when (> (+ pos 8) (length bytes))
    (signal 'nelisp-sexp-truncated (list 'f64 pos (length bytes))))
  (let ((u (logior (aref bytes pos)
                   (ash (aref bytes (+ pos 1))  8)
                   (ash (aref bytes (+ pos 2)) 16)
                   (ash (aref bytes (+ pos 3)) 24)
                   (ash (aref bytes (+ pos 4)) 32)
                   (ash (aref bytes (+ pos 5)) 40)
                   (ash (aref bytes (+ pos 6)) 48)
                   (ash (aref bytes (+ pos 7)) 56))))
    (cons (nelisp-sexp--f64-from-bits u) (+ pos 8))))

(defun nelisp-sexp--write-utf8 (buf str)
  "Emit u32 LE length + UTF-8 bytes of STR into BUF.
STR is encoded with `utf-8-unix' so multibyte content round-trips
losslessly across read/write."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (let* ((encoded (encode-coding-string str 'utf-8-unix t))
         (len (length encoded)))
    (cons encoded (nelisp-sexp--write-u32 buf len))))

(defun nelisp-sexp--read-utf8 (bytes pos)
  "Read u32 LE length + UTF-8 bytes from BYTES at POS.
Returns (STR . NEW-POS).  Signals `nelisp-sexp-truncated' on short
input."
  (let* ((lp (nelisp-sexp--read-u32 bytes pos))
         (len (car lp))
         (after-len (cdr lp)))
    (when (> (+ after-len len) (length bytes))
      (signal 'nelisp-sexp-truncated (list 'utf8 after-len len)))
    (cons (decode-coding-string
           (substring bytes after-len (+ after-len len))
           'utf-8-unix t)
          (+ after-len len))))

;;; --- §95.c top-level write-bytes ----------------------------------

(defun nelisp-sexp--write-into (buf sexp)
  "Internal recursive emitter: append SEXP encoding into BUF.
Returns the updated BUF (= list of unibyte strings, reversed)."
  (unless (nelisp-sexp-p sexp)
    (signal 'wrong-type-argument (list 'nelisp-sexp-p sexp)))
  (let ((tag (plist-get sexp :tag)))
    (cond
     ((null tag)
      (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-nil))
     ((eq tag t)
      (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-t))
     ((eq tag 'int)
      (nelisp-sexp--write-i64
       (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-int)
       (plist-get sexp :value)))
     ((eq tag 'float)
      (nelisp-sexp--write-f64
       (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-float)
       (plist-get sexp :value)))
     ((eq tag 'symbol)
      (nelisp-sexp--write-utf8
       (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-symbol)
       (plist-get sexp :value)))
     ((eq tag 'str)
      (nelisp-sexp--write-utf8
       (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-str)
       (plist-get sexp :value)))
     ((eq tag 'mut-str)
      (nelisp-sexp--write-utf8
       (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-mut-str)
       (plist-get sexp :value)))
     ((eq tag 'cons)
      (let ((b (nelisp-sexp--emit-byte
                buf nelisp-sexp-wire-tag-cons)))
        (setq b (nelisp-sexp--write-into b (plist-get sexp :car)))
        (nelisp-sexp--write-into b (plist-get sexp :cdr))))
     ((eq tag 'vector)
      (let* ((elts (plist-get sexp :elts))
             (b (nelisp-sexp--emit-byte
                 buf nelisp-sexp-wire-tag-vector))
             (b2 (nelisp-sexp--write-u32 b (length elts))))
        (dolist (e elts)
          (setq b2 (nelisp-sexp--write-into b2 e)))
        b2))
     ((eq tag 'char-table)
      (let* ((extras (plist-get sexp :extras))
             (b (nelisp-sexp--emit-byte
                 buf nelisp-sexp-wire-tag-char-table))
             (b2 (nelisp-sexp--write-into
                  b (plist-get sexp :default)))
             (b3 (nelisp-sexp--write-u32 b2 (length extras))))
        (dolist (e extras)
          (setq b3 (nelisp-sexp--write-into b3 e)))
        b3))
     ((eq tag 'bool-vector)
      (let* ((len (plist-get sexp :len))
             (init (plist-get sexp :init))
             (nbytes (/ (+ len 7) 8))
             (fill (if init #xFF #x00))
             ;; Final byte mask: clear bits past LEN.
             (b (nelisp-sexp--emit-byte
                 buf nelisp-sexp-wire-tag-bool-vector))
             (b2 (nelisp-sexp--write-u32 b len)))
        (when (> nbytes 0)
          (let ((last-bits (mod len 8)))
            (setq b2
                  (cons (concat (make-string (1- nbytes) fill)
                                (unibyte-string
                                 (if (and init (> last-bits 0))
                                     (1- (ash 1 last-bits))
                                   (if init #xFF #x00))))
                        b2))))
        b2))
     ((eq tag 'cell)
      (nelisp-sexp--write-into
       (nelisp-sexp--emit-byte buf nelisp-sexp-wire-tag-cell)
       (plist-get sexp :inner)))
     ((eq tag 'record)
      (let* ((type-sym (plist-get sexp :type-sym))
             (slots (plist-get sexp :slots))
             (b (nelisp-sexp--emit-byte
                 buf nelisp-sexp-wire-tag-record))
             ;; Encode TYPE-SYM as a Sexp::Symbol so the reader does
             ;; not need a parallel sub-format for the type tag.
             (b2 (nelisp-sexp--write-into
                  b (nelisp-sexp-make-symbol type-sym)))
             (b3 (nelisp-sexp--write-u32 b2 (length slots))))
        (dolist (e slots)
          (setq b3 (nelisp-sexp--write-into b3 e)))
        b3))
     (t (signal 'nelisp-sexp-unknown-tag (list tag))))))

(defun nelisp-sexp-write-bytes (sexp)
  "Serialize SEXP to a unibyte byte string per the §95.c wire format.
Round-trips with `nelisp-sexp-read-bytes' via `nelisp-sexp-eq'."
  (let ((parts (nelisp-sexp--write-into nil sexp)))
    (apply #'concat (nreverse parts))))

;;; --- §95.c top-level read-bytes -----------------------------------

(defun nelisp-sexp-read-bytes (bytes &optional pos)
  "Deserialize one Sexp value from BYTES at POS (default 0).
Returns `(SEXP . NEW-POS)'.  Signals `nelisp-sexp-truncated' on
short input or `nelisp-sexp-unknown-tag' on unrecognised tag byte."
  (unless (stringp bytes)
    (signal 'wrong-type-argument (list 'stringp bytes)))
  (let ((pos (or pos 0)))
    (when (>= pos (length bytes))
      (signal 'nelisp-sexp-truncated (list 'tag pos (length bytes))))
    (let ((tag (aref bytes pos))
          (after (1+ pos)))
      (cond
       ((= tag nelisp-sexp-wire-tag-nil)
        (cons (nelisp-sexp-make-nil) after))
       ((= tag nelisp-sexp-wire-tag-t)
        (cons (nelisp-sexp-make-t) after))
       ((= tag nelisp-sexp-wire-tag-int)
        (let ((vp (nelisp-sexp--read-i64 bytes after)))
          (cons (nelisp-sexp-make-int (car vp)) (cdr vp))))
       ((= tag nelisp-sexp-wire-tag-float)
        (let ((vp (nelisp-sexp--read-f64 bytes after)))
          (cons (nelisp-sexp-make-float (car vp)) (cdr vp))))
       ((= tag nelisp-sexp-wire-tag-symbol)
        (let ((vp (nelisp-sexp--read-utf8 bytes after)))
          (cons (nelisp-sexp-make-symbol (car vp)) (cdr vp))))
       ((= tag nelisp-sexp-wire-tag-str)
        (let ((vp (nelisp-sexp--read-utf8 bytes after)))
          (cons (nelisp-sexp-make-str (car vp)) (cdr vp))))
       ((= tag nelisp-sexp-wire-tag-mut-str)
        (let ((vp (nelisp-sexp--read-utf8 bytes after)))
          (cons (nelisp-sexp-make-mut-str (car vp)) (cdr vp))))
       ((= tag nelisp-sexp-wire-tag-cons)
        (let* ((car-pair (nelisp-sexp-read-bytes bytes after))
               (cdr-pair (nelisp-sexp-read-bytes bytes (cdr car-pair))))
          (cons (nelisp-sexp-make-cons (car car-pair) (car cdr-pair))
                (cdr cdr-pair))))
       ((= tag nelisp-sexp-wire-tag-vector)
        (let* ((lp (nelisp-sexp--read-u32 bytes after))
               (n (car lp))
               (p (cdr lp))
               (acc nil))
          (dotimes (_ n)
            (let ((ep (nelisp-sexp-read-bytes bytes p)))
              (push (car ep) acc)
              (setq p (cdr ep))))
          (cons (nelisp-sexp-make-vector (nreverse acc)) p)))
       ((= tag nelisp-sexp-wire-tag-char-table)
        (let* ((dp (nelisp-sexp-read-bytes bytes after))
               (lp (nelisp-sexp--read-u32 bytes (cdr dp)))
               (n (car lp))
               (p (cdr lp))
               (acc nil))
          (dotimes (_ n)
            (let ((ep (nelisp-sexp-read-bytes bytes p)))
              (push (car ep) acc)
              (setq p (cdr ep))))
          (cons (nelisp-sexp-make-char-table (car dp) (nreverse acc))
                p)))
       ((= tag nelisp-sexp-wire-tag-bool-vector)
        (let* ((lp (nelisp-sexp--read-u32 bytes after))
               (len (car lp))
               (p (cdr lp))
               (nbytes (/ (+ len 7) 8)))
          (when (> (+ p nbytes) (length bytes))
            (signal 'nelisp-sexp-truncated
                    (list 'bool-vector p nbytes)))
          ;; Round-trip uses :init alone (= §95.a layout has no per-bit
          ;; storage).  Recover :init by sampling the first bit, or
          ;; default to nil for len=0.
          (let ((init (cond
                       ((zerop len) nil)
                       (t (not (zerop (logand (aref bytes p) 1)))))))
            (cons (nelisp-sexp-make-bool-vector len init)
                  (+ p nbytes)))))
       ((= tag nelisp-sexp-wire-tag-cell)
        (let ((ip (nelisp-sexp-read-bytes bytes after)))
          (cons (nelisp-sexp-make-cell (car ip)) (cdr ip))))
       ((= tag nelisp-sexp-wire-tag-record)
        (let* ((tp (nelisp-sexp-read-bytes bytes after))
               (type-sexp (car tp))
               (_ (unless (nelisp-sexp-symbol-p type-sexp)
                    (signal 'nelisp-sexp-malformed-record
                            (list 'expected-symbol type-sexp))))
               (type-sym (intern (nelisp-sexp-symbol-name type-sexp)))
               (lp (nelisp-sexp--read-u32 bytes (cdr tp)))
               (n (car lp))
               (p (cdr lp))
               (acc nil))
          (dotimes (_ n)
            (let ((ep (nelisp-sexp-read-bytes bytes p)))
              (push (car ep) acc)
              (setq p (cdr ep))))
          (cons (nelisp-sexp-make-record type-sym (nreverse acc)) p)))
       (t (signal 'nelisp-sexp-unknown-tag
                  (list tag pos)))))))

;;; --- §95.c validation helpers -------------------------------------

(defun nelisp-sexp-bytes-round-trip-p (sexp)
  "Return non-nil when SEXP survives write-then-read structurally.
Uses `nelisp-sexp-eq' since plist tail order is implementation-
defined while structural identity is the wire-format contract."
  (let* ((bytes (nelisp-sexp-write-bytes sexp))
         (decoded (car (nelisp-sexp-read-bytes bytes 0))))
    (nelisp-sexp-eq sexp decoded)))

(defun nelisp-sexp-bytes-equal-p (a b)
  "Return non-nil iff `nelisp-sexp-write-bytes' of A and B agree byte
for byte.  Useful as a stricter test than `nelisp-sexp-eq' when the
serializer's determinism itself is under test."
  (equal (nelisp-sexp-write-bytes a)
         (nelisp-sexp-write-bytes b)))

;;; --- §95.c error symbols ------------------------------------------

(define-error 'nelisp-sexp-truncated
  "Sexp DSL byte buffer truncated before end of payload")
(define-error 'nelisp-sexp-unknown-tag
  "Sexp DSL wire format encountered unknown tag byte")
(define-error 'nelisp-sexp-malformed-record
  "Sexp DSL Record wire payload had wrong shape")

;;; --- §95.d JIT bridge / Rust ABI fixed-layout =====================
;;
;; Mirror the Rust `Sexp' `#[repr(C, u8)]' layout from
;; `build-tool/src/eval/sexp.rs' so the JIT trampoline can read elisp-
;; emitted Sexp values via pointer deref.  The fixed layout differs
;; from the §95.c freestanding wire format in two key ways:
;;
;;   1. Every value is exactly 32 bytes (= `size_of::<Sexp>()' on
;;      x86_64 + aarch64 LE, per `sexp_layout_alignment_and_size_sane'
;;      unit test in sexp.rs).
;;   2. Heap-backed variants (= Symbol / Str / MutStr / Cons / Vector
;;      / CharTable / BoolVector / Cell / Record) store an 8-byte LE
;;      pointer at offset 8, NOT the value content.  The caller (=
;;      Doc 91 ELF writer + Doc 93 linker chain) owns the relocation
;;      from placeholder pointer to actual heap address.
;;
;; Layout (§3.1 / §3.2 of Doc 95):
;;
;;   offset  size  field
;;     0       1   tag byte (= SEXP_TAG_*, see jit-tag constants below)
;;     1       7   align padding (= zero-filled)
;;     8       8   payload @ offset 8 (= i64 / f64 / u64-ptr)
;;    16      16   trailing pad to 32-byte total
;;
;; Tag byte values (= mirror `SEXP_TAG_*' in sexp.rs lines 217-229):
;;   NIL=0 T=1 INT=2 FLOAT=3 SYMBOL=4 STR=5 MUT_STR=6 CONS=7 VECTOR=8
;;   CHAR_TABLE=9 BOOL_VECTOR=10 CELL=11 RECORD=12
;;
;; Heap-fn deserialize protocol: caller supplies HEAP-FN with shape
;;   `(lambda (PTR LEN) BYTES)' returning the §95.c wire-format bytes
;; for the heap-backed payload at PTR.  Round-trip helpers below use
;; a hash-map heap-store + closure that captures it.
;;
;; This module produces / consumes the byte representation; the
;; actual JIT trampoline wiring + bake-images integration is §95.e.

;;; --- §95.d Rust ABI tag byte constants ----------------------------

(defconst nelisp-sexp-jit-tag-nil         #x00)
(defconst nelisp-sexp-jit-tag-t           #x01)
(defconst nelisp-sexp-jit-tag-int         #x02)
(defconst nelisp-sexp-jit-tag-float       #x03)
(defconst nelisp-sexp-jit-tag-symbol      #x04)
(defconst nelisp-sexp-jit-tag-str         #x05)
(defconst nelisp-sexp-jit-tag-mut-str     #x06)
(defconst nelisp-sexp-jit-tag-cons        #x07)
(defconst nelisp-sexp-jit-tag-vector      #x08)
(defconst nelisp-sexp-jit-tag-char-table  #x09)
(defconst nelisp-sexp-jit-tag-bool-vector #x0A)
(defconst nelisp-sexp-jit-tag-cell        #x0B)
(defconst nelisp-sexp-jit-tag-record      #x0C)

(defconst nelisp-sexp-jit-record-size 32
  "Fixed Sexp record byte size, mirroring `size_of::<Sexp>()'.")

(defconst nelisp-sexp-jit-payload-offset 8
  "Payload byte offset within a fixed-layout Sexp record.
Mirrors `SEXP_PAYLOAD_OFFSET' in sexp.rs (= line 270).")

;;; --- §95.d low-level bit helpers ----------------------------------

(defun nelisp-sexp--jit-u64-bytes (u)
  "Encode unsigned 64-bit U as 8-byte little-endian unibyte string."
  (unibyte-string (logand u           #xFF)
                  (logand (ash u  -8) #xFF)
                  (logand (ash u -16) #xFF)
                  (logand (ash u -24) #xFF)
                  (logand (ash u -32) #xFF)
                  (logand (ash u -40) #xFF)
                  (logand (ash u -48) #xFF)
                  (logand (ash u -56) #xFF)))

(defun nelisp-sexp--jit-i64-bytes (val)
  "Encode signed 64-bit VAL as 8-byte LE two's-complement string."
  (let ((u (if (< val 0) (+ val (ash 1 64)) val)))
    (nelisp-sexp--jit-u64-bytes u)))

(defun nelisp-sexp--jit-u64-from-bytes (bytes pos)
  "Decode 8-byte LE unsigned 64-bit from BYTES at POS."
  (logior (aref bytes pos)
          (ash (aref bytes (+ pos 1))  8)
          (ash (aref bytes (+ pos 2)) 16)
          (ash (aref bytes (+ pos 3)) 24)
          (ash (aref bytes (+ pos 4)) 32)
          (ash (aref bytes (+ pos 5)) 40)
          (ash (aref bytes (+ pos 6)) 48)
          (ash (aref bytes (+ pos 7)) 56)))

(defun nelisp-sexp--jit-i64-from-bytes (bytes pos)
  "Decode 8-byte LE signed 64-bit two's-complement from BYTES at POS."
  (let ((u (nelisp-sexp--jit-u64-from-bytes bytes pos)))
    (if (>= u (ash 1 63)) (- u (ash 1 64)) u)))

(defun nelisp-sexp--jit-f64-bytes (val)
  "Encode IEEE 754 binary64 VAL as 8-byte LE unibyte string.
Delegates to `nelisp-sexp--f64-to-bits' (= §95.c helper)."
  (nelisp-sexp--jit-u64-bytes (nelisp-sexp--f64-to-bits val)))

(defun nelisp-sexp--jit-f64-from-bytes (bytes pos)
  "Decode IEEE 754 binary64 LE from BYTES at POS via §95.c helper."
  (nelisp-sexp--f64-from-bits
   (nelisp-sexp--jit-u64-from-bytes bytes pos)))

;;; --- §95.d 32-byte record assembly --------------------------------

(defun nelisp-sexp--jit-record (tag payload-bytes)
  "Assemble fixed 32-byte Sexp record from TAG + 8-byte PAYLOAD-BYTES.
Layout: TAG @ 0, 7-byte zero pad, PAYLOAD-BYTES @ 8, 16-byte zero
tail pad.  PAYLOAD-BYTES must be exactly 8 bytes (= LE u64 / i64 /
f64 / ptr)."
  (unless (and (stringp payload-bytes) (= (length payload-bytes) 8))
    (signal 'wrong-type-argument
            (list 'nelisp-sexp-jit-8-byte-payload payload-bytes)))
  (concat (unibyte-string tag)
          (make-string 7 0)
          payload-bytes
          (make-string 16 0)))

;;; --- §95.d heap-store for round-trip helpers ----------------------
;;
;; A "heap store" is a hash-table mapping integer pointer keys to
;; unibyte byte strings (= §95.c wire-format encoding of the value
;; the pointer refers to).  Writers allocate fresh pointer keys via
;; an integer counter; readers look up keys via HEAP-FN.  In the
;; real Phase 47 bake chain, ptr values become ELF =.data= offsets
;; that Doc 93 linker patches to actual addresses; for §95.d we
;; only need the round-trip property, so any unique integer works.

(defun nelisp-sexp-jit-make-heap-store ()
  "Allocate a fresh heap-store hash-table used by write/read helpers.
Keys are integer pseudo-pointers, values are unibyte byte strings
(= §95.c wire-format encoding) representing the heap content for a
boxed variant payload."
  (cons 1 (make-hash-table :test 'eql)))

(defun nelisp-sexp--jit-heap-put (store bytes)
  "Allocate a fresh ptr in STORE, store BYTES at it, return the ptr.
STORE is a (NEXT-PTR . HASH) cons from `nelisp-sexp-jit-make-heap-store'.
Returned ptr is an integer used as the 8-byte u64 payload."
  (let ((ptr (car store)))
    (setcar store (1+ ptr))
    (puthash ptr bytes (cdr store))
    ptr))

(defun nelisp-sexp-jit-heap-store-fn (store)
  "Return a HEAP-FN closure that looks up STORE for `nelisp-sexp-jit-read-bytes'.
The closure has shape (lambda (PTR LEN) BYTES) and returns the byte
string previously emitted by `nelisp-sexp-jit-write-bytes' for PTR.
LEN is advisory (= caller hint) and ignored here since each heap
entry is self-describing via the §95.c wire format."
  (lambda (ptr _len) (gethash ptr (cdr store))))

;;; --- §95.d top-level layout serializer ----------------------------

(defun nelisp-sexp-jit-write-bytes (sexp &optional heap-store)
  "Emit the fixed-layout 32-byte Sexp record for SEXP.
Returns a 32-byte unibyte string mirroring the Rust ABI layout.

For boxed variants (= Symbol / Str / MutStr / Cons / Vector /
CharTable / BoolVector / Cell / Record), the heap payload is
written into HEAP-STORE (= cons-cell from
`nelisp-sexp-jit-make-heap-store').  If HEAP-STORE is omitted a
fresh store is allocated but discarded — caller must pass a store
to access the heap content for decode.

The heap entry for a boxed variant uses the §95.c wire format
(= recursive self-describing) so a single decode helper can walk
arbitrary nested structures without each variant carrying its own
length / shape header."
  (unless (nelisp-sexp-p sexp)
    (signal 'wrong-type-argument (list 'nelisp-sexp-p sexp)))
  (let* ((store (or heap-store (nelisp-sexp-jit-make-heap-store)))
         (tag (plist-get sexp :tag)))
    (cond
     ((null tag)
      (nelisp-sexp--jit-record nelisp-sexp-jit-tag-nil
                               (make-string 8 0)))
     ((eq tag t)
      (nelisp-sexp--jit-record nelisp-sexp-jit-tag-t
                               (make-string 8 0)))
     ((eq tag 'int)
      (nelisp-sexp--jit-record
       nelisp-sexp-jit-tag-int
       (nelisp-sexp--jit-i64-bytes (plist-get sexp :value))))
     ((eq tag 'float)
      (nelisp-sexp--jit-record
       nelisp-sexp-jit-tag-float
       (nelisp-sexp--jit-f64-bytes (plist-get sexp :value))))
     (t
      ;; Boxed variant: write payload to heap-store via §95.c wire
      ;; format, embed the ptr at offset 8 as 8-byte LE u64.
      (let* ((wire (nelisp-sexp-write-bytes sexp))
             (ptr (nelisp-sexp--jit-heap-put store wire))
             (jit-tag (nelisp-sexp--jit-tag-of tag)))
        (nelisp-sexp--jit-record
         jit-tag (nelisp-sexp--jit-u64-bytes ptr)))))))

(defun nelisp-sexp--jit-tag-of (tag)
  "Return Rust ABI tag byte for variant TAG symbol.
Signals `nelisp-sexp-unknown-tag' when TAG is unrecognised."
  (cond
   ((null tag) nelisp-sexp-jit-tag-nil)
   ((eq tag t) nelisp-sexp-jit-tag-t)
   ((eq tag 'int) nelisp-sexp-jit-tag-int)
   ((eq tag 'float) nelisp-sexp-jit-tag-float)
   ((eq tag 'symbol) nelisp-sexp-jit-tag-symbol)
   ((eq tag 'str) nelisp-sexp-jit-tag-str)
   ((eq tag 'mut-str) nelisp-sexp-jit-tag-mut-str)
   ((eq tag 'cons) nelisp-sexp-jit-tag-cons)
   ((eq tag 'vector) nelisp-sexp-jit-tag-vector)
   ((eq tag 'char-table) nelisp-sexp-jit-tag-char-table)
   ((eq tag 'bool-vector) nelisp-sexp-jit-tag-bool-vector)
   ((eq tag 'cell) nelisp-sexp-jit-tag-cell)
   ((eq tag 'record) nelisp-sexp-jit-tag-record)
   (t (signal 'nelisp-sexp-unknown-tag (list tag)))))

;;; --- §95.d top-level layout deserializer --------------------------

(defun nelisp-sexp-jit-read-bytes (bytes &optional pos heap-fn)
  "Decode one fixed-layout 32-byte Sexp record from BYTES at POS.
POS defaults to 0.  Returns the reconstructed Sexp plist.

For boxed variants (= tag >= 4 except as below) the 8-byte payload
@ offset 8 is interpreted as an integer ptr key, and HEAP-FN
(= (lambda (PTR LEN) BYTES)) is invoked to retrieve the §95.c
wire-format bytes that describe the value.  For inline variants
(= Nil / T / Int / Float) HEAP-FN is unused and may be nil.

Signals `nelisp-sexp-truncated' on short input or
`nelisp-sexp-unknown-tag' on unrecognised tag byte."
  (unless (stringp bytes)
    (signal 'wrong-type-argument (list 'stringp bytes)))
  (let ((pos (or pos 0)))
    (when (> (+ pos nelisp-sexp-jit-record-size) (length bytes))
      (signal 'nelisp-sexp-truncated
              (list 'jit-record pos (length bytes))))
    (let ((tag (aref bytes pos))
          (payload-pos (+ pos nelisp-sexp-jit-payload-offset)))
      (cond
       ((= tag nelisp-sexp-jit-tag-nil)
        (nelisp-sexp-make-nil))
       ((= tag nelisp-sexp-jit-tag-t)
        (nelisp-sexp-make-t))
       ((= tag nelisp-sexp-jit-tag-int)
        (nelisp-sexp-make-int
         (nelisp-sexp--jit-i64-from-bytes bytes payload-pos)))
       ((= tag nelisp-sexp-jit-tag-float)
        (nelisp-sexp-make-float
         (nelisp-sexp--jit-f64-from-bytes bytes payload-pos)))
       ((or (= tag nelisp-sexp-jit-tag-symbol)
            (= tag nelisp-sexp-jit-tag-str)
            (= tag nelisp-sexp-jit-tag-mut-str)
            (= tag nelisp-sexp-jit-tag-cons)
            (= tag nelisp-sexp-jit-tag-vector)
            (= tag nelisp-sexp-jit-tag-char-table)
            (= tag nelisp-sexp-jit-tag-bool-vector)
            (= tag nelisp-sexp-jit-tag-cell)
            (= tag nelisp-sexp-jit-tag-record))
        (unless (functionp heap-fn)
          (signal 'wrong-type-argument
                  (list 'nelisp-sexp-jit-heap-fn heap-fn)))
        (let* ((ptr (nelisp-sexp--jit-u64-from-bytes
                     bytes payload-pos))
               (heap-bytes (funcall heap-fn ptr nil)))
          (unless (stringp heap-bytes)
            (signal 'nelisp-sexp-jit-heap-miss (list ptr)))
          (car (nelisp-sexp-read-bytes heap-bytes 0))))
       (t (signal 'nelisp-sexp-unknown-tag (list tag pos)))))))

;;; --- §95.d round-trip helper --------------------------------------

(defun nelisp-sexp-jit-round-trip-p (sexp)
  "Return non-nil iff SEXP survives JIT layout write+heap+read.
Uses `nelisp-sexp-eq' since plist tail order is implementation-
defined while structural identity is the layout contract."
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (record (nelisp-sexp-jit-write-bytes sexp store))
         (decoded (nelisp-sexp-jit-read-bytes
                   record 0
                   (nelisp-sexp-jit-heap-store-fn store))))
    (nelisp-sexp-eq sexp decoded)))

;;; --- §95.d error symbols ------------------------------------------

(define-error 'nelisp-sexp-jit-heap-miss
  "Sexp DSL JIT heap-fn returned nil for a boxed-variant pointer")

;;; --- §95.e bake-images integration ================================
;;
;; §95.e couples the elisp serializer to the on-disk *.image artifact
;; produced by `build-tool/src/bin/nelisp-baker.rs'.  Investigation
;; (see Doc 95 §95.e SHIPPED block) found that the Rust baker emits a
;; THIRD wire format — NELIMG v3 frozen-heap (see
;; `build-tool/src/image.rs' §5.2) — distinct from both §95.c (=
;; variable-length single-Sexp tags) and §95.d (= 32-byte JIT record).
;; Because the current baker stashes the whole source string as a
;; single fallback-form entry (= empty env, 0 nodes / 0 globals / 1
;; fallback form), the v3 read/encode path is exercised only at the
;; "envelope" level.  The 3 entry points below operate on that
;; envelope so existing baker output round-trips byte-for-byte, and a
;; future Doc 95.f / Phase 48 dive into multi-node images can extend
;; the same helpers without touching §95.c/d.

;;; --- §95.e v3 envelope constants ----------------------------------

(defconst nelisp-sexp-bake-v3-magic
  (unibyte-string ?N ?E ?L ?I ?M ?G #x00 #x03)
  "NELIMG v3 magic header (= 8 bytes, matches `NELIMG_V3_MAGIC').")
(defconst nelisp-sexp-bake-v3-version 3
  "NELIMG v3 ABI version, u32 LE (= `NELIMG_V3_VERSION').")
(defconst nelisp-sexp-bake-v3-kind-frozen-heap #x01
  "NELIMG v3 KIND byte for frozen-heap container.")

;;; --- §95.e v3 envelope reader -------------------------------------

(defun nelisp-sexp-bake-read-image (file-path)
  "Parse Rust baker NELIMG v3 image at FILE-PATH into a plist.
Returns `(:magic-ok BOOL :version V :kind K :n-nodes N :n-globals G
:n-fallback-forms F :fallback-forms LIST :raw BYTES)'.  The :raw key
holds the full on-disk bytes for re-emit / byte diff."
  (unless (stringp file-path)
    (signal 'wrong-type-argument (list 'stringp file-path)))
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file-path)
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (>= (length bytes) 21)
      (signal 'nelisp-sexp-bake-truncated
              (list 'header (length bytes))))
    (let ((magic (substring bytes 0 8)))
      (unless (equal magic nelisp-sexp-bake-v3-magic)
        (signal 'nelisp-sexp-bake-bad-magic
                (list 'expected nelisp-sexp-bake-v3-magic 'got magic))))
    (let* ((vp (nelisp-sexp--read-u32 bytes 8))
           (version (car vp))
           (after-version (cdr vp)))
      (unless (= version nelisp-sexp-bake-v3-version)
        (signal 'nelisp-sexp-bake-bad-version (list version)))
      (let* ((kind (aref bytes after-version))
             (after-kind (1+ after-version)))
        (unless (= kind nelisp-sexp-bake-v3-kind-frozen-heap)
          (signal 'nelisp-sexp-bake-bad-kind (list kind)))
        (let* ((np (nelisp-sexp--read-u32 bytes after-kind))
               (n-nodes (car np))
               (p1 (cdr np)))
          ;; Envelope-only path: bail out if N_NODES > 0 (= Doc 95.f
          ;; will extend the reader once the baker switches to
          ;; encoding actual nodes).  Today every baked image has
          ;; N_NODES = 0 because `compile_elisp_to_image' uses the
          ;; fallback-form envelope exclusively.
          (when (> n-nodes 0)
            (signal 'nelisp-sexp-bake-unsupported
                    (list 'n-nodes n-nodes
                          'note "extend reader for Doc 95.f")))
          (let* ((gp (nelisp-sexp--read-u32 bytes p1))
                 (n-globals (car gp))
                 (p2 (cdr gp)))
            (when (> n-globals 0)
              (signal 'nelisp-sexp-bake-unsupported
                      (list 'n-globals n-globals
                            'note "extend reader for Doc 95.f")))
            (let* ((fp (nelisp-sexp--read-u32 bytes p2))
                   (n-fallback (car fp))
                   (p3 (cdr fp))
                   (forms nil))
              (dotimes (_ n-fallback)
                (let* ((sp (nelisp-sexp--read-utf8 bytes p3)))
                  (push (car sp) forms)
                  (setq p3 (cdr sp))))
              (unless (= p3 (length bytes))
                (signal 'nelisp-sexp-bake-trailing-bytes
                        (list 'pos p3 'len (length bytes))))
              (list :magic-ok t
                    :version version
                    :kind kind
                    :n-nodes n-nodes
                    :n-globals n-globals
                    :n-fallback-forms n-fallback
                    :fallback-forms (nreverse forms)
                    :raw bytes))))))))

;;; --- §95.e v3 envelope writer (= elisp side of byte-identity) -----

(defun nelisp-sexp-bake--encode-envelope (fallback-forms)
  "Encode an empty-env NELIMG v3 image holding FALLBACK-FORMS as bytes.
Mirrors `encode_v3_with_fallback' in `build-tool/src/image.rs' for
the envelope-only subset (= 0 nodes, 0 globals, N fallback forms).
FALLBACK-FORMS is a list of strings."
  (unless (listp fallback-forms)
    (signal 'wrong-type-argument (list 'listp fallback-forms)))
  (let ((buf nil))
    (setq buf (cons nelisp-sexp-bake-v3-magic buf))
    (setq buf (nelisp-sexp--write-u32 buf nelisp-sexp-bake-v3-version))
    (setq buf (nelisp-sexp--emit-byte
               buf nelisp-sexp-bake-v3-kind-frozen-heap))
    (setq buf (nelisp-sexp--write-u32 buf 0))    ; N_NODES
    (setq buf (nelisp-sexp--write-u32 buf 0))    ; N_GLOBALS
    (setq buf (nelisp-sexp--write-u32 buf (length fallback-forms)))
    (dolist (form fallback-forms)
      (setq buf (nelisp-sexp--write-utf8 buf form)))
    (apply #'concat (nreverse buf))))

;;; --- §95.e verify helper (= the core acceptance test) -------------

(defun nelisp-sexp-bake-verify-image (file-path)
  "Round-trip FILE-PATH through the elisp reader + writer.
Returns t when the re-encoded bytes are byte-identical to the on-
disk file (= byte-identity proof for `make bake-images').  Signals
`nelisp-sexp-bake-mismatch' with diagnostic plist when bytes diverge.
Signals the reader's error symbols on parse failure."
  (let* ((parsed (nelisp-sexp-bake-read-image file-path))
         (forms (plist-get parsed :fallback-forms))
         (raw (plist-get parsed :raw))
         (reemit (nelisp-sexp-bake--encode-envelope forms)))
    (cond
     ((equal raw reemit) t)
     (t (signal 'nelisp-sexp-bake-mismatch
                (list :file file-path
                      :on-disk-len (length raw)
                      :reemit-len (length reemit)
                      :first-diff
                      (cl-loop for i from 0 below (min (length raw)
                                                       (length reemit))
                               when (/= (aref raw i) (aref reemit i))
                               return i
                               finally return nil)))))))

;;; --- §95.e cross-impl fixture dump --------------------------------

(defun nelisp-sexp-bake-dump-fixture (fallback-forms file-path)
  "Write an elisp-encoded NELIMG v3 image of FALLBACK-FORMS to FILE-PATH.
The Rust baker can compare its own output against the produced bytes
to catch layout drift in either direction.  FALLBACK-FORMS is a list
of strings (= same shape `compile_elisp_to_image' passes to its
internal encoder)."
  (unless (stringp file-path)
    (signal 'wrong-type-argument (list 'stringp file-path)))
  (let ((bytes (nelisp-sexp-bake--encode-envelope fallback-forms))
        (coding-system-for-write 'no-conversion))
    (with-temp-file file-path
      (set-buffer-multibyte nil)
      (insert bytes))
    (length bytes)))

;;; --- §95.e error symbols ------------------------------------------

(define-error 'nelisp-sexp-bake-truncated
  "NELIMG v3 image truncated before end of envelope")
(define-error 'nelisp-sexp-bake-bad-magic
  "NELIMG v3 image magic header did not match")
(define-error 'nelisp-sexp-bake-bad-version
  "NELIMG v3 image ABI version unsupported")
(define-error 'nelisp-sexp-bake-bad-kind
  "NELIMG v3 image KIND byte not frozen-heap")
(define-error 'nelisp-sexp-bake-unsupported
  "NELIMG v3 image carries nodes/globals (not yet supported by §95.e)")
(define-error 'nelisp-sexp-bake-trailing-bytes
  "NELIMG v3 image has trailing bytes after fallback section")
(define-error 'nelisp-sexp-bake-mismatch
  "NELIMG v3 round-trip produced non-identical bytes")

(provide 'nelisp-sexp-dsl)

;;; nelisp-sexp-dsl.el ends here
