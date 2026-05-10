;;; nelisp-sexp-dsl.el --- Phase 47 Sexp layout DSL (§95.a spike)  -*- lexical-binding: t; -*-

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

(provide 'nelisp-sexp-dsl)

;;; nelisp-sexp-dsl.el ends here
