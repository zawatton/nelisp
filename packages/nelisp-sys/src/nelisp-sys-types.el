;;; nelisp-sys-types.el --- Type model for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The typed-IR type model (Doc 130 data model + Doc 131 type system).
;; Types are canonical s-expressions:
;;
;;   scalars   i8 u8 i16 u16 i32 u32 i64 u64 isize usize bool void f32 f64 char32
;;   pointer   (ptr T)
;;   array     (array T N)
;;   struct    (struct NAME)        ; resolved against a struct env
;;   slice     (slice T) (slice-mut T)
;;   owned     (owned T)
;;   borrow    (& T) (&mut T)
;;   function  (fn ((ARG TYPE)...) RET EFFECTS)
;;   sum       (result T E) (option T)
;;
;; This module owns: type predicates, structural equality, validity
;; checking, the struct definition environment, and Copy/move
;; classification.  Sizes and offsets live in `nelisp-sys-abi-layout' (they
;; depend on a target descriptor); this module is target-independent.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)

(define-error 'nelisp-sys-type-error
  "nelisp-sys type error" 'nelisp-sys-error)

(defconst nelisp-sys-types--scalars
  ;; NAME -> (:bits BITS-OR-ptr :class int|float|bool|void|char :signed BOOL)
  '((i8     :bits 8   :class int   :signed t)
    (u8     :bits 8   :class int   :signed nil)
    (i16    :bits 16  :class int   :signed t)
    (u16    :bits 16  :class int   :signed nil)
    (i32    :bits 32  :class int   :signed t)
    (u32    :bits 32  :class int   :signed nil)
    (i64    :bits 64  :class int   :signed t)
    (u64    :bits 64  :class int   :signed nil)
    (isize  :bits ptr :class int   :signed t)
    (usize  :bits ptr :class int   :signed nil)
    (bool   :bits 8   :class bool  :signed nil)
    (void   :bits 0   :class void  :signed nil)
    (f32    :bits 32  :class float :signed t)
    (f64    :bits 64  :class float :signed t)
    (char32 :bits 32  :class char  :signed nil))
  "Scalar type metadata.  `ptr' bits means target pointer width.")

(defun nelisp-sys-type-scalar-meta (type)
  "Return the scalar metadata plist for TYPE, or nil if TYPE is not scalar."
  (and (symbolp type) (cdr (assq type nelisp-sys-types--scalars))))

(defun nelisp-sys-type-scalar-p (type)
  "Return non-nil if TYPE is a scalar type symbol."
  (and (nelisp-sys-type-scalar-meta type) t))

(defun nelisp-sys-type-integer-p (type)
  "Return non-nil if TYPE is an integer scalar."
  (eq (plist-get (nelisp-sys-type-scalar-meta type) :class) 'int))

(defun nelisp-sys-type-float-p (type)
  "Return non-nil if TYPE is a floating-point scalar."
  (eq (plist-get (nelisp-sys-type-scalar-meta type) :class) 'float))

(defun nelisp-sys-type-bool-p (type)
  "Return non-nil if TYPE is the `bool' scalar."
  (eq type 'bool))

(defun nelisp-sys-type-void-p (type)
  "Return non-nil if TYPE is `void'."
  (eq type 'void))

(defun nelisp-sys-type-signed-p (type)
  "Return non-nil if integer scalar TYPE is signed.
Signals `nelisp-sys-type-error' if TYPE is not an integer scalar."
  (let ((m (nelisp-sys-type-scalar-meta type)))
    (unless (eq (plist-get m :class) 'int)
      (signal 'nelisp-sys-type-error
              (list (format "not an integer type: %S" type))))
    (plist-get m :signed)))

(defun nelisp-sys-type-pointer-p (type)
  "Return non-nil if TYPE is a raw pointer `(ptr T)'."
  (and (consp type) (eq (car type) 'ptr)))

(defun nelisp-sys-type-array-p (type)
  "Return non-nil if TYPE is `(array T N)'."
  (and (consp type) (eq (car type) 'array)))

(defun nelisp-sys-type-struct-ref-p (type)
  "Return non-nil if TYPE is a struct reference `(struct NAME)'."
  (and (consp type) (eq (car type) 'struct)))

(defun nelisp-sys-type-slice-p (type)
  "Return non-nil if TYPE is `(slice T)' or `(slice-mut T)'."
  (and (consp type) (memq (car type) '(slice slice-mut))))

(defun nelisp-sys-type-slice-mut-p (type)
  "Return non-nil if TYPE is `(slice-mut T)'."
  (and (consp type) (eq (car type) 'slice-mut)))

(defun nelisp-sys-type-owned-p (type)
  "Return non-nil if TYPE is `(owned T)'."
  (and (consp type) (eq (car type) 'owned)))

(defun nelisp-sys-type-ref-p (type)
  "Return non-nil if TYPE is a borrow `(& T)' or `(&mut T)'."
  (and (consp type) (memq (car type) '(& &mut))))

(defun nelisp-sys-type-mut-ref-p (type)
  "Return non-nil if TYPE is a mutable borrow `(&mut T)'."
  (and (consp type) (eq (car type) '&mut)))

(defun nelisp-sys-type-fn-p (type)
  "Return non-nil if TYPE is a function type `(fn ARGS RET EFFECTS)'."
  (and (consp type) (eq (car type) 'fn)))

(defun nelisp-sys-type-element (type)
  "Return the element/pointee type of a compound TYPE.
For (ptr T) (array T N) (slice T) (slice-mut T) (owned T) (& T) (&mut T)
return T.  Signals for non-compound types."
  (cond
   ((or (nelisp-sys-type-pointer-p type)
        (nelisp-sys-type-array-p type)
        (nelisp-sys-type-slice-p type)
        (nelisp-sys-type-owned-p type)
        (nelisp-sys-type-ref-p type))
    (nth 1 type))
   (t (signal 'nelisp-sys-type-error
              (list (format "type has no element type: %S" type))))))

;;; Struct environment.

(defun nelisp-sys-types-env-make ()
  "Return a fresh, empty struct definition environment."
  (make-hash-table :test 'eq))

(defun nelisp-sys-types-env-add (env name repr fields)
  "Register struct NAME in ENV with REPR (`c' or `sys') and FIELDS.
FIELDS is a list of (FIELD-NAME . TYPE) or (FIELD-NAME TYPE) pairs.
Signals on duplicate definition."
  (when (gethash name env)
    (signal 'nelisp-sys-type-error
            (list (format "duplicate struct definition: %S" name))))
  (let ((norm (mapcar (lambda (f)
                        (cons (car f)
                              (if (and (consp (cdr f)) (null (cddr f)))
                                  (cadr f) ; (NAME TYPE)
                                (cdr f)))) ; (NAME . TYPE)
                      fields)))
    (puthash name (list :repr repr :fields norm) env)
    name))

(defun nelisp-sys-types-env-get (env name)
  "Return the definition plist for struct NAME in ENV, or nil."
  (and env (gethash name env)))

(defun nelisp-sys-types-struct-repr (env name)
  "Return the :repr of struct NAME in ENV."
  (plist-get (nelisp-sys-types-env-get env name) :repr))

(defun nelisp-sys-types-struct-fields (env name)
  "Return the ((FIELD . TYPE)...) field list of struct NAME in ENV."
  (plist-get (nelisp-sys-types-env-get env name) :fields))

(defun nelisp-sys-types-struct-field-type (env name field)
  "Return the type of FIELD in struct NAME, or nil if absent."
  (cdr (assq field (nelisp-sys-types-struct-fields env name))))

;;; Validity.

(defun nelisp-sys-type-valid-p (type &optional env)
  "Return non-nil if TYPE is a well-formed type.
Struct references are validated against ENV when ENV is non-nil; with
no ENV a `(struct NAME)' is accepted structurally (forward reference)."
  (cond
   ((nelisp-sys-type-scalar-p type) t)
   ((nelisp-sys-type-pointer-p type)
    (let ((el (nth 1 type)))
      (or (nelisp-sys-type-void-p el)         ; (ptr void) = opaque pointer
          (nelisp-sys-type-valid-p el env))))
   ((nelisp-sys-type-array-p type)
    (and (integerp (nth 2 type)) (>= (nth 2 type) 0)
         (nelisp-sys-type-valid-p (nth 1 type) env)))
   ((or (nelisp-sys-type-slice-p type)
        (nelisp-sys-type-owned-p type)
        (nelisp-sys-type-ref-p type))
    (nelisp-sys-type-valid-p (nth 1 type) env))
   ((nelisp-sys-type-struct-ref-p type)
    (let ((name (nth 1 type)))
      (and (symbolp name)
           (or (null env) (nelisp-sys-types-env-get env name) t))))
   ((nelisp-sys-type-fn-p type)
    (and (cl-every (lambda (a) (nelisp-sys-type-valid-p (cadr a) env))
                   (nth 1 type))
         (nelisp-sys-type-valid-p (nth 2 type) env)))
   ((and (consp type) (memq (car type) '(result option)))
    (cl-every (lambda (tt) (nelisp-sys-type-valid-p tt env)) (cdr type)))
   (t nil)))

(defun nelisp-sys-type-check-valid (type &optional env)
  "Signal `nelisp-sys-type-error' unless TYPE is well-formed under ENV.
Return TYPE on success."
  (unless (nelisp-sys-type-valid-p type env)
    (signal 'nelisp-sys-type-error
            (list (format "invalid type: %S" type))))
  type)

;;; Structural equality.

(defun nelisp-sys-type-equal (a b)
  "Return non-nil if types A and B are structurally equal."
  (cond
   ((and (symbolp a) (symbolp b)) (eq a b))
   ((and (consp a) (consp b))
    (equal a b))
   (t nil)))

;;; Copy / move classification (Doc 131).

(defun nelisp-sys-type-copy-p (type &optional env)
  "Return non-nil if TYPE is a Copy type under struct ENV.
Copy: integers, bool, char, float, raw pointers, immutable borrows,
immutable slices, fixed arrays of Copy, and all-Copy plain structs.
Move-only: (owned T), (&mut T), (slice-mut T)."
  (cond
   ((nelisp-sys-type-scalar-p type)
    (not (nelisp-sys-type-void-p type)))
   ((nelisp-sys-type-pointer-p type) t)
   ((and (nelisp-sys-type-ref-p type) (not (nelisp-sys-type-mut-ref-p type))) t)
   ((nelisp-sys-type-mut-ref-p type) nil)
   ((nelisp-sys-type-owned-p type) nil)
   ((nelisp-sys-type-slice-p type)
    (not (nelisp-sys-type-slice-mut-p type)))
   ((nelisp-sys-type-array-p type)
    (nelisp-sys-type-copy-p (nth 1 type) env))
   ((nelisp-sys-type-struct-ref-p type)
    (let ((fields (nelisp-sys-types-struct-fields env (nth 1 type))))
      (and fields
           (cl-every (lambda (f) (nelisp-sys-type-copy-p (cdr f) env)) fields))))
   (t nil)))

(defun nelisp-sys-type-move-only-p (type &optional env)
  "Return non-nil if TYPE is move-only (a non-Copy value type)."
  (and (not (nelisp-sys-type-copy-p type env))
       (not (nelisp-sys-type-void-p type))
       (not (nelisp-sys-type-fn-p type))))

(provide 'nelisp-sys-types)

;;; nelisp-sys-types.el ends here
