;;; cl-preloaded-shim.el --- built-in-class type lattice for the bare reader  -*- lexical-binding: t; -*-

;; The standalone nelisp reader cannot load the real cl-preloaded.el: its
;; built-in-class system (cl--class / built-in-class / cl-structure-class
;; structs, cl--define-built-in-type, cl--struct-register-child, the parent
;; lattice) assumes the Emacs C-core type infrastructure that does not come up
;; standalone (cl-preloaded loads but registers nothing; cl--define-built-in-type
;; ends up unbound).  cl-generic dispatch on built-in types AND oclosure-define
;; both need this lattice: the typeof-generalizer does
;;   (cl--class-allparents (cl--find-class (cl-type-of VALUE)))
;; to compute a type's precedence list, and oclosure-define needs (cl--find-class
;; 'closure) as the base parent of every oclosure type.

;; This is a MINIMAL, self-contained re-implementation ("Path B", nelisp Doc 157)
;; built on the reader's own cl-defstruct (records).  It must be LOADED (not baked
;; into the AOT prelude): the type registrations are top-level `(put NAME 'cl--class
;; ...)' forms, and a top-level `put' in the baked prelude does not persist into
;; the boot image (only definitions do).  Load it after cl-lib.el.

;; Standalone only — host Emacs / the nemacs bundle use the real cl-preloaded.el.

;;; Code:

(when (not (boundp 'emacs-version))

  ;; --- class object structs -------------------------------------------------
  ;; cl--class: abstract base.  oclosure.el's `(cl-defstruct (oclosure--class
  ;; (:include cl--class) ...))' needs this registered in the reader's struct
  ;; registry so `:include' resolves its slots.
  (cl-defstruct (cl--class
                 (:constructor nil)
                 (:constructor cl--class-make (name docstring parents slots index-table)))
    name docstring parents slots index-table)

  ;; built-in-class: type descriptor for built-in types.
  (cl-defstruct (built-in-class
                 (:include cl--class)
                 (:constructor nil)
                 (:constructor built-in-class--make (name docstring parents)))
    )

  ;; --- parent-precedence list ----------------------------------------------
  ;; cl--class-allparents CLASS -> (NAME . merged-parents).  A depth-first,
  ;; first-seen-wins merge (close to Emacs' `merge-ordered-lists'); the SET is
  ;; exact, which is what type matching needs (order only affects precedence
  ;; among several applicable methods).
  (defun cl--class-allparents (class)
    (cons (cl--class-name class)
          (let ((acc nil))
            (dolist (parent (cl--class-parents class))
              (dolist (ancestor (cl--class-allparents parent))
                (unless (memq ancestor acc) (push ancestor acc))))
            (nreverse acc))))

  ;; --- type registration macro ---------------------------------------------
  ;; (cl--define-built-in-type NAME PARENTS [DOCSTRING] SLOTS...) registers a
  ;; built-in-class for NAME whose parents are the (already-registered) PARENTS.
  ;; DOCSTRING/SLOTS are accepted and ignored (no slot access on built-ins here).
  (defmacro cl--define-built-in-type (name parents &optional _docstring &rest _slots)
    (unless (listp parents) (setq parents (list parents)))
    (list 'put (list 'quote name) (list 'quote 'cl--class)
          (list 'built-in-class--make (list 'quote name) nil
                (list 'mapcar (list 'function 'cl--find-class)
                      (list 'quote parents)))))

  ;; --- value -> type --------------------------------------------------------
  ;; cl-type-of returns the most specific built-in type of OBJECT, used by
  ;; cl-generic's typeof-generalizer.  Predicate-based (the reader's `type-of'
  ;; is unreliable for some objects).
  (defun cl-type-of (object)
    (cond ((null object) 'null)
          ((integerp object) 'fixnum)
          ((floatp object) 'float)
          ((symbolp object) 'symbol)
          ((stringp object) 'string)
          ((consp object) 'cons)
          ((vectorp object) 'vector)
          ((recordp object) 'record)
          ((and (fboundp 'hash-table-p) (hash-table-p object)) 'hash-table)
          ((functionp object) 'function)
          (t 'atom)))

  ;; --- the built-in type lattice (parents declared before children) --------
  (cl--define-built-in-type t nil)
  (cl--define-built-in-type atom t)
  (cl--define-built-in-type sequence t)
  (cl--define-built-in-type list sequence)
  (cl--define-built-in-type array (sequence atom))
  (cl--define-built-in-type number-or-marker atom)
  (cl--define-built-in-type number (number-or-marker))
  (cl--define-built-in-type float (number))
  (cl--define-built-in-type integer-or-marker (number-or-marker))
  (cl--define-built-in-type integer (number integer-or-marker))
  (cl--define-built-in-type fixnum (integer))
  (cl--define-built-in-type bignum (integer))
  (cl--define-built-in-type marker (integer-or-marker))
  (cl--define-built-in-type symbol atom)
  (cl--define-built-in-type boolean (symbol))
  (cl--define-built-in-type null (boolean list))
  (cl--define-built-in-type record (atom))
  (cl--define-built-in-type vector (array))
  (cl--define-built-in-type bool-vector (array))
  (cl--define-built-in-type char-table (array))
  (cl--define-built-in-type string (array))
  (cl--define-built-in-type cons (list))
  (cl--define-built-in-type function (atom))
  (cl--define-built-in-type compiled-function (function))
  (cl--define-built-in-type closure (function))
  (cl--define-built-in-type byte-code-function (compiled-function closure))
  (cl--define-built-in-type hash-table atom)
  (cl--define-built-in-type buffer atom)
  (cl--define-built-in-type marker (integer-or-marker)))

(provide 'cl-preloaded-shim)
;;; cl-preloaded-shim.el ends here
