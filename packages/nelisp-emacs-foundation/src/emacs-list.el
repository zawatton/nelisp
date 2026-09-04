;;; emacs-list.el --- NeLisp port of Emacs C core list accessors  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2.1 — Layer 2.
;;
;; Ports the list-accessor primitives from `fns.c' / `data.c' that
;; subr.el and most Elisp libraries assume.  Polyfills use only
;; bootstrap-eval primitives + the earlier `emacs-fns' helpers (=
;; mapcar / nreverse).  Each is gated on `unless (fboundp ...)`.

;;; Code:

;;;; --- safe cons access ---------------------------------------------------

(unless (fboundp 'car-safe)
  (defun car-safe (object)
    "Return the car of OBJECT if it is a cons cell, otherwise nil."
    (if (consp object) (car object) nil)))

(unless (fboundp 'cdr-safe)
  (defun cdr-safe (object)
    "Return the cdr of OBJECT if it is a cons cell, otherwise nil."
    (if (consp object) (cdr object) nil)))

;;;; --- copy / shape -------------------------------------------------------

(unless (fboundp 'copy-sequence)
  (defun copy-sequence (sequence)
    "Return a shallow copy of SEQUENCE (= proper-list polyfill)."
    (let ((acc nil)
          (cur sequence))
      (while cur
        (setq acc (cons (car cur) acc))
        (setq cur (cdr cur)))
      (nreverse acc))))

;; Record-aware `copy-sequence' upgrade (Doc 33 item 243).  The NeLisp
;; stdlib prelude's `copy-sequence' handles nil / cons / string / vector
;; and falls through to `(t seq)' for everything else -- including
;; native records -- so copying a record returns the SAME object.  EIEIO
;; relies on real record copying in a load-bearing spot: `make-instance'
;; is `(copy-sequence (eieio--class-default-object-cache class))', so
;; with the identity fallback every constructed object IS the class's
;; shared default-object cache -- all instances alias one another and
;; each `oset' scribbles over every other live instance (first visible
;; casualty: magit's section objects).  Gate = a runtime self-probe: only
;; wrap when a freshly-made record round-trips `eq' through the current
;; `copy-sequence' (host Emacs's C implementation copies records fine and
;; never trips this).
(when (and (fboundp 'make-record)
           (fboundp 'recordp)
           (fboundp 'nelisp--record-length)
           (fboundp 'nelisp--record-type)
           (fboundp 'nelisp--record-ref)
           (fboundp 'nelisp--record-set)
           (condition-case nil
               (let ((probe (make-record 'emacs-list--copy-probe 1 nil)))
                 (and (recordp probe)
                      (not (recordp '(emacs-list--copy-probe)))
                      (eq (copy-sequence probe) probe)))
             (error nil)))
  (defalias 'emacs-list--copy-sequence-nonrecord
    (symbol-function 'copy-sequence)
    "The pre-upgrade `copy-sequence' (correct for all non-record types).")

  ;; Doc 33 item 244 (CRITICAL, upgrades item 243's record copy): the
  ;; runtime has NO working record-length primitive.  Native `length' on
  ;; a tag-12 record returns 0, and the stdlib prelude's
  ;; `nelisp--record-length' is just `(length rec)', so it returns 0 for
  ;; every record.  Item 243's copy loop therefore ran ZERO iterations
  ;; and, far worse, `(make-record TYPE 0 nil)' produced a ZERO-slot
  ;; copy: EIEIO's `make-instance' copies the class's default-object
  ;; cache through `copy-sequence', so every constructed object was a
  ;; 0-slot record whose subsequent `oset' calls (slot indexes 1..N)
  ;; were silent OUT-OF-BOUNDS heap writes -- wild scribbles over
  ;; whatever the allocator placed next.  This is the likely true root
  ;; of the session's \"GC collects live lexframe children\"-signature
  ;; SIGSEGVs (heap corruption, not only the Doc 155 collector gap).
  ;; Since no primitive can recover a record's slot count after creation
  ;; (and `eq' hash tables cannot look records back up either --
  ;; `puthash' with a record key stores an entry `gethash' never finds),
  ;; recover it from the record's TYPE TAG instead:
  ;;   - symbol tag (cl-defstruct-shaped records: `eieio--class',
  ;;     `cl-slot-descriptor', ...): every instance of the type has the
  ;;     same arity, so stash the count on the tag symbol's plist at
  ;;     creation via wrapped `make-record'/`record'.
  ;;   - record tag (EIEIO objects: the tag is the class record): the
  ;;     class's own slots vector length is authoritative.
  ;;   - anything else: fall back to `nelisp--record-length' (returns 0
  ;;     today; kept so a future fixed runtime wins).

  (defalias 'emacs-list--orig-make-record (symbol-function 'make-record)
    "The runtime's own `make-record' (correct except for length recovery).")
  (defun make-record (type slots init)
    "Create a record; remember the type's slot count (Doc 33 item 244)."
    (when (and (symbolp type)
               (< (or (get type 'emacs-list--record-slot-count) -1) slots))
      (put type 'emacs-list--record-slot-count slots))
    (emacs-list--orig-make-record type slots init))

  (when (fboundp 'record)
    (defalias 'emacs-list--orig-record (symbol-function 'record)
      "The runtime's own `record' builtin.")
    (defun record (type &rest slots)
      "Create a record from SLOTS; remember the type's slot count."
      (let ((n (length slots)))
        (when (and (symbolp type)
                   (< (or (get type 'emacs-list--record-slot-count) -1) n))
          (put type 'emacs-list--record-slot-count n)))
      (apply 'emacs-list--orig-record type slots)))

  (defun emacs-list--record-length (rec)
    "Return REC's slot count (excluding the type slot), best effort.
See the item 244 note above for the recovery order."
    (let ((tag (nelisp--record-type rec)))
      (or (and (symbolp tag) (get tag 'emacs-list--record-slot-count))
          ;; EIEIO object: the tag is the class metadata object itself
          ;; (a list-backed cl-defstruct under this substrate, so no
          ;; `recordp' pre-check -- `eieio--class-p' is the test).
          (and (not (symbolp tag))
               (fboundp 'eieio--class-p)
               (fboundp 'eieio--class-slots)
               (condition-case nil
                   (and (eieio--class-p tag)
                        (length (eieio--class-slots tag)))
                 (error nil)))
          (nelisp--record-length rec))))

  (defun copy-sequence (sequence)
    "Return a shallow copy of SEQUENCE, records included.
Records go through the `nelisp--record-*' accessors with the item 244
length recovery; every other type delegates to the previous
implementation (see the upgrade comments above)."
    (if (recordp sequence)
        (let* ((n (emacs-list--record-length sequence))
               (new (make-record (nelisp--record-type sequence) n nil))
               (i 0))
          (while (< i n)
            (nelisp--record-set new i (nelisp--record-ref sequence i))
            (setq i (1+ i)))
          new)
      (emacs-list--copy-sequence-nonrecord sequence))))

(unless (fboundp 'copy-tree)
  (defun copy-tree (tree &optional vecp)
    "Return a recursive copy of TREE.  Conses are recursively copied."
    (ignore vecp)
    (cond
     ((not (consp tree)) tree)
     (t (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))))

;; Doc 33 item 244 (M2 completion blocker): `copy-alist' is an ordinary
;; host Emacs subr that every prior caller in this substrate happened
;; never to need, so it was never polyfilled -- until EIEIO's
;; `eieio-defclass-internal' hit it while building any CHILD class's
;; own `initarg-tuples'/allocation alists from its parent's (`magit-
;; section' itself has no user-defined parent, so its defclass never
;; took this path; any subclass, e.g. `magit-commit-section', does).
;; Missing `copy-alist' made the whole `(defclass CHILD (PARENT) ...)'
;; form abort with `(void-function copy-alist)' right after the child
;; had already been pushed onto the parent's `eieio--class-children'
;; (an earlier, unrelated step), leaving the child's own `cl--class'
;; unset and its constructor function undefined -- the root cause of
;; `(void-function magit-unpushed-section)'.
(unless (fboundp 'copy-alist)
  (defun copy-alist (alist)
    "Return a copy of ALIST.
Only the top-level list structure and each element's own cons cell are
copied; each element's key and value are shared with ALIST, matching
real Emacs's `copy-alist'."
    (mapcar (lambda (elt) (if (consp elt) (cons (car elt) (cdr elt)) elt))
            alist)))

(unless (fboundp 'last)
  (defun last (list &optional n)
    "Return the last cons-cell of LIST (= a list of length N at the tail).
N defaults to 1.  Linear walk."
    (let ((n (or n 1))
          (len 0)
          (cur list))
      (while cur (setq len (+ len 1)) (setq cur (cdr cur)))
      (let ((skip (- len n))
            (c list))
        (while (and c (> skip 0))
          (setq c (cdr c))
          (setq skip (- skip 1)))
        c))))

(unless (fboundp 'butlast)
  (defun butlast (list &optional n)
    "Return a copy of LIST with the last N elements removed (default 1)."
    (let* ((n (or n 1))
           (len 0)
           (cur list))
      (while cur (setq len (+ len 1)) (setq cur (cdr cur)))
      (let ((keep (- len n))
            (acc nil)
            (c list))
        (while (> keep 0)
          (setq acc (cons (car c) acc))
          (setq c (cdr c))
          (setq keep (- keep 1)))
        (nreverse acc)))))

(unless (fboundp 'nbutlast)
  (defun nbutlast (list &optional n)
    "Modify LIST to remove the last N elements (default 1); return it.
Destructive counterpart of `butlast' (cf. subr.el).  Used by
org-element-ast's node-property handling (`nbutlast props 2')."
    (let ((m (length list)))
      (or n (setq n 1))
      (and (< n m)
           (progn
             (when (> n 0) (setcdr (nthcdr (- (1- m) n) list) nil))
             list)))))


;;;; --- positional access --------------------------------------------------

(unless (fboundp 'nth)
  (defun nth (n list)
    "Return Nth element of LIST (0-indexed), or nil if out of range."
    (let ((c list))
      (while (and c (> n 0))
        (setq c (cdr c))
        (setq n (- n 1)))
      (car c))))

(unless (fboundp 'nthcdr)
  (defun nthcdr (n list)
    "Return the Nth cdr of LIST, or nil."
    (let ((c list))
      (while (and c (> n 0))
        (setq c (cdr c))
        (setq n (- n 1)))
      c)))

(unless (fboundp 'cadr)
  (defun cadr (list) "Return (car (cdr LIST))." (car (cdr list))))

(unless (fboundp 'cddr)
  (defun cddr (list) "Return (cdr (cdr LIST))." (cdr (cdr list))))

(unless (fboundp 'caddr)
  (defun caddr (list) "Return (car (cdr (cdr LIST)))." (car (cdr (cdr list)))))


;;;; --- membership ---------------------------------------------------------

(unless (fboundp 'memq)
  (defun memq (element list)
    "Return tail of LIST whose car is `eq' to ELEMENT, or nil."
    (let ((c list)
          (found nil))
      (while (and c (not found))
        (if (eq (car c) element) (setq found c)
          (setq c (cdr c))))
      found)))

(unless (fboundp 'member)
  (defun member (element list)
    "Return tail of LIST whose car is `equal' to ELEMENT, or nil."
    (let ((c list)
          (found nil))
      (while (and c (not found))
        (if (equal (car c) element) (setq found c)
          (setq c (cdr c))))
      found)))


;;;; --- alist access -------------------------------------------------------

(unless (fboundp 'assq)
  (defun assq (key alist)
    "Return first cons-cell in ALIST whose car is `eq' to KEY."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell) (eq (car cell) key))
              (setq found cell)
            (setq c (cdr c)))))
      found)))

(unless (fboundp 'assoc)
  (defun assoc (key alist &optional testfn)
    "Return first cons-cell in ALIST whose car is `equal' to KEY.
TESTFN, when supplied, is called as (TESTFN KEY CAR)."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell)
                   (if testfn
                       (funcall testfn key (car cell))
                     (equal key (car cell))))
              (setq found cell)
            (setq c (cdr c)))))
      found)))

(unless (fboundp 'rassq)
  (defun rassq (value alist)
    "Return first cons-cell in ALIST whose cdr is `eq' to VALUE."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell) (eq (cdr cell) value))
              (setq found cell)
            (setq c (cdr c)))))
      found)))

(unless (fboundp 'rassoc)
  (defun rassoc (value alist)
    "Return first cons-cell in ALIST whose cdr is `equal' to VALUE."
    (let ((c alist)
          (found nil))
      (while (and c (not found))
        (let ((cell (car c)))
          (if (and (consp cell) (equal (cdr cell) value))
              (setq found cell)
            (setq c (cdr c)))))
      found)))


;;;; --- removal ------------------------------------------------------------

(unless (fboundp 'delq)
  (defun delq (element list)
    "Return LIST with all `eq'-matching ELEMENT cells removed (copy)."
    (let ((acc nil)
          (cur list))
      (while cur
        (unless (eq (car cur) element)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'delete)
  (defun delete (element list)
    "Return LIST with all `equal'-matching ELEMENT cells removed (copy)."
    (let ((acc nil)
          (cur list))
      (while cur
        (unless (equal (car cur) element)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'remq)
  (defalias 'remq 'delq))

(unless (fboundp 'remove)
  (defalias 'remove 'delete))

;; `nconc' was a nil stub in the standalone runtime; install the real
;; destructive concatenation (stub-aware so it overrides the stub).
(unless (and (fboundp 'nconc) (not (get 'nconc 'emacs-stub-bulk)))
  (defun nconc (&rest lists)
    "Concatenate LISTS by destructively modifying all but the last.
Nil arguments are ignored; the last argument may be any object."
    (let ((result nil) (tail nil))
      (dolist (l lists)
        (when l
          (if tail (setcdr tail l) (setq result l))
          (setq tail l)
          (while (and (consp tail) (consp (cdr tail)))
            (setq tail (cdr tail)))))
      result))
  (put 'nconc 'emacs-stub-bulk nil))


;;;; --- length helpers -----------------------------------------------------

(unless (fboundp 'safe-length)
  (defun safe-length (list)
    "Return the length of LIST, treating dotted-pair tails as terminators."
    (let ((n 0)
          (c list))
      (while (consp c)
        (setq n (+ n 1))
        (setq c (cdr c)))
      n)))


;;;; --- alist deletion --------------------------------------------------------

(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    "Return ALIST with all entries whose car is `eq' to KEY removed.
Non-cons elements are preserved.  The returned list shares
structure with the tail of ALIST after the last removed cell."
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (eq (car cell) key))
          (push cell out)))
      (nreverse out))))

(unless (fboundp 'assoc-delete-all)
  (defun assoc-delete-all (key alist &optional test)
    "Return ALIST with all entries whose car matches KEY removed.
TEST defaults to `equal'."
    (unless test (setq test #'equal))
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (funcall test (car cell) key))
          (push cell out)))
      (nreverse out))))

(unless (fboundp 'rassoc-delete-all)
  (defun rassoc-delete-all (value alist)
    "Return ALIST with all entries whose cdr is `equal' to VALUE removed."
    (let (out)
      (dolist (cell alist)
        (unless (and (consp cell) (equal (cdr cell) value))
          (push cell out)))
      (nreverse out))))

(provide 'emacs-list)

;;; emacs-list.el ends here
