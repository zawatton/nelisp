;;; emacs-char-table.el --- char-table substrate for standalone NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 08 §2 (2026-06-05) — Layer 2 (nemacs substrate).
;;
;; The standalone NeLisp bootstrap only had `nil'-stubs for the
;; char-table API (in `emacs-stub-bulk.el'), and the lightweight
;; case-table substrate (`case-table.el', 259-element fixed vectors)
;; is not wired into the cold-boot bundle.  That left vendor code such
;; as isearch.el's `isearch-mode-map' defvar unable to evaluate:
;;
;;     (let ((map (make-keymap)))
;;       (or (char-table-p (nth 1 map))
;;           (error "..."))                       ; <- assertion failed
;;       (set-char-table-range (nth 1 map) (cons #x100 (max-char)) ...))
;;
;; This module provides a real, sparse char-table substrate that:
;;
;;   - satisfies `char-table-p' (so the assertion passes once
;;     `make-keymap' embeds one — see `emacs-keymap.el'),
;;   - stores huge ranges such as `(#x100 . #x3FFFFF)' sparsely instead
;;     of materialising ~4M slots (which would crash or OOM), and
;;   - supplies the missing `max-char' / `char-table-subtype' /
;;     `char-table-parent' primitives.
;;
;; Representation (tagged vector — `vectorp' ops are always available,
;; unlike a hypothetical reader-level char-table type):
;;
;;   [--nemacs-char-table SUBTYPE DEFAULT PARENT ASCII-VEC RANGES EXTRA]
;;     0 TAG       = `--nemacs-char-table' (the `char-table-p' key)
;;     1 SUBTYPE   = the `make-char-table' subtype argument
;;     2 DEFAULT   = value for characters with no explicit entry
;;     3 PARENT    = parent char-table, or nil
;;     4 ASCII-VEC = 256 nil-sentinel slots, fast path for char < 256
;;     5 RANGES    = list of ((FROM . TO) . VAL), newest first, char >= 256
;;     6 EXTRA     = (make-vector N nil), with N fixed at construction
;;
;; Length is 7 so `(> (length x) 6)' holds, distinguishing a char-table
;; from incidental short vectors.
;;
;; Two-mode design (mirrors `case-table.el' / `emacs-keymap-builtins.el'):
;; under host Emacs the real C primitives win (the unprefixed names are
;; only installed when not already `fboundp'); under standalone NeLisp we
;; install our implementations unconditionally so they override the
;; earlier `emacs-stub-bulk.el' nil-stubs.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- char-tables use the library char-table representation.
;;; Code:

(defconst emacs-char-table--tag '--nemacs-char-table
  "Symbol stored in slot 0 that identifies a NeLisp char-table.")

(defconst emacs-char-table--ascii-size 256
  "Number of fast direct-indexed slots (characters 0..255).")

(defconst emacs-char-table--extra-slots 10
  "Legacy extra-slot capacity.
New tables allocate the count specified by their subtype property.")

(defconst emacs-char-table--category-set-size 128
  "Number of category slots in a character's category set.")

(defvar emacs-char-table--standard-category-table nil
  "Standalone standard category table.")

(defvar emacs-char-table--current-category-table nil
  "Standalone current-buffer category table.
The lightweight runtime has no native per-buffer category-table slot yet;
the shared table still gives vendor libraries real mutation semantics.")

(defconst emacs-char-table--max-char #x3FFFFF
  "Largest character code (Doc 05: UTF-8 / Unicode range).")

;; Slot indices.
(defconst emacs-char-table--i-subtype 1)
(defconst emacs-char-table--i-default 2)
(defconst emacs-char-table--i-parent 3)
(defconst emacs-char-table--i-ascii 4)
(defconst emacs-char-table--i-ranges 5)
(defconst emacs-char-table--i-extra 6)

;; Capture the array primitives before the standalone compatibility bridge is
;; installed.  `defvar' deliberately preserves the original functions when
;; this file is reloaded after `aref' and `aset' have been wrapped.
(defvar emacs-char-table--raw-aref (symbol-function 'aref))
(defvar emacs-char-table--raw-aset (symbol-function 'aset))

(defun emacs-char-table--native-vector-bridge-p ()
  "Return non-nil when NeLisp dispatches tagged vectors natively."
  (and (fboundp 'nelisp--char-table-vector-bridge-p)
       (nelisp--char-table-vector-bridge-p)))

(defun emacs-char-table--primitive-ref (array index)
  "Read ARRAY's physical slot INDEX, without char-table dispatch."
  (if (emacs-char-table--native-vector-bridge-p)
      (nelisp--raw-aref array index)
    (funcall emacs-char-table--raw-aref array index)))

(defun emacs-char-table--native-syntax-p (object)
  "Recognize the runtime's bootstrap syntax-table record."
  (and (recordp object)
       (= (length object) 3)
       (eq (emacs-char-table--primitive-ref object 0) 'nelisp--syntax-table)
       (hash-table-p (emacs-char-table--primitive-ref object 1))))

(defvar emacs-char-table--native-syntax-storage
  (make-hash-table :test 'eq :weakness 'key)
  "Canonical sparse storage for retained bootstrap syntax-table records.")

(defun emacs-char-table--syntax-entry (class char)
  "Translate the bootstrap CLASS character for CHAR into a syntax entry."
  (if (consp class) class
    (let ((code 0) (spec " .w_()'\"$\\/<>@!|"))
      (while (and (< code (length spec)) (/= class (aref spec code)))
        (setq code (1+ code)))
      (cons (if (< code (length spec)) code 2)
            (cdr (assq char '((40 . 41) (41 . 40) (91 . 93) (93 . 91)
                             (123 . 125) (125 . 123))))))))

(defun emacs-char-table--storage (table)
  "Return TABLE's sparse storage, retaining bootstrap record identity.
Bootstrap entries are class characters; library entries include matching
characters and flags.  A retained record gets one shared sparse view rather
than being discarded in favor of the standard table."
  (if (not (emacs-char-table--native-syntax-p table)) table
    (or (gethash table emacs-char-table--native-syntax-storage)
        (let* ((parent (emacs-char-table--primitive-ref table 2))
               (view (emacs-char-table-make 'syntax-table
                                            (unless parent (cons 2 nil)))))
          (puthash table view emacs-char-table--native-syntax-storage)
          (emacs-char-table--raw-set view emacs-char-table--i-parent parent)
          ;; Preserve the bootstrap representation's implicit classes.  Its
          ;; default is word outside ASCII, word for ASCII letters/digits,
          ;; whitespace for these five characters, and symbol otherwise.
          (unless parent
            (let ((char 0))
              (while (< char 128)
                (emacs-char-table-set
                 view char
                 (emacs-char-table--syntax-entry
                  (cond ((memq char '(9 10 12 13 32)) 32)
                        ((or (<= 48 char 57) (<= 65 char 90)
                             (<= 97 char 122)) 119)
                        (t 95)) char))
                (setq char (1+ char)))))
          (maphash (lambda (char class)
                     (emacs-char-table-set
                      view char (emacs-char-table--syntax-entry class char)))
                   (emacs-char-table--primitive-ref table 1))
          view))))

(defun emacs-char-table--raw-ref (array index)
  "Read storage slot INDEX of ARRAY, including bootstrap syntax tables."
  (when (recordp array) (setq array (emacs-char-table--storage array)))
  (if (emacs-char-table--native-vector-bridge-p)
      (nelisp--raw-aref array index)
    (funcall emacs-char-table--raw-aref array index)))

(defun emacs-char-table--raw-set (array index value)
  "Write storage slot INDEX of ARRAY, including bootstrap syntax tables."
  (when (recordp array) (setq array (emacs-char-table--storage array)))
  (if (emacs-char-table--native-vector-bridge-p)
      (nelisp--raw-aset array index value)
    (funcall emacs-char-table--raw-aset array index value)))

(defun emacs-char-table--standalone-p ()
  "Return non-nil under standalone NeLisp.
The NeLisp reader binds `emacs-version' just like host Emacs, so a bare
`(not (boundp 'emacs-version))' test misfires there.  Detect the
standalone path by a NeLisp-only primitive (`nl-write-file'), matching
the marker used in `emacs-keymap.el' / `emacs-fileio-builtins.el'."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(defun emacs-char-table--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this substrate."
  (if (emacs-char-table--standalone-p)
      t
    (not (fboundp symbol))))

;;;; --- constructor / predicate ----------------------------------------

(defun emacs-char-table-make (subtype &optional init)
  "Return a fresh char-table with symbol SUBTYPE and initial value INIT.
The subtype's `char-table-extra-slots' property determines its extra
slot count, which remains fixed for the lifetime of the table."
  (unless (symbolp subtype)
    (signal 'wrong-type-argument (list 'symbolp subtype)))
  (let ((slots (or (get subtype 'char-table-extra-slots)
                   ;; The standalone does not preload these symbol properties.
                   (cdr (assq subtype '((case-table . 3) (category-table . 2))))
                   0)))
    (unless (and (integerp slots) (>= slots 0))
      (signal 'wrong-type-argument (list 'wholenump slots)))
    (let ((ct (make-vector 7 nil))
          (ascii (make-vector emacs-char-table--ascii-size init)))
      (emacs-char-table--raw-set ct 0 emacs-char-table--tag)
      (emacs-char-table--raw-set ct emacs-char-table--i-subtype subtype)
      (emacs-char-table--raw-set ct emacs-char-table--i-default init)
      (emacs-char-table--raw-set ct emacs-char-table--i-parent nil)
      (emacs-char-table--raw-set ct emacs-char-table--i-ascii ascii)
      (emacs-char-table--raw-set
       ct emacs-char-table--i-ranges
       (when init
         (list (cons (cons emacs-char-table--ascii-size
                           emacs-char-table--max-char) init))))
      (emacs-char-table--raw-set ct emacs-char-table--i-extra
                                 (make-vector slots nil))
      ct)))

(defun emacs-char-table-p (object)
  "Return non-nil when OBJECT is a NeLisp char-table."
  (or (emacs-char-table--native-syntax-p object)
      (and (vectorp object)
           (> (length object) 6)
           (eq (emacs-char-table--raw-ref object 0) emacs-char-table--tag))))

(defun emacs-char-table-ascii-vector (ct)
  "Return CT's raw 256-slot ASCII vector (characters 0..255)."
  (emacs-char-table--raw-ref ct emacs-char-table--i-ascii))

;;;; --- single-character access ----------------------------------------

(defun emacs-char-table--range-lookup (ct char)
  "Return the stored value for CHAR from CT's RANGES, or the symbol
`emacs-char-table--unset' when no range covers CHAR."
  (let ((ranges (emacs-char-table--raw-ref ct emacs-char-table--i-ranges))
        (result 'emacs-char-table--unset))
    (while (and ranges (eq result 'emacs-char-table--unset))
      (let* ((entry (car ranges))
             (key (car entry)))
        (when (and (<= (car key) char) (<= char (cdr key)))
          (setq result (cdr entry))))
      (setq ranges (cdr ranges)))
    result))

(defun emacs-char-table--fallback (ct char)
  "Return CT's own default or its parent's value for CHAR."
  (let ((default
         (emacs-char-table--raw-ref ct emacs-char-table--i-default))
        (parent
         (emacs-char-table--raw-ref ct emacs-char-table--i-parent)))
    (cond
     (default default)
     (parent (emacs-char-table-ref parent char))
     (t nil))))

(defun emacs-char-table-ref (ct char)
  "Return CT's value for character CHAR (with default / parent fallback)."
  (cond
   ((not (integerp char)) (emacs-char-table--raw-ref ct emacs-char-table--i-default))
   ((and (>= char 0) (< char emacs-char-table--ascii-size))
    (let ((v (emacs-char-table--raw-ref
              (emacs-char-table--raw-ref ct emacs-char-table--i-ascii) char)))
      (if v v (emacs-char-table--fallback ct char))))
   (t
    (let ((v (emacs-char-table--range-lookup ct char)))
      (if (and (not (eq v 'emacs-char-table--unset)) v)
          v
        (emacs-char-table--fallback ct char))))))

(defun emacs-char-table-set (ct char value)
  "Set CT's value for a single character CHAR to VALUE."
  (if (and (integerp char) (>= char 0) (< char emacs-char-table--ascii-size))
      (emacs-char-table--raw-set
       (emacs-char-table--raw-ref ct emacs-char-table--i-ascii) char value)
    (emacs-char-table--raw-set
     ct emacs-char-table--i-ranges
     (cons (cons (cons char char) value)
           (emacs-char-table--raw-ref ct emacs-char-table--i-ranges))))
  value)

;;;; --- category-table integration ------------------------------------

(defun emacs-char-table-make-category-table ()
  "Construct an empty standalone category table.
Each character maps to a 128-slot category set, matching the index space
of host Emacs's bool-vector returned by `char-category-set'."
  (emacs-char-table-make 'category-table
                         (make-vector emacs-char-table--category-set-size nil)))

(defun emacs-char-table-category-table-p (object)
  "Return non-nil when OBJECT is a standalone category table."
  (and (emacs-char-table-p object)
       (eq (emacs-char-table-subtype object) 'category-table)))

(defun emacs-char-table-standard-category-table ()
  "Return the standalone standard category table."
  (or emacs-char-table--standard-category-table
      (setq emacs-char-table--standard-category-table
            (emacs-char-table-make-category-table))))

(defun emacs-char-table-category-table ()
  "Return the standalone current category table."
  (or emacs-char-table--current-category-table
      (setq emacs-char-table--current-category-table
            (emacs-char-table-standard-category-table))))

(defun emacs-char-table-set-category-table (table)
  "Use category TABLE for the standalone current buffer and return TABLE."
  (unless (emacs-char-table-category-table-p table)
    (signal 'wrong-type-argument (list 'category-table-p table)))
  (setq emacs-char-table--current-category-table table))

(defun emacs-char-table-copy-category-table (&optional table)
  "Return a copy of category TABLE, defaulting to the standard table."
  (let* ((source (or table (emacs-char-table-standard-category-table)))
         (_ (unless (emacs-char-table-category-table-p source)
              (signal 'wrong-type-argument (list 'category-table-p source))))
         (copy (emacs-char-table-copy
                source
                (lambda (value)
                  ;; Category sets are vectors stored as char-table values.
                  ;; Keep copied tables independent, as
                  ;; `copy-category-table' does on the host.
                  (if (vectorp value) (copy-sequence value) value)))))
    ;; The default value is not visited by `emacs-char-table-copy's VALFN.
    ;; Copy it separately so direct `char-category-set' mutation in the copy
    ;; cannot alter the source table's default set.
    (emacs-char-table--raw-set
     copy emacs-char-table--i-default
     (copy-sequence
      (emacs-char-table--raw-ref copy emacs-char-table--i-default)))
    (when (and (boundp 'emacs-cc-category-1--docs)
               (fboundp 'emacs-cc-category-1--docs-for))
      (puthash copy (copy-sequence (emacs-cc-category-1--docs-for source))
               emacs-cc-category-1--docs))
    copy))

(defun emacs-char-table--category-set (table character)
  "Return TABLE's category set for CHARACTER, creating a valid default."
  (unless (and (integerp character)
               (>= character 0)
               (<= character emacs-char-table--max-char))
    (signal 'wrong-type-argument (list 'characterp character)))
  (let ((set (emacs-char-table-ref table character)))
    (if (and (vectorp set)
             (= (length set) emacs-char-table--category-set-size))
        set
      (let ((new (make-vector emacs-char-table--category-set-size nil)))
        (emacs-char-table-set table character new)
        new))))

(defun emacs-char-table-char-category-set (character)
  "Return CHARACTER's 128-slot standalone category set."
  (emacs-char-table--category-set (emacs-char-table-category-table) character))

(defun emacs-char-table-modify-category-entry
    (character category &optional table reset)
  "Add CATEGORY to CHARACTER's category set in TABLE.
CHARACTER may be one character or an inclusive (FROM . TO) range;
RESET removes CATEGORY.  CATEGORY must be defined in TABLE."
  (let (from to)
    (if (integerp character)
        (setq from character to character)
      (unless (consp character)
        (signal 'wrong-type-argument (list 'consp character)))
      (setq from (car character) to (cdr character)))
    (dolist (c (list from to))
      (unless (and (integerp c) (<= 0 c) (<= c emacs-char-table--max-char))
        (signal 'wrong-type-argument (list 'characterp c))))
    (unless (and (integerp category) (>= category #x20) (<= category #x7e))
      (signal 'wrong-type-argument (list 'categoryp category)))
    (let ((table (or table (emacs-char-table-category-table))))
      (unless (emacs-char-table-category-table-p table)
        (signal 'wrong-type-argument (list 'category-table-p table)))
      (unless (category-docstring category table)
        (error "Undefined category: %c" category))
      (while (<= from to)
        ;; Copy the shared default before modifying an individual character.
        (let ((set (copy-sequence
                    (emacs-char-table--category-set table from))))
          (aset set category (not reset))
          (emacs-char-table-set table from set))
        (setq from (1+ from))))
    nil))

;;;; --- range access ---------------------------------------------------

(defun emacs-char-table--fill-ascii (ct from to value)
  "Store VALUE into CT's ASCII vector for chars FROM..TO (clamped 0..255)."
  (let ((i (max from 0))
        (hi (min to (1- emacs-char-table--ascii-size)))
        (vec (emacs-char-table--raw-ref ct emacs-char-table--i-ascii)))
    (while (<= i hi)
      (emacs-char-table--raw-set vec i value)
      (setq i (1+ i)))))

(defun emacs-char-table-set-range (ct range value)
  "Set CT entries selected by RANGE to VALUE.
RANGE is nil (the default value), t (the whole table), a character,
or a cons (FROM . TO).  Large supra-ASCII ranges are stored sparsely
rather than materialised."
  (emacs-char-table--check-table ct)
  (cond
   ((null range)
    (emacs-char-table--raw-set ct emacs-char-table--i-default value))
   ((eq range t)
    (emacs-char-table--fill-ascii ct 0 (1- emacs-char-table--ascii-size) value)
    (emacs-char-table--raw-set
     ct emacs-char-table--i-ranges
     (list (cons (cons emacs-char-table--ascii-size
                       emacs-char-table--max-char) value))))
   ((and (integerp range) (<= 0 range)
         (<= range emacs-char-table--max-char))
    (emacs-char-table-set ct range value))
   ((consp range)
    (let ((from (car range))
          (to (cdr range)))
      (dolist (character (list from to))
        (unless (and (integerp character) (<= 0 character)
                     (<= character emacs-char-table--max-char))
          (signal 'wrong-type-argument (list 'characterp character))))
      (when (<= from to)
        (emacs-char-table--fill-ascii ct from to value)
        (when (>= to emacs-char-table--ascii-size)
          (emacs-char-table--raw-set
           ct emacs-char-table--i-ranges
           (cons (cons (cons (max from emacs-char-table--ascii-size) to)
                       value)
                 (emacs-char-table--raw-ref
                  ct emacs-char-table--i-ranges)))))))
   (t (error "Invalid RANGE argument to ‘set-char-table-range’")))
  value)

(defun emacs-char-table-range (ct range)
  "Return CT's default or its value at RANGE's first character."
  (emacs-char-table--check-table ct)
  (cond
   ((null range) (emacs-char-table--raw-ref ct emacs-char-table--i-default))
   ((and (integerp range) (<= 0 range)
         (<= range emacs-char-table--max-char))
    (emacs-char-table-ref ct range))
   ((consp range)
    (dolist (c (list (car range) (cdr range)))
      (unless (and (integerp c) (<= 0 c) (<= c emacs-char-table--max-char))
        (signal 'wrong-type-argument (list 'characterp c))))
    (emacs-char-table-ref ct (car range)))
   (t (error "Invalid RANGE argument to ‘char-table-range’"))))

;;;; --- parent / subtype / extra slots ---------------------------------

(defun emacs-char-table--check-table (ct)
  "Signal a type error unless CT is a char-table."
  (unless (emacs-char-table-p ct)
    (signal 'wrong-type-argument (list 'char-table-p ct))))

(defun emacs-char-table-parent (ct)
  "Return CT's parent char-table, or nil."
  (emacs-char-table--check-table ct)
  (emacs-char-table--raw-ref ct emacs-char-table--i-parent))

(defun emacs-char-table-set-parent (ct parent)
  "Set CT's parent to PARENT (a char-table or nil).  Returns PARENT."
  (emacs-char-table--check-table ct)
  (when parent
    (unless (emacs-char-table-p parent)
      (signal 'wrong-type-argument (list 'char-table-p parent)))
    (let ((ancestor parent))
      (while (and ancestor (emacs-char-table-p ancestor))
        (when (eq ancestor ct)
          (error "Attempt to make a chartable be its own parent"))
        (setq ancestor (emacs-char-table--raw-ref
                        ancestor emacs-char-table--i-parent)))))
  (emacs-char-table--raw-set ct emacs-char-table--i-parent parent)
  parent)

(defun emacs-char-table-subtype (ct)
  "Return CT's subtype."
  (emacs-char-table--check-table ct)
  (emacs-char-table--raw-ref ct emacs-char-table--i-subtype))

(defun emacs-char-table-extra-slot (ct n)
  "Return CT's extra slot N."
  (emacs-char-table--check-table ct)
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'fixnump n)))
  (let ((slots (length (emacs-char-table--raw-ref
                        ct emacs-char-table--i-extra))))
    (unless (and (<= 0 n) (< n slots))
      (signal 'args-out-of-range (list ct n))))
  (emacs-char-table--raw-ref
   (emacs-char-table--raw-ref ct emacs-char-table--i-extra) n))

(defun emacs-char-table-set-extra-slot (ct n value)
  "Set CT's extra slot N to VALUE."
  (emacs-char-table--check-table ct)
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'fixnump n)))
  (let ((slots (emacs-char-table--raw-ref ct emacs-char-table--i-extra)))
    (unless (and (<= 0 n) (< n (length slots)))
      (signal 'args-out-of-range (list ct n))))
  (emacs-char-table--raw-set
   (emacs-char-table--raw-ref ct emacs-char-table--i-extra) n value))

;;;; --- iteration ------------------------------------------------------

(defun emacs-char-table--map-boundaries (ct)
  "Return sorted character boundaries of CT and its parent tables.
Only ASCII slots and sparse range endpoints are examined; the full
character space is never enumerated."
  (let ((boundaries (list 0 (1+ emacs-char-table--max-char)))
        (table ct))
    (while table
      (let ((vec (emacs-char-table--raw-ref table emacs-char-table--i-ascii))
            (ranges (emacs-char-table--raw-ref table emacs-char-table--i-ranges))
            (i 1))
        (while (< i emacs-char-table--ascii-size)
          (unless (eq (emacs-char-table--raw-ref vec (1- i))
                      (emacs-char-table--raw-ref vec i))
            (push i boundaries))
          (setq i (1+ i)))
        (push emacs-char-table--ascii-size boundaries)
        (dolist (entry ranges)
          (let ((from (max emacs-char-table--ascii-size (car (car entry))))
                (to (min emacs-char-table--max-char (cdr (car entry)))))
            (when (<= from to)
              (push from boundaries)
              (push (1+ to) boundaries)))))
      ;; A non-nil local default masks every parent entry.
      (setq table
            (unless (emacs-char-table--raw-ref table emacs-char-table--i-default)
              (emacs-char-table--raw-ref table emacs-char-table--i-parent))))
    (sort boundaries #'<)))

(defun emacs-char-table--map-run (function key from to value)
  "Call FUNCTION for a non-nil run FROM..TO with VALUE.
Reuse KEY for ranges, as GNU Emacs does; callers must copy retained keys."
  (if (= from to)
      (when value (funcall function from value))
    (setcar key from)
    (setcdr key to)
    (when value (funcall function key value))))

(defun emacs-char-table-map (function ct)
  "Call FUNCTION with (KEY VALUE) for each non-nil effective run of CT.
Adjacent characters with `eq' values form a range, including values
inherited from defaults and parents.  Return nil."
  (emacs-char-table--check-table ct)
  (let ((boundaries (emacs-char-table--map-boundaries ct))
        (key (cons nil nil))
        (from 0)
        ;; GNU seeds the pending run from the first stored slot or parent,
        ;; before traversal applies the local default to nil slots.
        (value (let ((first (emacs-char-table--raw-ref
                             (emacs-char-table--raw-ref
                              ct emacs-char-table--i-ascii) 0))
                     (parent (emacs-char-table--raw-ref
                              ct emacs-char-table--i-parent)))
                 (or first
                     (if parent
                         (emacs-char-table-ref parent 0)
                       (emacs-char-table--raw-ref
                        ct emacs-char-table--i-default))))))
    (while boundaries
      (let ((next (car boundaries)))
        (when (and (>= next from) (<= next emacs-char-table--max-char))
          (let ((next-value (emacs-char-table-ref ct next)))
            (unless (eq value next-value)
              (emacs-char-table--map-run function key from (1- next) value)
              (setq from next value next-value)))))
      (setq boundaries (cdr boundaries)))
    (emacs-char-table--map-run function key from emacs-char-table--max-char value))
  nil)

(defun emacs-char-table-copy (ct &optional valfn)
  "Return a copy of CT.
When VALFN is non-nil it transforms each non-nil value (used by
`copy-keymap' to recurse into nested keymaps)."
  (let ((new (emacs-char-table-make
              (emacs-char-table--raw-ref ct emacs-char-table--i-subtype)
              (emacs-char-table--raw-ref ct emacs-char-table--i-default))))
    (emacs-char-table--raw-set
     new emacs-char-table--i-parent
     (emacs-char-table--raw-ref ct emacs-char-table--i-parent))
    (let ((src (emacs-char-table--raw-ref ct emacs-char-table--i-ascii))
          (dst (emacs-char-table--raw-ref new emacs-char-table--i-ascii))
          (i 0))
      (while (< i emacs-char-table--ascii-size)
        (let ((v (emacs-char-table--raw-ref src i)))
          (emacs-char-table--raw-set
           dst i (if (and valfn v) (funcall valfn v) v)))
        (setq i (1+ i))))
    (emacs-char-table--raw-set
     new emacs-char-table--i-ranges
     (mapcar (lambda (e)
               (cons (car e)
                     (if (and valfn (cdr e)) (funcall valfn (cdr e)) (cdr e))))
             (emacs-char-table--raw-ref ct emacs-char-table--i-ranges)))
    (emacs-char-table--raw-set
     new emacs-char-table--i-extra
     (copy-sequence
      (emacs-char-table--raw-ref ct emacs-char-table--i-extra)))
    new))

;;;; --- public array bridge -------------------------------------------

(defun emacs-char-table--check-character (index)
  "Signal the host-compatible type error when INDEX is not a character."
  (unless (integerp index)
    (signal 'wrong-type-argument (list 'fixnump index)))
  (unless (and (>= index 0) (<= index emacs-char-table--max-char))
    (signal 'wrong-type-argument (list 'characterp index))))

(defun emacs-char-table-aref (array index)
  "Return ARRAY's element at INDEX, including sparse NeLisp char-tables."
  (if (emacs-char-table-p array)
      (progn
        (emacs-char-table--check-character index)
        (emacs-char-table-ref array index))
    (emacs-char-table--raw-ref array index)))

(defun emacs-char-table-aset (array index value)
  "Set ARRAY's element at INDEX to VALUE, including NeLisp char-tables."
  (if (emacs-char-table-p array)
      (progn
        (emacs-char-table--check-character index)
        (emacs-char-table-set array index value))
    (emacs-char-table--raw-set array index value)))

(defun emacs-char-table-max-char (&optional unicode)
  "Return the largest character code, or Unicode maximum when UNICODE is non-nil."
  (if unicode #x10FFFF emacs-char-table--max-char))

(defun emacs-char-table--bootstrap-syntax-lookup (table char)
  "Return the class character for CHAR in either syntax-table representation."
  (let ((entry (emacs-char-table-ref table char)))
    (aref " .w_()'\"$\\/<>@!|" (logand (if entry (car entry) 2) 255))))

(defun emacs-char-table--bootstrap-syntax-put (table char class)
  "Store bootstrap CLASS in TABLE's canonical syntax storage."
  (emacs-char-table-set table char (emacs-char-table--syntax-entry class char)))

;;;; --- install unprefixed names ---------------------------------------

(when (emacs-char-table--standalone-p)
  ;; Keep ordinary arrays on the native bridge.  Syntax-table installation
  ;; normalizes retained records to their sparse views before exposing them
  ;; as the active table; the prefixed accessors also accept legacy records.
  (unless (emacs-char-table--native-vector-bridge-p)
    (fset 'aref #'emacs-char-table-aref)
    (fset 'aset #'emacs-char-table-aset))
  (when (fboundp 'nelisp--syntax-lookup)
    (fset 'nelisp--syntax-lookup #'emacs-char-table--bootstrap-syntax-lookup)
    (fset 'nelisp--syntax-put #'emacs-char-table--bootstrap-syntax-put))
  (fset 'char-table-p #'emacs-char-table-p)
  (fset 'make-char-table #'emacs-char-table-make)
  (fset 'char-table-range #'emacs-char-table-range)
  (fset 'set-char-table-range #'emacs-char-table-set-range)
  (fset 'char-table-parent #'emacs-char-table-parent)
  (fset 'set-char-table-parent #'emacs-char-table-set-parent)
  (fset 'char-table-subtype #'emacs-char-table-subtype)
  (fset 'char-table-extra-slot #'emacs-char-table-extra-slot)
  (fset 'set-char-table-extra-slot #'emacs-char-table-set-extra-slot)
  (fset 'map-char-table #'emacs-char-table-map)
  (fset 'max-char #'emacs-char-table-max-char)
  (fset 'make-category-table #'emacs-char-table-make-category-table)
  (fset 'category-table-p #'emacs-char-table-category-table-p)
  (fset 'category-table #'emacs-char-table-category-table)
  (fset 'standard-category-table #'emacs-char-table-standard-category-table)
  (fset 'set-category-table #'emacs-char-table-set-category-table)
  (fset 'copy-category-table #'emacs-char-table-copy-category-table)
  (fset 'char-category-set #'emacs-char-table-char-category-set)
  (fset 'modify-category-entry #'emacs-char-table-modify-category-entry))

(when (emacs-char-table--install-function-p 'char-table-p)
  (defalias 'char-table-p #'emacs-char-table-p))
(when (emacs-char-table--install-function-p 'make-char-table)
  (defalias 'make-char-table #'emacs-char-table-make))
(when (emacs-char-table--install-function-p 'char-table-range)
  (defalias 'char-table-range #'emacs-char-table-range))
(when (emacs-char-table--install-function-p 'set-char-table-range)
  (defalias 'set-char-table-range #'emacs-char-table-set-range))
(when (emacs-char-table--install-function-p 'char-table-parent)
  (defalias 'char-table-parent #'emacs-char-table-parent))
(when (emacs-char-table--install-function-p 'set-char-table-parent)
  (defalias 'set-char-table-parent #'emacs-char-table-set-parent))
(when (emacs-char-table--install-function-p 'char-table-subtype)
  (defalias 'char-table-subtype #'emacs-char-table-subtype))
(when (emacs-char-table--install-function-p 'char-table-extra-slot)
  (defalias 'char-table-extra-slot #'emacs-char-table-extra-slot))
(when (emacs-char-table--install-function-p 'set-char-table-extra-slot)
  (defalias 'set-char-table-extra-slot #'emacs-char-table-set-extra-slot))
(when (emacs-char-table--install-function-p 'map-char-table)
  (defalias 'map-char-table #'emacs-char-table-map))
(when (emacs-char-table--install-function-p 'max-char)
  (defalias 'max-char #'emacs-char-table-max-char))
(when (emacs-char-table--install-function-p 'make-category-table)
  (defalias 'make-category-table #'emacs-char-table-make-category-table))
(when (emacs-char-table--install-function-p 'category-table-p)
  (defalias 'category-table-p #'emacs-char-table-category-table-p))
(when (emacs-char-table--install-function-p 'category-table)
  (defalias 'category-table #'emacs-char-table-category-table))
(when (emacs-char-table--install-function-p 'standard-category-table)
  (defalias 'standard-category-table #'emacs-char-table-standard-category-table))
(when (emacs-char-table--install-function-p 'set-category-table)
  (defalias 'set-category-table #'emacs-char-table-set-category-table))
(when (emacs-char-table--install-function-p 'copy-category-table)
  (defalias 'copy-category-table #'emacs-char-table-copy-category-table))
(when (emacs-char-table--install-function-p 'char-category-set)
  (defalias 'char-category-set #'emacs-char-table-char-category-set))
(when (emacs-char-table--install-function-p 'modify-category-entry)
  (defalias 'modify-category-entry #'emacs-char-table-modify-category-entry))

(provide 'emacs-char-table)

;;; emacs-char-table.el ends here
