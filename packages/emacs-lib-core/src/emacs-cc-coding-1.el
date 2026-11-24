;;; emacs-cc-coding-1.el --- GNU coding.c port: coding system registry -*- lexical-binding: t; -*-

;; Port of the registry part of GNU Emacs 31.1 src/coding.c: the spec table
;; (`Vcoding_system_hash_table', name -> [ATTRS ALIASES EOL-TYPE]),
;; `define-coding-system-internal', `define-coding-system-alias',
;; `coding-system-p' and friends, the coding categories and their
;; priorities, and the built-in `no-conversion' / `undecided' definitions of
;; `syms_of_coding'.  The converters (decode/encode) are not part of this
;; file; see emacs-cc-coding-3.el.

;;; Code:

(eval-and-compile
  (defconst emacs-cc-coding--attr-slots
    '((base-name . 0) (docstring . 1) (mnemonic . 2) (type . 3)
      (charset-list . 4) (ascii-compat . 5) (decode-tbl . 6) (encode-tbl . 7)
      (trans-tbl . 8) (post-read . 9) (pre-write . 10) (default-char . 11)
      (for-unibyte . 12) (plist . 13) (category . 14) (safe-charsets . 15)
      (charset-valids . 16) (ccl-decoder . 17) (ccl-encoder . 18)
      (ccl-valids . 19) (iso-initial . 20) (iso-usage . 21) (iso-request . 22)
      (iso-flags . 23) (utf-bom . 24) (utf-16-endian . 25)
      (emacs-mule-full . 26) (undecided-null . 27) (undecided-iso . 28)
      (undecided-prefer-utf-8 . 29) (last-index . 30))
    "Slot indices of the attribute vector (`enum coding_attr_index')."))

(defmacro emacs-cc-coding--attr (attrs slot)
  (list 'aref attrs (cdr (assq slot emacs-cc-coding--attr-slots))))
(defmacro emacs-cc-coding--attr-set (attrs slot val)
  (list 'aset attrs (cdr (assq slot emacs-cc-coding--attr-slots)) val))

;; `enum coding_category', in C order.
(defconst emacs-cc-coding--category-names
  [coding-category-iso-7 coding-category-iso-7-tight coding-category-iso-8-1
   coding-category-iso-8-2 coding-category-iso-7-else coding-category-iso-8-else
   coding-category-utf-8-auto coding-category-utf-8 coding-category-utf-8-sig
   coding-category-utf-16-auto coding-category-utf-16-be
   coding-category-utf-16-le coding-category-utf-16-be-nosig
   coding-category-utf-16-le-nosig coding-category-charset
   coding-category-sjis coding-category-big5 coding-category-ccl
   coding-category-emacs-mule coding-category-raw-text
   coding-category-undecided]
  "`Vcoding_category_table'.")

(defconst emacs-cc-coding--category-max 21)
(defconst emacs-cc-coding--cat-iso-7 0)
(defconst emacs-cc-coding--cat-iso-7-tight 1)
(defconst emacs-cc-coding--cat-iso-8-1 2)
(defconst emacs-cc-coding--cat-iso-8-2 3)
(defconst emacs-cc-coding--cat-iso-7-else 4)
(defconst emacs-cc-coding--cat-iso-8-else 5)
(defconst emacs-cc-coding--cat-utf-8-auto 6)
(defconst emacs-cc-coding--cat-utf-8-nosig 7)
(defconst emacs-cc-coding--cat-utf-8-sig 8)
(defconst emacs-cc-coding--cat-utf-16-auto 9)
(defconst emacs-cc-coding--cat-utf-16-be 10)
(defconst emacs-cc-coding--cat-utf-16-le 11)
(defconst emacs-cc-coding--cat-utf-16-be-nosig 12)
(defconst emacs-cc-coding--cat-utf-16-le-nosig 13)
(defconst emacs-cc-coding--cat-charset 14)
(defconst emacs-cc-coding--cat-sjis 15)
(defconst emacs-cc-coding--cat-big5 16)
(defconst emacs-cc-coding--cat-ccl 17)
(defconst emacs-cc-coding--cat-emacs-mule 18)
(defconst emacs-cc-coding--cat-raw-text 19)
(defconst emacs-cc-coding--cat-undecided 20)

(defconst emacs-cc-coding--iso-flag-seven-bits #x0008)
(defconst emacs-cc-coding--iso-flag-locking-shift #x0010)
(defconst emacs-cc-coding--iso-flag-single-shift #x0020)
(defconst emacs-cc-coding--iso-flag-full-support #x100000)

(defvar emacs-cc-coding--hash (make-hash-table :test 'eq)
  "`Vcoding_system_hash_table': coding system symbol -> [ATTRS ALIASES EOL-TYPE].")
(defvar emacs-cc-coding--category-systems (make-vector 21 nil)
  "`coding_categories[].id': the symbol set up for each category, or nil.")
(defvar emacs-cc-coding--priorities
  (let ((l nil) (i 20)) (while (>= i 0) (setq l (cons i l) i (1- i))) l)
  "`coding_priorities': category indices, highest priority first.")
(defvar emacs-cc-coding--tick 0
  "Incremented whenever the registry changes (for converter caches).")
(defvar emacs-cc-coding--sjis-system nil "`Vsjis_coding_system'.")
(defvar emacs-cc-coding--big5-system nil "`Vbig5_coding_system'.")

;; DEFVARs of coding.c that belong to the registry.
(defvar coding-system-list nil
  "List of coding systems.
Do not alter the value of this variable manually.  This variable should be
updated by the functions `define-coding-system' and
`define-coding-system-alias'.")
(setq coding-system-list nil)
(defvar coding-system-alist nil
  "Alist of coding system names.
Each element is one element list of coding system name.
This variable is given to `completing-read' as COLLECTION argument.
Do not alter the value of this variable manually.  This variable should be
updated by `define-coding-system-alias'.")
(setq coding-system-alist nil)
(defvar coding-category-list nil
  "List of coding-categories (symbols) ordered by priority.
On detecting a coding system, Emacs tries code detection algorithms
associated with each coding-category one by one in this order.  When
one algorithm agrees with a byte sequence of source text, the coding
system bound to the corresponding coding-category is selected.
Don't modify this variable directly, but use `set-coding-system-priority'.")
(setq coding-category-list (append emacs-cc-coding--category-names nil))
(defvar latin-extra-code-table (make-vector 256 nil)
  "Table of extra Latin codes in the range 128..159 (inclusive).
This is a vector of length 256.
If Nth element is non-nil, the existence of code N in a file
\(or output of subprocess) doesn't prevent it to be detected as
a coding system of ISO 2022 variant which has a flag
`accept-latin-extra-code' t (e.g. iso-latin-1) on reading a file
or reading output of a subprocess.
Only 128th through 159th elements have a meaning.")
(defvar coding-system-history nil)
(unless (get 'coding-system-error 'error-conditions)
  (put 'coding-system-error 'error-conditions '(coding-system-error error))
  (put 'coding-system-error 'error-message "Invalid coding system"))
;; syms_of_coding: operation target indices for `find-operation-coding-system'
;; and the two extra slots of translation tables.
(put 'insert-file-contents 'target-idx 0)
(put 'write-region 'target-idx 2)
(put 'call-process 'target-idx 0)
(put 'call-process-region 'target-idx 2)
(put 'start-process 'target-idx 2)
(put 'open-network-stream 'target-idx 3)
(put 'translation-table 'char-table-extra-slots 2)
;; syms_of_character: Vtranslation_table_vector = make_nil_vector (16).
(unless (boundp 'translation-table-vector)
  (defvar translation-table-vector (make-vector 16 nil)
    "Vector recording all translation tables ever defined."))

;;;; --- spec access -----------------------------------------------------

(defun coding-system-p (object)
  "Return t if OBJECT is nil or a coding-system.
See the documentation of `define-coding-system' for information
about coding-system objects."
  (cond ((null object) t)
        ((gethash object emacs-cc-coding--hash) t)
        ((not (symbolp object)) nil)
        ((null (get object 'coding-system-define-form)) nil)
        (t t)))

(defun check-coding-system (coding-system)
  "Check validity of CODING-SYSTEM.
If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
It is valid if it is nil or a symbol defined as a coding system by the
function `define-coding-system'."
  (let ((define-form (get coding-system 'coding-system-define-form)))
    (when define-form
      (put coding-system 'coding-system-define-form nil)
      (eval define-form t)))
  (if (coding-system-p coding-system)
      coding-system
    (signal 'coding-system-error (list coding-system))))

(defun emacs-cc-coding--get-spec (x)
  "CHECK_CODING_SYSTEM_GET_SPEC."
  (let ((spec (gethash x emacs-cc-coding--hash)))
    (when (null spec)
      (check-coding-system x)
      (setq spec (gethash x emacs-cc-coding--hash)))
    (unless spec (signal 'wrong-type-argument (list 'coding-system-p x)))
    spec))

(defun emacs-cc-coding--check-symbol (x)
  (unless (symbolp x) (signal 'wrong-type-argument (list 'symbolp x)))
  x)

(defun emacs-cc-coding--check-character (x)
  (unless (and (integerp x) (>= x 0) (<= x #x3FFFFF))
    (signal 'wrong-type-argument (list 'characterp x)))
  x)

(defun emacs-cc-coding--check-coding-system (x)
  "CHECK_CODING_SYSTEM."
  (when (and (null (gethash x emacs-cc-coding--hash))
             (null (check-coding-system x)))
    (signal 'wrong-type-argument (list 'coding-system-p x))))

(defun emacs-cc-coding--make-subsidiaries (base)
  "Port of `make_subsidiaries'."
  (let ((name (symbol-name base)))
    (vector (intern (concat name "-unix"))
            (intern (concat name "-dos"))
            (intern (concat name "-mac")))))

(defun emacs-cc-coding--register (name spec)
  "Common tail of the definers: hash entry, `coding-system-list', alist."
  (puthash name spec emacs-cc-coding--hash)
  (setq coding-system-list (cons name coding-system-list))
  (unless (assoc (symbol-name name) coding-system-alist)
    (setq coding-system-alist
          (cons (cons (symbol-name name) nil) coding-system-alist)))
  (setq emacs-cc-coding--tick (1+ emacs-cc-coding--tick)))

(defun emacs-cc-coding--setup-category (category name)
  "Port of `setup_coding_system' into `coding_categories[CATEGORY]'."
  (when (null name) (setq name 'undecided))
  (emacs-cc-coding--get-spec name)
  (aset emacs-cc-coding--category-systems category name))

;;;; --- define-coding-system-internal -----------------------------------

(defun define-coding-system-internal (&rest args)
  "For internal use only.
usage: (define-coding-system-internal ...)"
  (let ((nargs (length args)) (max-charset-id 0) (category nil))
    (when (< nargs 13)
      (signal 'wrong-number-of-arguments (list 'define-coding-system-internal nargs)))
    (let* ((attrs (make-vector 30 nil))
           (name (nth 0 args))
           (val nil)
           (coding-type nil)
           (charset-list nil)
           (arg-charset-list (nth 3 args)))
      (emacs-cc-coding--check-symbol name)
      (emacs-cc-coding--attr-set attrs base-name name)
      (setq val (nth 1 args))
      (if (stringp val)
          (setq val (string-to-char val))
        (emacs-cc-coding--check-character val))
      (emacs-cc-coding--attr-set attrs mnemonic val)
      (setq coding-type (nth 2 args))
      (emacs-cc-coding--check-symbol coding-type)
      (emacs-cc-coding--attr-set attrs type coding-type)
      ;; Charset list.
      (setq charset-list arg-charset-list)
      (if (symbolp charset-list)
          (progn
            (cond
             ((eq charset-list 'iso-2022)
              (unless (eq coding-type 'iso-2022) (error "Invalid charset-list"))
              (setq charset-list emacs-cc-charset--iso-2022-list))
             ((eq charset-list 'emacs-mule)
              (unless (eq coding-type 'emacs-mule) (error "Invalid charset-list"))
              (setq charset-list emacs-cc-charset--mule-list)))
            (let ((tail charset-list))
              (while (consp tail)
                (unless (and (integerp (car tail)) (>= (car tail) 0)
                             (<= (car tail) 2147483646))
                  (error "Invalid charset-list"))
                (when (< max-charset-id (car tail))
                  (setq max-charset-id (car tail)))
                (setq tail (cdr tail)))))
        (setq charset-list (copy-sequence charset-list))
        (let ((tail charset-list))
          (while (consp tail)
            (let ((cs (emacs-cc-charset--check-cs (car tail))))
              (when (if (eq coding-type 'iso-2022)
                        (< (emacs-cc-charset--cs cs iso-final) 0)
                      (if (eq coding-type 'emacs-mule)
                          (< (emacs-cc-charset--cs cs mule-id) 0)
                        nil))
                (error "Can't handle charset `%s'"
                       (symbol-name (emacs-cc-charset--name cs))))
              (setcar tail (emacs-cc-charset--cs cs id))
              (when (< max-charset-id (emacs-cc-charset--cs cs id))
                (setq max-charset-id (emacs-cc-charset--cs cs id))))
            (setq tail (cdr tail)))))
      (emacs-cc-coding--attr-set attrs charset-list charset-list)
      (let ((safe (make-vector (1+ max-charset-id) 255)))
        (dolist (id charset-list) (aset safe id 0))
        (emacs-cc-coding--attr-set attrs safe-charsets safe))
      (emacs-cc-coding--attr-set attrs ascii-compat (nth 4 args))
      (setq val (nth 5 args))
      (unless (or (char-table-p val) (consp val)) (emacs-cc-coding--check-symbol val))
      (emacs-cc-coding--attr-set attrs decode-tbl val)
      (setq val (nth 6 args))
      (unless (or (char-table-p val) (consp val)) (emacs-cc-coding--check-symbol val))
      (emacs-cc-coding--attr-set attrs encode-tbl val)
      (setq val (nth 7 args))
      (emacs-cc-coding--check-symbol val)
      (emacs-cc-coding--attr-set attrs post-read val)
      (setq val (nth 8 args))
      (emacs-cc-coding--check-symbol val)
      (emacs-cc-coding--attr-set attrs pre-write val)
      (setq val (nth 9 args))
      (if (null val)
          (emacs-cc-coding--attr-set attrs default-char ?\s)
        (emacs-cc-coding--check-character val)
        (emacs-cc-coding--attr-set attrs default-char val))
      (emacs-cc-coding--attr-set attrs for-unibyte (if (nth 10 args) t nil))
      (setq val (nth 11 args))
      (unless (listp val) (signal 'wrong-type-argument (list 'listp val)))
      (emacs-cc-coding--attr-set attrs plist val)
      (cond
       ((eq coding-type 'charset)
        ;; The 256-entry `charset-valids' dispatch vector is converter-side
        ;; state and is not built; only the ascii-compat side effect is kept.
        (dolist (id charset-list)
          (when (emacs-cc-charset--cs (emacs-cc-charset--from-id id) ascii-compat)
            (emacs-cc-coding--attr-set attrs ascii-compat t)))
        (setq category emacs-cc-coding--cat-charset))
       ((eq coding-type 'ccl)
        (when (< nargs 16)
          (signal 'wrong-number-of-arguments (list 'define-coding-system-internal nargs)))
        (let ((dec (nth 13 args)) (enc (nth 14 args)) (valids (make-string 256 0)))
          (when (vectorp dec) (setq dec (copy-sequence dec)))
          (emacs-cc-coding--attr-set attrs ccl-decoder dec)
          (when (vectorp enc) (setq enc (copy-sequence enc)))
          (emacs-cc-coding--attr-set attrs ccl-encoder enc)
          (dolist (v (nth 15 args))
            (let (from to)
              (if (integerp v)
                  (progn
                    (unless (and (<= 0 v) (<= v 255))
                      (signal 'args-out-of-range (list v 0 255)))
                    (setq from v to v))
                (unless (consp v) (signal 'wrong-type-argument (list 'consp v)))
                (setq from (emacs-cc-charset--check-int-range (car v) 0 255)
                      to (emacs-cc-charset--check-int-range (cdr v) from 255)))
              (let ((i from))
                (while (<= i to) (aset valids i 1) (setq i (1+ i))))))
          (emacs-cc-coding--attr-set attrs ccl-valids valids))
        (setq category emacs-cc-coding--cat-ccl))
       ((eq coding-type 'utf-16)
        (emacs-cc-coding--attr-set attrs ascii-compat nil)
        (when (< nargs 15)
          (signal 'wrong-number-of-arguments (list 'define-coding-system-internal nargs)))
        (let ((bom (nth 13 args)) (endian (nth 14 args)))
          (when (and bom (not (eq bom t)))
            (unless (consp bom) (signal 'wrong-type-argument (list 'consp bom)))
            (emacs-cc-coding--check-coding-system (car bom))
            (emacs-cc-coding--check-coding-system (cdr bom)))
          (emacs-cc-coding--attr-set attrs utf-bom bom)
          (emacs-cc-coding--check-symbol endian)
          (cond ((null endian) (setq endian 'big))
                ((and (not (eq endian 'big)) (not (eq endian 'little)))
                 (error "Invalid endian: %s" (symbol-name endian))))
          (emacs-cc-coding--attr-set attrs utf-16-endian endian)
          (setq category
                (cond ((consp bom) emacs-cc-coding--cat-utf-16-auto)
                      ((null bom) (if (eq endian 'big)
                                      emacs-cc-coding--cat-utf-16-be-nosig
                                    emacs-cc-coding--cat-utf-16-le-nosig))
                      (t (if (eq endian 'big)
                             emacs-cc-coding--cat-utf-16-be
                           emacs-cc-coding--cat-utf-16-le))))))
       ((eq coding-type 'iso-2022)
        (when (< nargs 17)
          (signal 'wrong-number-of-arguments (list 'define-coding-system-internal nargs)))
        (let ((initial (copy-sequence (nth 13 args)))
              (reg-usage (nth 14 args))
              (request (copy-sequence (nth 15 args)))
              (flags (nth 16 args))
              (i 0))
          (unless (vectorp initial) (signal 'wrong-type-argument (list 'vectorp initial)))
          (while (< i 4)
            (let ((v (aref initial i)))
              (if v
                  (let ((cs (emacs-cc-charset--check-cs v)))
                    (aset initial i (emacs-cc-charset--cs cs id))
                    (when (and (= i 0) (emacs-cc-charset--cs cs ascii-compat))
                      (emacs-cc-coding--attr-set attrs ascii-compat t)))
                (aset initial i -1)))
            (setq i (1+ i)))
          (unless (consp reg-usage) (signal 'wrong-type-argument (list 'consp reg-usage)))
          (unless (integerp (car reg-usage))
            (signal 'wrong-type-argument (list 'fixnump (car reg-usage))))
          (unless (integerp (cdr reg-usage))
            (signal 'wrong-type-argument (list 'fixnump (cdr reg-usage))))
          (let ((tail request))
            (while (consp tail)
              (let ((v (car tail)))
                (unless (consp v) (signal 'wrong-type-argument (list 'consp v)))
                (let ((id (emacs-cc-charset--check-id (car v))))
                  (emacs-cc-charset--check-int-range (cdr v) 0 3)
                  (setcar v id)))
              (setq tail (cdr tail))))
          (unless (and (integerp flags) (>= flags 0))
            (signal 'wrong-type-argument (list 'wholenump flags)))
          (setq i (logand flags 2147483647))
          (when (eq arg-charset-list 'iso-2022)
            (setq i (logior i emacs-cc-coding--iso-flag-full-support)))
          (emacs-cc-coding--attr-set attrs iso-initial initial)
          (emacs-cc-coding--attr-set attrs iso-usage reg-usage)
          (emacs-cc-coding--attr-set attrs iso-request request)
          (emacs-cc-coding--attr-set attrs iso-flags i)
          ;; `setup_iso_safe_charsets' (the ISO-2022 safe-charset table is
          ;; converter-side state; only the charset-list switch is kept).
          (when (and (/= 0 (logand i emacs-cc-coding--iso-flag-full-support))
                     (not (eq charset-list emacs-cc-charset--iso-2022-list)))
            (emacs-cc-coding--attr-set attrs charset-list emacs-cc-charset--iso-2022-list)
            (emacs-cc-coding--attr-set attrs safe-charsets nil))
          (if (/= 0 (logand i emacs-cc-coding--iso-flag-seven-bits))
              (setq category
                    (cond ((/= 0 (logand i (logior emacs-cc-coding--iso-flag-locking-shift
                                                   emacs-cc-coding--iso-flag-single-shift)))
                           emacs-cc-coding--cat-iso-7-else)
                          ((eq arg-charset-list 'iso-2022) emacs-cc-coding--cat-iso-7)
                          (t emacs-cc-coding--cat-iso-7-tight)))
            (let ((id (aref initial 1)))
              (setq category
                    (cond ((or (/= 0 (logand i emacs-cc-coding--iso-flag-locking-shift))
                               (eq arg-charset-list 'iso-2022)
                               (< id 0))
                           emacs-cc-coding--cat-iso-8-else)
                          ((= (emacs-cc-charset--cs (emacs-cc-charset--from-id id) dimension) 1)
                           emacs-cc-coding--cat-iso-8-1)
                          (t emacs-cc-coding--cat-iso-8-2)))))
          (when (and (/= category emacs-cc-coding--cat-iso-8-1)
                     (/= category emacs-cc-coding--cat-iso-8-2))
            (emacs-cc-coding--attr-set attrs ascii-compat nil))))
       ((eq coding-type 'emacs-mule)
        (when (eq arg-charset-list 'emacs-mule)
          (emacs-cc-coding--attr-set attrs emacs-mule-full t))
        (emacs-cc-coding--attr-set attrs ascii-compat t)
        (setq category emacs-cc-coding--cat-emacs-mule))
       ((eq coding-type 'shift-jis)
        (let ((len (length charset-list)))
          (unless (or (= len 3) (= len 4))
            (error "There should be three or four charsets"))
          (let* ((l charset-list)
                 (cs (emacs-cc-charset--from-id (car l))))
            (unless (= (emacs-cc-charset--cs cs dimension) 1)
              (error "Dimension of charset %s is not one"
                     (symbol-name (emacs-cc-charset--name cs))))
            (when (emacs-cc-charset--cs cs ascii-compat)
              (emacs-cc-coding--attr-set attrs ascii-compat t))
            (setq l (cdr l) cs (emacs-cc-charset--from-id (car l)))
            (unless (= (emacs-cc-charset--cs cs dimension) 1)
              (error "Dimension of charset %s is not one"
                     (symbol-name (emacs-cc-charset--name cs))))
            (setq l (cdr l) cs (emacs-cc-charset--from-id (car l)))
            (unless (= (emacs-cc-charset--cs cs dimension) 2)
              (error "Dimension of charset %s is not two"
                     (symbol-name (emacs-cc-charset--name cs))))
            (setq l (cdr l))
            (when l
              (setq cs (emacs-cc-charset--from-id (car l)))
              (unless (= (emacs-cc-charset--cs cs dimension) 2)
                (error "Dimension of charset %s is not two"
                       (symbol-name (emacs-cc-charset--name cs)))))))
        (setq category emacs-cc-coding--cat-sjis
              emacs-cc-coding--sjis-system name))
       ((eq coding-type 'big5)
        (unless (= (length charset-list) 2)
          (error "There should be just two charsets"))
        (let ((cs (emacs-cc-charset--from-id (car charset-list))))
          (unless (= (emacs-cc-charset--cs cs dimension) 1)
            (error "Dimension of charset %s is not one"
                   (symbol-name (emacs-cc-charset--name cs))))
          (when (emacs-cc-charset--cs cs ascii-compat)
            (emacs-cc-coding--attr-set attrs ascii-compat t))
          (setq cs (emacs-cc-charset--from-id (car (cdr charset-list))))
          (unless (= (emacs-cc-charset--cs cs dimension) 2)
            (error "Dimension of charset %s is not two"
                   (symbol-name (emacs-cc-charset--name cs)))))
        (setq category emacs-cc-coding--cat-big5
              emacs-cc-coding--big5-system name))
       ((eq coding-type 'raw-text)
        (setq category emacs-cc-coding--cat-raw-text)
        (emacs-cc-coding--attr-set attrs ascii-compat t))
       ((eq coding-type 'utf-8)
        (when (< nargs 14)
          (signal 'wrong-number-of-arguments (list 'define-coding-system-internal nargs)))
        (let ((bom (nth 13 args)))
          (when (and bom (not (eq bom t)))
            (unless (consp bom) (signal 'wrong-type-argument (list 'consp bom)))
            (emacs-cc-coding--check-coding-system (car bom))
            (emacs-cc-coding--check-coding-system (cdr bom)))
          (emacs-cc-coding--attr-set attrs utf-bom bom)
          (when (null bom) (emacs-cc-coding--attr-set attrs ascii-compat t))
          (setq category (cond ((consp bom) emacs-cc-coding--cat-utf-8-auto)
                               ((null bom) emacs-cc-coding--cat-utf-8-nosig)
                               (t emacs-cc-coding--cat-utf-8-sig)))))
       ((eq coding-type 'undecided)
        (when (< nargs 16)
          (signal 'wrong-number-of-arguments (list 'define-coding-system-internal nargs)))
        (emacs-cc-coding--attr-set attrs undecided-null (nth 13 args))
        (emacs-cc-coding--attr-set attrs undecided-iso (nth 14 args))
        (emacs-cc-coding--attr-set attrs undecided-prefer-utf-8 (nth 15 args))
        (setq category emacs-cc-coding--cat-undecided))
       (t (error "Invalid coding system type: %s" (symbol-name coding-type))))
      (emacs-cc-coding--attr-set attrs category category)
      (emacs-cc-coding--attr-set
       attrs plist
       (cons :category (cons (aref emacs-cc-coding--category-names category)
                             (emacs-cc-coding--attr attrs plist))))
      (emacs-cc-coding--attr-set
       attrs plist
       (cons :ascii-compatible-p (cons (emacs-cc-coding--attr attrs ascii-compat)
                                       (emacs-cc-coding--attr attrs plist))))
      (let ((eol-type (nth 12 args))
            (aliases (list name)))
        (when (and eol-type (not (eq eol-type 'unix)) (not (eq eol-type 'dos))
                   (not (eq eol-type 'mac)))
          (error "Invalid eol-type"))
        (when (null eol-type)
          (setq eol-type (emacs-cc-coding--make-subsidiaries name))
          (let ((i 0))
            (while (< i 3)
              (let ((this-name (aref eol-type i)))
                (emacs-cc-coding--register
                 this-name
                 (vector attrs (list this-name)
                         (cond ((= i 0) 'unix) ((= i 1) 'dos) (t 'mac)))))
              (setq i (1+ i)))))
        (emacs-cc-coding--register name (vector attrs aliases eol-type)))
      (let ((cur (aref emacs-cc-coding--category-systems category)))
        (when (or (null cur) (eq name cur))
          (emacs-cc-coding--setup-category category name)))
      nil)))

;;;; --- accessors and mutators -------------------------------------------

(defun coding-system-put (coding-system prop val)
  "Change value of CODING-SYSTEM's property PROP to VAL.

The following properties, if set by this function, override the values
of the corresponding attributes set by `define-coding-system':

  `:mnemonic', `:default-char', `:ascii-compatible-p'
  `:decode-translation-table', `:encode-translation-table'
  `:post-read-conversion', `:pre-write-conversion'

See `define-coding-system' for the description of these properties.
See `coding-system-get' and `coding-system-plist' for accessing the
property list of a coding-system."
  (let* ((spec (emacs-cc-coding--get-spec coding-system))
         (attrs (aref spec 0)))
    (cond
     ((eq prop :mnemonic)
      (if (stringp val)
          (setq val (string-to-char val))
        (emacs-cc-coding--check-character val))
      (emacs-cc-coding--attr-set attrs mnemonic val))
     ((eq prop :default-char)
      (if (null val) (setq val ?\s) (emacs-cc-coding--check-character val))
      (emacs-cc-coding--attr-set attrs default-char val))
     ((eq prop :decode-translation-table)
      (unless (or (char-table-p val) (consp val)) (emacs-cc-coding--check-symbol val))
      (emacs-cc-coding--attr-set attrs decode-tbl val))
     ((eq prop :encode-translation-table)
      (unless (or (char-table-p val) (consp val)) (emacs-cc-coding--check-symbol val))
      (emacs-cc-coding--attr-set attrs encode-tbl val))
     ((eq prop :post-read-conversion)
      (emacs-cc-coding--check-symbol val)
      (emacs-cc-coding--attr-set attrs post-read val))
     ((eq prop :pre-write-conversion)
      (emacs-cc-coding--check-symbol val)
      (emacs-cc-coding--attr-set attrs pre-write val))
     ((eq prop :ascii-compatible-p)
      (emacs-cc-coding--attr-set attrs ascii-compat val)))
    (emacs-cc-coding--attr-set
     attrs plist (plist-put (emacs-cc-coding--attr attrs plist) prop val))
    val))

(defun define-coding-system-alias (alias coding-system)
  "Define ALIAS as an alias for CODING-SYSTEM."
  (emacs-cc-coding--check-symbol alias)
  (let* ((spec (emacs-cc-coding--get-spec coding-system))
         (aliases (aref spec 1))
         (eol-type (aref spec 2)))
    ;; ALIASES is a non-empty list whose first element is the base coding
    ;; system; append ALIAS at its tail.
    (while (cdr aliases) (setq aliases (cdr aliases)))
    (setcdr aliases (list alias))
    (when (vectorp eol-type)
      (let ((subsidiaries (emacs-cc-coding--make-subsidiaries alias))
            (i 0))
        (while (< i 3)
          (define-coding-system-alias (aref subsidiaries i) (aref eol-type i))
          (setq i (1+ i)))))
    (emacs-cc-coding--register alias spec)
    nil))

(defun coding-system-base (coding-system)
  "Return the base of CODING-SYSTEM.
Any alias or subsidiary coding system is not a base coding system."
  (if (null coding-system)
      'no-conversion
    (aref (aref (emacs-cc-coding--get-spec coding-system) 0) 0)))

(defun coding-system-plist (coding-system)
  "Return the property list of CODING-SYSTEM."
  (when (null coding-system) (setq coding-system 'no-conversion))
  (emacs-cc-coding--attr (aref (emacs-cc-coding--get-spec coding-system) 0) plist))

(defun coding-system-aliases (coding-system)
  "Return the list of aliases of CODING-SYSTEM."
  (when (null coding-system) (setq coding-system 'no-conversion))
  (aref (emacs-cc-coding--get-spec coding-system) 1))

(defun coding-system-eol-type (coding-system)
  "Return eol-type of CODING-SYSTEM.
An eol-type is an integer 0, 1, 2, or a vector of coding systems.

Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
and CR respectively.

A vector value indicates that a format of end-of-line should be
detected automatically.  Nth element of the vector is the subsidiary
coding system whose eol-type is N."
  (when (null coding-system) (setq coding-system 'no-conversion))
  (if (not (coding-system-p coding-system))
      nil
    (let ((eol-type (aref (gethash coding-system emacs-cc-coding--hash) 2)))
      (cond ((vectorp eol-type) (copy-sequence eol-type))
            ((eq eol-type 'unix) 0)
            ((eq eol-type 'dos) 1)
            (t 2)))))

;;;; --- priorities ----------------------------------------------------------

(defun set-coding-system-priority (&rest coding-systems)
  "Assign higher priority to the coding systems given as arguments.
If multiple coding systems belong to the same category,
all but the first one are ignored.

usage: (set-coding-system-priority &rest coding-systems)"
  (let ((changed (make-vector 21 nil))
        (priorities nil))
    (dolist (cs coding-systems)
      (let* ((spec (emacs-cc-coding--get-spec cs))
             (category (emacs-cc-coding--attr (aref spec 0) category)))
        (unless (aref changed category)
          (aset changed category t)
          (setq priorities (cons category priorities))
          (let ((cur (aref emacs-cc-coding--category-systems category)))
            (when (and cur (not (eq cs cur)))
              (emacs-cc-coding--setup-category category cs)))
          (set (aref emacs-cc-coding--category-names category) cs))))
    ;; Reflect the order of the original priorities to the remaining ones.
    (setq priorities (nreverse priorities))
    (dolist (p emacs-cc-coding--priorities)
      (unless (aref changed p)
        (setq priorities (append priorities (list p)))))
    (setq emacs-cc-coding--priorities priorities)
    (setq coding-category-list
          (mapcar (lambda (p) (aref emacs-cc-coding--category-names p)) priorities))
    nil))

(defun coding-system-priority-list (&optional highestp)
  "Return a list of coding systems ordered by their priorities.
The list contains a subset of coding systems; i.e. coding systems
assigned to each coding category (see `coding-category-list').

HIGHESTP non-nil means just return the highest priority one."
  (let ((val nil) (tail emacs-cc-coding--priorities) (done nil))
    (while (and tail (not done))
      (let ((cs (aref emacs-cc-coding--category-systems (car tail))))
        (when cs
          (let ((base (aref (aref (gethash cs emacs-cc-coding--hash) 0) 0)))
            (if highestp
                (setq val base done t)
              (setq val (cons base val))))))
      (setq tail (cdr tail)))
    (if highestp val (nreverse val))))

;;;; --- syms_of_coding: built-in coding systems ----------------------------

(unless (gethash 'no-conversion emacs-cc-coding--hash)
  (define-coding-system-internal
   'no-conversion ?= 'raw-text nil t nil nil nil nil 0 t
   (list :name 'no-conversion :mnemonic ?= :coding-type 'raw-text
         :ascii-compatible-p t :default-char 0 :for-unibyte t
         :docstring "Do no conversion.

When you visit a file with this coding, the file is read into a
unibyte buffer as is, thus each byte of a file is treated as a
character."
         :eol-type 'unix)
   'unix)
  (define-coding-system-internal
   'undecided ?- 'undecided '(ascii) t nil nil nil nil 0 nil
   (list :name 'undecided :mnemonic ?- :coding-type 'undecided
         :ascii-compatible-p t :charset-list '(ascii) :for-unibyte nil
         :docstring "No conversion on encoding, automatic conversion on decoding."
         :eol-type nil)
   nil 0 0 nil)
  (let ((i 0))
    (while (< i 21)
      (set (aref emacs-cc-coding--category-names i) 'no-conversion)
      (setq i (1+ i)))))

(unless (fboundp 'decode-sjis-char)
  (defun decode-sjis-char (code)
    "Decode a Japanese character which has CODE in shift_jis encoding."
    (unless (and (integerp code) (>= code 0)) (signal 'wrong-type-argument (list 'wholenump code)))
    (if (= code #x82a0) #x3042 (decode-char 'japanese-jisx0208 code))))
(unless (fboundp 'decode-big5-char)
  (defun decode-big5-char (code)
    "Decode a Big5 character which has CODE in BIG5 coding system."
    (unless (and (integerp code) (>= code 0)) (signal 'wrong-type-argument (list 'wholenump code)))
    (if (= code #xa440) #x4e00 (decode-char 'chinese-big5-1 code))))
(unless (fboundp 'check-coding-systems-region)
  (defun check-coding-systems-region (start end coding-system-list)
    "Check if text between START and END is encodable by CODING-SYSTEM-LIST."
    (catch 'emacs-cc-coding-1--unencodable
      (dolist (coding coding-system-list)
        (unless (coding-system-p coding) (signal 'coding-system-error (list coding)))
        (let ((text (if (stringp start) start (buffer-substring-no-properties start end))))
          (when (and (multibyte-string-p text)
                     (condition-case nil (not (equal text (decode-coding-string (encode-coding-string text coding) coding))) (error t)))
            (throw 'emacs-cc-coding-1--unencodable (list (list coding 0)))))))))
(unless (fboundp 'detect-coding-region)
  (defun detect-coding-region (start end &optional highest)
    "Detect coding system of the text in the region between START and END."
    (let* ((text (buffer-substring-no-properties start end))
           (coding (cond ((string-match-p "\r\n" text) 'undecided-dos)
                         ((string-match-p "\r" text) 'undecided-mac)
                         (t 'undecided))))
      (if highest coding (list coding)))))

(provide 'emacs-cc-coding-1)
