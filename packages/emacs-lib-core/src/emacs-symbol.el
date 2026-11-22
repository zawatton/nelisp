;;; emacs-symbol.el --- NeLisp port of Emacs C core symbol property API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2.1 — Layer 2.
;;
;; Ports the symbol property-list accessors (`put', `get',
;; `symbol-plist', `setplist') from Emacs's C core.  These are
;; foundational — many subr.el / cl-lib helpers store metadata via
;; them, and `define-error' (= our own polyfill) needs `put' to land
;; the error-conditions list on the symbol.
;;
;; Polyfill strategy: maintain a Lisp-side hash table mapping
;; (SYMBOL → PLIST).  This avoids depending on a NeLisp builtin that
;; mutates the symbol's intrinsic property cell — bootstrap eval
;; does not yet expose one.  All accessors operate on this table.

;;; Code:

(defvar emacs-symbol--plist-table (make-hash-table :test 'eq)
  "Hash table mapping symbol → property list (plist).
Used by the `put' / `get' polyfills to store symbol metadata when
the underlying NeLisp runtime has no native property-cell.")

(unless (fboundp 'put)
  (defun put (symbol property value)
    "Store VALUE under PROPERTY in SYMBOL's plist.  Returns VALUE."
    (let ((current (gethash symbol emacs-symbol--plist-table)))
      (puthash symbol (plist-put (or current nil) property value)
               emacs-symbol--plist-table)
      value)))

(unless (fboundp 'get)
  (defun get (symbol property)
    "Return the value stored under PROPERTY in SYMBOL's plist, or nil."
    (plist-get (gethash symbol emacs-symbol--plist-table) property)))

(unless (fboundp 'symbol-plist)
  (defun symbol-plist (symbol)
    "Return SYMBOL's full property list, or nil."
    (gethash symbol emacs-symbol--plist-table)))

(unless (fboundp 'setplist)
  (defun setplist (symbol new-plist)
    "Replace SYMBOL's property list with NEW-PLIST."
    (puthash symbol new-plist emacs-symbol--plist-table)
    new-plist))

(unless (fboundp 'obarray-make)
  (defun obarray-make (&optional size)
    "Return a new obarray with SIZE as a hint for the expected symbol count."
    (emacs-symbol--make-obarray size)))

;; Keep the prelude's marker and name table, adding private bucket state
;; only to newly allocated records.  Old records remain valid for all the
;; existing obarray operations, but their discarded size and insertion
;; history cannot be recovered by `internal--obarray-buckets'.
(defun emacs-symbol--obarray-p (object)
  "Recognize both prelude obarrays and obarrays with bucket state."
  (and (recordp object)
       (memq (nelisp--record-length object) '(3 4))
       (eq (nelisp--record-type object) 'obarray)
       (eq (nelisp--record-ref object 0) nelisp--obarray-marker)
       (hash-table-p (nelisp--record-ref object 1))
       (eq (hash-table-test (nelisp--record-ref object 1)) 'equal)))

(defun emacs-symbol--make-obarray (&rest arguments)
  "Allocate a standalone obarray, retaining GNU's bucket size hint."
  (when (> (length arguments) 1)
    (signal 'wrong-number-of-arguments
            (list 'obarray-make (length arguments))))
  (let ((size (car arguments)) (bits 3))
    (when size
      (unless (and (integerp size) (>= size 0))
        (signal 'wrong-type-argument (list 'wholenump size)))
      ;; GNU's 64-bit build permits at most 31 bucket-index bits.
      (when (>= size 2147483648)
        (signal 'args-out-of-range size))
      (setq bits 0)
      (while (> size 0)
        (setq bits (1+ bits) size (ash size -1))))
    (nelisp--make-record
     'obarray nelisp--obarray-marker (make-hash-table :test 'equal)
     (vector bits (make-vector (ash 1 bits) nil)))))

(defun emacs-symbol--obarray-object (object)
  "Validate OBJECT, lazily adopting a nonempty legacy vector."
  (let ((array object))
    (when (and (vectorp object) (> (length object) 0))
      (setq array (aref object 0))
      (when (eq array 0)
        (setq array (emacs-symbol--make-obarray 0))
        (aset object 0 array)))
    (unless (emacs-symbol--obarray-p array)
      (signal 'wrong-type-argument (list 'obarrayp object)))
    array))

(defun emacs-symbol--obarray-table (object)
  "Return OBJECT's name table, accepting prelude and legacy obarrays."
  (nelisp--record-ref (emacs-symbol--obarray-object object) 1))

(defun emacs-symbol--obarray-state (array)
  "Return ARRAY's bucket state, or nil for an old prelude record."
  (when (= (nelisp--record-length array) 4)
    (nelisp--record-ref array 2)))

(defmacro emacs-symbol--word-at (name offset count)
  "Little-endian word of COUNT (2 or 4) bytes of NAME at OFFSET, inline."
  (if (= count 4)
      `(logior (string-byte ,name ,offset)
               (ash (string-byte ,name (+ ,offset 1)) 8)
               (ash (string-byte ,name (+ ,offset 2)) 16)
               (ash (string-byte ,name (+ ,offset 3)) 24))
    `(logior (string-byte ,name ,offset)
             (ash (string-byte ,name (+ ,offset 1)) 8))))

(defmacro emacs-symbol--hash-step (hi lo high low)
  "Combine the 64-bit hash held as 32-bit halves HI / LO with HIGH / LOW.
The same arithmetic as `emacs-symbol--hash-combine', on two variables."
  (let ((sum (make-symbol "sum")) (h (make-symbol "high")) (l (make-symbol "low")))
    `(let* ((,h ,high) (,l ,low)
            (,sum (+ (logand (ash ,lo 4) #xffffffff) (ash ,hi -28) ,l)))
       (setq ,hi (logand (+ (ash ,hi 4) (ash ,lo -28) ,h (ash ,sum -32))
                         #xffffffff)
             ,lo (logand ,sum #xffffffff)))))

(defun emacs-symbol--name-reduced (name)
  "Return NAME's GNU Emacs 31.1 symbol hash, folded to 32 bits.
Hash at most nine words using native byte access, without copying NAME.
The 64-bit hash is kept as two 32-bit halves in variables, not a pair."
  (let* ((length (string-bytes name))
         (hi 0) (lo length) (offset 0)
         (step (max 8 (ash length -3))))
    (if (>= length 8)
        (progn
          (while (<= (+ offset 8) length)
            (emacs-symbol--hash-step
             hi lo (emacs-symbol--word-at name (+ offset 4) 4)
             (emacs-symbol--word-at name offset 4))
            (setq offset (+ offset step)))
          (emacs-symbol--hash-step
           hi lo (emacs-symbol--word-at name (- length 4) 4)
           (emacs-symbol--word-at name (- length 8) 4)))
      (let ((tail 0))
        (when (>= length 4)
          (setq tail (emacs-symbol--word-at name 0 4) offset 4))
        (when (>= (- length offset) 2)
          (setq tail (+ (ash tail 16) (emacs-symbol--word-at name offset 2))
                offset (+ offset 2)))
        (when (< offset length)
          (setq tail (+ (ash tail 8) (string-byte name offset))))
        (emacs-symbol--hash-step hi lo (ash tail -32) (logand tail #xffffffff))))
    (logxor hi lo)))

(defvar emacs-symbol--reduced-cache (make-hash-table :test 'equal :size 256)
  "Folded name hashes by name, for names whose length is their byte count.
Hashing a name runs interpreted (about 28 us); intern, unintern and
growth of the same names reuse it.  Only such names are cached, so two
`equal' keys always have the same bytes.  Cleared beyond 4096 entries.")

(defun emacs-symbol--bucket-index (name bits)
  "Return NAME's GNU Emacs 31.1 bucket index for BITS index bits."
  (let* ((plain (= (length name) (string-bytes name)))
         (reduced (or (and plain (gethash name emacs-symbol--reduced-cache))
                      (let ((value (emacs-symbol--name-reduced name)))
                        (when plain
                          (when (> (hash-table-count emacs-symbol--reduced-cache) 4096)
                            (clrhash emacs-symbol--reduced-cache))
                          (puthash (copy-sequence name) value
                                   emacs-symbol--reduced-cache))
                        value)))
         (low (logand reduced #xffff)) (high (ash reduced -16))
         ;; Low 32 bits of multiplication by Knuth's 2654435769.
         (product (logand (+ (* low 31161)
                             (ash (+ (* low 40503) (* high 31161)) 16))
                          #xffffffff)))
    (ash product (- bits 32))))

(defun emacs-symbol--grow-obarray (state)
  "Double STATE's bucket count, rehashing chains in GNU's traversal order."
  (let* ((old (aref state 1)) (bits (1+ (aref state 0)))
         (buckets nil) (i 0))
    (when (> bits 31) (error "Obarray too big"))
    (setq buckets (make-vector (ash 1 bits) nil))
    (while (< i (length old))
      (dolist (symbol (aref old i))
        (let ((index (emacs-symbol--bucket-index (symbol-name symbol) bits)))
          (aset buckets index (cons symbol (aref buckets index)))))
      (setq i (1+ i)))
    (aset state 0 bits)
    (aset state 1 buckets)))

(defun emacs-symbol--obarray-intern (name obarray)
  "Intern NAME in OBARRAY, maintaining its name table and bucket chains."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((array (emacs-symbol--obarray-object obarray))
         (table (nelisp--record-ref array 1))
         (symbol (gethash name table)))
    (or symbol
        (let ((fresh (make-symbol name))
              (state (emacs-symbol--obarray-state array)))
          (when state
            (let* ((buckets (aref state 1))
                   (index (emacs-symbol--bucket-index name (aref state 0))))
              (aset buckets index (cons fresh (aref buckets index)))))
          (puthash name fresh table)
          (when (and state (> (hash-table-count table) (length (aref state 1))))
            (emacs-symbol--grow-obarray state))
          fresh))))

(defun emacs-symbol--unintern (&rest arguments)
  "Remove a name or identical symbol, keeping bucket chains synchronized."
  (unless (and arguments (<= (length arguments) 2))
    (signal 'wrong-number-of-arguments (list 'unintern (length arguments))))
  (let ((name (car arguments)) (obarray (cadr arguments)))
    (if (null obarray)
        (progn
          (nelisp--obarray-name name)
          (signal 'unsupported-feature '(global-unintern)))
      (let* ((array (emacs-symbol--obarray-object obarray))
             (table (nelisp--record-ref array 1))
             (key (nelisp--obarray-name name)) (found (gethash key table)))
        (when (and found (or (stringp name) (eq found name)))
          (let ((state (emacs-symbol--obarray-state array)))
            (when state
              (let* ((buckets (aref state 1))
                     (index (emacs-symbol--bucket-index key (aref state 0))))
                (aset buckets index (delq found (aref buckets index))))))
          (remhash key table)
          t)))))

(defun emacs-symbol--mapatoms (function &optional obarray)
  "Call FUNCTION on every symbol, using bucket order for managed obarrays."
  (unless obarray (signal 'unsupported-feature '(global-mapatoms)))
  (let* ((array (emacs-symbol--obarray-object obarray))
         (state (emacs-symbol--obarray-state array)))
    (if state
        (let ((i 0))
          (while (< i (length (aref state 1)))
            (dolist (symbol (aref (aref state 1) i))
              (funcall function symbol))
            (setq i (1+ i))))
      (maphash (lambda (_name symbol) (funcall function symbol))
               (nelisp--record-ref array 1))))
  nil)

(defun emacs-symbol--obarray-clear (&rest arguments)
  "Clear OBARRAY and reset managed buckets to GNU's default size."
  (unless (= (length arguments) 1)
    (signal 'wrong-number-of-arguments
            (list 'obarray-clear (length arguments))))
  (let ((obarray (car arguments)))
    (unless (emacs-symbol--obarray-p obarray)
      (signal 'wrong-type-argument (list 'obarrayp obarray)))
    (let ((state (emacs-symbol--obarray-state obarray)))
      (clrhash (nelisp--record-ref obarray 1))
      (when state
        (aset state 0 3)
        (aset state 1 (make-vector 8 nil)))))
  nil)

;; The runtime already binds this family in its prelude, so its original
;; fboundp guards cannot install the package implementations.  Replace only
;; the standalone providers; GNU's native providers remain untouched.
(when (fboundp 'nelisp--write-stdout-bytes)
  (defalias 'obarrayp #'emacs-symbol--obarray-p)
  (defalias 'obarray-make #'emacs-symbol--make-obarray)
  (defalias 'nelisp--obarray-table #'emacs-symbol--obarray-table)
  (defalias 'nelisp--obarray-intern #'emacs-symbol--obarray-intern)
  (defalias 'unintern #'emacs-symbol--unintern)
  (defalias 'mapatoms #'emacs-symbol--mapatoms)
  (defalias 'obarray-clear #'emacs-symbol--obarray-clear)
  ;; The native `intern' stores a symbol in a user obarray's name table
  ;; without recording it in the bucket chains `mapatoms' walks.
  (unless (fboundp 'emacs-symbol--native-intern)
    (fset 'emacs-symbol--native-intern (symbol-function 'intern)))
  ;; gnu562: the parameter is not named `obarray', the special variable: a
  ;; special parameter bound it dynamically on every call and kept `intern'
  ;; off the VM.
  (defun intern (name &optional ob)
    "Return the canonical symbol named NAME, interning it if necessary.
Optional OB is the obarray to intern in."
    (if (and ob (stringp name) (emacs-symbol--obarray-p ob))
        (emacs-symbol--obarray-intern name ob)
      (emacs-symbol--native-intern name ob))))


(unless (fboundp 'intern-soft)
  (defun intern-soft (name &optional _obarray)
    "Return the canonical symbol interned as NAME, or nil if none exists.
Unlike `intern', this does not create a new symbol on a miss.

On NeLisp standalone this dispatches the string case to the native
`nelisp--intern-lookup' probe-without-insert primitive (see Doc 163
`dev/nelisp' \\=`docs/design/163-magit-bundle-intern-soft-hang.org\\=' §8)
so a never-interned NAME reports nil instead of interning it.  The
previous unconditional `intern' call never soft-failed, which hung
Magit-bundle loads on message.el's `message-cited-text-N' discovery
loop."
    (if (symbolp name)
        name
      (nelisp--intern-lookup name))))

(provide 'emacs-symbol)

;;; emacs-symbol.el ends here
