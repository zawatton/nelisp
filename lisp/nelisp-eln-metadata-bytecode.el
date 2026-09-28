;;; nelisp-eln-metadata-bytecode.el --- fallback reader for `#[...]' literals -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `nelisp-eln-metadata.el' reads a genuine GNU .eln's `text_data_reloc_blob'
;; / `text_data_reloc_eph_blob' payloads by binding `read-circle' and
;; `load-file-name' and calling the ambient `read-from-string'.  On host GNU
;; Emacs that reader is the real C reader and handles every printed form
;; these payloads contain, including `#[ARGDESC CODE CONSTANTS DEPTH ...]'
;; compiled-function literals.
;;
;; On NeLisp standalone, the ambient reader's own `#[...]' handling
;; validates a byte-code literal's ARGDESC/CODE/CONSTANTS/DEPTH fields
;; eagerly, before a same-document `#N#' backreference to an *already
;; fully printed* `#N=' label occupying one of those fields has been
;; resolved to its value: it substitutes an unresolved placeholder there
;; instead of the real value, so validation sees a cons where it expects a
;; string (or a vector), and rejects the whole literal with
;; `(invalid-read-syntax "#[")' -- even though the literal is perfectly
;; well-formed GNU output.  This is common: GNU's native compiler
;; deduplicates `equal' byte-code instruction strings across unrelated
;; closures in the same compilation unit, so two structurally different
;; `#[...]' objects often share one instruction string via `#N='/`#N#',
;; confirmed empirically across hundreds of real installed 31.1 .eln files
;; (see the S6 eln-hashbracket worklog for the survey).
;;
;; This module is a self-contained fallback reader for exactly the grammar
;; subset that occurs in these two relocation-blob payloads, used only when
;; the ambient reader raises exactly `(invalid-read-syntax "#[")'.  Unlike
;; the ambient reader, a backreference to an already-fully-defined `#N='
;; label resolves to that label's real value immediately, so it never
;; needs to hand a placeholder to `#[...]' validation.  Besides `#[...]'
;; itself, the grammar covers what real relocation payloads were observed
;; to contain alongside it: nil/t/symbols/integers, strings (with GNU's
;; escape set), lists (incl. dotted pairs), vectors, `#$', `#:NAME', `'',
;; `` ` '', `,'/`,@' and `#'' (GNU prints `(quote X)'/`(function X)' as
;; `'X'/`#'X' whenever `print-quoted' is non-nil, the default), and
;; `#s(TYPE ...)' hash-table/record literals, `#("STRING" START END PLIST
;; ...)' propertized strings (this runtime attaches no text properties to
;; strings at all, so -- matching the convention already used elsewhere in
;; this codebase for the same reason -- every START/END/PLIST triple is
;; read and discarded, keeping just STRING), `#&LENGTH"BYTES"' bool
;; vectors, and `##' (the empty-name interned symbol).  It fails closed --
;; with `nelisp-eln-metadata-bytecode-error' -- on any syntax outside that
;; subset (radix integers, floats, character literals, and genuinely
;; circular `#N='/`#N#' pairs where the reference is nested inside its own
;; definition) rather than guessing, and on any `#[...]' shape
;; `make-byte-code' itself would not accept.

;;; Code:

(define-error 'nelisp-eln-metadata-bytecode-error
  "Unsupported .eln relocation-blob syntax")

;; Shared with nelisp-stdlib-reader.el's own `#[...]' interpreted-closure
;; decline; only ever defined once, whichever module runs first.
(unless (get 'unsupported-feature 'error-conditions)
  (define-error 'unsupported-feature "Unsupported feature" 'error))

(defconst nelisp-eln-metadata-bytecode--fixnum-min -2305843009213693952)
(defconst nelisp-eln-metadata-bytecode--fixnum-max 2305843009213693951)

(defun nelisp-eln-metadata-bytecode--fail (reason detail)
  (signal 'nelisp-eln-metadata-bytecode-error (list reason detail)))

(defun nelisp-eln-metadata-bytecode--fixnum-p (value)
  (and (integerp value)
       (<= nelisp-eln-metadata-bytecode--fixnum-min value)
       (<= value nelisp-eln-metadata-bytecode--fixnum-max)))

;; ---------------------------------------------------------------------------
;; Low-level scanning helpers.  STATE is a 3-element vector
;; `[TEXT POS LABELS]'; POS is mutated in place, LABELS is an alist of
;; `(NUMBER STATUS . VALUE)' with STATUS one of `pending' / `done'.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--make-state (text)
  (vector text 0 nil))

(defun nelisp-eln-metadata-bytecode--pos (state) (aref state 1))
(defun nelisp-eln-metadata-bytecode--set-pos (state pos) (aset state 1 pos))
(defun nelisp-eln-metadata-bytecode--text (state) (aref state 0))

(defun nelisp-eln-metadata-bytecode--peek (state)
  (let ((text (aref state 0)) (pos (aref state 1)))
    (and (< pos (length text)) (aref text pos))))

(defun nelisp-eln-metadata-bytecode--peek-at (state offset)
  (let* ((text (aref state 0)) (pos (+ (aref state 1) offset)))
    (and (< pos (length text)) (>= pos 0) (aref text pos))))

(defun nelisp-eln-metadata-bytecode--skip-ws (state)
  (let ((text (aref state 0)) (n (length (aref state 0))) (pos (aref state 1)))
    (while (and (< pos n) (memq (aref text pos) '(?\s ?\t ?\n ?\r ?\f)))
      (setq pos (1+ pos)))
    (aset state 1 pos)))

(defconst nelisp-eln-metadata-bytecode--delimiters
  '(?\s ?\t ?\n ?\r ?\f ?\( ?\) ?\[ ?\] ?\"))

(defun nelisp-eln-metadata-bytecode--atom-end (state)
  "Return the index just past the atom token starting at STATE's position.
A backslash escapes the next character unconditionally, so a symbol name
containing a delimiter (GNU prints e.g. the symbol `c-per-(-match' as
`c-per-\\(-match') does not end the token early."
  (let ((text (aref state 0)) (n (length (aref state 0))) (pos (aref state 1)))
    (while (and (< pos n)
                (not (memq (aref text pos)
                           nelisp-eln-metadata-bytecode--delimiters)))
      (setq pos (if (and (eq (aref text pos) ?\\) (< (1+ pos) n))
                    (+ pos 2)
                  (1+ pos))))
    pos))

(defun nelisp-eln-metadata-bytecode--integer-token-p (text)
  (and (> (length text) 0)
       (let ((i 0) (n (length text)))
         (when (memq (aref text 0) '(?+ ?-)) (setq i 1))
         (and (< i n)
              (progn
                (while (and (< i n) (<= ?0 (aref text i) ?9)) (setq i (1+ i)))
                (= i n))))))

(defun nelisp-eln-metadata-bytecode--unescape-symbol (text)
  (let ((out nil) (i 0) (n (length text)))
    (while (< i n)
      (let ((c (aref text i)))
        (if (and (eq c ?\\) (< (1+ i) n))
            (progn (push (aref text (1+ i)) out) (setq i (+ i 2)))
          (push c out) (setq i (1+ i)))))
    (apply #'string (nreverse out))))

;; ---------------------------------------------------------------------------
;; String literals.  Mirrors the escape set GNU's own reader accepts
;; inside a Lisp string: named C-like escapes, `\NNN' octal (1-3 digits),
;; `\xHH' hex, and line-continuation via a backslash-newline pair.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-string (state)
  ;; POS is at the opening `"'.
  (nelisp-eln-metadata-bytecode--set-pos
   state (1+ (nelisp-eln-metadata-bytecode--pos state)))
  (let ((parts nil) (done nil))
    (while (not done)
      (let ((c (nelisp-eln-metadata-bytecode--peek state)))
        (cond
         ((null c)
          (nelisp-eln-metadata-bytecode--fail 'unterminated-string nil))
         ((eq c ?\")
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (setq done t))
         ((eq c ?\\)
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (let ((esc (nelisp-eln-metadata-bytecode--peek state)))
            (when (null esc)
              (nelisp-eln-metadata-bytecode--fail 'unterminated-string nil))
            (nelisp-eln-metadata-bytecode--set-pos
             state (1+ (nelisp-eln-metadata-bytecode--pos state)))
            (cond
             ((eq esc ?\n) nil)
             ((eq esc ?\r)
              (when (eq (nelisp-eln-metadata-bytecode--peek state) ?\n)
                (nelisp-eln-metadata-bytecode--set-pos
                 state (1+ (nelisp-eln-metadata-bytecode--pos state)))))
             ((eq esc ?n) (push 10 parts))
             ((eq esc ?t) (push 9 parts))
             ((eq esc ?r) (push 13 parts))
             ((eq esc ?e) (push 27 parts))
             ((eq esc ?s) (push 32 parts))
             ((eq esc ?b) (push 8 parts))
             ((eq esc ?d) (push 127 parts))
             ((eq esc ?a) (push 7 parts))
             ((eq esc ?f) (push 12 parts))
             ((eq esc ?v) (push 11 parts))
             ((eq esc ?\\) (push 92 parts))
             ((eq esc ?\") (push 34 parts))
             ((<= ?0 esc ?7)
              (let ((value (- esc ?0)) (count 1))
                (while (and (< count 3)
                            (let ((next (nelisp-eln-metadata-bytecode--peek
                                         state)))
                              (and next (<= ?0 next ?7))))
                  (setq value (+ (* value 8)
                                  (- (nelisp-eln-metadata-bytecode--peek state)
                                     ?0)))
                  (setq count (1+ count))
                  (nelisp-eln-metadata-bytecode--set-pos
                   state (1+ (nelisp-eln-metadata-bytecode--pos state))))
                (push value parts)))
             ((eq esc ?x)
              (let ((h1 (nelisp-eln-metadata-bytecode--peek state)))
                (unless h1
                  (nelisp-eln-metadata-bytecode--fail 'bad-hex-escape nil))
                (nelisp-eln-metadata-bytecode--set-pos
                 state (1+ (nelisp-eln-metadata-bytecode--pos state)))
                (let ((h2 (nelisp-eln-metadata-bytecode--peek state)))
                  (unless h2
                    (nelisp-eln-metadata-bytecode--fail 'bad-hex-escape nil))
                  (nelisp-eln-metadata-bytecode--set-pos
                   state (1+ (nelisp-eln-metadata-bytecode--pos state)))
                  (let ((v1 (nelisp-eln-metadata-bytecode--hex-digit h1))
                        (v2 (nelisp-eln-metadata-bytecode--hex-digit h2)))
                    (unless (and v1 v2)
                      (nelisp-eln-metadata-bytecode--fail 'bad-hex-escape nil))
                    (push (logior (ash v1 4) v2) parts)))))
             (t (push esc parts)))))
         (t
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (push c parts)))))
    (apply #'string (nreverse parts))))

(defun nelisp-eln-metadata-bytecode--hex-digit (c)
  (cond ((<= ?0 c ?9) (- c ?0))
        ((<= ?a c ?f) (+ 10 (- c ?a)))
        ((<= ?A c ?F) (+ 10 (- c ?A)))
        (t nil)))

;; ---------------------------------------------------------------------------
;; Sequence bodies (list / vector / byte-code bracket body): read forms
;; until the matching close token.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-list (state)
  ;; POS is just past the opening `('.
  (let ((elements nil) (tail nil) (done nil))
    (while (not done)
      (nelisp-eln-metadata-bytecode--skip-ws state)
      (let ((c (nelisp-eln-metadata-bytecode--peek state)))
        (cond
         ((null c) (nelisp-eln-metadata-bytecode--fail 'unterminated-list nil))
         ((eq c ?\))
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (setq done t))
         ((and (eq c ?.)
               (let ((next (nelisp-eln-metadata-bytecode--peek-at state 1)))
                 (or (null next)
                     (memq next nelisp-eln-metadata-bytecode--delimiters))))
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (setq tail (nelisp-eln-metadata-bytecode--read-one state))
          (nelisp-eln-metadata-bytecode--skip-ws state)
          (unless (eq (nelisp-eln-metadata-bytecode--peek state) ?\))
            (nelisp-eln-metadata-bytecode--fail 'malformed-dotted-pair nil))
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (setq done t))
         (t (push (nelisp-eln-metadata-bytecode--read-one state) elements)))))
    (let ((result tail))
      (dolist (el elements) (setq result (cons el result)))
      result)))

(defun nelisp-eln-metadata-bytecode--read-bracket-elements (state)
  ;; POS is just past the opening `['.  Returns a list of forms.
  (let ((elements nil) (done nil))
    (while (not done)
      (nelisp-eln-metadata-bytecode--skip-ws state)
      (let ((c (nelisp-eln-metadata-bytecode--peek state)))
        (cond
         ((null c)
          (nelisp-eln-metadata-bytecode--fail 'unterminated-vector nil))
         ((eq c ?\])
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (setq done t))
         (t (push (nelisp-eln-metadata-bytecode--read-one state) elements)))))
    (nreverse elements)))

;; ---------------------------------------------------------------------------
;; `#N='/`#N#' read labels.  A backreference to a label whose definition
;; has already fully finished resolves to that label's real value right
;; away.  A backreference nested inside its own still-open definition
;; (genuine circularity) is outside what this fallback supports -- real
;; .eln relocation constants have not been observed to need it -- and
;; fails closed instead of guessing.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-label-def (state number)
  (let ((labels (aref state 2)))
    (when (assq number labels)
      (nelisp-eln-metadata-bytecode--fail 'duplicate-label number))
    (let ((entry (list number 'pending nil)))
      (aset state 2 (cons entry labels))
      (let ((value (nelisp-eln-metadata-bytecode--read-one state)))
        (setcar (cdr entry) 'done)
        (setcar (cddr entry) value)
        value))))

(defun nelisp-eln-metadata-bytecode--read-label-ref (state number)
  (let ((entry (assq number (aref state 2))))
    (unless entry
      (nelisp-eln-metadata-bytecode--fail 'undefined-label-reference number))
    (unless (eq (nth 1 entry) 'done)
      (nelisp-eln-metadata-bytecode--fail
       'circular-reference-unsupported number))
    (nth 2 entry)))

;; ---------------------------------------------------------------------------
;; `#s(TYPE ...)' record / hash-table literals.  GNU's real hash-table
;; print format lists non-default fields as bare-symbol TAG/VALUE pairs
;; (`test', `data', `size', ...); this reader honors `test' and `data'
;; (the only two that affect the table's observable contents) and ignores
;; the rest.  A non-`hash-table' type tag builds a generic `record'.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-sharps-paren (state)
  ;; POS is just past the opening `(' of `#s('.
  (let* ((forms (nelisp-eln-metadata-bytecode--read-list-body state))
         (type-tag (car forms))
         (rest (cdr forms)))
    (if (eq type-tag 'hash-table)
        (let* ((test-tail (memq 'test rest))
               (data-tail (memq 'data rest))
               (test (if test-tail (cadr test-tail) 'eql))
               (data (if data-tail (cadr data-tail) nil))
               (table (make-hash-table :test test)))
          (unless (listp data)
            (nelisp-eln-metadata-bytecode--fail
             'invalid-hash-table-data data))
          (let ((tail data))
            (while tail
              (unless (cdr tail)
                (nelisp-eln-metadata-bytecode--fail
                 'invalid-hash-table-data data))
              (puthash (car tail) (cadr tail) table)
              (setq tail (cddr tail))))
          table)
      (apply #'record type-tag rest))))

(defun nelisp-eln-metadata-bytecode--read-list-body (state)
  "Read a `(...)' body whose opening `(' has already been consumed."
  (nelisp-eln-metadata-bytecode--read-list state))

;; ---------------------------------------------------------------------------
;; `#("STRING" START END PLIST ...)' propertized strings.  This runtime
;; attaches no text properties to strings at all, so -- matching the
;; convention already used for the same reason elsewhere in this codebase
;; -- every START/END/PLIST triple is read (for cursor correctness) and
;; discarded, keeping just STRING.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-propertized-string (state)
  ;; POS is just past the opening `(' of `#('.
  (nelisp-eln-metadata-bytecode--skip-ws state)
  (unless (eq (nelisp-eln-metadata-bytecode--peek state) ?\")
    (nelisp-eln-metadata-bytecode--fail 'invalid-propertized-string nil))
  (let ((string (nelisp-eln-metadata-bytecode--read-string state))
        (done nil))
    (while (not done)
      (nelisp-eln-metadata-bytecode--skip-ws state)
      (let ((c (nelisp-eln-metadata-bytecode--peek state)))
        (cond
         ((null c)
          (nelisp-eln-metadata-bytecode--fail 'invalid-propertized-string nil))
         ((eq c ?\))
          (nelisp-eln-metadata-bytecode--set-pos
           state (1+ (nelisp-eln-metadata-bytecode--pos state)))
          (setq done t))
         (t (nelisp-eln-metadata-bytecode--read-one state)))))
    string))

;; ---------------------------------------------------------------------------
;; `#&LENGTH"BYTES"' bool-vector literals (GNU Emacs lread.c): each byte
;; holds 8 bits low-bit-first, trailing bits of the last byte are 0.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-bool-vector (state)
  ;; POS is just past `#&'.
  (let* ((text (nelisp-eln-metadata-bytecode--text state))
         (n (length text))
         (start (nelisp-eln-metadata-bytecode--pos state))
         (j start))
    (while (and (< j n) (<= ?0 (aref text j) ?9)) (setq j (1+ j)))
    (when (or (= j start) (>= j n) (not (eq (aref text j) ?\")))
      (nelisp-eln-metadata-bytecode--fail 'invalid-bool-vector nil))
    (let ((bit-length (string-to-number (substring text start j))))
      (nelisp-eln-metadata-bytecode--set-pos state j)
      (let* ((bytes (nelisp-eln-metadata-bytecode--read-string state))
             (need (/ (+ bit-length 7) 8)))
        (when (< (length bytes) need)
          (nelisp-eln-metadata-bytecode--fail 'invalid-bool-vector nil))
        (let ((bv (make-bool-vector bit-length nil)) (bi 0))
          (while (< bi bit-length)
            (let ((byte (aref bytes (/ bi 8))) (boff (mod bi 8)))
              (aset bv bi (/= (logand (ash byte (- boff)) 1) 0)))
            (setq bi (1+ bi)))
          bv)))))

;; ---------------------------------------------------------------------------
;; `#[ARGDESC CODE CONSTANTS DEPTH [DOC [INTERACTIVE]]]' compiled-function
;; literals.  Field validation mirrors `make-byte-code''s real acceptance
;; rules (see GNU's lread.c read_1): 3-6 elements, ARGDESC nil / a cons /
;; a fixnum, and either CODE a string with CONSTANTS a vector and DEPTH a
;; non-negative fixnum, or CODE a cons with CONSTANTS nil-or-cons (GNU's
;; lazy/interpreted representation, which this runtime declines the same
;; way the ambient reader does).
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-byte-code (state)
  ;; POS is just past the opening `[' of `#['.
  (let* ((fields (nelisp-eln-metadata-bytecode--read-bracket-elements state))
         (count (length fields))
         (arglist (nth 0 fields))
         (code (nth 1 fields))
         (constants (nth 2 fields))
         (depth (nth 3 fields)))
    (unless (and (<= 3 count 6)
                 (or (null arglist) (consp arglist)
                     (nelisp-eln-metadata-bytecode--fixnum-p arglist)))
      (nelisp-eln-metadata-bytecode--fail 'invalid-byte-code-object fields))
    (cond
     ((and (stringp code) (vectorp constants) (>= count 4)
           (nelisp-eln-metadata-bytecode--fixnum-p depth) (>= depth 0))
      (setcar (cdr fields) (apply #'unibyte-string (string-to-list code)))
      (apply #'make-byte-code fields))
     ((and (consp code) (or (null constants) (consp constants)))
      (signal 'unsupported-feature '(interpreted-function-byte-code)))
     (t (nelisp-eln-metadata-bytecode--fail 'invalid-byte-code-object fields)))))

;; ---------------------------------------------------------------------------
;; Dispatcher.
;; ---------------------------------------------------------------------------

(defun nelisp-eln-metadata-bytecode--read-one (state)
  (nelisp-eln-metadata-bytecode--skip-ws state)
  (let ((c (nelisp-eln-metadata-bytecode--peek state)))
    (cond
     ((null c) (nelisp-eln-metadata-bytecode--fail 'unexpected-eof nil))
     ((eq c ?\()
      (nelisp-eln-metadata-bytecode--set-pos
       state (1+ (nelisp-eln-metadata-bytecode--pos state)))
      (nelisp-eln-metadata-bytecode--read-list state))
     ((eq c ?\[)
      (nelisp-eln-metadata-bytecode--set-pos
       state (1+ (nelisp-eln-metadata-bytecode--pos state)))
      (apply #'vector
             (nelisp-eln-metadata-bytecode--read-bracket-elements state)))
     ((eq c ?\")
      (nelisp-eln-metadata-bytecode--read-string state))
     ((eq c ?\')
      (nelisp-eln-metadata-bytecode--set-pos
       state (1+ (nelisp-eln-metadata-bytecode--pos state)))
      (list 'quote (nelisp-eln-metadata-bytecode--read-one state)))
     ((eq c ?\`)
      (nelisp-eln-metadata-bytecode--set-pos
       state (1+ (nelisp-eln-metadata-bytecode--pos state)))
      (list (intern "`") (nelisp-eln-metadata-bytecode--read-one state)))
     ((eq c ?,)
      (nelisp-eln-metadata-bytecode--set-pos
       state (1+ (nelisp-eln-metadata-bytecode--pos state)))
      (if (eq (nelisp-eln-metadata-bytecode--peek state) ?@)
          (progn
            (nelisp-eln-metadata-bytecode--set-pos
             state (1+ (nelisp-eln-metadata-bytecode--pos state)))
            (list (intern ",@") (nelisp-eln-metadata-bytecode--read-one state)))
        (list (intern ",") (nelisp-eln-metadata-bytecode--read-one state))))
     ((eq c ?#)
      (let ((c1 (nelisp-eln-metadata-bytecode--peek-at state 1)))
        (cond
         ((eq c1 ?\[)
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (nelisp-eln-metadata-bytecode--read-byte-code state))
         ((eq c1 ?$)
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (symbol-value 'load-file-name))
         ((eq c1 ?\')
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (list 'function (nelisp-eln-metadata-bytecode--read-one state)))
         ((eq c1 ?s)
          (let ((c2 (nelisp-eln-metadata-bytecode--peek-at state 2)))
            (unless (eq c2 ?\()
              (nelisp-eln-metadata-bytecode--fail
               'unsupported-sharpsign-syntax
               (list c1 (nelisp-eln-metadata-bytecode--pos state))))
            (nelisp-eln-metadata-bytecode--set-pos
             state (+ 3 (nelisp-eln-metadata-bytecode--pos state)))
            (nelisp-eln-metadata-bytecode--read-sharps-paren state)))
         ((eq c1 ?\#)
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (intern ""))
         ((eq c1 ?\()
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (nelisp-eln-metadata-bytecode--read-propertized-string state))
         ((eq c1 ?&)
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (nelisp-eln-metadata-bytecode--read-bool-vector state))
         ((eq c1 ?:)
          (nelisp-eln-metadata-bytecode--set-pos
           state (+ 2 (nelisp-eln-metadata-bytecode--pos state)))
          (let ((end (nelisp-eln-metadata-bytecode--atom-end state))
                (start (nelisp-eln-metadata-bytecode--pos state)))
            (nelisp-eln-metadata-bytecode--set-pos state end)
            (make-symbol
             (nelisp-eln-metadata-bytecode--unescape-symbol
              (substring (nelisp-eln-metadata-bytecode--text state)
                         start end)))))
         ((and c1 (<= ?0 c1 ?9))
          (let* ((text (nelisp-eln-metadata-bytecode--text state))
                 (n (length text))
                 (j (1+ (nelisp-eln-metadata-bytecode--pos state))))
            (while (and (< j n) (<= ?0 (aref text j) ?9)) (setq j (1+ j)))
            (let ((number (string-to-number
                           (substring text
                                      (1+ (nelisp-eln-metadata-bytecode--pos
                                           state))
                                      j)))
                  (delimiter (and (< j n) (aref text j))))
              (cond
               ((eq delimiter ?=)
                (nelisp-eln-metadata-bytecode--set-pos state (1+ j))
                (nelisp-eln-metadata-bytecode--read-label-def state number))
               ((eq delimiter ?#)
                (nelisp-eln-metadata-bytecode--set-pos state (1+ j))
                (nelisp-eln-metadata-bytecode--read-label-ref state number))
               (t (nelisp-eln-metadata-bytecode--fail
                   'unsupported-sharpsign-syntax
                   (substring text
                              (nelisp-eln-metadata-bytecode--pos state)
                              (min n (+ j 1)))))))))
         (t (nelisp-eln-metadata-bytecode--fail
             'unsupported-sharpsign-syntax
             (list c1 (nelisp-eln-metadata-bytecode--pos state)))))))
     ((eq c ?\))
      (nelisp-eln-metadata-bytecode--fail 'unexpected-close-paren nil))
     ((eq c ?\])
      (nelisp-eln-metadata-bytecode--fail 'unexpected-close-bracket nil))
     (t
      (let* ((end (nelisp-eln-metadata-bytecode--atom-end state))
             (start (nelisp-eln-metadata-bytecode--pos state))
             (text (substring (nelisp-eln-metadata-bytecode--text state)
                               start end)))
        (when (= start end)
          (nelisp-eln-metadata-bytecode--fail 'unsupported-atom-syntax c))
        (nelisp-eln-metadata-bytecode--set-pos state end)
        (cond
         ((string= text "nil") nil)
         ((string= text "t") t)
         ((nelisp-eln-metadata-bytecode--integer-token-p text)
          (string-to-number text))
         (t (intern (nelisp-eln-metadata-bytecode--unescape-symbol text)))))))))

(defun nelisp-eln-metadata-bytecode-read (text)
  "Read one printed Lisp form from TEXT, GNU .eln relocation-blob grammar.
Returns `(cons OBJECT END-INDEX)', matching `read-from-string''s contract.
Signals `nelisp-eln-metadata-bytecode-error' on any syntax this reduced
grammar does not cover, rather than approximating it."
  (let* ((state (nelisp-eln-metadata-bytecode--make-state text))
         (object (nelisp-eln-metadata-bytecode--read-one state)))
    (cons object (nelisp-eln-metadata-bytecode--pos state))))

(provide 'nelisp-eln-metadata-bytecode)

;;; nelisp-eln-metadata-bytecode.el ends here
