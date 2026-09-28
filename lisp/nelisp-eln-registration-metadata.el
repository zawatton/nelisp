;;; nelisp-eln-registration-metadata.el --- metadata-only GNU graph views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Bounded GNU 31.1 graph materialization for the single-leaf registration
;; profile. Symbol fields are opaque; this is not general symbol ABI support.

;;; Code:

(require 'cl-lib)
(require 'nelisp-eln-abi)
(require 'nelisp-eln-objects)
(require 'nelisp-eln-string)
(require 'nelisp-eln-metadata-arena)
(require 'nl-ffi-memory)

(define-error 'nelisp-eln-registration-metadata-error
  "Invalid GNU registration metadata" 'nelisp-eln-abi-error)
(defconst nelisp-eln-registration-metadata--marker
  'nelisp-eln-registration-metadata-token)
(defconst nelisp-eln-registration-metadata--profiles
  '(gnu-single-leaf gnu-eval-subr gnu-eval-subr-pair gnu-verified-subr
    gnu-require-subr)
  "Registration profiles whose exact top_level_run shape was admitted by
`nelisp-eln-registration--top-level-code' before metadata is created.")
(defvar nelisp-eln-registration-metadata--live nil)

(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))

(defun nelisp-eln-registration-metadata--fail (reason &optional detail)
  (signal 'nelisp-eln-registration-metadata-error (list reason detail)))

(defun nelisp-eln-registration-metadata--vector-p (object)
  (and (vectorp object) (not (stringp object))
       (not (bool-vector-p object)) (not (recordp object))))

(defun nelisp-eln-registration-metadata--discover (data docs)
  "Validate both roots and return one plan for every reachable heap object."
  (unless (and (nelisp-eln-registration-metadata--vector-p data)
               (nelisp-eln-registration-metadata--vector-p docs))
    (nelisp-eln-registration-metadata--fail 'data-and-docs-must-be-vectors))
  (let ((todo (list data docs)) (seen nil) (plans nil))
    (while todo
      (let ((object (pop todo)))
        (cond
         ((null object) nil)
         ((integerp object) (nelisp-eln-abi-encode-fixnum object))
         ((or (symbolp object) (consp object) (stringp object)
              (nelisp-eln-registration-metadata--vector-p object))
          (unless (assq object seen)
            (let ((kind (cond ((symbolp object) 'symbol)
                              ((consp object) 'cons)
                              ((stringp object) 'string)
                              (t 'vector)))
                  (detail nil) (plan nil))
              (when (eq kind 'string)
                (setq detail (nelisp-eln-string--preflight object)))
              ;; Plan fields: SOURCE, KIND, DETAIL, WORD, ADDRESS, SNAPSHOT.
              (setq plan (vector object kind detail nil nil
                                 (pcase kind
                                   ('cons (cons (car object) (cdr object)))
                                   ('vector (copy-sequence object)))))
              (push (cons object plan) seen)
              (push plan plans)
              (pcase kind
                ('symbol (push (symbol-name object) todo))
                ('cons (push (cdr object) todo) (push (car object) todo))
                ('vector
                 (let ((i (1- (length object))))
                   (while (>= i 0)
                     (push (aref object i) todo)
                     (setq i (1- i)))))))))
         (t (nelisp-eln-registration-metadata--fail
             'unsupported-object object)))))
    (nreverse plans)))

(defun nelisp-eln-registration-metadata--size (plan)
  (pcase (aref plan 1)
    ('cons 16)
    ('symbol 48)
    ('string (+ nelisp-eln-string--descriptor-bytes
                (nth 1 (aref plan 2)) 1))
    ('vector (* 8 (1+ (length (aref plan 0)))))))

(defun nelisp-eln-registration-metadata--source-word (token object)
  (cond
   ((null object) (nelisp-eln-abi-encode-nil))
   ((integerp object) (nelisp-eln-abi-encode-fixnum object))
   (t (let ((entry (assq object (aref token 2))))
        (unless entry
          (nelisp-eln-registration-metadata--fail 'object-not-in-capability object))
        (cdr entry)))))

(defun nelisp-eln-registration-metadata--plan-word (plan arena-token)
  (let* ((kind (aref plan 1))
         (address (nelisp-eln-metadata-arena-reserve
                   arena-token (nelisp-eln-registration-metadata--size plan) 8)))
    (aset plan 4 address)
    (pcase kind
      ('cons (+ address 3))
      ('string (+ address 4))
      ('vector (+ address 5))
      ('symbol (nelisp-eln-objects--symbol-word address)))))

(defun nelisp-eln-registration-metadata--write-symbol (token plan address)
  (let* ((symbol (aref plan 0))
         (name-word (nelisp-eln-registration-metadata--source-word
                     token (symbol-name symbol)))
         (interned (eq (intern-soft (symbol-name symbol)) symbol)))
    ;; Match the GNU 31.1 Lisp_Symbol field widths and initial-obarray byte
    ;; recorded by the existing registration-only symbol writer.
    (nelisp-eln-abi-write-word
     address 0
     (if interned
         (lsh nelisp-eln-objects--symbol-interned-in-initial-obarray 5)
       0))
    (nelisp-eln-abi-write-word address 8 name-word)
    (nelisp-eln-abi-write-word address 16 48)
    (nelisp-eln-abi-write-word address 24 0)
    (nelisp-eln-abi-write-word address 32 0)
    (nelisp-eln-abi-write-word address 40 0)))

(defun nelisp-eln-registration-metadata--fill-plan (token plan arena-token)
  (let* ((object (aref plan 0))
         (kind (aref plan 1))
         (address (aref plan 4))
         (snapshot (aref plan 5)))
    (pcase kind
      ('cons
       (nelisp-eln-abi-write-word address 0
                                  (nelisp-eln-registration-metadata--source-word
                                   token (car snapshot)))
       (nelisp-eln-abi-write-word address 8
                                  (nelisp-eln-registration-metadata--source-word
                                   token (cdr snapshot))))
      ('string
       (let ((shape (aref plan 2)))
         (nelisp-eln-string--write-descriptor
          address (nth 0 shape) (nth 1 shape) (nth 2 shape))
         (nelisp-eln-string--write-bytes
          (+ address nelisp-eln-string--descriptor-bytes)
          (nth 3 shape) (nth 1 shape))))
      ('vector
       (nelisp-eln-abi-write-word address 0 (length snapshot))
       (let ((i 0))
         (while (< i (length snapshot))
           (nelisp-eln-abi-write-word
            address (* 8 (1+ i))
            (nelisp-eln-registration-metadata--source-word
             token (aref snapshot i)))
           (setq i (1+ i)))))
      ('symbol (nelisp-eln-registration-metadata--write-symbol
                token plan address)))))

(defun nelisp-eln-registration-metadata--live-token (token)
  (unless (and (vectorp token) (= (length token) 8)
               (eq (aref token 0) nelisp-eln-registration-metadata--marker)
               (eq (aref token 6) 'open)
               (memq token nelisp-eln-registration-metadata--live))
    (nelisp-eln-registration-metadata--fail 'stale-or-invalid-token))
  token)

(defun nelisp-eln-registration-metadata-create (profile data docs)
  "Create a metadata-only graph view for PROFILE, DATA, and DOCS vectors."
  (unless (memq profile nelisp-eln-registration-metadata--profiles)
    (nelisp-eln-registration-metadata--fail 'unverified-profile profile))
  (let* ((plans (nelisp-eln-registration-metadata--discover data docs))
         (arena-token (nelisp-eln-metadata-arena-acquire))
         (sources nil) (words nil) (vectors nil) (token nil) (complete nil))
    (unwind-protect
        (progn
          ;; Reserve every address and publish both identity maps before any
          ;; child is encoded, so shared edges and cycles preserve identity.
          (dolist (plan plans)
            (let* ((object (aref plan 0))
                   (word (nelisp-eln-registration-metadata--plan-word
                          plan arena-token)))
              (aset plan 3 word)
              (when (eq (aref plan 1) 'vector)
                (push (cons object (aref plan 5)) vectors))
              (push (cons object word) sources)
              (push (cons word object) words)
              (nelisp-eln-metadata-arena-retain arena-token object)))
          (setq token (vector nelisp-eln-registration-metadata--marker
                              arena-token sources words data docs 'open vectors))
          (dolist (plan plans)
            (nelisp-eln-registration-metadata--fill-plan token plan arena-token))
          (push token nelisp-eln-registration-metadata--live)
          (setq complete t)
          token)
      (unless complete
        (when arena-token
          (nelisp-eln-metadata-arena-release arena-token))))))

(defun nelisp-eln-registration-metadata--root-word (token root)
  (nelisp-eln-registration-metadata--live-token token)
  (nelisp-eln-registration-metadata--source-word token root))

(defun nelisp-eln-registration-metadata-data (token)
  "Return TOKEN's source data vector." (nelisp-eln-registration-metadata--live-token token)
  (aref token 4))
(defun nelisp-eln-registration-metadata-docs (token)
  "Return TOKEN's source documentation vector." (nelisp-eln-registration-metadata--live-token token)
  (aref token 5))
(defun nelisp-eln-registration-metadata-data-word (token)
  "Return TOKEN's GNU word for its data vector."
  (nelisp-eln-registration-metadata--root-word
   token (nelisp-eln-registration-metadata-data token)))
(defun nelisp-eln-registration-metadata-docs-word (token)
  "Return TOKEN's GNU word for its documentation vector."
  (nelisp-eln-registration-metadata--root-word
   token (nelisp-eln-registration-metadata-docs token)))
(defun nelisp-eln-registration-metadata-slot-word (token vector index)
  "Return VECTOR slot INDEX's GNU word; VECTOR may be `data' or `docs'."
  (nelisp-eln-registration-metadata--live-token token)
  (setq vector (pcase vector ('data (aref token 4))
                       ('docs (aref token 5)) (_ vector)))
  (let ((entry (assq vector (aref token 7))))
  (unless (and entry (nelisp-eln-registration-metadata--vector-p vector)
               (integerp index) (<= 0 index) (< index (length vector)))
    (nelisp-eln-registration-metadata--fail 'invalid-vector-slot
                                             (list vector index)))
    (let ((snapshot (cdr entry)) (i 0) (same t))
      (while (and (< i (length vector)) same)
        (let ((current (aref vector i)) (saved (aref snapshot i)))
          (setq same (if (integerp current) (eql current saved)
                       (eq current saved))))
        (setq i (1+ i)))
      (unless same
        (nelisp-eln-registration-metadata--fail 'source-vector-mutated vector))
      (nelisp-eln-registration-metadata--source-word
       token (aref snapshot index)))))
(defun nelisp-eln-registration-metadata-type-word (token &optional index)
  "Return the data vector's `subr-type' slot word.
INDEX defaults to 0, the `gnu-single-leaf' position; the S6 eval profiles
pass the index their admitted top_level_run call site loads."
  (nelisp-eln-registration-metadata-slot-word token 'data (or index 0)))

(defun nelisp-eln-registration-metadata-decode (token word)
  "Decode WORD under live TOKEN, preserving exact heap-object identity."
  (nelisp-eln-registration-metadata--live-token token)
  (let* ((bits (nelisp-eln-abi-normalize-word word))
         (kind (nelisp-eln-abi-classify-word bits)))
    (cond ((eq kind 'nil) nil)
          ((eq kind 'fixnum) (nelisp-eln-abi-decode-fixnum bits))
          (t (let ((entry (cl-assoc bits (aref token 3) :test #'eql)))
               (if entry (cdr entry)
                 (nelisp-eln-registration-metadata--fail
                  'word-outside-capability word)))))))

(defun nelisp-eln-registration-metadata-release (token)
  "Clear all source maps and roots, then return TOKEN's arena to the pool."
  (nelisp-eln-registration-metadata--live-token token)
  (setq nelisp-eln-registration-metadata--live
        (delq token nelisp-eln-registration-metadata--live))
  (let ((arena-token (aref token 1)))
    (aset token 1 nil) (aset token 2 nil) (aset token 3 nil)
    (aset token 4 nil) (aset token 5 nil) (aset token 7 nil)
    (aset token 6 'closed)
    (nelisp-eln-metadata-arena-release arena-token))
  t)

(provide 'nelisp-eln-registration-metadata)
;;; nelisp-eln-registration-metadata.el ends here
