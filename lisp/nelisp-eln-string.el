;;; nelisp-eln-string.el --- owned GNU Lisp_String views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Build one temporary GNU Emacs 31.1 x86_64 Lisp_String view backed by an
;; explicitly owned external mapping. This is not a general object decoder or
;; an identity table; aggregate unit ownership is responsible for canonical
;; identity. Text properties are rejected because this owner does not create
;; an INTERVAL graph. Multibyte strings are restricted to Unicode scalar
;; values, whose GNU internal bytes equal UTF-8; Emacs raw-byte and extended
;; character codes are rejected. Unibyte strings preserve all 256 byte values.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nl-ffi-memory)

(define-error 'nelisp-eln-string-error
  "Unsupported or invalid GNU Lisp_String view" 'nelisp-eln-abi-error)

(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u64 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))

(defconst nelisp-eln-string--owner-marker 'nelisp-eln-string-owner)
(defconst nelisp-eln-string--input-plan-marker 'nelisp-eln-string-input-plan)
(defconst nelisp-eln-string--sync-plan-marker 'nelisp-eln-string-sync-plan)
(defconst nelisp-eln-string--descriptor-bytes 32)
(defconst nelisp-eln-string--field-size 0)
(defconst nelisp-eln-string--field-size-byte 8)
(defconst nelisp-eln-string--field-intervals 16)
(defconst nelisp-eln-string--field-data 24)
(defconst nelisp-eln-string--unibyte-size-byte #xffffffffffffffff)
(defconst nelisp-eln-string--word-mask #xffffffffffffffff)
(defconst nelisp-eln-string--max-unicode #x10ffff)

(defun nelisp-eln-string--fail (reason &optional detail)
  (signal 'nelisp-eln-string-error (list reason detail)))

(defun nelisp-eln-string--unicode-scalar-p (character)
  (and (integerp character)
       (<= 0 character nelisp-eln-string--max-unicode)
       (not (and (<= #xd800 character) (<= character #xdfff)))))

(defun nelisp-eln-string--has-properties-p (string)
  (unless (fboundp 'text-properties-at)
    (nelisp-eln-string--fail 'property-query-unavailable))
  (let ((index 0) (length (length string)) (found nil))
    (while (and (< index length) (not found))
      (when (text-properties-at index string)
        (setq found t))
      (setq index (1+ index)))
    found))

(defun nelisp-eln-string--preflight (string)
  "Return (CHARS BYTES UNIBYTE ENCODED) after validating STRING."
  (unless (stringp string)
    (nelisp-eln-string--fail 'not-a-string string))
  (when (nelisp-eln-string--has-properties-p string)
    (nelisp-eln-string--fail 'text-properties-unsupported))
  (let* ((chars (length string))
         (unibyte (not (multibyte-string-p string)))
         (encoded nil)
         (bytes 0))
    (if unibyte
        (setq encoded string
              bytes chars)
      (let ((index 0))
        (while (< index chars)
          (let ((character (aref string index)))
            (unless (nelisp-eln-string--unicode-scalar-p character)
              (nelisp-eln-string--fail
               'non-unicode-multibyte-character (list index character))))
          (setq index (1+ index))))
      (unless (and (fboundp 'encode-coding-string)
                   (fboundp 'decode-coding-string))
        (nelisp-eln-string--fail 'utf8-conversion-unavailable))
      (condition-case data
          (setq encoded (encode-coding-string string 'utf-8 t))
        (error (nelisp-eln-string--fail 'utf8-encoding-failed data)))
      (unless (and (stringp encoded)
                   (not (multibyte-string-p encoded)))
        (nelisp-eln-string--fail 'utf8-encoder-did-not-return-bytes))
      (setq bytes (string-bytes encoded))
      (unless (condition-case nil
                  (equal string (decode-coding-string encoded 'utf-8 t))
                (error nil))
        (nelisp-eln-string--fail 'utf8-roundtrip-failed)))
    (unless (and (integerp bytes) (>= bytes 0)
                 (< (+ nelisp-eln-string--descriptor-bytes bytes 1)
                    (ash 1 63)))
      (nelisp-eln-string--fail 'string-size-out-of-range bytes))
    (list chars bytes unibyte encoded)))

(defun nelisp-eln-string--write-descriptor (base chars bytes unibyte)
  (ptr-write-u64 base nelisp-eln-string--field-size chars)
  (ptr-write-u64 base nelisp-eln-string--field-size-byte
                 (if unibyte nelisp-eln-string--unibyte-size-byte bytes))
  (ptr-write-u64 base nelisp-eln-string--field-intervals 0)
  (ptr-write-u64 base nelisp-eln-string--field-data
                 (+ base nelisp-eln-string--descriptor-bytes)))

(defun nelisp-eln-string--write-bytes (data-address bytes length)
  (let ((index 0))
    (while (< index length)
      (ptr-write-u8 data-address index (aref bytes index))
      (setq index (1+ index)))
    (ptr-write-u8 data-address length 0)))

(defun nelisp-eln-string--read-u64 (address offset)
  (logand (ptr-read-u64 address offset) nelisp-eln-string--word-mask))

(defun nelisp-eln-string-prepare (string)
  "Validate STRING and return a no-mapping input plan for unit preflight."
  (let ((shape (nelisp-eln-string--preflight string)))
    (vector nelisp-eln-string--input-plan-marker string
            (copy-sequence string) shape)))

(defun nelisp-eln-string--validate-input-plan (plan)
  (unless (and (vectorp plan) (= (length plan) 4)
               (eq (aref plan 0) nelisp-eln-string--input-plan-marker)
               (stringp (aref plan 1))
               (stringp (aref plan 2))
               (equal (aref plan 1) (aref plan 2))
               (equal (aref plan 3)
                      (nelisp-eln-string--preflight (aref plan 1))))
    (nelisp-eln-string--fail 'stale-or-invalid-input-plan))
  plan)

(defun nelisp-eln-string-allocate (string)
  "Create an owned GNU Lisp_String descriptor view of STRING.
The returned owner strongly retains STRING and an external mmap. Pass its
descriptor address to trusted native code only while the owner is live; use
`unwind-protect' and `nelisp-eln-string-release' to bound that lifetime."
  (nelisp-eln-string-allocate-prepared (nelisp-eln-string-prepare string)))

(defun nelisp-eln-string-allocate-prepared (plan)
  "Allocate a GNU string owner from validated no-mapping input PLAN."
  (nelisp-eln-string--validate-input-plan plan)
  (let* ((string (aref plan 1))
         (shape (aref plan 3))
         (chars (nth 0 shape))
         (bytes (nth 1 shape))
         (unibyte (nth 2 shape))
         (encoded (nth 3 shape))
         ;; Allocate the Lisp owner before mmap so later Lisp allocation cannot
         ;; strand an external mapping.
         (owner (vector nelisp-eln-string--owner-marker string nil
                        chars bytes unibyte nil))
         (external (nl-ffi-memory-allocate
                    (+ nelisp-eln-string--descriptor-bytes bytes 1)))
         (base nil)
         (ready nil))
    (aset owner 2 external)
    (unwind-protect
        (progn
          (setq base (nl-ffi-memory-address external))
          (nelisp-eln-string--write-descriptor base chars bytes unibyte)
          (nelisp-eln-string--write-bytes
           (+ base nelisp-eln-string--descriptor-bytes) encoded bytes)
          (aset owner 6 t)
          (setq ready t)
          owner)
      (unless ready
        (nl-ffi-memory-release external)))))

(defun nelisp-eln-string--live-base (owner)
  (unless (and (vectorp owner) (= (length owner) 7)
               (eq (aref owner 0) nelisp-eln-string--owner-marker)
               (aref owner 6))
    (nelisp-eln-string--fail 'closed-or-invalid-owner))
  (condition-case data
      (nl-ffi-memory-address (aref owner 2))
    (error (nelisp-eln-string--fail 'closed-or-invalid-owner data))))

(defun nelisp-eln-string--validate-descriptor (owner)
  (let* ((base (nelisp-eln-string--live-base owner))
         (chars (aref owner 3))
         (bytes (aref owner 4))
         (unibyte (aref owner 5))
         (size (nelisp-eln-string--read-u64 base nelisp-eln-string--field-size))
         (size-byte (nelisp-eln-string--read-u64
                     base nelisp-eln-string--field-size-byte))
         (intervals (nelisp-eln-string--read-u64
                     base nelisp-eln-string--field-intervals))
         (data (nelisp-eln-string--read-u64 base nelisp-eln-string--field-data))
         (expected-size-byte
          (if unibyte nelisp-eln-string--unibyte-size-byte bytes))
         (expected-data (+ base nelisp-eln-string--descriptor-bytes)))
    ;; Compare the pointer with our own mapping before reading any byte through
    ;; it. Never dereference a pointer supplied or modified by native code.
    (unless (and (= size chars) (= size-byte expected-size-byte)
                 (= intervals 0) (= data expected-data))
      (nelisp-eln-string--fail
       'descriptor-changed
       (list :size size :size-byte size-byte :intervals intervals
             :data data :expected-data expected-data)))
    (list base expected-data chars bytes unibyte)))

(defun nelisp-eln-string--read-data (owner)
  (let* ((descriptor (nelisp-eln-string--validate-descriptor owner))
         (data (nth 1 descriptor))
         (chars (nth 2 descriptor))
         (bytes (nth 3 descriptor))
         (unibyte (nth 4 descriptor))
         (raw (string-make-unibyte (make-string bytes 0)))
         (index 0))
    (while (< index bytes)
      (aset raw index (ptr-read-u8 data index))
      (setq index (1+ index)))
    (unless (= (ptr-read-u8 data bytes) 0)
      (nelisp-eln-string--fail 'missing-c-terminator))
    (if unibyte
        (progn
          (unless (= (length raw) chars)
            (nelisp-eln-string--fail 'unibyte-size-mismatch
                                     (list chars (length raw))))
          raw)
      (let ((decoded
             (condition-case data
                 (decode-coding-string raw 'utf-8 t)
               (error (nelisp-eln-string--fail 'utf8-decoding-failed data)))))
        (let ((index 0))
          (while (< index (length decoded))
            (unless (nelisp-eln-string--unicode-scalar-p (aref decoded index))
              (nelisp-eln-string--fail
               'utf8-decoded-non-unicode-character
               (list index (aref decoded index))))
            (setq index (1+ index))))
        (unless (and (multibyte-string-p decoded)
                     (= (length decoded) chars)
                     (equal (encode-coding-string decoded 'utf-8 t) raw))
          (nelisp-eln-string--fail
           'multibyte-data-shape-mismatch
           (list :expected-chars chars :actual-chars (length decoded)
                 :expected-bytes bytes :actual-bytes (string-bytes decoded))))
        decoded))))

(defun nelisp-eln-string-address (owner)
  "Return the owned descriptor address for OWNER after validating its fields."
  (car (nelisp-eln-string--validate-descriptor owner)))

(defun nelisp-eln-string-read (owner)
  "Return a decoded copy of the current native bytes in OWNER's view.
The returned string does not replace the canonical Lisp string held by OWNER."
  (nelisp-eln-string--read-data owner))

(defun nelisp-eln-string-prepare-sync-from (owner)
  "Validate native OWNER bytes and stage a canonical string update plan."
  (let* ((descriptor (nelisp-eln-string--validate-descriptor owner))
         (original (aref owner 1))
         (decoded (nelisp-eln-string--read-data owner)))
    (when (nelisp-eln-string--has-properties-p original)
      (nelisp-eln-string--fail 'text-properties-added))
    (unless (and (= (length original) (length decoded))
                 (eq (not (multibyte-string-p original))
                     (not (multibyte-string-p decoded)))
                 (= (string-bytes original) (string-bytes decoded)))
      (nelisp-eln-string--fail
       'canonical-shape-changed
       (list :original-chars (length original)
             :decoded-chars (length decoded)
             :original-bytes (string-bytes original)
             :decoded-bytes (string-bytes decoded))))
    (let ((index 0))
      ;; Preflight every changed position first. GNU's fixed-width multibyte
      ;; `aset' contract only permits ASCII-to-ASCII replacements.
      (while (< index (length original))
        (let ((old (aref original index))
              (new (aref decoded index)))
          (when (and (multibyte-string-p original)
                     (/= old new)
                     (or (> old 127) (> new 127)))
            (nelisp-eln-string--fail
             'nonascii-sync-unsupported (list index old new))))
        (setq index (1+ index)))
      (vector nelisp-eln-string--sync-plan-marker 'from owner original
              (copy-sequence original) decoded (nth 0 descriptor)
              (nth 1 descriptor)))))

(defun nelisp-eln-string-prepare-sync-to (owner)
  "Validate canonical OWNER string and stage same-shape native bytes."
  (let* ((descriptor (nelisp-eln-string--validate-descriptor owner))
         (original (aref owner 1))
         (shape (nelisp-eln-string--preflight original))
         (data (nth 1 descriptor)))
    (unless (and (= (nth 0 shape) (aref owner 3))
                 (= (nth 1 shape) (aref owner 4))
                 (eq (nth 2 shape) (aref owner 5)))
      (nelisp-eln-string--fail 'canonical-shape-changed))
    (vector nelisp-eln-string--sync-plan-marker 'to owner original
            (copy-sequence original) (nth 3 shape) (nth 0 descriptor) data)))

(defun nelisp-eln-string-validate-sync-plan (plan)
  "Check that PLAN still refers to a live owner and unchanged canonical input."
  (unless (and (vectorp plan) (= (length plan) 8)
               (eq (aref plan 0) nelisp-eln-string--sync-plan-marker)
               (memq (aref plan 1) '(from to)))
    (nelisp-eln-string--fail 'stale-or-invalid-sync-plan))
  (let* ((owner (aref plan 2))
         (original (aref plan 3))
         (base (nelisp-eln-string--live-base owner))
         (descriptor (nelisp-eln-string--validate-descriptor owner)))
    (unless (and (eq original (aref owner 1))
                 (equal original (aref plan 4))
                 (= base (aref plan 6))
                 (= (nth 1 descriptor) (aref plan 7))
                 (if (eq (aref plan 1) 'to)
                     (let ((shape (nelisp-eln-string--preflight original)))
                       (and (= (nth 0 shape) (aref owner 3))
                            (= (nth 1 shape) (aref owner 4))
                            (eq (nth 2 shape) (aref owner 5))
                            (equal (aref plan 5) (nth 3 shape))))
                   (and (stringp (aref plan 5))
                        (not (nelisp-eln-string--has-properties-p original))
                        (= (length original) (length (aref plan 5)))
                        (= (string-bytes original)
                           (string-bytes (aref plan 5)))
                        (let ((index 0) (valid t))
                          (while (and valid (< index (length original)))
                            (let ((old (aref original index))
                                  (new (aref (aref plan 5) index)))
                              (when (and (multibyte-string-p original)
                                         (/= old new)
                                         (or (> old 127) (> new 127)))
                                (setq valid nil)))
                            (setq index (1+ index)))
                          valid))))
      (nelisp-eln-string--fail 'stale-sync-plan))
    t))

(defun nelisp-eln-string-commit-sync-plan (plan)
  "Apply a previously validated sync PLAN at the caller's boundary."
  (nelisp-eln-string-validate-sync-plan plan)
  (let ((owner (aref plan 2))
        (original (aref plan 3))
        (payload (aref plan 5)))
    (if (eq (aref plan 1) 'to)
        (nelisp-eln-string--write-bytes (aref plan 7) (aref plan 5)
                                        (aref owner 4))
      (let ((index 0))
        (while (< index (length original))
          (unless (= (aref original index) (aref payload index))
            (aset original index (aref payload index)))
          (setq index (1+ index)))))
    original))

(defun nelisp-eln-string-sync (owner)
  "Copy same-shape native mutations into OWNER's original Lisp string."
  (let ((plan (nelisp-eln-string-prepare-sync-from owner)))
    (nelisp-eln-string-validate-sync-plan plan)
    (nelisp-eln-string-commit-sync-plan plan)))

(defun nelisp-eln-string-release (owner)
  "Release OWNER's external mapping once; failed unmaps remain retryable."
  (nelisp-eln-string--live-base owner)
  (nl-ffi-memory-release (aref owner 2))
  (aset owner 6 nil)
  t)

(provide 'nelisp-eln-string)

;;; nelisp-eln-string.el ends here
