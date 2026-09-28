;;; nelisp-eln-bignum.el --- owned GNU bignum views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module projects out-of-fixnum NeLisp integers into externally owned,
;; read-only GNU 31.1 x86-64 bignum bytes for narrowly authenticated NeLisp
;; adapters. These bytes are not GNU heap objects: never pass them to GNU's
;; heap allocator, collector, or native code that mutates or retains objects.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nl-ffi-memory)

(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))

(define-error 'nelisp-eln-bignum-error "Invalid temporary GNU bignum view")

(defconst nelisp-eln-bignum--marker 'nelisp-eln-bignum-view)
(defconst nelisp-eln-bignum--tag 5)
(defconst nelisp-eln-bignum--header-flag 4611686018427387904)
(defconst nelisp-eln-bignum--pvec-bignum 2)
(defvar nelisp-eln-bignum--live nil)
(defvar nelisp-eln-bignum--pending-cleanups nil)

(defun nelisp-eln-bignum--header ()
  "Return the measured GNU 31.1 PVEC_BIGNUM header (two non-Lisp words)."
  (+ nelisp-eln-bignum--header-flag
     (ash nelisp-eln-bignum--pvec-bignum 24)
     (ash 2 12)))

(defun nelisp-eln-bignum--live-view (token)
  (unless (and (vectorp token) (= (length token) 6)
               (eq (aref token 0) nelisp-eln-bignum--marker)
               (eq (aref token 4) 'open)
               (memq token nelisp-eln-bignum--live))
    (signal 'nelisp-eln-bignum-error (list 'stale-view token)))
  token)

(defun nelisp-eln-bignum--integer-limbs (value)
  "Return VALUE's nonzero magnitude as little-endian unsigned 64-bit limbs."
  (let ((magnitude (abs value)) limbs)
    (while (> magnitude 0)
      (push (logand magnitude nelisp-eln-abi-word-mask) limbs)
      (setq magnitude (ash magnitude -64)))
    (nreverse limbs)))

(defun nelisp-eln-bignum-allocate (value)
  "Create an owned read-only GNU-layout view of out-of-fixnum integer VALUE.
The returned token pins VALUE and all storage until release."
  (unless (and (integerp value)
               (or (< value nelisp-eln-abi-fixnum-min)
                   (> value nelisp-eln-abi-fixnum-max)))
    (signal 'nelisp-eln-bignum-error (list 'not-out-of-fixnum-integer value)))
  (let* ((limbs (nelisp-eln-bignum--integer-limbs value))
         (count (length limbs))
         (bytes (+ 24 (* 8 count)))
         (memory nil) (address nil) (token nil) (failure nil))
    (condition-case err
        (progn
          (setq memory (nl-ffi-memory-allocate bytes)
                address (nl-ffi-memory-address memory))
          (unless (and (integerp address) (> address 0) (= (logand address 7) 0))
            (signal 'nelisp-eln-bignum-error (list 'invalid-aligned-address address)))
          (nelisp-eln-abi-write-word address 0 (nelisp-eln-bignum--header))
          (ptr-write-u32 address 8 count)
          (ptr-write-u32 address 12 (if (< value 0) (- count) count))
          (nelisp-eln-abi-write-word address 16 (+ address 24))
          (let ((offset 24))
            (dolist (limb limbs)
              (nelisp-eln-abi-write-word address offset limb)
              (setq offset (+ offset 8))))
          (setq token (vector nelisp-eln-bignum--marker value memory address
                              'open nil)
                nelisp-eln-bignum--live
                (cons token nelisp-eln-bignum--live)))
      (error (setq failure err)))
    (when failure
      (when memory
        (condition-case nil
            (nl-ffi-memory-release memory)
          (error (push (cons 'memory memory)
                       nelisp-eln-bignum--pending-cleanups))))
      (signal (car failure) (cdr failure)))
    token))

(defalias 'nelisp-eln-bignum-create #'nelisp-eln-bignum-allocate)

(defun nelisp-eln-bignum-word (token)
  "Return TOKEN's GNU vectorlike word; it is valid only while TOKEN is live."
  (setq token (nelisp-eln-bignum--live-view token))
  (+ (aref token 3) nelisp-eln-bignum--tag))

(defun nelisp-eln-bignum-source (token)
  "Return TOKEN's strongly pinned canonical NeLisp integer source."
  (aref (nelisp-eln-bignum--live-view token) 1))

(defun nelisp-eln-bignum-address (token)
  "Return TOKEN's descriptor address while TOKEN is live."
  (aref (nelisp-eln-bignum--live-view token) 3))

(defun nelisp-eln-bignum-decode (token word)
  "Decode only TOKEN's exact projected WORD to its pinned NeLisp integer."
  (setq token (nelisp-eln-bignum--live-view token))
  (unless (= (nelisp-eln-abi-normalize-word word)
             (nelisp-eln-abi-normalize-word
              (+ (aref token 3) nelisp-eln-bignum--tag)))
    (signal 'nelisp-eln-bignum-error (list 'foreign-word word)))
  (aref token 1))

(defun nelisp-eln-bignum-release (token)
  "Release TOKEN. Releasing an already closed token is harmless."
  (unless (and (vectorp token) (= (length token) 6)
               (eq (aref token 0) nelisp-eln-bignum--marker))
    (signal 'nelisp-eln-bignum-error (list 'invalid-token token)))
  (when (eq (aref token 4) 'open)
    (unless (memq token nelisp-eln-bignum--live)
      (signal 'nelisp-eln-bignum-error (list 'unregistered-token token)))
    (setq nelisp-eln-bignum--live (delq token nelisp-eln-bignum--live))
    (aset token 4 'closed)
    (let ((memory (aref token 2)))
      (aset token 2 nil)
      (aset token 1 nil)
      (aset token 3 nil)
      (condition-case err
          (nl-ffi-memory-release memory)
        (error
         (aset token 4 'cleanup-pending)
         (aset token 2 memory)
         (push token nelisp-eln-bignum--pending-cleanups)
         (signal (car err) (cdr err))))))
  t)

(defun nelisp-eln-bignum-retry-pending-cleanup ()
  "Retry externally owned storage releases that previously failed."
  (let ((remaining nil))
    (dolist (entry (copy-sequence nelisp-eln-bignum--pending-cleanups))
      (condition-case nil
          (progn
            (if (and (vectorp entry) (eq (aref entry 0)
                                         nelisp-eln-bignum--marker))
                (progn
                  (nl-ffi-memory-release (aref entry 2))
                  (aset entry 2 nil)
                  (aset entry 4 'closed))
              (nl-ffi-memory-release (cdr entry)))
            (setq nelisp-eln-bignum--pending-cleanups
                  (delq entry nelisp-eln-bignum--pending-cleanups)))
        (error (push entry remaining))))
    (setq nelisp-eln-bignum--pending-cleanups (nreverse remaining)))
  t)

(provide 'nelisp-eln-bignum)

;;; nelisp-eln-bignum.el ends here
