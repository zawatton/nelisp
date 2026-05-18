;;; nelisp-stdlib-os-int-helpers.el --- Little-endian raw-mem helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Width helpers for `nelisp-stdlib-os.el' after the libffi backend
;; removal.  16/32-bit raw-mem grammar ops stay deleted; this layer
;; rebuilds them from `ptr-read-u8' / `ptr-write-u8'.  64-bit paths use
;; the surviving `ptr-read-u64' / `ptr-write-u64' primitives directly.

;;; Code:

(defun nelisp-os--int-sign-extend (value bits)
  "Interpret VALUE as a signed BITS-wide little-endian integer."
  (let ((sign-bit (lsh 1 (1- bits)))
        (modulus  (lsh 1 bits)))
    (if (= 0 (logand value sign-bit))
        value
      (- value modulus))))

(defun nelisp-os-read-u16 (buf off)
  (+ (ptr-read-u8 buf off)
     (lsh (ptr-read-u8 buf (+ off 1)) 8)))

(defun nelisp-os-read-i16 (buf off)
  (nelisp-os--int-sign-extend (nelisp-os-read-u16 buf off) 16))

(defun nelisp-os-read-u32 (buf off)
  (+ (ptr-read-u8 buf off)
     (lsh (ptr-read-u8 buf (+ off 1)) 8)
     (lsh (ptr-read-u8 buf (+ off 2)) 16)
     (lsh (ptr-read-u8 buf (+ off 3)) 24)))

(defun nelisp-os-read-i32 (buf off)
  (nelisp-os--int-sign-extend (nelisp-os-read-u32 buf off) 32))

(defun nelisp-os-read-u64 (buf off)
  (ptr-read-u64 buf off))

(defun nelisp-os-read-i64 (buf off)
  (ptr-read-u64 buf off))

(defun nelisp-os-write-u16 (buf off val)
  (ptr-write-u8 buf off       (logand val #xFF))
  (ptr-write-u8 buf (+ off 1) (logand (lsh val -8) #xFF))
  val)

(defun nelisp-os-write-i16 (buf off val)
  (nelisp-os-write-u16 buf off val))

(defun nelisp-os-write-u32 (buf off val)
  (ptr-write-u8 buf off       (logand val #xFF))
  (ptr-write-u8 buf (+ off 1) (logand (lsh val -8) #xFF))
  (ptr-write-u8 buf (+ off 2) (logand (lsh val -16) #xFF))
  (ptr-write-u8 buf (+ off 3) (logand (lsh val -24) #xFF))
  val)

(defun nelisp-os-write-i32 (buf off val)
  (nelisp-os-write-u32 buf off val))

(defun nelisp-os-write-u64 (buf off val)
  (ptr-write-u64 buf off val)
  val)

(defun nelisp-os-write-i64 (buf off val)
  (nelisp-os-write-u64 buf off val))

(provide 'nelisp-stdlib-os-int-helpers)

;;; nelisp-stdlib-os-int-helpers.el ends here
