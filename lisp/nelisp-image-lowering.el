;;; nelisp-image-lowering.el --- Lower Elisp S-expressions to NlImage heap bytes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Elisp port of `build-tool/src/image_lowering.rs`.

;;; Code:

(require 'cl-lib)

(defconst nelisp-image-lowering--slot-size 8)
(defconst nelisp-image-lowering--cell-size 16)
(defconst nelisp-image-lowering--symbol-size 16)
(defconst nelisp-image-lowering--u64-mask #xffffffffffffffff)

;; Runtime tag/constants copied from `nelisp-runtime/src/image/value.rs`.
(defconst nelisp-image-lowering--nl-value-tag-bits 3) ; value.rs:23
(defconst nelisp-image-lowering--nl-value-tag-int 1) ; value.rs:29
(defconst nelisp-image-lowering--nl-value-tag-cons 2) ; value.rs:37
(defconst nelisp-image-lowering--nl-value-tag-nil 3) ; value.rs:42
(defconst nelisp-image-lowering--nl-value-tag-string 4) ; value.rs:50
(defconst nelisp-image-lowering--nl-value-tag-symbol 5) ; value.rs:57
(defconst nelisp-image-lowering--nl-value-tag-float 6) ; value.rs:75
(defconst nelisp-image-lowering--nl-value-tag-vector 7) ; value.rs:86
(defconst nelisp-image-lowering--nl-immediate-t 11) ; value.rs:66

;; Runtime reloc kind copied from `nelisp-runtime/src/image/reloc.rs`.
(defconst nelisp-image-lowering--nl-reloc-kind-heap-base-plus-offset 1) ; reloc.rs:15

(cl-defstruct (nelisp-image-lowering--state
               (:constructor nelisp-image-lowering--state-create))
  (heap (unibyte-string) :type string)
  (relocs nil :type list))

(defun nelisp-image-lowering--u64 (n)
  (logand n nelisp-image-lowering--u64-mask))

(defun nelisp-image-lowering--tag-int (n)
  (nelisp-image-lowering--u64
   (logior (lsh n nelisp-image-lowering--nl-value-tag-bits)
           nelisp-image-lowering--nl-value-tag-int)))

(defun nelisp-image-lowering--next-multiple-of-8 (n)
  (* nelisp-image-lowering--slot-size
     (/ (+ n (1- nelisp-image-lowering--slot-size))
        nelisp-image-lowering--slot-size)))

(defun nelisp-image-lowering--append-zeroes (state count)
  (setf (nelisp-image-lowering--state-heap state)
        (concat (nelisp-image-lowering--state-heap state)
                (make-string count 0))))

(defun nelisp-image-lowering--alloc-bytes (state count)
  (let ((offset (length (nelisp-image-lowering--state-heap state))))
    (nelisp-image-lowering--append-zeroes state count)
    offset))

(defun nelisp-image-lowering--alloc-slot (state)
  (nelisp-image-lowering--alloc-bytes state nelisp-image-lowering--slot-size))

(defun nelisp-image-lowering--alloc-cell (state)
  (nelisp-image-lowering--alloc-bytes state nelisp-image-lowering--cell-size))

(defun nelisp-image-lowering--alloc-symbol (state)
  (nelisp-image-lowering--alloc-bytes state nelisp-image-lowering--symbol-size))

(defun nelisp-image-lowering--write-u64 (state offset word)
  (let ((heap (nelisp-image-lowering--state-heap state))
        (value (nelisp-image-lowering--u64 word)))
    (dotimes (i nelisp-image-lowering--slot-size)
      (aset heap (+ offset i) (logand (lsh value (* -8 i)) #xff)))))

(defun nelisp-image-lowering--push-reloc (state write-at addend)
  (setf (nelisp-image-lowering--state-relocs state)
        (append (nelisp-image-lowering--state-relocs state)
                (list (list :write-at write-at
                            :addend addend
                            :kind nelisp-image-lowering--nl-reloc-kind-heap-base-plus-offset)))))

(defun nelisp-image-lowering--utf8-bytes (s)
  (encode-coding-string s 'utf-8 t))

(defun nelisp-image-lowering--alloc-string (state string)
  (let* ((bytes (nelisp-image-lowering--utf8-bytes string))
         (byte-len (length bytes))
         (padded (nelisp-image-lowering--next-multiple-of-8 byte-len))
         (offset (nelisp-image-lowering--alloc-bytes state
                                                     (+ nelisp-image-lowering--slot-size
                                                        padded)))
         (heap (nelisp-image-lowering--state-heap state))
         (content-start (+ offset nelisp-image-lowering--slot-size)))
    (nelisp-image-lowering--write-u64 state offset byte-len)
    (dotimes (i byte-len)
      (aset heap (+ content-start i) (aref bytes i)))
    offset))

(defun nelisp-image-lowering--f64-bits (float)
  (cond
   ((not (floatp float))
    (signal 'wrong-type-argument (list 'floatp float)))
   ((= float 0.0)
    0)
   (t
    (let* ((negative (< float 0.0))
           (abs-float (abs float))
           (parts (frexp abs-float))
           (significand (car parts))
           (exponent (cdr parts))
           (biased-exponent (+ exponent 1022)))
      (if (> biased-exponent 0)
          (let* ((fraction (- (* significand 2.0) 1.0))
                 (mantissa (truncate (* fraction (expt 2 52)))))
            (nelisp-image-lowering--u64
             (logior (if negative (lsh 1 63) 0)
                     (lsh biased-exponent 52)
                     mantissa)))
        (let ((mantissa (truncate (* abs-float (expt 2 1074)))))
          (nelisp-image-lowering--u64
           (logior (if negative (lsh 1 63) 0)
                   mantissa))))))))

(defun nelisp-image-lowering--alloc-float (state float)
  (let ((offset (nelisp-image-lowering--alloc-slot state)))
    (nelisp-image-lowering--write-u64 state offset
                                      (nelisp-image-lowering--f64-bits float))
    offset))

(defun nelisp-image-lowering--alloc-vector (state len)
  (let ((offset (nelisp-image-lowering--alloc-bytes state
                                                    (+ nelisp-image-lowering--slot-size
                                                       (* len nelisp-image-lowering--slot-size)))))
    (nelisp-image-lowering--write-u64 state offset len)
    offset))

(defun nelisp-image-lowering--write-value-at (state slot-offset value)
  (cond
   ((null value)
    (nelisp-image-lowering--write-u64 state slot-offset
                                      nelisp-image-lowering--nl-value-tag-nil))
   ((eq value t)
    (nelisp-image-lowering--write-u64 state slot-offset
                                      nelisp-image-lowering--nl-immediate-t))
   ((integerp value)
    (nelisp-image-lowering--write-u64 state slot-offset
                                      (nelisp-image-lowering--tag-int value)))
   ((floatp value)
    (let ((float-offset (nelisp-image-lowering--alloc-float state value)))
      (nelisp-image-lowering--push-reloc
       state slot-offset
       (logior float-offset nelisp-image-lowering--nl-value-tag-float))))
   ((stringp value)
    (let ((string-offset (nelisp-image-lowering--alloc-string state value)))
      (nelisp-image-lowering--push-reloc
       state slot-offset
       (logior string-offset nelisp-image-lowering--nl-value-tag-string))))
   ((symbolp value)
    (let* ((name-offset (nelisp-image-lowering--alloc-string state (symbol-name value)))
           (symbol-offset (nelisp-image-lowering--alloc-symbol state)))
      (nelisp-image-lowering--push-reloc
       state symbol-offset
       (logior name-offset nelisp-image-lowering--nl-value-tag-string))
      (nelisp-image-lowering--write-u64 state (+ symbol-offset nelisp-image-lowering--slot-size)
                                        nelisp-image-lowering--nl-value-tag-nil)
      (nelisp-image-lowering--push-reloc
       state slot-offset
       (logior symbol-offset nelisp-image-lowering--nl-value-tag-symbol))))
   ((consp value)
    (let ((cell-offset (nelisp-image-lowering--alloc-cell state)))
      (nelisp-image-lowering--push-reloc
       state slot-offset
       (logior cell-offset nelisp-image-lowering--nl-value-tag-cons))
      (nelisp-image-lowering--write-value-at state cell-offset (car value))
      (nelisp-image-lowering--write-value-at state (+ cell-offset nelisp-image-lowering--slot-size)
                                             (cdr value))))
   ((vectorp value)
    (let* ((len (length value))
           (vector-offset (nelisp-image-lowering--alloc-vector state len)))
      (nelisp-image-lowering--push-reloc
       state slot-offset
       (logior vector-offset nelisp-image-lowering--nl-value-tag-vector))
      (dotimes (i len)
        (nelisp-image-lowering--write-value-at
         state
         (+ vector-offset nelisp-image-lowering--slot-size
            (* i nelisp-image-lowering--slot-size))
         (aref value i)))))
   (t
    (error "Unsupported value for nelisp-image-lower: %S" value))))

;;;###autoload
(defun nelisp-image-lower (sexp)
  "Lower SEXP to (cons HEAP-BYTES RELOCS).
HEAP-BYTES is a unibyte string with the heap segment.
RELOCS is a list of plists with :write-at OFFSET :addend ADDEND :kind KIND."
  (let* ((state (nelisp-image-lowering--state-create))
         (head-slot (nelisp-image-lowering--alloc-slot state)))
    (unless (= head-slot 0)
      (error "Head slot must land at heap[0..8], got %d" head-slot))
    (nelisp-image-lowering--write-value-at state head-slot sexp)
    (cons (nelisp-image-lowering--state-heap state)
          (nelisp-image-lowering--state-relocs state))))

(provide 'nelisp-image-lowering)

;;; nelisp-image-lowering.el ends here
