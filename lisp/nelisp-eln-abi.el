;;; nelisp-eln-abi.el --- GNU native Lisp word ABI profile -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module describes one observed GNU Emacs 31.1 x86_64 Linux producer
;; profile.  A matching producer hash/layout is provenance correspondence,
;; not proof that NeLisp implements the GNU object, call, or GC ABI.
;; Immediate nil/fixnum words are portable through this exact profile;
;; symbol, string, cons, vector, and float objects are classified but cannot
;; be decoded as NeLisp values.

;;; Code:

(define-error 'nelisp-eln-abi-error "Unsupported GNU .eln ABI")
(define-error 'nelisp-eln-abi-unsupported-object
  "GNU .eln heap object has no NeLisp decoder" 'nelisp-eln-abi-error)

(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))

;; Keep this as decimal: NeLisp reads the equivalent 64-bit hex literal as -1.
(defconst nelisp-eln-abi-word-mask 18446744073709551615
  "Mask for one GNU 64-bit Lisp_Word.")
(defconst nelisp-eln-abi-signed-word-min (- (ash 1 63))
  "Minimum signed 64-bit Lisp_Word input.")
(defconst nelisp-eln-abi-signed-word-max (1- (ash 1 63))
  "Maximum signed 64-bit Lisp_Word input.")
(defconst nelisp-eln-abi-fixnum-min (- (ash 1 61))
  "Minimum fixnum in the Emacs 31.1 64-bit, 3-tag-bit profile.")
(defconst nelisp-eln-abi-fixnum-max (1- (ash 1 61))
  "Maximum fixnum in the Emacs 31.1 64-bit, 3-tag-bit profile.")

(defconst nelisp-eln-abi-gnu-31-1-x86_64
  '(:id gnu-emacs-31.1-linux-x86_64-ba35c031
    :producer "GNU Emacs 31.1"
    :producer-version "31.1"
    :producer-abi-hash "ba35c031"
    :source "https://github.com/emacs-mirror/emacs/blob/emacs-31.1/src/lisp.h (Lisp_Bits, Lisp_Type, make_fixnum)"
    :elf-class 64 :byte-order little :machine x86_64
    :word-bits 64 :gctypebits 3 :use-lsb-tag t
    :fixnum-payload-bits 62 :fixnum-tag 2
    :nil-word 0
    :tag-map ((symbol . 0) (unused . 1) (fixnum . 2)
              (cons . 3) (string . 4) (vectorlike . 5) (fixnum-alt . 6)
              (float . 7))
    :builtin-symbol-word "signed byte offset from lispsym, tag 0"
    :dynamic-symbol-word "signed byte offset from lispsym, tag 0; not an absolute pointer"
    :cons-layout "two adjacent Lisp_Object words: car, cdr"
    :string-layout "ptrdiff_t size,size_byte; INTERVAL*; unsigned char*"
    :runtime-compatibility unsupported)
  "One measured producer profile; matching it does not claim runtime ABI readiness.

The hash is build-specific: Emacs 31.1 also hashes its version, system
configuration/options, and native subroutine signatures.  Do not treat this
single hash as a wildcard for every Emacs 31.1 build.")

(defun nelisp-eln-abi-normalize-word (word)
  "Normalize signed or unsigned 64-bit WORD to its unsigned bit pattern.
Signal `nelisp-eln-abi-error' when WORD is outside the 64-bit range."
  (unless (and (integerp word)
               (or (and (<= nelisp-eln-abi-signed-word-min word)
                        (<= word nelisp-eln-abi-signed-word-max))
                   (and (<= 0 word)
                        (<= word nelisp-eln-abi-word-mask))))
    (signal 'nelisp-eln-abi-error (list "not a signed/unsigned 64-bit word" word)))
  (logand word nelisp-eln-abi-word-mask))

(defun nelisp-eln-abi-write-word (address offset word)
  "Store signed or unsigned 64-bit WORD at ADDRESS plus OFFSET.
Use two 32-bit operations so raw GNU words never cross Lisp's integer ABI."
  (let* ((bits (nelisp-eln-abi-normalize-word word))
         (low (logand bits #xffffffff))
         (high (logand (ash bits -32) #xffffffff)))
    (ptr-write-u32 address offset low)
    (ptr-write-u32 address (+ offset 4) high)))

(defun nelisp-eln-abi-read-word (address offset)
  "Read an unsigned GNU 64-bit word at ADDRESS plus OFFSET exactly."
  (+ (ptr-read-u32 address offset)
     (ash (ptr-read-u32 address (+ offset 4)) 32)))

(defun nelisp-eln-abi-classify-word (word)
  "Return the GNU .eln object category for signed/unsigned 64-bit WORD.
This classifies pointer tags only; it neither dereferences nor decodes them."
  (let* ((bits (nelisp-eln-abi-normalize-word word))
         (tag (logand bits 7)))
    (cond
     ((= bits 0) 'nil)
     ((= (logand bits 3) 2) 'fixnum)
     ((= tag 0) 'symbol)
     ((= tag 1) 'unused)
     ((= tag 3) 'cons)
     ((= tag 4) 'string)
     ((= tag 5) 'vectorlike)
     ((= tag 7) 'float)
     (t (signal 'nelisp-eln-abi-error (list "invalid tag" tag bits))))))

(defun nelisp-eln-abi-encode-nil ()
  "Return GNU 31.1's canonical nil word."
  (plist-get nelisp-eln-abi-gnu-31-1-x86_64 :nil-word))

(defun nelisp-eln-abi-encode-fixnum (value)
  "Encode signed integer VALUE as a GNU 31.1 x86_64 fixnum word."
  (unless (and (integerp value)
               (<= nelisp-eln-abi-fixnum-min value)
               (<= value nelisp-eln-abi-fixnum-max))
    (signal 'nelisp-eln-abi-error (list "fixnum out of range" value)))
  (nelisp-eln-abi-normalize-word (+ (ash value 2) 2)))

(defun nelisp-eln-abi-decode-fixnum (word)
  "Decode fixnum WORD, rejecting nil and every non-fixnum object tag."
  (let* ((bits (nelisp-eln-abi-normalize-word word))
         (kind (nelisp-eln-abi-classify-word bits)))
    (unless (eq kind 'fixnum)
      (signal 'nelisp-eln-abi-error (list "not a GNU fixnum" kind bits)))
    (ash (if (> bits nelisp-eln-abi-signed-word-max)
             (- bits (1+ nelisp-eln-abi-word-mask))
           bits)
         -2)))

(defun nelisp-eln-abi-decode-immediate (word)
  "Decode only nil and fixnum WORD; refuse all heap-object representations."
  (let ((kind (nelisp-eln-abi-classify-word word)))
    (cond
     ((eq kind 'nil) nil)
     ((eq kind 'fixnum) (nelisp-eln-abi-decode-fixnum word))
     (t (signal 'nelisp-eln-abi-unsupported-object
                (list kind (nelisp-eln-abi-normalize-word word)))))))

(defun nelisp-eln-abi-producer-profile-matches-p (metadata)
  "Return non-nil when ELF/producer METADATA matches the known GNU profile.
This checks producer correspondence only; it says nothing about NeLisp runtime
compatibility.  METADATA is a plist and should come from a validated ELF
container parser, not from caller-supplied assertions in production."
  (let ((profile nelisp-eln-abi-gnu-31-1-x86_64)
        (keys '(:producer-version :producer-abi-hash :elf-class :byte-order
                :machine :word-bits :gctypebits :use-lsb-tag))
        (matches (listp metadata)))
    (while (and keys matches)
      (let ((key (car keys)))
        (setq matches (equal (plist-get profile key)
                             (plist-get metadata key))))
      (setq keys (cdr keys)))
    matches))

(provide 'nelisp-eln-abi)

;;; nelisp-eln-abi.el ends here
