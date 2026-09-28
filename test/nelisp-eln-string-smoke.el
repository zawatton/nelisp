;;; nelisp-eln-string-smoke.el --- standalone GNU string-view probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(let ((root (getenv "NELISP_ELN_STRING_SOURCE_ROOT")))
  (unless root
    (error "NELISP_ELN_STRING_SOURCE_ROOT is required"))
  (load (concat root "/packages/nl-ffi/src/nl-ffi-memory.el"))
  (load (concat root "/lisp/nelisp-eln-string.el")))

(defun nelisp-eln-string-smoke--assert (condition message)
  (unless condition (error "ELN string smoke: %s" message)))

(let* ((unicode (copy-sequence "雪 café"))
       (owner (nelisp-eln-string-allocate unicode))
       (base (nelisp-eln-string-address owner))
       (data (+ base 32))
       (ascii (copy-sequence "aあ"))
       (ascii-owner nil)
       (raw (copy-sequence (unibyte-string 0 128 255)))
       (raw-owner nil)
       (nul-owner nil))
  (unwind-protect
      (progn
        (garbage-collect)
        (nelisp-eln-string-smoke--assert
         (= (ptr-read-u64 base 0) (length unicode)) "character count")
        (nelisp-eln-string-smoke--assert
         (= (ptr-read-u64 base 8) 9) "multibyte byte count")
        (nelisp-eln-string-smoke--assert
         (= (ptr-read-u64 base 16) 0) "interval pointer")
        (nelisp-eln-string-smoke--assert
         (= (ptr-read-u64 base 24) data) "owned data pointer")
        (nelisp-eln-string-smoke--assert
         (equal (nelisp-eln-string-read owner) unicode) "GC retained owner")
        (setq ascii-owner (nelisp-eln-string-allocate ascii))
        (let ((ascii-base (nelisp-eln-string-address ascii-owner)))
          (ptr-write-u8 (+ ascii-base 32) 0 ?x)
          (nelisp-eln-string-smoke--assert
           (equal (nelisp-eln-string-read ascii-owner) "xあ") "native read")
          (nelisp-eln-string-smoke--assert
           (eq (nelisp-eln-string-sync ascii-owner) ascii) "sync identity")
          (nelisp-eln-string-smoke--assert
           (equal ascii "xあ") "sync contents"))
        (nelisp-eln-string-smoke--assert
         (eq (nelisp-eln-string-sync owner) unicode)
         "unchanged Unicode identity")
        ;; A corrupted pointer is rejected before any read through that pointer.
        (ptr-write-u64 base 24 0)
        (nelisp-eln-string-smoke--assert
         (condition-case nil (progn (nelisp-eln-string-read owner) nil)
           (nelisp-eln-string-error t))
         "corrupted pointer rejection")
        (ptr-write-u64 base 24 data)
        (nelisp-eln-string-release owner)
        (nelisp-eln-string-smoke--assert
         (condition-case nil (progn (nelisp-eln-string-read owner) nil)
           (nelisp-eln-string-error t))
         "closed owner rejection")
        (setq owner nil)
        (setq raw-owner (nelisp-eln-string-allocate raw))
        (let ((raw-base (nelisp-eln-string-address raw-owner)))
          (nelisp-eln-string-smoke--assert
           (= (ptr-read-u64 raw-base 8) #xffffffffffffffff)
           "unibyte sentinel")
          (nelisp-eln-string-smoke--assert
           (equal (nelisp-eln-string-read raw-owner) raw)
           "unibyte 0/128/255 bytes")
          (ptr-write-u8 (+ raw-base 32) 0 255)
          (ptr-write-u8 (+ raw-base 32) 1 0)
          (ptr-write-u8 (+ raw-base 32) 2 128)
          (nelisp-eln-string-smoke--assert
           (eq (nelisp-eln-string-sync raw-owner) raw)
           "unibyte sync identity")
          (nelisp-eln-string-smoke--assert
           (equal (append raw nil) '(255 0 128)) "unibyte sync values"))
        (setq nul-owner (nelisp-eln-string-allocate (string 65 0 66)))
        (nelisp-eln-string-smoke--assert
         (equal (nelisp-eln-string-read nul-owner) (string 65 0 66))
         "embedded NUL")
        (princ "NELISP-ELN-STRING-PASS\n"))
    (when owner (nelisp-eln-string-release owner))
    (when ascii-owner (nelisp-eln-string-release ascii-owner))
    (when raw-owner (nelisp-eln-string-release raw-owner))
    (when nul-owner (nelisp-eln-string-release nul-owner))))

;;; nelisp-eln-string-smoke.el ends here
