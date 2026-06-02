;;; nelisp-macos-dataops-test.el --- macOS arm64 data-op emit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side (Linux/CI) regression guard for the aarch64 / Darwin
;; Phase-47 emit of the "data op" batch: width-specific pointer
;; load/store (`ptr-{read,write}-u{8,16,32}'), `dealloc-bytes',
;; `cons-set-car' / `cons-set-cdr', `cond', and `and'/`or' logic.
;;
;; Compilation (`nelisp-macos-build--emit-text') is host-independent —
;; only *executing* the produced Mach-O needs Apple Silicon + codesign
;; (that end-to-end path is `tools/macos-selfhost-test.sh').  These ERT
;; cases assert that:
;;   1. each op compiles to aarch64 __text bytes WITHOUT signalling
;;      `:<op>-aarch64-unsupported' (= the dispatch arm is wired), and
;;   2. the width-specific pointer ops emit the exact LDR{B,H,}/STR{B,H,}
;;      register-offset instructions (golden opcode presence check).

;;; Code:

(require 'ert)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (d '("../lisp" "../src" "../scripts"))
    (add-to-list 'load-path (expand-file-name d here))))

(require 'nelisp-macos-build)

;; ---- helpers ----

(defun nelisp-macos-dataops-test--emit (sexp)
  "Compile program SEXP through the aarch64 / Darwin emit path.
Returns the __text unibyte string; propagates any compile error."
  (nelisp-macos-build--emit-text sexp))

(defun nelisp-macos-dataops-test--words (bytes)
  "Return the list of 32-bit little-endian words in BYTES."
  (let ((n (length bytes)) (i 0) (acc '()))
    (while (<= (+ i 4) n)
      (push (logior (aref bytes i)
                    (ash (aref bytes (+ i 1)) 8)
                    (ash (aref bytes (+ i 2)) 16)
                    (ash (aref bytes (+ i 3)) 24))
            acc)
      (setq i (+ i 4)))
    (nreverse acc)))

;; ---- ptr-read/write u8/u16/u32 -----------------------------------

(ert-deftest nelisp-macos-dataops/ptr-widths-compile-and-encode ()
  "ptr-{read,write}-u{8,16,32} compile and emit the right reg-offset ops.
The program writes a u32/u16/u8 and reads each width back; the emit
must contain STR{B,H,}/LDR{B,H,} `[X1, X2]' words (val→x3, result→x0)."
  (let* ((bytes (nelisp-macos-dataops-test--emit
                 '(seq
                   (defun run ()
                     (seq
                      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                      (ptr-write-u32 8589934592 256 197121)
                      (ptr-write-u16 8589934592 260 40)
                      (ptr-write-u8  8589934592 262 5)
                      (ptr-read-u8 8589934592 256)
                      (ptr-read-u16 8589934592 260)
                      (ptr-read-u32 8589934592 264)))
                   (exit (run)))))
         (words (nelisp-macos-dataops-test--words bytes)))
    (should (> (length bytes) 0))
    ;; Stores: val=x3, base=x1, index=x2  -> base|(2<<16)|(1<<5)|3 = 0x..226823.
    (should (memq #x38226823 words))    ; STRB W3,[X1,X2]
    (should (memq #x78226823 words))    ; STRH W3,[X1,X2]
    (should (memq #xB8226823 words))    ; STR  W3,[X1,X2]
    ;; Loads: dst=x0, base=x1, index=x2 -> base|(2<<16)|(1<<5)|0 = 0x..626820.
    (should (memq #x38626820 words))    ; LDRB W0,[X1,X2]
    (should (memq #x78626820 words))    ; LDRH W0,[X1,X2]
    (should (memq #xB8626820 words))))  ; LDR  W0,[X1,X2]

;; ---- dealloc-bytes -----------------------------------------------

(ert-deftest nelisp-macos-dataops/dealloc-bytes-compiles ()
  "dealloc-bytes compiles on aarch64 (BL nl_dealloc_bytes + sentinel)."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_alloc_bytes (size align)
                    (atomic-fetch-add 8589934592 size))
                  (defun nl_dealloc_bytes (ptr size align) 0)
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934592 0 (+ 8589934592 16))
                     (+ (dealloc-bytes (alloc-bytes 16 8) 16 8) 41)))
                  (exit (run))))))
    (should (> (length bytes) 0))))

;; ---- cons-set-car / cons-set-cdr ---------------------------------

(ert-deftest nelisp-macos-dataops/cons-set-car-compiles ()
  "cons-set-car compiles on aarch64 (resolve box, BL nl_consbox_set_car)."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_alloc_bytes (size align)
                    (atomic-fetch-add 8589934592 size))
                  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
                  (defun nl_consbox_set_car (box valptr)
                    (seq (ptr-write-u64 box 0 (ptr-read-u64 valptr 0))
                         (ptr-write-u64 box 8 (ptr-read-u64 valptr 8))
                         (ptr-write-u64 box 16 (ptr-read-u64 valptr 16))
                         (ptr-write-u64 box 24 (ptr-read-u64 valptr 24))))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934592 0 8589935104)
                     (sexp-int-make 8589934656 1)
                     (sexp-int-make 8589934720 2)
                     (cons-make 8589934656 8589934720 8589934784)
                     (sexp-int-make 8589934912 9)
                     (cons-set-car 8589934784 8589934912)
                     (cons-car 8589934784 8589934848)
                     (ptr-read-u64 8589934848 8)))
                  (exit (run))))))
    (should (> (length bytes) 0))))

(ert-deftest nelisp-macos-dataops/cons-set-cdr-compiles ()
  "cons-set-cdr compiles on aarch64 (resolve box, BL nl_consbox_set_cdr)."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_alloc_bytes (size align)
                    (atomic-fetch-add 8589934592 size))
                  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
                  (defun nl_consbox_set_cdr (box valptr)
                    (seq (ptr-write-u64 box 32 (ptr-read-u64 valptr 0))
                         (ptr-write-u64 box 40 (ptr-read-u64 valptr 8))
                         (ptr-write-u64 box 48 (ptr-read-u64 valptr 16))
                         (ptr-write-u64 box 56 (ptr-read-u64 valptr 24))))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934592 0 8589935104)
                     (sexp-int-make 8589934656 1)
                     (sexp-int-make 8589934720 2)
                     (cons-make 8589934656 8589934720 8589934784)
                     (sexp-int-make 8589934912 9)
                     (cons-set-cdr 8589934784 8589934912)
                     (cons-cdr 8589934784 8589934848)
                     (ptr-read-u64 8589934848 8)))
                  (exit (run))))))
    (should (> (length bytes) 0))))

;; ---- cond / logic ------------------------------------------------

(ert-deftest nelisp-macos-dataops/cond-compiles ()
  "cond first-match dispatch compiles on aarch64 (cmp xN,xzr + b.cond)."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun classify (x)
                    (cond ((= x 1) 100) ((= x 2) 50) (t 7)))
                  (exit (classify 2))))))
    (should (> (length bytes) 0))))

(ert-deftest nelisp-macos-dataops/logic-and-or-compiles ()
  "`and'/`or' short-circuit logic compiles on aarch64."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun run () (+ (if (and 1 1) 10 0) (if (or 0 1) 5 0)))
                  (exit (run))))))
    (should (> (length bytes) 0))))

;; ---- cons-make-with-clone (fused constructor) --------------------

(ert-deftest nelisp-macos-dataops/cons-make-with-clone-compiles ()
  "cons-make-with-clone compiles on aarch64 (box alloc + two clone BLs)."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_alloc_bytes (size align)
                    (atomic-fetch-add 8589934592 size))
                  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
                  (defun nl_sexp_clone_into (src dst)
                    (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
                         (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
                         (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
                         (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934592 0 8589935104)
                     (sexp-int-make 8589934656 20)
                     (sexp-int-make 8589934720 3)
                     (cons-make-with-clone 8589934656 8589934720 8589934784)
                     (cons-car 8589934784 8589934976)
                     (cons-cdr 8589934784 8589935040)
                     (+ (ptr-read-u64 8589934976 8)
                        (ptr-read-u64 8589935040 8))))
                  (exit (run))))))
    (should (> (length bytes) 0))))

;; ---- addr-of + call-ptr (function pointers) ----------------------

(ert-deftest nelisp-macos-dataops/addr-of-call-ptr-compile-and-encode ()
  "addr-of + call-ptr compile on aarch64 and emit ADR + BLR.
`(call-ptr (addr-of add2) 30 12)' takes add2's PC-relative address
(ADR x0), shuffles args into x0/x1, then invokes it (BLR x9)."
  (let* ((bytes (nelisp-macos-dataops-test--emit
                 '(seq
                   (defun add2 (a b) (+ a b))
                   (defun run () (call-ptr (addr-of add2) 30 12))
                   (exit (run)))))
         (words (nelisp-macos-dataops-test--words bytes))
         (adr nil) (blr nil))
    (should (> (length bytes) 0))
    (dolist (w words)
      ;; ADR: bit31=0, bits[28:24]=0b10000 (mask 0x9F000000 == 0x10000000).
      (when (= (logand w #x9F000000) #x10000000) (setq adr t))
      ;; BLR Xn: 0xD63F0000 | (Rn<<5) (mask out the Rn field).
      (when (= (logand w #xFFFFFC1F) #xD63F0000) (setq blr t)))
    (should adr)
    (should blr)))

(provide 'nelisp-macos-dataops-test)

;;; nelisp-macos-dataops-test.el ends here
