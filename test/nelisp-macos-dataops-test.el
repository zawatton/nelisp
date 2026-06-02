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
(require 'nelisp-phase47-compiler)

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
    (should (member #x38226823 words))  ; STRB W3,[X1,X2]
    (should (member #x78226823 words))  ; STRH W3,[X1,X2]
    (should (member #xB8226823 words))  ; STR  W3,[X1,X2]
    ;; Loads: dst=x0, base=x1, index=x2 -> base|(2<<16)|(1<<5)|0 = 0x..626820.
    (should (member #x38626820 words))  ; LDRB W0,[X1,X2]
    (should (member #x78626820 words))  ; LDRH W0,[X1,X2]
    (should (member #xB8626820 words)))) ; LDR  W0,[X1,X2]

;; ---- write syscall -------------------------------------------------

(ert-deftest nelisp-macos-dataops/write-compiles-as-darwin-arm64-syscall ()
  "`(write STR)' compiles to Darwin arm64 write(2), not x86_64 bytes."
  (let* ((bytes (nelisp-macos-dataops-test--emit
                 '(seq (write "hi\n") (exit 0))))
         (words (nelisp-macos-dataops-test--words bytes)))
    (should (> (length bytes) 0))
    (should (equal (substring bytes (- (length bytes) 3)) "hi\n"))
    (should (member #xD2800090 words))  ; MOV X16, #4  (Darwin write)
    (should (member #xD4001001 words)))) ; SVC #0x80

(ert-deftest nelisp-macos-dataops/static-imm32-table-lookup-compiles ()
  "static-imm32-table-lookup compiles on aarch64 with table rodata."
  (let* ((table-bytes (unibyte-string 1 0 0 0 42 0 0 0 7 0 0 0))
         (bytes (nelisp-macos-dataops-test--emit
                 '(seq
                   (static-imm32-table-define "t" (1 42 7))
                   (defun look (i) (static-imm32-table-lookup "t" i))
                   (exit (look 1)))))
         (words (nelisp-macos-dataops-test--words bytes)))
    (should (> (length bytes) (length table-bytes)))
    (should (equal (substring bytes (- (length bytes) (length table-bytes)))
                   table-bytes))
    (should (member #x8B000002 words))  ; ADD X2, X0, X0
    (should (member #x8B020042 words))  ; ADD X2, X2, X2
    (should (member #xB8626820 words)))) ; LDR W0, [X1, X2]

;; ---- atomic-compare-exchange -------------------------------------

(ert-deftest nelisp-macos-dataops/atomic-compare-exchange-compile-and-encode ()
  "atomic-compare-exchange compiles on aarch64 and emits CASAL.
The compiler lowers ptr/expected/new-val to x1/x2/x3, so the CAS word is
CASAL X2, X3, [X1] = 0xC8E2FC23."
  (let* ((bytes (nelisp-macos-dataops-test--emit
                 '(seq
                   (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                   (ptr-write-u64 8589934592 0 7)
                   (atomic-compare-exchange 8589934592 7 42))))
         (words (nelisp-macos-dataops-test--words bytes)))
    (should (> (length bytes) 0))
    (should (member #xC8E2FC23 words))))

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

(ert-deftest nelisp-macos-dataops/setq-local-compiles-and-stores-frame-slot ()
  "local `setq' compiles on aarch64 and stores x0 back to a frame slot."
  (let* ((bytes (nelisp-macos-dataops-test--emit
                 '(seq
                   (defun bump (x)
                     (let ((i 0))
                       (seq
                        (setq i (+ i x))
                        i)))
                   (exit (bump 15)))))
         (words (nelisp-macos-dataops-test--words bytes)))
    (should (> (length bytes) 0))
    ;; In this defun, x is slot 0 and runtime-let i is slot 1, so both the
    ;; initializer and setq-local write use STUR X0, [X29, #-32].
    (should (member #xF81E03A0 words))))

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

;; ---- record / vector / cell boxed data ops -----------------------

(ert-deftest nelisp-macos-dataops/boxed-record-vector-cell-compiles ()
  "record/vector/cell data ops compile on aarch64."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_alloc_bytes (size align)
                    (atomic-fetch-add 8589934592 size))
                  (defun nl_sexp_clone_into (src dst)
                    (seq
                     (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
                     (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
                     (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
                     (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
                  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
                  (defun nl_alloc_cell (valptr)
                    (let ((box (nl_alloc_bytes 40 8)))
                      (seq (nl_sexp_clone_into valptr box) box)))
                  (defun nl_cell_set_value (box valptr)
                    (nl_sexp_clone_into valptr box))
                  (defun nl_alloc_vector (cap)
                    (let ((box (nl_alloc_bytes 32 8)))
                      (seq
                       (ptr-write-u64 box 8 (nl_alloc_bytes (* cap 32) 8))
                       (ptr-write-u64 box 16 cap)
                       box)))
                  (defun nl_vector_set_slot (vec idx valptr)
                    (nl_sexp_clone_into valptr (+ (ptr-read-u64 vec 8) (* idx 32))))
                  (defun nl_alloc_record (tagptr count)
                    (let ((box (nl_alloc_bytes 64 8)))
                      (seq
                       (nl_sexp_clone_into tagptr box)
                       (ptr-write-u64 box 40 (nl_alloc_bytes (* count 32) 8))
                       (ptr-write-u64 box 48 count)
                       box)))
                  (defun nl_record_set_slot (rec idx valptr)
                    (nl_sexp_clone_into valptr (+ (ptr-read-u64 rec 40) (* idx 32))))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934592 0 8589938688)
                     (sexp-int-make 8589934656 5)
                     (sexp-int-make 8589934720 17)
                     (sexp-int-make 8589934784 19)
                     (cell-make 8589934720 8589934848)
                     (cell-value 8589934848 8589934912)
                     (cell-set-value 8589934848 8589934784)
                     (cell-value 8589934848 8589934976)
                     (vector-make 2 8589935040)
                     (vector-slot-set 8589935040 0 8589934720)
                     (vector-slot-set 8589935040 1 8589934784)
                     (vector-ref 8589935040 1 8589935104)
                     (record-make 8589934656 2 8589935168)
                     (record-slot-set 8589935168 0 8589934720)
                     (record-slot-set 8589935168 1 8589934784)
                     (record-slot-ref 8589935168 0 8589935232)
                     (record-type-tag 8589935168 8589935296)
                     (cons-make 8589934720 8589934784 8589935360)
                     (+ (+ (ptr-read-u64 8589934912 8)
                           (ptr-read-u64 8589934976 8))
                        (+ (vector-len 8589935040)
                           (+ (sexp-int-unwrap (vector-ref-ptr 8589935040 0))
                              (+ (ptr-read-u64 8589935104 8)
                                 (+ (record-slot-count 8589935168)
                                    (+ (sexp-int-unwrap (record-slot-ref-ptr 8589935168 1))
                                       (+ (ptr-read-u64 8589935232 8)
                                          (+ (ptr-read-u64 8589935296 8)
                                             (+ (if (< 0 (sexp-payload-ptr-record 8589935168)) 1 0)
                                                (if (= (cons-cdr-raw 8589935360) 0) 3 0))))))))))))
                  (exit (run))))))
    (should (> (length bytes) 0))))

;; ---- string / symbol name ops ------------------------------------

(ert-deftest nelisp-macos-dataops/string-symbol-name-ops-compile ()
  "string/symbol name ops and sexp-write-nil/t compile on aarch64."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_str_bytes_ptr (ptr) (str-bytes ptr))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934848 0 25185)
                     (ptr-write-u64 8589934656 0 4)
                     (ptr-write-u64 8589934656 16 8589934848)
                     (ptr-write-u64 8589934656 24 2)
                     (ptr-write-u64 8589934720 0 4)
                     (ptr-write-u64 8589934720 16 8589934848)
                     (ptr-write-u64 8589934720 24 2)
                     (ptr-write-u64 8589934784 0 5)
                     (ptr-write-u64 8589934784 16 8589934848)
                     (ptr-write-u64 8589934784 24 2)
                     (sexp-write-nil 8589934912)
                     (sexp-write-t 8589934976)
                     (+ (ptr-read-u8 (str-bytes 8589934784) 0)
                        (+ (* 3 (str-eq 8589934784 8589934784))
                           (+ (* 5 (symbol-eq 8589934656 8589934720))
                              (+ (* 7 (symbol-name-eq 8589934656 "ab"))
                                 (+ (* 11 (sexp-name-eq 8589934784 "ab"))
                                    (+ (* 13 (sexp-tag 8589934912))
                                       (+ (* 17 (sexp-tag 8589934976))
                                          (* 19 (if (= (str-bytes-ptr 8589934784)
                                                       (str-bytes 8589934784)) 1 0)))))))))))
                  (exit (run))))))
    (should (> (length bytes) 0))))

(ert-deftest nelisp-macos-dataops/string-writers-mutstr-utf8-helpers-compile ()
  "string writers, mut-str builder, and UTF-8 helper calls compile."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_alloc_str (bytes len slot)
                    (seq
                     (ptr-write-u64 slot 0 5)
                     (ptr-write-u64 slot 16 bytes)
                     (ptr-write-u64 slot 24 len)
                     slot))
                  (defun nl_alloc_symbol (bytes len slot)
                    (seq
                     (ptr-write-u64 slot 0 4)
                     (ptr-write-u64 slot 16 bytes)
                     (ptr-write-u64 slot 24 len)
                     slot))
                  (defun nl_alloc_mut_str (cap slot)
                    (seq (ptr-write-u64 slot 0 6) (ptr-write-u64 slot 8 cap) slot))
                  (defun nl_mut_str_push_byte (ptr byte) 0)
                  (defun nl_mut_str_push_codepoint (ptr cp) 0)
                  (defun nl_mut_str_len (ptr) 2)
                  (defun nl_mut_str_finalize (ptr slot)
                    (seq (ptr-write-u64 slot 0 5) slot))
                  (defun nl_str_char_count (ptr) (ptr-read-u64 ptr 24))
                  (defun nl_str_codepoint_at (ptr idx cp-slot width-slot)
                    (seq
                     (ptr-write-u64 cp-slot 0 97)
                     (ptr-write-u64 width-slot 0 1)
                     1))
                  (defun nl_str_is_alphanumeric_at (ptr idx) 1)
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (ptr-write-u64 8589934848 0 25185)
                     (sexp-write-str 8589934656 8589934848 2)
                     (sexp-write-symbol 8589934720 8589934848 2)
                     (mut-str-make-empty 8589934784 3)
                     (str-codepoint-at 8589934656 0 8589934976 8589935040)
                     (+ (sexp-tag 8589934656)
                        (+ (sexp-tag 8589934720)
                           (+ (sexp-tag 8589934784)
                              (+ (mut-str-push-byte 8589934784 97)
                                 (+ (mut-str-push-codepoint 8589934784 98)
                                    (+ (mut-str-len 8589934784)
                                       (+ (sexp-tag (mut-str-finalize 8589934784 8589934912))
                                          (+ (str-char-count 8589934656)
                                             (+ 1
                                                (+ (ptr-read-u64 8589934976 0)
                                                   (+ (ptr-read-u64 8589935040 0)
                                                      (str-is-alphanumeric-at 8589934656 0))))))))))))))
                  (exit (run))))))
    (should (> (length bytes) 0))))

(ert-deftest nelisp-macos-dataops/string-symbol-literal-writers-compile ()
  "sexp-write-symbol-lit / str-lit compile on aarch64."
  (let* ((bytes (nelisp-macos-dataops-test--emit
                 '(seq
                   (defun nl_alloc_symbol (bytes len slot)
                     (seq
                      (ptr-write-u64 slot 0 4)
                      (ptr-write-u64 slot 8 (ptr-read-u8 bytes 0))
                      (ptr-write-u64 slot 24 len)
                      slot))
                   (defun nl_alloc_str (bytes len slot)
                     (seq
                      (ptr-write-u64 slot 0 5)
                      (ptr-write-u64 slot 8 (ptr-read-u8 bytes 0))
                      (ptr-write-u64 slot 24 len)
                      slot))
                   (defun run ()
                     (seq
                      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                      (sexp-write-symbol-lit 8589934656 "ab")
                      (sexp-write-str-lit 8589934720 "ab")
                      (+ (sexp-tag 8589934656)
                         (+ (ptr-read-u64 8589934656 8)
                            (+ (sexp-tag 8589934720)
                               (ptr-read-u64 8589934720 8))))))
                   (exit (run)))))
         (words (nelisp-macos-dataops-test--words bytes)))
    (should (> (length bytes) 0))
    (should (member #x910003E0 words)))) ; ADD X0, SP, #0 (not ORR/MOV X0,SP)

;; ---- extern-call --------------------------------------------------

(ert-deftest nelisp-macos-dataops/extern-call-gp-compiles ()
  "GP-only extern-call compiles on aarch64 with local BL fixups."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun add6 (a b c d e f)
                    (+ (+ (+ a b) (+ c d)) (+ e f)))
                  (defun run () (extern-call add6 1 2 3 4 5 6))
                  (exit (run))))))
    (should (> (length bytes) 0))))

(ert-deftest nelisp-macos-dataops/extern-call-aarch64-object-reloc-compiles ()
  "aarch64 ELF object output records extern-call as R_AARCH64_CALL26."
  (let ((path (make-temp-file "nelisp-aarch64-extern-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun probe (x)
              (extern-call ext_add x 2))
           path :arch 'aarch64 :format 'elf)
          (should (> (nth 7 (file-attributes path)) 0)))
      (ignore-errors (delete-file path)))))

;; ---- AOT landing machine ops -------------------------------------

(ert-deftest nelisp-macos-dataops/aot-machine-landing-jump-compiles ()
  "aot-current-sp and aot-machine-landing-jump compile on aarch64."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun run (value)
                    (seq
                     (aot-machine-landing-jump
                      (aot-current-sp) doc129_landing_pad)
                     99
                     (aot-landing-label doc129_landing_pad value)))
                  (exit (run 37))))))
    (should (> (length bytes) 0))))

(ert-deftest nelisp-macos-dataops/aot-root-scope-compiles ()
  "auto aot-root-scope compiles on aarch64."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nelisp_aot_materialize_roots
                      (mirror frames roots out scratch)
                    roots)
                  (defun nelisp_aot_push_roots
                      (mirror frames roots out scratch)
                    0)
                  (defun nelisp_aot_pop_roots
                      (mirror frames roots out scratch)
                    0)
                  (defun nl_alloc_str (bytes len slot)
                    (seq
                     (ptr-write-u64 slot 0 5)
                     slot))
                  (defun make-str
                      ((out :type sexp)
                       (mirror :type sexp)
                       (frames :type sexp)
                       (scratch :type sexp)
                       (roots :type sexp)
                       bytes)
                    (sexp-write-str out bytes 2))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (make-str 8589934656 1 2 3 4 8589934848)
                     (sexp-tag 8589934656)))
                  (exit (run))))))
    (should (> (length bytes) 0))))

;; ---- f64 Sexp ops -------------------------------------------------

(ert-deftest nelisp-macos-dataops/f64-sexp-ops-compile ()
  "i64/f64 conversion, sexp-write-float, and float unwrap compile."
  (let ((bytes (nelisp-macos-dataops-test--emit
                '(seq
                  (defun nl_sexp_write_float (slot val)
                    (seq
                     (ptr-write-u64 slot 0 3)
                     (ptr-write-u64 slot 8 4631107791820423168)
                     slot))
                  (defun run ()
                    (seq
                     (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
                     (sexp-write-float 8589934656 (i64-to-f64 42))
                     (+ (sexp-tag 8589934656)
                        (f64-to-i64-trunc
                         (bits-to-f64
                          (sexp-float-unwrap 8589934656))))))
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
