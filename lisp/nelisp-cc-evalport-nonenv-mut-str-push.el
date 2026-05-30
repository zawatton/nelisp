;;; nelisp-cc-evalport-nonenv-mut-str-push.el --- Phase 47 nl_mut_str_push_byte + nl_mut_str_push_codepoint  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 128 §128.B — nl_mut_str_push_byte + nl_mut_str_push_codepoint.
;;
;; Lowered from packages/nelisp-sys/eval-port/nonenv-mut-str-push.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;;
;; Exports 2 C-ABI symbols deleted from nlstr.rs:
;;   nl_mut_str_push_byte(mut_str_ptr, byte) -> i64
;;   nl_mut_str_push_codepoint(mut_str_ptr, codepoint) -> i64
;;
;; Private helpers:
;;   nl_msp_copy_loop, nl_msp_grow_and_push,
;;   nl_msp_push_bytes_2, nl_msp_push_bytes_3, nl_msp_push_bytes_4
;;
;; Linux-x86_64 only — alloc-bytes / ptr-read-u64 / extern-call ABI.

;;; Code:

(defconst nelisp-cc-evalport-nonenv-mut-str-push--source
  '(seq
    (defun nl_msp_copy_loop (src dst i n)
      (if (= i n)
          1
        (seq (nelisp_ptr_write_u8 dst i (nelisp_ptr_read_u8 src i))
             (nl_msp_copy_loop src dst (+ i 1) n))))
    (defun nl_msp_grow_and_push (nlstr cap data_ptr len byte)
      (let ((new_cap (if (< (* cap 2) 8) 8 (* cap 2)))
            (new_buf (nelisp_alloc_bytes new_cap 1)))
        (seq (nl_msp_copy_loop data_ptr new_buf 0 len)
             (nelisp_ptr_write_u8 new_buf len byte)
             (nelisp_dealloc_bytes data_ptr cap 1)
             (seq (ptr-write-u64 nlstr 0 new_cap)
                  (ptr-write-u64 (+ nlstr 8) 0 new_buf)
                  (ptr-write-u64 (+ nlstr 16) 0 (+ len 1))))))
    (defun nl_mut_str_push_byte (mut_str_ptr byte)
      (let ((nlstr (ptr-read-u64 (+ mut_str_ptr 8) 0))
            (cap (ptr-read-u64 nlstr 0))
            (data_ptr (ptr-read-u64 (+ nlstr 8) 0))
            (len (ptr-read-u64 (+ nlstr 16) 0)))
        (if (< len cap)
            (seq (nelisp_ptr_write_u8 data_ptr len byte)
                 (ptr-write-u64 (+ nlstr 16) 0 (+ len 1)))
          (nl_msp_grow_and_push nlstr cap data_ptr len byte))))
    (defun nl_msp_push_bytes_2 (ptr b0 b1)
      (seq (nl_mut_str_push_byte ptr b0)
           (nl_mut_str_push_byte ptr b1)))
    (defun nl_msp_push_bytes_3 (ptr b0 b1 b2)
      (seq (nl_mut_str_push_byte ptr b0)
           (nl_mut_str_push_byte ptr b1)
           (nl_mut_str_push_byte ptr b2)))
    (defun nl_msp_push_bytes_4 (ptr b0 b1 b2 b3)
      (seq (nl_mut_str_push_byte ptr b0)
           (nl_mut_str_push_byte ptr b1)
           (nl_mut_str_push_byte ptr b2)
           (nl_mut_str_push_byte ptr b3)))
    (defun nl_mut_str_push_codepoint (mut_str_ptr codepoint)
      (let ((cp (if (< codepoint 0)
                    65533
                  (if (> codepoint 1114111)
                      65533
                    (if (< codepoint 55296)
                        codepoint
                      (if (< codepoint 57344)
                          65533
                        codepoint))))))
        (if (< cp 128)
            (nl_mut_str_push_byte mut_str_ptr cp)
          (if (< cp 2048)
              (nl_msp_push_bytes_2
               mut_str_ptr
               (+ 192 (/ cp 64))
               (+ 128 (- cp (* (/ cp 64) 64))))
            (if (< cp 65536)
                (nl_msp_push_bytes_3
                 mut_str_ptr
                 (+ 224 (/ cp 4096))
                 (+ 128 (- (/ cp 64) (* (/ cp 4096) 64)))
                 (+ 128 (- cp (* (/ cp 64) 64))))
              (nl_msp_push_bytes_4
               mut_str_ptr
               (+ 240 (/ cp 262144))
               (+ 128 (- (/ cp 4096) (* (/ cp 262144) 64)))
               (+ 128 (- (/ cp 64) (* (/ cp 4096) 64)))
               (+ 128 (- cp (* (/ cp 64) 64))))))))))
  "Doc 128 §128.B Phase 47 source for nl_mut_str_push_byte + nl_mut_str_push_codepoint.

Multi-entry `(seq DEFUN ...)' manifest.

Lowered from packages/nelisp-sys/eval-port/nonenv-mut-str-push.nl.

Public exports: nl_mut_str_push_byte / nl_mut_str_push_codepoint.
Net Rust delta: zero.  Resolves 2 undefined symbols.")

(provide 'nelisp-cc-evalport-nonenv-mut-str-push)

;;; nelisp-cc-evalport-nonenv-mut-str-push.el ends here
