;;; nelisp-cc-evalport-nonenv-char-table.el --- Phase 47 nl_char_table_get_raw + nl_char_table_set_raw  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage — nl_char_table_get_raw + nl_char_table_set_raw.
;;
;; Lowered from packages/nelisp-sys/eval-port/nonenv-char-table.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;;
;; Exports 2 C-ABI symbols deleted from nlchartable.rs:
;;   nl_char_table_get_raw(arg, idx, out) -> i64
;;   nl_char_table_set_raw(arg, idx, val, out) -> i64
;;
;; Private helpers:
;;   nl_ct_copy_u64s, nl_ct_copy_entries, nl_ct_grow_entries,
;;   nl_ct_get_search, nl_ct_set_step, nl_ct_push_new
;;
;; Linux-x86_64 only — alloc-bytes / ptr-read-u64 / extern-call ABI.

;;; Code:

(defconst nelisp-cc-evalport-nonenv-char-table--source
  '(seq
    (defun nl_ct_copy_u64s (src dst i n)
      (if (= i n)
          1
        (let ((word_off (* i 8))
              (val (ptr-read-u64 (+ src word_off) 0)))
          (seq (ptr-write-u64 (+ dst word_off) 0 val)
               (nl_ct_copy_u64s src dst (+ i 1) n)))))
    (defun nl_ct_copy_entries (src_ptr dst_ptr i n)
      (if (= i n)
          1
        (let ((entry_off (* i 40)))
          (seq (nl_ct_copy_u64s (+ src_ptr entry_off) (+ dst_ptr entry_off) 0 5)
               (nl_ct_copy_entries src_ptr dst_ptr (+ i 1) n)))))
    (defun nl_ct_grow_entries (box old_ptr old_cap old_len)
      (let ((new_cap (if (< (* old_cap 2) 4) 4 (* old_cap 2)))
            (new_ptr (nelisp_alloc_bytes (* new_cap 40) 8)))
        (seq (nl_ct_copy_entries old_ptr new_ptr 0 old_len)
             (if (> old_cap 0)
                 (nelisp_dealloc_bytes old_ptr (* old_cap 40) 8)
               1)
             (seq (ptr-write-u64 (+ box 64) 0 new_ptr)
                  (ptr-write-u64 (+ box 72) 0 new_cap))
             new_ptr)))
    (defun nl_char_table_get_raw (arg idx out)
      (let ((tag (nelisp_ptr_read_u8 arg 0)))
        (if (= tag 9)
            (let ((box (ptr-read-u64 (+ arg 8) 0)))
              (let ((entries_ptr (ptr-read-u64 (+ box 64) 0))
                    (entries_len (ptr-read-u64 (+ box 80) 0)))
                (nl_ct_get_search box entries_ptr entries_len idx out 0)))
          1)))
    (defun nl_ct_get_search (box eptr elen idx out i)
      (if (= i elen)
          (let ((parent_ptr (ptr-read-u64 (+ box 88) 0)))
            (if (= parent_ptr 0)
                (nl_sexp_clone_into out (+ box 32))
              (let ((tmp (alloc-bytes 32 8)))
                (seq (ptr-write-u64 tmp 0 9)
                     (ptr-write-u64 (+ tmp 8) 0 parent_ptr)
                     (seq (ptr-write-u64 (+ tmp 16) 0 0)
                          (ptr-write-u64 (+ tmp 24) 0 0))
                     (nl_char_table_get_raw tmp idx out)))))
        (let ((entry_base (+ eptr (* i 40))))
          (let ((key (ptr-read-u64 entry_base 0)))
            (if (= key idx)
                (nl_sexp_clone_into out (+ entry_base 8))
              (nl_ct_get_search box eptr elen idx out (+ i 1)))))))
    (defun nl_char_table_set_raw (arg idx val out)
      (let ((tag (nelisp_ptr_read_u8 arg 0)))
        (if (= tag 9)
            (let ((box (ptr-read-u64 (+ arg 8) 0)))
              (nl_ct_set_step box idx val out 0))
          1)))
    (defun nl_ct_set_step (box idx val out i)
      (let ((elen (ptr-read-u64 (+ box 80) 0)))
        (if (= i elen)
            (nl_ct_push_new box idx val out elen)
          (let ((eptr (ptr-read-u64 (+ box 64) 0))
                (entry_base (+ eptr (* i 40))))
            (let ((key (ptr-read-u64 entry_base 0)))
              (if (= key idx)
                  (seq (nl_sexp_clone_into (+ entry_base 8) val)
                       (nl_sexp_clone_into out val))
                (nl_ct_set_step box idx val out (+ i 1))))))))
    (defun nl_ct_push_new (box idx val out elen)
      (let ((eptr (ptr-read-u64 (+ box 64) 0))
            (ecap (ptr-read-u64 (+ box 72) 0)))
        (let ((new_eptr (if (= elen ecap)
                            (nl_ct_grow_entries box eptr ecap elen)
                          eptr)))
          (let ((new_entry (+ new_eptr (* elen 40))))
            (seq (ptr-write-u64 new_entry 0 idx)
                 (nl_sexp_clone_into (+ new_entry 8) val)
                 (ptr-write-u64 (+ box 80) 0 (+ elen 1))
                 (nl_sexp_clone_into out val)))))))
  "Doc 135 Stage Phase 47 source for nl_char_table_get_raw + nl_char_table_set_raw.

Multi-entry `(seq DEFUN ...)' manifest.

Lowered from packages/nelisp-sys/eval-port/nonenv-char-table.nl.

Public exports: nl_char_table_get_raw / nl_char_table_set_raw.
Net Rust delta: zero.  Resolves 2 undefined symbols.")

(provide 'nelisp-cc-evalport-nonenv-char-table)

;;; nelisp-cc-evalport-nonenv-char-table.el ends here
