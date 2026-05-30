;;; nelisp-cc-evalport-combiner-entry.el --- Phase 47 nl_eval / nl_eval_ctx_make / nl_eval_in_ctx  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.D -- nl_eval + nl_eval_ctx_make.
;;
;; Lowered from packages/nelisp-sys/eval-port/combiner-entry.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;;
;; Public exports: nl_eval / nl_eval_ctx_make / nl_eval_in_ctx.
;; Linux-x86_64 only.

;;; Code:

(defconst nelisp-cc-evalport-combiner-entry--source
  '(seq (defun nl_entry_write_nil (slot) (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0) (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0)) (defun nl_entry_build_scratch_box (sym_slot_5 unbound_ptr) (let ((box_ptr (nl_alloc_vector 11)) (sym_buf (alloc-bytes 16 1))) (seq (seq (ptr-write-u64 sym_buf 0 7290602597431212403) (ptr-write-u64 (+ sym_buf 8) 0 2037544046) (nl_alloc_symbol sym_buf 12 sym_slot_5) (nl_vector_set_slot box_ptr 5 sym_slot_5)) box_ptr))) (defun nl_entry_stash_max_depth (env) (let ((err_buf (alloc-bytes 8 1)) (err_slot (alloc-bytes 32 8)) (depth_buf (alloc-bytes 24 1)) (depth_slot (alloc-bytes 32 8)) (nil_slot (alloc-bytes 32 8)) (inner_pair (alloc-bytes 32 8)) (signal_slot (alloc-bytes 32 8))) (seq (seq (ptr-write-u64 err_buf 0 547519316581) (nl_alloc_symbol err_buf 5 err_slot)) (seq (ptr-write-u64 depth_buf 0 8243116665881813101) (ptr-write-u64 (+ depth_buf 8) 0 4840666116958122080) (ptr-write-u64 (+ depth_buf 16) 0 1684) (nl_alloc_symbol depth_buf 20 depth_slot)) (nl_entry_write_nil nil_slot) (nelisp_cons_construct depth_slot nil_slot inner_pair) (nelisp_cons_construct err_slot inner_pair signal_slot) (let ((name_sym_slot (alloc-bytes 32 8)) (name_buf (alloc-bytes 24 1)) (mirror_ptr (+ env 0)) (frames_ptr (+ env 32)) (unbound_ptr (+ env 64)) (sym5_slot (alloc-bytes 32 8)) (vec_slot (alloc-bytes 32 8))) (seq (seq (ptr-write-u64 name_buf 0 3255381746650998126) (ptr-write-u64 (+ name_buf 8) 0 7451613697525637484) (ptr-write-u64 (+ name_buf 16) 0 7022344801864147310) (nl_alloc_symbol name_buf 24 name_sym_slot)) (let ((box_ptr (nl_entry_build_scratch_box sym5_slot unbound_ptr))) (let ((data_ptr (ptr-read-u64 (+ box_ptr 8) 0))) (seq (seq (nl_sexp_clone_into (+ data_ptr 224) signal_slot) (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr) (ptr-write-u64 vec_slot 0 8) (ptr-write-u64 (+ vec_slot 8) 0 box_ptr) (ptr-write-u64 (+ vec_slot 16) 0 0) (ptr-write-u64 (+ vec_slot 24) 0 0)) (nelisp_env_bind_local mirror_ptr frames_ptr name_sym_slot signal_slot vec_slot 0) 1)))))))) (defun nl_entry_stash_quit (env) (let ((quit_buf (alloc-bytes 8 1)) (quit_slot (alloc-bytes 32 8)) (nil_slot (alloc-bytes 32 8)) (signal_slot (alloc-bytes 32 8))) (seq (seq (ptr-write-u64 quit_buf 0 1953068401) (nl_alloc_symbol quit_buf 4 quit_slot)) (nl_entry_write_nil nil_slot) (nelisp_cons_construct quit_slot nil_slot signal_slot) (let ((name_sym_slot (alloc-bytes 32 8)) (name_buf (alloc-bytes 24 1)) (mirror_ptr (+ env 0)) (frames_ptr (+ env 32)) (unbound_ptr (+ env 64)) (sym5_slot (alloc-bytes 32 8)) (vec_slot (alloc-bytes 32 8))) (seq (seq (ptr-write-u64 name_buf 0 3255381746650998126) (ptr-write-u64 (+ name_buf 8) 0 7451613697525637484) (ptr-write-u64 (+ name_buf 16) 0 7022344801864147310) (nl_alloc_symbol name_buf 24 name_sym_slot)) (let ((box_ptr (nl_entry_build_scratch_box sym5_slot unbound_ptr))) (let ((data_ptr (ptr-read-u64 (+ box_ptr 8) 0))) (seq (seq (nl_sexp_clone_into (+ data_ptr 224) signal_slot) (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr) (ptr-write-u64 vec_slot 0 8) (ptr-write-u64 (+ vec_slot 8) 0 box_ptr) (ptr-write-u64 (+ vec_slot 16) 0 0) (ptr-write-u64 (+ vec_slot 24) 0 0)) (nelisp_env_bind_local mirror_ptr frames_ptr name_sym_slot signal_slot vec_slot 0) 1)))))))) (defun nl_eval (form_ptr env out) (let ((quit_flag_ptr (nl_quit_flag_ptr)) (ctx_base env) (env_ptr env)) (let ((quit_val (nelisp_tty_take_atomic quit_flag_ptr))) (if (not (= quit_val 0)) (nl_entry_stash_quit env) (let ((rec_cur_addr (+ ctx_base 96)) (rec_max_addr (+ ctx_base 104))) (let ((rec_cur (ptr-read-u64 rec_cur_addr 0)) (rec_max (ptr-read-u64 rec_max_addr 0))) (if (>= rec_cur rec_max) (nl_entry_stash_max_depth env) (ptr-write-u64 rec_cur_addr 0 (+ rec_cur 1))))))))) (defun nl_eval_ctx_make (globals_ptr frames_ptr_arg unbound_ptr rec_max out_ctx_slot) (let ((ctx (alloc-bytes 120 8))) (seq (nl_sexp_clone_into ctx globals_ptr) (nl_sexp_clone_into (+ ctx 32) frames_ptr_arg) (nl_sexp_clone_into (+ ctx 64) unbound_ptr) (ptr-write-u64 (+ ctx 96) 0 0) (ptr-write-u64 (+ ctx 104) 0 rec_max) (ptr-write-u64 (+ ctx 112) 0 0) (seq (ptr-write-u64 out_ctx_slot 0 2) (ptr-write-u64 (+ out_ctx_slot 8) 0 ctx) (ptr-write-u64 (+ out_ctx_slot 16) 0 0) (ptr-write-u64 (+ out_ctx_slot 24) 0 0)) 0))) (defun nl_eval_in_ctx (form_ptr env out) (nl_eval form_ptr env out)))
  "Doc 135 Stage 135.D -- nl_eval + nl_eval_ctx_make.

Lowered from packages/nelisp-sys/eval-port/combiner-entry.nl.

Public exports: nl_eval / nl_eval_ctx_make / nl_eval_in_ctx.
Net Rust delta: zero.")

(provide 'nelisp-cc-evalport-combiner-entry)

;;; nelisp-cc-evalport-combiner-entry.el ends here
