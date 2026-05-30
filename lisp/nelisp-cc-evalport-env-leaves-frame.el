;;; nelisp-cc-evalport-env-leaves-frame.el --- Phase 47 env-leaf frame-compose ctx-accessors  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.C — FRAME-COMPOSE env-leaf ctx-accessors.
;;
;; Lowered from packages/nelisp-sys/eval-port/env-leaves-frame.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;;
;; Exports 2 C-ABI symbols deleted by commit fa8932eb:
;;   nl_push_and_bind(formals_ptr, args_list_ptr, env) -> i64
;;   nl_env_push_captured(env, alist_ptr) -> i64
;;
;; Private helpers:
;;   nl_frame_scratch_write_lexframe_sym, nl_frame_scratch_write_hashsym,
;;   nl_frame_build_push_scratch, nl_frame_write_symentry,
;;   nl_frame_build_bind_scratch, nl_push_captured_walk
;;
;; Linux-x86_64 only — alloc-bytes / ptr-read-u64 / extern-call ABI.

;;; Code:

(defconst nelisp-cc-evalport-env-leaves-frame--source
  '(seq
    (defun nl_frame_scratch_write_lexframe_sym (sym_slot)
      (let ((buf (alloc-bytes 16 1)))
        (seq (ptr-write-u64 buf 0 7795010171040458094)
             (ptr-write-u64 (+ buf 8) 0 28549237946349669)
             (nl_alloc_symbol buf 15 sym_slot))))
    (defun nl_frame_scratch_write_hashsym (sym_slot)
      (let ((buf (alloc-bytes 16 1)))
        (seq (ptr-write-u64 buf 0 8314040931539181926)
             (ptr-write-u64 (+ buf 8) 0 28548142445374824)
             (nl_alloc_symbol buf 15 sym_slot))))
    (defun nl_frame_build_push_scratch (out_vec_sexp)
      (let ((box_ptr (nl_alloc_vector 7))
            (sym0_slot (alloc-bytes 32 8))
            (sym1_slot (alloc-bytes 32 8)))
        (seq (nl_frame_scratch_write_lexframe_sym sym0_slot)
             (nl_frame_scratch_write_hashsym sym1_slot)
             (seq (nl_vector_set_slot box_ptr 0 sym0_slot)
                  (nl_vector_set_slot box_ptr 1 sym1_slot)
                  (ptr-write-u64 out_vec_sexp 0 8)
                  (ptr-write-u64 (+ out_vec_sexp 8) 0 box_ptr)
                  (ptr-write-u64 (+ out_vec_sexp 16) 0 0)
                  (ptr-write-u64 (+ out_vec_sexp 24) 0 0)
                  out_vec_sexp))))
    (defun nl_frame_write_symentry (sym_slot)
      (let ((buf (alloc-bytes 16 1)))
        (seq (ptr-write-u64 buf 0 7290602597431212403)
             (ptr-write-u64 (+ buf 8) 0 2037544046)
             (nl_alloc_symbol buf 12 sym_slot))))
    (defun nl_frame_build_bind_scratch (val_ptr unbound_ptr out_sexp_vec_slot)
      (let ((box_ptr (nl_alloc_vector 11))
            (sym_slot (alloc-bytes 32 8)))
        (let ((data_ptr (ptr-read-u64 (+ box_ptr 8) 0)))
          (seq (nl_frame_write_symentry sym_slot)
               (seq (nl_vector_set_slot box_ptr 5 sym_slot)
                    (nl_sexp_clone_into (+ data_ptr 224) val_ptr)
                    (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr)
                    (ptr-write-u64 out_sexp_vec_slot 0 8)
                    (ptr-write-u64 (+ out_sexp_vec_slot 8) 0 box_ptr)
                    (ptr-write-u64 (+ out_sexp_vec_slot 16) 0 0)
                    (ptr-write-u64 (+ out_sexp_vec_slot 24) 0 0)
                    out_sexp_vec_slot)))))
    (defun nl_push_and_bind (formals_ptr args_list_ptr env)
      (let ((frames_ptr (+ env 32))
            (push_scratch (alloc-bytes 32 8)))
        (seq (nl_frame_build_push_scratch push_scratch)
             (nelisp_frame_push frames_ptr push_scratch)
             (let ((rc (nl_bind_formals_impl formals_ptr args_list_ptr env 0)))
               (if (= rc 0)
                   0
                 (let ((pop_scratch (alloc-bytes 32 8)))
                   (seq (seq (ptr-write-u64 pop_scratch 0 0)
                             (ptr-write-u64 (+ pop_scratch 8) 0 0)
                             (ptr-write-u64 (+ pop_scratch 16) 0 0)
                             (ptr-write-u64 (+ pop_scratch 24) 0 0)
                             (nelisp_frame_pop frames_ptr pop_scratch))
                        rc)))))))
    (defun nl_push_captured_walk (alist_ptr mirror_ptr frames_ptr unbound_ptr)
      (if (= (ptr-read-u64 alist_ptr 0) 7)
          (let ((pair_ptr (nl_cons_car_ptr alist_ptr))
                (rest_ptr (nl_cons_cdr_ptr alist_ptr)))
            (if (= (ptr-read-u64 pair_ptr 0) 7)
                (let ((name_ptr (nl_cons_car_ptr pair_ptr))
                      (val_ptr (nl_cons_cdr_ptr pair_ptr))
                      (bind_scratch (alloc-bytes 32 8)))
                  (seq (nl_frame_build_bind_scratch val_ptr unbound_ptr bind_scratch)
                       (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr val_ptr bind_scratch 0)
                       (nl_push_captured_walk rest_ptr mirror_ptr frames_ptr unbound_ptr)))
              (nl_push_captured_walk rest_ptr mirror_ptr frames_ptr unbound_ptr)))
        0))
    (defun nl_env_push_captured (env alist_ptr)
      (let ((mirror_ptr (+ env 0))
            (frames_ptr (+ env 32))
            (unbound_ptr (+ env 64))
            (push_scratch (alloc-bytes 32 8)))
        (seq (nl_frame_build_push_scratch push_scratch)
             (nelisp_frame_push frames_ptr push_scratch)
             (nl_push_captured_walk alist_ptr mirror_ptr frames_ptr unbound_ptr)))))
  "Doc 135 Stage 135.C Phase 47 source for frame-compose env-leaf ctx-accessors.

Multi-entry `(seq DEFUN ...)' manifest.

Lowered from packages/nelisp-sys/eval-port/env-leaves-frame.nl.

Public exports: nl_push_and_bind / nl_env_push_captured.
Net Rust delta: zero.  Resolves 2 undefined symbols.")

(provide 'nelisp-cc-evalport-env-leaves-frame)

;;; nelisp-cc-evalport-env-leaves-frame.el ends here
