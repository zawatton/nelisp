;;; nelisp-cc-evalport-combiner-arglist.el --- Phase 47 nl_eval_arg_list  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.D -- eval_arg_list combiner helper.
;;
;; Lowered from packages/nelisp-sys/eval-port/combiner-arglist.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;;
;; Public exports: nl_eval_arg_list.
;; Linux-x86_64 only.

;;; Code:

(defconst nelisp-cc-evalport-combiner-arglist--source
  '(seq (defun nl_write_nil_slot (slot) (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0) (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0)) (defun nl_eval_arg_list_walk (cur_ptr env_ptr acc_slot) (if (= (ptr-read-u64 cur_ptr 0) 7) (let ((car_ptr (nl_cons_car_ptr cur_ptr)) (cdr_ptr (nl_cons_cdr_ptr cur_ptr)) (eval_slot (alloc-bytes 32 8)) (rest_slot (alloc-bytes 32 8))) (let ((rc_eval (nelisp_eval_call car_ptr env_ptr eval_slot))) (if (= rc_eval 0) (let ((rc_rest (nl_eval_arg_list_walk cdr_ptr env_ptr rest_slot))) (if (= rc_rest 0) (nelisp_cons_construct eval_slot rest_slot acc_slot) 1)) 1))) (nl_write_nil_slot acc_slot))) (defun nl_eval_arg_list (args_ptr env out_list_slot) (let ((env_ptr env)) (nl_eval_arg_list_walk args_ptr env_ptr out_list_slot))))
  "Doc 135 Stage 135.D -- eval_arg_list combiner helper.

Lowered from packages/nelisp-sys/eval-port/combiner-arglist.nl.

Public exports: nl_eval_arg_list.
Net Rust delta: zero.")

(provide 'nelisp-cc-evalport-combiner-arglist)

;;; nelisp-cc-evalport-combiner-arglist.el ends here
