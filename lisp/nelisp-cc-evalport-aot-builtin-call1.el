;;; nelisp-cc-evalport-aot-builtin-call1.el --- Phase 47 nelisp_aot_builtin_call1  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.E (M2) -- native provider for the Doc 129.6
;; nelisp_aot_builtin_call1(mirror, frames, name, arg, out, scratch) ABI.
;; Builds the (builtin NAME) sentinel + (ARG) list, forwards to the kept Rust
;; nelisp_apply_function (env-independent builtins; frames stands in for env).
;; Lowered from packages/nelisp-sys/eval-port/nonenv-aot-builtin-call1.nl via
;; nelisp-sys-backend-lower-module (x86_64-unknown-linux-gnu).

;;; Code:

(defconst nelisp-cc-evalport-aot-builtin-call1--source
  '(defun nelisp_aot_builtin_call1 (mirror frames name arg out scratch) (let ((nil_slot (alloc-bytes 32 8)) (builtin_sym (alloc-bytes 32 8)) (name_buf (alloc-bytes 8 1)) (args_list (alloc-bytes 32 8)) (inner (alloc-bytes 32 8)) (func (alloc-bytes 32 8))) (seq (ptr-write-u64 nil_slot 0 0) (ptr-write-u64 (+ nil_slot 8) 0 0) (ptr-write-u64 (+ nil_slot 16) 0 0) (ptr-write-u64 (+ nil_slot 24) 0 0) (ptr-write-u64 name_buf 0 31078196194145634) (nl_alloc_symbol name_buf 7 builtin_sym) (nelisp_cons_construct arg nil_slot args_list) (nelisp_cons_construct name nil_slot inner) (nelisp_cons_construct builtin_sym inner func) (nelisp_apply_function func args_list frames out) out)))
  "Doc 135 Phase 47 lowered source for nelisp_aot_builtin_call1.")

(provide 'nelisp-cc-evalport-aot-builtin-call1)

;;; nelisp-cc-evalport-aot-builtin-call1.el ends here
